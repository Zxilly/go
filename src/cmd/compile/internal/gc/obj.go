// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"cmd/compile/internal/base"
	"cmd/compile/internal/ir"
	"cmd/compile/internal/noder"
	"cmd/compile/internal/objw"
	"cmd/compile/internal/pkginit"
	"cmd/compile/internal/reflectdata"
	"cmd/compile/internal/staticdata"
	"cmd/compile/internal/typecheck"
	"cmd/compile/internal/types"
	"cmd/internal/archive"
	"cmd/internal/bio"
	"cmd/internal/obj"
	"cmd/internal/objabi"
	"cmd/internal/dwarf" // Import dwarf package
	"encoding/json"
	"fmt"
	"strings"
)

// These modes say which kind of object file to generate.
// The default use of the toolchain is to set both bits,
// generating a combined compiler+linker object, one that
// serves to describe the package to both the compiler and the linker.
// In fact the compiler and linker read nearly disjoint sections of
// that file, though, so in a distributed build setting it can be more
// efficient to split the output into two files, supplying the compiler
// object only to future compilations and the linker object only to
// future links.
//
// By default a combined object is written, but if -linkobj is specified
// on the command line then the default -o output is a compiler object
// and the -linkobj output is a linker object.
const (
	modeCompilerObj = 1 << iota
	modeLinkerObj
)

func dumpobj() {
	if base.Flag.LinkObj == "" {
		dumpobj1(base.Flag.LowerO, modeCompilerObj|modeLinkerObj)
		return
	}
	dumpobj1(base.Flag.LowerO, modeCompilerObj)
	dumpobj1(base.Flag.LinkObj, modeLinkerObj)
}

func dumpobj1(outfile string, mode int) {
	bout, err := bio.Create(outfile)
	if err != nil {
		base.FlushErrors()
		fmt.Printf("can't create %s: %v\n", outfile, err)
		base.ErrorExit()
	}

	bout.WriteString("!<arch>\n")

	if mode&modeCompilerObj != 0 {
		start := startArchiveEntry(bout)
		dumpCompilerObj(bout)
		finishArchiveEntry(bout, start, "__.PKGDEF")
	}
	if mode&modeLinkerObj != 0 {
		start := startArchiveEntry(bout)
		dumpLinkerObj(bout)
		finishArchiveEntry(bout, start, "_go_.o")
	}

	if err := bout.Close(); err != nil {
		base.FlushErrors()
		fmt.Printf("error while writing to file %s: %v\n", outfile, err)
		base.ErrorExit()
	}
}

func printObjHeader(bout *bio.Writer) {
	bout.WriteString(objabi.HeaderString())
	if base.Flag.BuildID != "" {
		fmt.Fprintf(bout, "build id %q\n", base.Flag.BuildID)
	}
	if types.LocalPkg.Name == "main" {
		fmt.Fprintf(bout, "main\n")
	}
	fmt.Fprintf(bout, "\n") // header ends with blank line
}

func startArchiveEntry(bout *bio.Writer) int64 {
	var arhdr [archive.HeaderSize]byte
	bout.Write(arhdr[:])
	return bout.Offset()
}

func finishArchiveEntry(bout *bio.Writer, start int64, name string) {
	bout.Flush()
	size := bout.Offset() - start
	if size&1 != 0 {
		bout.WriteByte(0)
	}
	bout.MustSeek(start-archive.HeaderSize, 0)

	var arhdr [archive.HeaderSize]byte
	archive.FormatHeader(arhdr[:], name, size)
	bout.Write(arhdr[:])
	bout.Flush()
	bout.MustSeek(start+size+(size&1), 0)
}

func dumpCompilerObj(bout *bio.Writer) {
	printObjHeader(bout)
	noder.WriteExports(bout)
}

func dumpdata() {
	reflectdata.WriteGCSymbols()
	reflectdata.WritePluginTable()
	dumpembeds()

	if reflectdata.ZeroSize > 0 {
		zero := base.PkgLinksym("go:map", "zero", obj.ABI0)
		objw.Global(zero, int32(reflectdata.ZeroSize), obj.DUPOK|obj.RODATA)
		zero.Set(obj.AttrStatic, true)
	}

	staticdata.WriteFuncSyms()
	addGCLocals()
}

func dumpLinkerObj(bout *bio.Writer) {
	printObjHeader(bout)

	if len(typecheck.Target.CgoPragmas) != 0 {
		// write empty export section; must be before cgo section
		fmt.Fprintf(bout, "\n$$\n\n$$\n\n")
		fmt.Fprintf(bout, "\n$$  // cgo\n")
		if err := json.NewEncoder(bout).Encode(typecheck.Target.CgoPragmas); err != nil {
			base.Fatalf("serializing pragcgobuf: %v", err)
		}
		fmt.Fprintf(bout, "\n$$\n\n")
	}

	fmt.Fprintf(bout, "\n!\n")

	obj.WriteObjFile(base.Ctxt, bout)
}

func dumpGlobal(n *ir.Name) {
	if n.Type() == nil {
		base.Fatalf("external %v nil type\n", n)
	}
	if n.Class == ir.PFUNC {
		return
	}
	if n.Sym().Pkg != types.LocalPkg {
		return
	}
	types.CalcSize(n.Type())
	ggloblnod(n)
	if n.CoverageAuxVar() || n.Linksym().Static() {
		return
	}
	base.Ctxt.DwarfGlobal(types.TypeSymName(n.Type()), n.Linksym())
}

// dwarfStringConstTypeSym holds the DWARF symbol for the "const string" type.
// It's initialized once per compilation unit.
var dwarfStringConstTypeSym *obj.LSym

// getOrInitDwarfStringConstTypeSym ensures the DWARF "const string" type symbol is created and initialized once.
func getOrInitDwarfStringConstTypeSym() *obj.LSym {
	if dwarfStringConstTypeSym == nil {
		s := base.Ctxt.Lookup(dwarf.InfoPrefix + ".string_const_type")
		s.Set(obj.AttrReadOnly, true)
		s.Set(obj.AttrLocal, true) // It's an auxiliary type, not meant for direct linking.
		s.Type = objabi.SDWARFINFO // Mark as a DWARF informational entry.
		// base.Ctxt provides the dwarf.Context. PutStringConstType defines the DIE.
		dwarf.PutStringConstType(base.Ctxt, s, s, "const string")
		dwarfStringConstTypeSym = s
	}
	return dwarfStringConstTypeSym
}

// createDwarfConstSymAndPut creates a DWARF symbol for a constant value, sets its basic attributes,
// and then calls populateDieFn to fill in the DWARF information.
func createDwarfConstSymAndPut(dwarfConstantSymName string, populateDieFn func(createdSym *obj.LSym)) {
	dwarfSym := base.Ctxt.Lookup(dwarfConstantSymName)
	dwarfSym.Set(obj.AttrReadOnly, true)
	dwarfSym.Set(obj.AttrLocal, true) // DWARF constants are generally not linked against by name.
	dwarfSym.Type = objabi.SDWARFCONST // Mark as a DWARF constant value entry.
	populateDieFn(dwarfSym)
}

func dumpGlobalConst(n *ir.Name) {
	// only export typed constants
	t := n.Type()
	if t == nil {
		return
	}
	if n.Sym().Pkg != types.LocalPkg {
		return
	}

	// Initialize string const type symbol (no-op if already done)
	strTypeSym := getOrInitDwarfStringConstTypeSym()

	constName := n.Sym().Name // Common for all constant types

	if t.IsInteger() || t.IsBoolean() {
		// Handle integer and boolean constants.
		// Booleans are typically represented as integers in DWARF (e.g., 0 or 1).
		v := n.Val()
		dtype := t // Use the original type for value interpretation and type name.
		if t.IsUntyped() {
			// Default untyped integers to 'int', untyped booleans to 'bool'.
			if t.IsBoolean() {
				dtype = types.Types[types.TBOOL]
			} else {
				dtype = types.Types[types.TINT]
			}
			// Check for overflow only if it's not a boolean (booleans won't overflow standard types).
			if !t.IsBoolean() && ir.ConstOverflow(v, dtype) {
				return
			}
		} else {
			// For typed constants, including named boolean types, ensure their type is resolvable.
			// If the type of the constant is an instantiated generic, we need to emit
			// that type so the linker knows about it. See issue 51245.
			_ = reflectdata.TypeLinksym(dtype) // Ensures type symbol is created if generic.
		}

		// The DWARF type symbol for "int", "bool", etc.
		actualTypeSym := base.Ctxt.Lookup(dwarf.InfoPrefix + types.TypeSymName(dtype))
		val := ir.IntVal(dtype, v)

		createDwarfConstSymAndPut(dwarf.ConstInfoPrefix+constName, func(newDwarfSym *obj.LSym) {
			dwarf.PutIntConst(base.Ctxt, newDwarfSym, actualTypeSym, constName, val)
		})

	} else if t.IsString() {
		// Handle string constants.
		if n.Val() == nil || n.Val().U == nil { // Ensure Val and Val.U are not nil
			base.Errorf("dumpGlobalConst: string constant %v has nil Val or Val.U", n)
			return
		}
		strVal, ok := n.Val().U.(string)
		if !ok {
			base.Errorf("dumpGlobalConst: expected string value for %v, got %T", n, n.Val().U)
			return
		}

		createDwarfConstSymAndPut(dwarf.ConstInfoPrefix+constName, func(newDwarfSym *obj.LSym) {
			dwarf.PutStringConst(base.Ctxt, newDwarfSym, strTypeSym, constName, strVal)
		})
	}
	// Other types of constants (e.g., floats, complex) are not currently handled for DWARF.
}

// addGCLocals adds gcargs, gclocals, gcregs, and stack object symbols to Ctxt.Data.
//
// This is done during the sequential phase after compilation, since
// global symbols can't be declared during parallel compilation.
func addGCLocals() {
	for _, s := range base.Ctxt.Text {
		fn := s.Func()
		if fn == nil {
			continue
		}
		for _, gcsym := range []*obj.LSym{fn.GCArgs, fn.GCLocals} {
			if gcsym != nil && !gcsym.OnList() {
				objw.Global(gcsym, int32(len(gcsym.P)), obj.RODATA|obj.DUPOK)
			}
		}
		if x := fn.StackObjects; x != nil {
			objw.Global(x, int32(len(x.P)), obj.RODATA)
			x.Set(obj.AttrStatic, true)
		}
		if x := fn.OpenCodedDeferInfo; x != nil {
			objw.Global(x, int32(len(x.P)), obj.RODATA|obj.DUPOK)
		}
		if x := fn.ArgInfo; x != nil {
			objw.Global(x, int32(len(x.P)), obj.RODATA|obj.DUPOK)
			x.Set(obj.AttrStatic, true)
		}
		if x := fn.ArgLiveInfo; x != nil {
			objw.Global(x, int32(len(x.P)), obj.RODATA|obj.DUPOK)
			x.Set(obj.AttrStatic, true)
		}
		if x := fn.WrapInfo; x != nil && !x.OnList() {
			objw.Global(x, int32(len(x.P)), obj.RODATA|obj.DUPOK)
			x.Set(obj.AttrStatic, true)
		}
		for _, jt := range fn.JumpTables {
			objw.Global(jt.Sym, int32(len(jt.Targets)*base.Ctxt.Arch.PtrSize), obj.RODATA)
		}
	}
}

func ggloblnod(nam *ir.Name) {
	s := nam.Linksym()

	// main_inittask and runtime_inittask in package runtime (and in
	// test/initempty.go) aren't real variable declarations, but
	// linknamed variables pointing to the compiler's generated
	// .inittask symbol. The real symbol was already written out in
	// pkginit.Task, so we need to avoid writing them out a second time
	// here, otherwise base.Ctxt.Globl will fail.
	if strings.HasSuffix(s.Name, "..inittask") && s.OnList() {
		return
	}

	s.Gotype = reflectdata.TypeLinksym(nam.Type())
	flags := 0
	if nam.Readonly() {
		flags = obj.RODATA
	}
	if nam.Type() != nil && !nam.Type().HasPointers() {
		flags |= obj.NOPTR
	}
	size := nam.Type().Size()
	linkname := nam.Sym().Linkname
	name := nam.Sym().Name

	var saveType objabi.SymKind
	if nam.CoverageAuxVar() {
		saveType = s.Type
	}

	// We've skipped linkname'd globals's instrument, so we can skip them here as well.
	if base.Flag.ASan && linkname == "" && pkginit.InstrumentGlobalsMap[name] != nil {
		// Write the new size of instrumented global variables that have
		// trailing redzones into object file.
		rzSize := pkginit.GetRedzoneSizeForGlobal(size)
		sizeWithRZ := rzSize + size
		base.Ctxt.Globl(s, sizeWithRZ, flags)
	} else {
		base.Ctxt.Globl(s, size, flags)
	}
	if nam.Libfuzzer8BitCounter() {
		s.Type = objabi.SLIBFUZZER_8BIT_COUNTER
	}
	if nam.CoverageAuxVar() && saveType == objabi.SCOVERAGE_COUNTER {
		// restore specialized counter type (which Globl call above overwrote)
		s.Type = saveType
	}
	if nam.Sym().Linkname != "" {
		// Make sure linkname'd symbol is non-package. When a symbol is
		// both imported and linkname'd, s.Pkg may not set to "_" in
		// types.Sym.Linksym because LSym already exists. Set it here.
		s.Pkg = "_"
	}
}

func dumpembeds() {
	for _, v := range typecheck.Target.Embeds {
		staticdata.WriteEmbed(v)
	}
}
