// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package wasm

import (
	"cmd/internal/sys"
	"cmd/link/internal/ld"
)

// Init returns the architecture configuration for the wasm linker.
// is64Bit selects between wasm (true) and wasm32 (false).
func Init(is64Bit bool) (*sys.Arch, ld.Arch) {
	theArch := ld.Arch{
		Funcalign: 16,
		Maxalign:  32,
		Minalign:  1,

		Archinit:      archinit,
		AssignAddress: assignAddress,
		Asmb:          asmb,
		Asmb2:         asmb2,
		Gentext:       gentext,
	}

	if is64Bit {
		return sys.ArchWasm, theArch
	}
	return sys.ArchWasm32, theArch
}

func archinit(ctxt *ld.Link) {
	if *ld.FlagRound == -1 {
		*ld.FlagRound = 4096
	}
	if *ld.FlagTextAddr == -1 {
		// See asm.go:assignAddress for the meaning of PC on Wasm
		// and the value here.
		*ld.FlagTextAddr = pcBase
	}
}
