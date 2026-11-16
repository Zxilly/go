// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package wasm

import (
	"bytes"
	"encoding/binary"
	"internal/testenv"
	"os"
	"path/filepath"
	"testing"
)

// TestThreadModule checks the host ABI and data initialization contract of an
// actual linked executable. In particular, instantiating a worker must not run
// a start function or copy active data over another instance's memory.
func TestThreadModule(t *testing.T) {
	testenv.MustHaveGoBuild(t)
	if testing.Short() {
		t.Skip("cross-compiles a wasm executable")
	}
	dir := t.TempDir()
	src := filepath.Join(dir, "main.go")
	const source = `package main
//go:wasmexport answer
func answer() int32 { return 42 }
func main() { println(answer()) }
`
	if err := os.WriteFile(src, []byte(source), 0666); err != nil {
		t.Fatal(err)
	}
	for _, tc := range []struct{ goos, feature, mode string }{
		{"wasip1", "", "exe"},
		{"wasip1", "wasip1-threads", "exe"},
		{"wasip1", "wasip1-threads,stackswitch", "exe"},
		{"wasip1", "wasip1-threads", "c-shared"},
	} {
		t.Run(tc.goos+"/"+tc.feature+"/"+tc.mode, func(t *testing.T) {
			threads := tc.feature != ""
			out := filepath.Join(dir, "main.wasm")
			cmd := testenv.Command(t, testenv.GoToolPath(t), "build", "-buildmode="+tc.mode, "-o", out, src)
			cmd.Env = append(cmd.Environ(), "GOOS="+tc.goos, "GOARCH=wasm", "GOWASM="+tc.feature, "CGO_ENABLED=0", "GOFLAGS=")
			if b, err := cmd.CombinedOutput(); err != nil {
				t.Fatalf("build: %v\n%s", err, b)
			}
			b, err := os.ReadFile(out)
			if err != nil {
				t.Fatal(err)
			}
			if len(b) < 8 || !bytes.Equal(b[:8], []byte{0, 'a', 's', 'm', 1, 0, 0, 0}) {
				t.Fatal("bad wasm header")
			}
			sections := make(map[byte]*bytes.Reader)
			var threadInfo []byte
			r := bytes.NewReader(b[8:])
			for r.Len() != 0 {
				id, _ := r.ReadByte()
				n := readULEB(t, r)
				if n > uint64(r.Len()) {
					t.Fatal("truncated section")
				}
				data := make([]byte, n)
				r.Read(data)
				section := bytes.NewReader(data)
				if id == sectionCustom {
					if readString(t, section) == "go:threads" {
						threadInfo = data[len(data)-section.Len():]
					}
				} else {
					sections[id] = section
				}
			}
			if sections[sectionStart] != nil {
				t.Fatal("instantiation must not execute Go initialization")
			}
			if (sections[sectionMemory] == nil) != threads {
				t.Fatal("thread memory must be imported; default memory must be defined")
			}
			imports := sections[sectionImport]
			shared := false
			var memoryMin, memoryMax uint64
			for n := readULEB(t, imports); n > 0; n-- {
				module, name := readString(t, imports), readString(t, imports)
				kind, _ := imports.ReadByte()
				switch kind {
				case 0:
					readULEB(t, imports)
				case 2:
					if module != "env" || name != "memory" || readULEB(t, imports) != 3 {
						t.Fatal("expected shared env.memory import with maximum")
					}
					memoryMin, memoryMax = readULEB(t, imports), readULEB(t, imports)
					if memoryMin == 0 || memoryMin > memoryMax || memoryMax != 65536 {
						t.Fatalf("invalid shared memory limits %d, %d", memoryMin, memoryMax)
					}
					shared = true
				default:
					t.Fatalf("unexpected import kind %d", kind)
				}
			}
			if shared != threads {
				t.Fatal("unexpected shared memory selection")
			}
			exports := sections[sectionExport]
			threadStart := false
			threadEntry := "wasi_thread_start"
			mainEntry := "_start"
			if tc.goos == "js" {
				threadEntry, mainEntry = "go_thread_start", "run"
			} else if tc.mode == "c-shared" {
				mainEntry = "_initialize"
			}
			mainStart := false
			answer := false
			for n := readULEB(t, exports); n > 0; n-- {
				name := readString(t, exports)
				kind, _ := exports.ReadByte()
				readULEB(t, exports)
				threadStart = threadStart || name == threadEntry && kind == 0
				mainStart = mainStart || name == mainEntry && kind == 0
				answer = answer || name == "answer" && kind == 0
			}
			if !mainStart {
				t.Fatalf("missing %s export", mainEntry)
			}
			if !answer {
				t.Fatal("missing go:wasmexport function")
			}
			if threadStart != threads {
				t.Fatalf("unexpected %s export", threadEntry)
			}
			if !threads && threadInfo != nil {
				t.Fatal("unexpected go:threads custom section")
			}
			if threads {
				if len(threadInfo) != 12 || binary.LittleEndian.Uint32(threadInfo[:4]) != 1 {
					t.Fatalf("invalid go:threads custom section: % x", threadInfo)
				}
				if min, max := binary.LittleEndian.Uint32(threadInfo[4:8]), binary.LittleEndian.Uint32(threadInfo[8:]); uint64(min) != memoryMin || uint64(max) != memoryMax {
					t.Fatalf("go:threads memory limits %d, %d do not match import %d, %d", min, max, memoryMin, memoryMax)
				}
				data := sections[sectionData]
				n := readULEB(t, data)
				if n == 0 || n != readULEB(t, sections[sectionDataCount]) {
					t.Fatal("data count does not match passive segments")
				}
				for ; n > 0; n-- {
					if readULEB(t, data) != 1 {
						t.Fatal("thread module contains active data")
					}
					size := readULEB(t, data)
					if size > uint64(data.Len()) {
						t.Fatal("truncated data segment")
					}
					data.Seek(int64(size), 1)
				}
			}
		})
	}
}

func readULEB(t *testing.T, r *bytes.Reader) uint64 {
	t.Helper()
	if r == nil {
		t.Fatal("missing section")
	}
	n, err := binary.ReadUvarint(r)
	if err != nil {
		t.Fatal(err)
	}
	return n
}

func readString(t *testing.T, r *bytes.Reader) string {
	t.Helper()
	n := readULEB(t, r)
	if n > uint64(r.Len()) {
		t.Fatal("truncated name")
	}
	b := make([]byte, n)
	r.Read(b)
	return string(b)
}
