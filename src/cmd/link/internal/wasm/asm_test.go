// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package wasm

import (
	"cmd/internal/sys"
	"cmd/link/internal/ld"
	"slices"
	"testing"
)

func TestPCBase(t *testing.T) {
	if got := pcBase(sys.ArchWasm32); got != ld.WasmMinDataAddr {
		t.Fatalf("pcBase(wasm32) = %#x, want %#x", got, ld.WasmMinDataAddr)
	}
	if got := pcBase(sys.ArchWasm); got != legacyWasmPCBase {
		t.Fatalf("pcBase(wasm) = %#x, want %#x", got, int64(legacyWasmPCBase))
	}
}

func TestNextFuncAddress(t *testing.T) {
	const pc = uint64(0x3000)

	if got, ok := nextFuncAddress(sys.ArchWasm32, pc, 7); !ok || got != pc+7 {
		t.Fatalf("nextFuncAddress(wasm32) = (%#x, %v), want (%#x, true)", got, ok, pc+7)
	}
	if got, ok := nextFuncAddress(sys.ArchWasm, 1<<63, 7); !ok || got != 1<<63+1<<16 {
		t.Fatalf("nextFuncAddress(wasm) = (%#x, %v), want (%#x, true)", got, ok, uint64(1<<63+1<<16))
	}
	if _, ok := nextFuncAddress(sys.ArchWasm32, 1<<32-4, 7); ok {
		t.Fatal("nextFuncAddress accepted a wasm32 function whose PC tokens wrap")
	}
}

func TestTableEntries(t *testing.T) {
	if got, err := tableEntries(sys.ArchWasm, make([]uint32, 3)); err != nil {
		t.Fatal(err)
	} else if want := []uint32{0, 1, 2}; !slices.Equal(got, want) {
		t.Fatalf("tableEntries(wasm) = %v, want %v", got, want)
	}

	// The final function order may differ from the order in which handles
	// were assigned. Table slots must continue to follow the handles.
	if got, err := tableEntries(sys.ArchWasm32, []uint32{3, 1, 2}); err != nil {
		t.Fatal(err)
	} else if want := []uint32{1, 2, 0}; !slices.Equal(got, want) {
		t.Fatalf("tableEntries(wasm32) = %v, want %v", got, want)
	}
	if _, err := tableEntries(sys.ArchWasm32, []uint32{1, 1}); err == nil {
		t.Fatal("tableEntries accepted duplicate wasm32 handles")
	}
	if got := indirectTableOffset(sys.ArchWasm32); got != 1 {
		t.Fatalf("indirectTableOffset(wasm32) = %d, want 1", got)
	}
}

func TestWasmPCRelocValue(t *testing.T) {
	const (
		entry  = int64(0x345678)
		pcB    = int64(0x1234)
		handle = uint32(77)
	)

	if got, want := wasmPCRelocValue(sys.ArchWasm32, entry, pcB, 8, handle), int64(uint64(handle)<<32|uint64(entry+pcB)); got != want {
		t.Fatalf("wasmPCRelocValue(wasm32) = %#x, want %#x", got, want)
	}
	if got, want := wasmRelocValue(entry, pcB, 8), entry+pcB; got != want {
		t.Fatalf("wasmRelocValue() = %#x, want %#x", got, want)
	}
	if got, want := wasmPCRelocValue(sys.ArchWasm, legacyWasmPCBase+entry, pcB, 8, 0), legacyWasmPCBase+entry+pcB; got != want {
		t.Fatalf("wasmPCRelocValue(wasm) = %#x, want %#x", got, want)
	}
}
