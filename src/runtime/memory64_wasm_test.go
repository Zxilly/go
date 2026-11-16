// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm.threads

package runtime_test

import (
	"encoding/binary"
	"math"
	"testing"
	"unsafe"
)

//go:noinline
func wasmLoadWord(p unsafe.Pointer) uint64 {
	return *(*uint64)(p)
}

//go:noinline
func wasmStoreWord(p unsafe.Pointer, v uint64) {
	*(*uint64)(p) = v
}

//go:noinline
func wasmLoadFloat(p unsafe.Pointer) float64 {
	return *(*float64)(p)
}

//go:noinline
func wasmStoreFloat(p unsafe.Pointer, v float64) {
	*(*float64)(p) = v
}

func TestWasmMemory64Alignment(t *testing.T) {
	var words [3]uint64
	b := unsafe.Slice((*byte)(unsafe.Pointer(&words[0])), 24)
	for off := 0; off < 8; off++ {
		p := unsafe.Pointer(&b[off])
		const word = uint64(0xFEDCBA9876543210)
		wasmStoreWord(p, word)
		if got := binary.LittleEndian.Uint64(b[off:]); got != word {
			t.Fatalf("word store at offset %d: %#x", off, got)
		}
		binary.LittleEndian.PutUint64(b[off:], ^word)
		if got := wasmLoadWord(p); got != ^word {
			t.Fatalf("word load at offset %d: %#x", off, got)
		}
		for _, bits := range []uint64{0x8000000000000000, 0x7FF8000000000123, 0x3FF123456789ABCD} {
			wasmStoreFloat(p, math.Float64frombits(bits))
			if got := binary.LittleEndian.Uint64(b[off:]); got != bits {
				t.Fatalf("float store at offset %d: %#x, want %#x", off, got, bits)
			}
			if got := math.Float64bits(wasmLoadFloat(p)); got != bits {
				t.Fatalf("float load at offset %d: %#x, want %#x", off, got, bits)
			}
		}
	}
}
