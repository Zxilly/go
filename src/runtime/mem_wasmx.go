// Copyright 2023 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm || wasm32

package runtime

import "unsafe"

const wasmMemoryLimit = uint64(1) << 32

func wasmMemoryEnd(bl uint64, n uintptr) (uint64, bool) {
	rounded, ok := alignUp64(uint64(n), uint64(physPageSize))
	if !ok || rounded > uint64(^uintptr(0)) {
		return 0, false
	}
	end := bl + rounded
	return end, end >= bl && end <= wasmMemoryLimit
}

func sbrk(n uintptr) unsafe.Pointer {
	bl := bloc
	end, ok := wasmMemoryEnd(bl, n)
	if !ok {
		return nil
	}
	if end > blocMax {
		grow := (end - blocMax) / uint64(physPageSize)
		if grow > 1<<31-1 {
			return nil
		}
		size := growMemory(int32(grow))
		if size < 0 {
			return nil
		}
		resetMemoryDataView()
		blocMax = end
	}
	bloc = end
	return unsafe.Pointer(uintptr(bl))
}

// Implemented in src/runtime/sys_wasm*.s.
func growMemory(pages int32) int32
func currentMemory() int32
