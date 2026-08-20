// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm32

package runtime

import "unsafe"

func wasmFuncEntryAddr() uintptr
func wasmFuncCount() uintptr
func wasmGoexitHandle() uintptr

//go:linkname reflect_funcHandleToPC reflect.funcHandleToPC
func reflect_funcHandleToPC(handle uintptr) uintptr {
	return funcHandleToPC(handle)
}

//go:linkname reflect_funcPCToHandle reflect.funcPCToHandle
func reflect_funcPCToHandle(pc uintptr) uintptr {
	return funcPCToHandle(pc)
}

//go:nosplit
func wasmFuncPC(handle uintptr) uintptr {
	base := wasmFuncEntryAddr()
	return uintptr(*(*uint32)(unsafe.Pointer(base + handle*4)))
}

//go:nosplit
func funcHandleToPC(handle uintptr) uintptr {
	return wasmFuncPC(handle)
}

//go:nosplit
func funcPCToHandle(pc uintptr) uintptr {
	if pc == 0 {
		return 0
	}
	f := findfunc(pc)
	if !f.valid() {
		throw("invalid wasm32 function PC")
	}
	entry := f.entry()

	count := wasmFuncCount()
	base := wasmFuncEntryAddr()
	lo, hi := uintptr(1), count+1
	for lo < hi {
		mid := lo + (hi-lo)/2
		midPC := uintptr(*(*uint32)(unsafe.Pointer(base + mid*4)))
		if midPC < entry {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	if lo > count || uintptr(*(*uint32)(unsafe.Pointer(base + lo*4))) != entry {
		throw("missing wasm32 function handle")
	}
	return lo
}

// gobufSetPC sets a wasm32 continuation. gobuf.lr is the PC_F handle paired
// with gobuf.pc, not a link register. Assembly paths that copy an existing
// continuation copy both fields directly.
//
//go:nosplit
func gobufSetPC(buf *gobuf, pc uintptr) {
	buf.pc = pc
	buf.lr = funcPCToHandle(pc)
}

//go:nosplit
func gobufSetPCWithHandle(buf *gobuf, pc, handle uintptr) {
	if handle == 0 {
		throw("missing wasm32 function handle")
	}
	buf.pc = pc
	buf.lr = handle
}

// gobufTracebackLR hides wasm32's PC_F from the generic unwinder, which
// treats this field as a caller PC on link-register architectures.
//
//go:nosplit
func gobufTracebackLR(*gobuf) uintptr {
	return 0
}
