// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm32

package runtime

import "internal/abi"

const WasmMemoryLimit = wasmMemoryLimit

var WasmMemoryEnd = wasmMemoryEnd
var WasmFuncName = fnName
var WasmSysAllocRangeEnd = sysAllocRangeEnd

func WasmSpanContains(base, limit, addr uintptr) bool {
	ms := AllocMSpan()
	s := (*mspan)(ms)
	s.startAddr = base
	s.limit = limit
	contains := s.contains(addr)
	FreeMSpan(ms)
	return contains
}

func WasmStackContains(lo, hi, addr uintptr) bool {
	return (stack{lo: lo, hi: hi}).contains(addr)
}

func WasmStackContainsSP(lo, hi, sp uintptr) bool {
	return (stack{lo: lo, hi: hi}).containsSP(sp)
}

func WasmLinearAllocTerminal(base, size uintptr) (first, second, next, mapped, end uintptr, exhausted bool) {
	var l linearAlloc
	l.init(base, size, false)
	half := size / 2
	first = uintptr(l.alloc(half, physPageSize, nil, ""))
	second = uintptr(l.alloc(size-half, 1, nil, ""))
	exhausted = l.alloc(1, 1, nil, "") == nil
	return first, second, l.next, l.mapped, l.end, exhausted
}

func WasmTypePointersAtEnd(base uintptr) (first, second, done uintptr) {
	typ := abi.TypeOf((*byte)(nil))
	tp := typePointers{elem: base, addr: base, mask: 1, typ: typ}
	tp, first = tp.nextFast()
	tp, second = tp.next(0)
	_, done = tp.next(0)
	return
}
