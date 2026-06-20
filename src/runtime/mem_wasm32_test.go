// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm32

package runtime_test

import (
	"runtime"
	"testing"
)

func TestWasmMemoryEnd(t *testing.T) {
	const page = uint64(64 << 10)
	if got, ok := runtime.WasmMemoryEnd(runtime.WasmMemoryLimit-page, uintptr(page)); !ok || got != runtime.WasmMemoryLimit {
		t.Fatalf("last page end = (%#x, %v), want (%#x, true)", got, ok, runtime.WasmMemoryLimit)
	}
	if _, ok := runtime.WasmMemoryEnd(runtime.WasmMemoryLimit-page, uintptr(page+1)); ok {
		t.Fatal("wasmMemoryEnd accepted a range past 4 GiB")
	}
	if _, ok := runtime.WasmMemoryEnd(runtime.WasmMemoryLimit, uintptr(page)); ok {
		t.Fatal("wasmMemoryEnd accepted an allocation starting at 4 GiB")
	}
	if _, ok := runtime.WasmMemoryEnd(0, ^uintptr(0)); ok {
		t.Fatal("wasmMemoryEnd accepted a request whose page-rounded size does not fit in uintptr")
	}
}

func TestPageAllocAddressSpaceEnd(t *testing.T) {
	chunkBytes := uintptr(runtime.PallocChunkPages) * runtime.PageSize
	base := ^uintptr(0) - chunkBytes + 1
	if end, ok := runtime.WasmSysAllocRangeEnd(base, chunkBytes); !ok || end != 0 {
		t.Fatalf("terminal sysAlloc range end = (%#x, %v), want (0, true)", end, ok)
	}
	if _, ok := runtime.WasmSysAllocRangeEnd(base, chunkBytes+1); ok {
		t.Fatal("sysAlloc accepted a range past the end of the address space")
	}
	r := runtime.MakeAddrRange(base, 0)
	if r.Size() != chunkBytes || !r.Contains(base) || !r.Contains(^uintptr(0)) || r.Contains(base-1) {
		t.Fatal("terminal address range has inconsistent size or containment")
	}
	front := r
	if got, ok := front.TakeFromFront(runtime.PageSize, 8); !ok || got != base || front.Size() != chunkBytes-runtime.PageSize {
		t.Fatalf("TakeFromFront = (%#x, %v), remaining %#x", got, ok, front.Size())
	}
	back := r
	wantBack := uintptr(0)
	wantBack -= runtime.PageSize
	if got, ok := back.TakeFromBack(runtime.PageSize, 8); !ok || got != wantBack || back.Size() != chunkBytes-runtime.PageSize {
		t.Fatalf("TakeFromBack = (%#x, %v), remaining %#x", got, ok, back.Size())
	}
	if got := r.RemoveGreaterEqual(base + runtime.PageSize); got.Size() != runtime.PageSize {
		t.Fatalf("trimmed terminal range size = %#x, want %#x", got.Size(), runtime.PageSize)
	}

	lastChunk := runtime.ChunkIdx(^uintptr(0) / chunkBytes)
	p := runtime.NewPageAlloc(map[runtime.ChunkIdx][]runtime.BitRange{lastChunk: nil}, nil)
	defer runtime.FreePageAlloc(p)

	start, end := p.Bounds()
	if start != lastChunk || end != lastChunk+1 {
		t.Fatalf("page allocator bounds = [%d, %d), want [%d, %d)", start, end, lastChunk, lastChunk+1)
	}
	ranges := p.InUse()
	if len(ranges) != 1 || ranges[0].Base() != runtime.PageBase(lastChunk, 0) || ranges[0].Limit() != 0 || ranges[0].Size() != chunkBytes {
		t.Fatalf("page allocator in-use ranges = %#v, want one terminal chunk", ranges)
	}

	if addr, _ := p.Alloc(runtime.PallocChunkPages); addr != runtime.PageBase(lastChunk, 0) {
		t.Fatalf("page allocator returned %#x, want %#x", addr, runtime.PageBase(lastChunk, 0))
	}
}

func TestRuntimeAddressSpaceEnd(t *testing.T) {
	base := ^uintptr(0) - runtime.PageSize + 1
	last := ^uintptr(0)

	if !runtime.WasmSpanContains(base, 0, base) || !runtime.WasmSpanContains(base, 0, last) {
		t.Fatal("terminal span does not contain its own address range")
	}
	if runtime.WasmSpanContains(base, 0, base-1) || runtime.WasmSpanContains(base, 0, 0) {
		t.Fatal("terminal span contains an address outside its range")
	}
	if runtime.WasmSpanContains(base, base, base) {
		t.Fatal("empty span contains its base")
	}

	if !runtime.WasmStackContains(base, 0, base) || !runtime.WasmStackContains(base, 0, last) {
		t.Fatal("terminal stack does not contain its own address range")
	}
	if runtime.WasmStackContains(base, 0, base-1) || runtime.WasmStackContains(base, 0, 0) {
		t.Fatal("terminal stack contains an address outside its range")
	}
	if !runtime.WasmStackContainsSP(base, 0, 0) {
		t.Fatal("terminal stack does not contain its initial stack pointer")
	}
	if runtime.WasmStackContainsSP(0, 0, 0) {
		t.Fatal("empty stack contains a stack pointer")
	}

	chunkBytes := uintptr(runtime.PallocChunkPages) * runtime.PageSize
	linearBase := ^uintptr(0) - chunkBytes + 1
	first, second, next, mapped, end, exhausted := runtime.WasmLinearAllocTerminal(linearBase, chunkBytes)
	if first != linearBase || second != linearBase+chunkBytes/2 {
		t.Fatalf("terminal linear allocations = (%#x, %#x), want (%#x, %#x)", first, second, linearBase, linearBase+chunkBytes/2)
	}
	if next != 0 || mapped != 0 || end != 0 || !exhausted {
		t.Fatalf("terminal linear allocator state = (next=%#x, mapped=%#x, end=%#x, exhausted=%v)", next, mapped, end, exhausted)
	}

	ptrBase := ^uintptr(0) - 2*runtime.PtrSize + 1
	ptr0, ptr1, done := runtime.WasmTypePointersAtEnd(ptrBase)
	if ptr0 != ptrBase || ptr1 != ptrBase+runtime.PtrSize || done != 0 {
		t.Fatalf("terminal type pointers = (%#x, %#x, %#x), want (%#x, %#x, 0)", ptr0, ptr1, done, ptrBase, ptrBase+runtime.PtrSize)
	}
}
