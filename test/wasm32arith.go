// run

// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm32

package main

import "unsafe"

//go:noinline
func indexAfterAdd(a []int, x uint) int {
	return a[x+1]
}

//go:noinline
func indexAfterMul(a []int, x uint) int {
	return a[x*2]
}

//go:noinline
func indexAfterSignedAdd(a []int, x, y int) int {
	return a[x+y]
}

//go:noinline
func indexAfterShift(a []int, x uint) int {
	return a[x<<1]
}

//go:noinline
func indexAfterComplement(a []int, x uint) int {
	return a[^x]
}

//go:noinline
func pointerAfterAdd(x uintptr) unsafe.Pointer {
	return unsafe.Pointer(x + 1)
}

//go:noinline
func dereferenceAfterAdd(x uintptr) byte {
	return *(*byte)(unsafe.Pointer(x + 1))
}

func mustPanic(f func()) {
	defer func() {
		if recover() == nil {
			panic("wrapped nil dereference did not panic")
		}
	}()
	f()
}

func main() {
	a := []int{42}
	if indexAfterAdd(a, ^uint(0)) != 42 {
		panic("MaxUint+1 did not wrap")
	}
	if indexAfterMul(a, uint(1)<<31) != 42 {
		panic("uint multiplication did not wrap")
	}
	minInt := -int(^uint(0)>>1) - 1
	if indexAfterSignedAdd(a, minInt, minInt) != 42 {
		panic("signed addition did not wrap")
	}
	if indexAfterShift(a, uint(1)<<31) != 42 {
		panic("left shift did not wrap")
	}
	if indexAfterComplement(a, ^uint(0)) != 42 {
		panic("complement did not wrap")
	}
	if pointerAfterAdd(^uintptr(0)) != nil {
		panic("pointer arithmetic did not wrap")
	}
	mustPanic(func() {
		dereferenceAfterAdd(^uintptr(0))
	})
}
