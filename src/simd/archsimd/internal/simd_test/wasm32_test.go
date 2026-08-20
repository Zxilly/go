// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build goexperiment.simd && wasm32

package simd_test

import (
	"simd/archsimd"
	"testing"
	"unsafe"
)

func TestUint32LanePointer(t *testing.T) {
	a := [4]uint32{1 << 31}
	v := archsimd.LoadUint32x4Array(&a)
	got := unsafe.Pointer(uintptr(v.GetElem(0)))
	want := unsafe.Pointer(uintptr(1) << 31)
	if got != want {
		t.Errorf("pointer from uint32 lane = %p, want %p", got, want)
	}
}
