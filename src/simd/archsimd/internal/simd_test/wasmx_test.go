// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build goexperiment.simd && (wasm || wasm32)

package simd_test

import (
	"simd/archsimd"
	"testing"
)

var int8Sink int8

func TestLaneIndexBounds(t *testing.T) {
	var a [16]int8
	v := archsimd.LoadInt8x16Array(&a)
	t.Run("get", func(t *testing.T) {
		mustPanic(t, func() {
			int8Sink = v.GetElem(16)
		})
	})
	t.Run("set", func(t *testing.T) {
		mustPanic(t, func() {
			int8x16Sink = v.SetElem(16, 1)
		})
	})
}
