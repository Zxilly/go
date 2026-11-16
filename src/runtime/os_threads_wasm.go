// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm && wasm.threads

package runtime

// These helpers use raw WebAssembly signatures, not the Go ABI. They are
// called only by compiler-generated and assembly code.
func wasmMove()
func wasmZero()
func wasmLoad64()
func wasmStore64()
