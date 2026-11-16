// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm.threads

#include "textflag.h"

// These helpers use the raw WebAssembly calling convention. Unlike ordinary
// i64 loads and stores, aligned atomic operations cannot tear a Go word or
// pointer. Unaligned accesses are used by byte merging and retain the ordinary
// instruction semantics. Neither helper can grow the stack or yield.

// (address i32, offset i32) -> i64; R2 is an i64 local.
TEXT runtime·wasmLoad64(SB),NOSPLIT,$0
	Get R0
	I64ExtendI32U
	Get R1
	I64ExtendI32U
	I64Add
	Set R2
	// WebAssembly memory offsets do not wrap at the end of memory32.
	Get R2
	I64Const $0xFFFFFFFF
	I64GtU
	If
		Unreachable
	End
	Get R2
	I64Const $7
	I64And
	I64Eqz
	If $2 // result i64
		Get R2
		I32WrapI64
		I64AtomicLoad $0
	Else
		Get R2
		I32WrapI64
		I64Load $0
	End
	Return

// (address i32, value i64, offset i32) -> (); R3 is an i64 local.
TEXT runtime·wasmStore64(SB),NOSPLIT,$0
	Get R0
	I64ExtendI32U
	Get R2
	I64ExtendI32U
	I64Add
	Set R3
	Get R3
	I64Const $0xFFFFFFFF
	I64GtU
	If
		Unreachable
	End
	Get R3
	I64Const $7
	I64And
	I64Eqz
	If
		Get R3
		I32WrapI64
		Get R1
		I64AtomicStore $0
	Else
		Get R3
		I32WrapI64
		Get R1
		I64Store $0
	End
	Return
