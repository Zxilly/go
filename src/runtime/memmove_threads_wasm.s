// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm.threads

#include "textflag.h"

// memory.copy does not guarantee indivisible pointer writes. When the
// addresses and length are word-aligned, copy each word atomically so a
// concurrent GC cannot observe a partially copied pointer.
TEXT runtime·memmove(SB),NOSPLIT,$0-24
	I32Load to+0(FP)
	I32Load from+8(FP)
	I32Load n+16(FP)
	Call runtime·wasmMove(SB)
	RET

// Raw WebAssembly ABI, shared with compiler-generated bulk copies.
TEXT runtime·wasmMove(SB),NOSPLIT,$0
	Get R0
	I64ExtendI32U
	Set R3
	Get R1
	I64ExtendI32U
	Set R4
	Get R2
	I64ExtendI32U
	Set R5
	Get R3
	Get R4
	I64Or
	Get R5
	I64Or
	I64Const $7
	I64And
	I64Eqz
	If
		Get R5
		I64Eqz
		If
			Return
		End
		Get R3
		Get R4
		I64LtU
		If
forward:
			Loop
				Get R3
				I32WrapI64
				Get R4
				I32WrapI64
				I64AtomicLoad $0
				I64AtomicStore $0
				Get R3
				I64Const $8
				I64Add
				Set R3
				Get R4
				I64Const $8
				I64Add
				Set R4
				Get R5
				I64Const $8
				I64Sub
				Tee R5
				I64Eqz
				I32Eqz
				BrIf forward
			End
		Else
			Get R3
			Get R5
			I64Add
			Set R3
			Get R4
			Get R5
			I64Add
			Set R4
backward:
			Loop
				Get R3
				I64Const $8
				I64Sub
				Tee R3
				I32WrapI64
				Get R4
				I64Const $8
				I64Sub
				Tee R4
				I32WrapI64
				I64AtomicLoad $0
				I64AtomicStore $0
				Get R5
				I64Const $8
				I64Sub
				Tee R5
				I64Eqz
				I32Eqz
				BrIf backward
			End
		End
	Else
		Get R3
		I32WrapI64
		Get R4
		I32WrapI64
		Get R5
		I32WrapI64
		MemoryCopy
	End
	Return
