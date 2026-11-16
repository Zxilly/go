// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm.threads

#include "textflag.h"

// memory.fill can tear pointer writes. Clear word-aligned regions using
// atomic stores, preserving the contract of memclrNoHeapPointers.
TEXT runtime·memclrNoHeapPointers(SB),NOSPLIT,$0-16
	I32Load ptr+0(FP)
	I32Load n+8(FP)
	Call runtime·wasmZero(SB)
	RET

// Raw WebAssembly ABI, shared with compiler-generated bulk clears.
TEXT runtime·wasmZero(SB),NOSPLIT,$0
	Get R0
	I64ExtendI32U
	Set R3
	Get R1
	I64ExtendI32U
	Set R4
	Get R3
	Get R4
	I64Or
	I64Const $7
	I64And
	I64Eqz
	If
		Get R4
		I64Eqz
		If
			Return
		End
clear:
		Loop
			Get R3
			I32WrapI64
			I64Const $0
			I64AtomicStore $0
			Get R3
			I64Const $8
			I64Add
			Set R3
			Get R4
			I64Const $8
			I64Sub
			Tee R4
			I64Eqz
			I32Eqz
			BrIf clear
		End
	Else
		Get R3
		I32WrapI64
		I32Const $0
		Get R4
		I32WrapI64
		MemoryFill
	End
	Return
