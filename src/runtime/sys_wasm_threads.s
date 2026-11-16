// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm && wasm.threads

#include "go_asm.h"
#include "textflag.h"

TEXT ·publicationBarrier(SB),NOSPLIT,$0-0
	AtomicFence
	RET

// func wasmWait(addr *uint32, val uint32, ns int64) uint32
TEXT runtime·wasmWait(SB),NOSPLIT,$0-28
	Get SP
	I32Load addr+0(FP)
	I32Load val+8(FP)
	I64Load ns+16(FP)
	MemoryAtomicWait32 $0
	I32Store ret+24(FP)
	RET

// func wasmNotify(addr *uint32, count uint32) uint32
TEXT runtime·wasmNotify(SB),NOSPLIT,$0-20
	Get SP
	I32Load addr+0(FP)
	I32Load count+8(FP)
	MemoryAtomicNotify $0
	I32Store ret+16(FP)
	RET

// Unwind to the host entry point before allowing this M's stack to be freed.
TEXT runtime·exitThread(SB),NOSPLIT,$0-8
	I32Const $1
	Set PAUSE
	RETUNWIND

// Called by the host, with tid in R0 (i32) and mp in R1 (i32).
// This function does not follow the Go ABI.
TEXT _rt0_wasm_thread(SB),NOSPLIT,$0
	Get R1
	I64Load $m_g0
	Set g
	Get R1
	Get R0
	I64ExtendI32U
	I64Store $m_procid

	MOVD (g_stack+stack_hi)(g), SP
	Get SP
	I32Const $16
	I32Sub
	Set SP
	I32Const $0
	Call runtime·mstart(SB)
	Drop
	Call wasm_pc_f_loop(SB)

	// No linear-stack access is permitted after this store: another M may
	// immediately recycle both the stack and the M structure.
	Get g
	I32WrapI64
	I64Load $g_m
	I32WrapI64
	I32Const $0
	I32AtomicStore $m_freeWait
	Return

// Keep the anchor's Go frame intact. A later host event can reuse the anchor
// instead of retaining one additional Go frame for every event.
TEXT runtime·wasmHostPause(SB),NOSPLIT,$0-0
	CALLNORESUME runtime·wasmHostSuspend(SB)
	Get g
	I32WrapI64
	I64Load $(g_stack+stack_hi)
	Get g
	I32WrapI64
	I64Load $g_m
	I32WrapI64
	I64Load $(m_mOS+mOS_wasmHost+wasmHostState_sp)
	I64Sub
	I32WrapI64
	Set SP
	I32Const $1
	Set PAUSE
	I32Const $1
	Return
