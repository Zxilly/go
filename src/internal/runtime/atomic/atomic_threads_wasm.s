// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm.threads

#include "textflag.h"

TEXT ·Load8(SB), NOSPLIT, $0-9
	I32Load ptr+0(FP)
	I32AtomicLoad8U $0
	I64ExtendI32U
	Set R0
	MOVB R0, ret+8(FP)
	RET

TEXT ·Load(SB), NOSPLIT, $0-12
	I32Load ptr+0(FP)
	I32AtomicLoad $0
	I64ExtendI32U
	Set R0
	MOVW R0, ret+8(FP)
	RET

TEXT ·Load64(SB), NOSPLIT, $0-16
	I32Load ptr+0(FP)
	I64AtomicLoad $0
	Set R0
	MOVD R0, ret+8(FP)
	RET

TEXT ·Store8(SB), NOSPLIT, $0-9
	I32Load ptr+0(FP)
	I32Load8U val+8(FP)
	I32AtomicStore8 $0
	RET

TEXT ·Store(SB), NOSPLIT, $0-12
	I32Load ptr+0(FP)
	I32Load val+8(FP)
	I32AtomicStore $0
	RET

TEXT ·Store64(SB), NOSPLIT, $0-16
	I32Load ptr+0(FP)
	I64Load val+8(FP)
	I64AtomicStore $0
	RET

TEXT ·Cas(SB), NOSPLIT, $0-17
	I32Load ptr+0(FP)
	I32Load old+8(FP)
	I32Load new+12(FP)
	I32AtomicRmwCmpxchg $0
	I32Load old+8(FP)
	I32Eq
	I64ExtendI32U
	Set R0
	MOVB R0, ret+16(FP)
	RET

TEXT ·Cas64(SB), NOSPLIT, $0-25
	I32Load ptr+0(FP)
	I64Load old+8(FP)
	I64Load new+16(FP)
	I64AtomicRmwCmpxchg $0
	I64Load old+8(FP)
	I64Eq
	I64ExtendI32U
	Set R0
	MOVB R0, ret+24(FP)
	RET

TEXT ·Xadd(SB), NOSPLIT, $0-20
	I32Load ptr+0(FP)
	I32Load delta+8(FP)
	I32AtomicRmwAdd $0
	I32Load delta+8(FP)
	I32Add
	I64ExtendI32U
	Set R0
	MOVW R0, ret+16(FP)
	RET

TEXT ·Xadd64(SB), NOSPLIT, $0-24
	I32Load ptr+0(FP)
	I64Load delta+8(FP)
	I64AtomicRmwAdd $0
	I64Load delta+8(FP)
	I64Add
	Set R0
	MOVD R0, ret+16(FP)
	RET

TEXT ·Xchg8(SB), NOSPLIT, $0-17
	I32Load ptr+0(FP)
	I32Load8U new+8(FP)
	I32AtomicRmw8XchgU $0
	I64ExtendI32U
	Set R0
	MOVB R0, ret+16(FP)
	RET

TEXT ·Xchg(SB), NOSPLIT, $0-20
	I32Load ptr+0(FP)
	I32Load new+8(FP)
	I32AtomicRmwXchg $0
	I64ExtendI32U
	Set R0
	MOVW R0, ret+16(FP)
	RET

TEXT ·Xchg64(SB), NOSPLIT, $0-24
	I32Load ptr+0(FP)
	I64Load new+8(FP)
	I64AtomicRmwXchg $0
	Set R0
	MOVD R0, ret+16(FP)
	RET

TEXT ·And8(SB), NOSPLIT, $0-9
	I32Load ptr+0(FP)
	I32Load8U val+8(FP)
	I32AtomicRmw8AndU $0
	Drop
	RET

TEXT ·And32(SB), NOSPLIT, $0-20
	I32Load ptr+0(FP)
	I32Load val+8(FP)
	I32AtomicRmwAnd $0
	I64ExtendI32U
	Set R0
	MOVW R0, ret+16(FP)
	RET

TEXT ·And64(SB), NOSPLIT, $0-24
	I32Load ptr+0(FP)
	I64Load val+8(FP)
	I64AtomicRmwAnd $0
	Set R0
	MOVD R0, ret+16(FP)
	RET

TEXT ·Or8(SB), NOSPLIT, $0-9
	I32Load ptr+0(FP)
	I32Load8U val+8(FP)
	I32AtomicRmw8OrU $0
	Drop
	RET

TEXT ·Or32(SB), NOSPLIT, $0-20
	I32Load ptr+0(FP)
	I32Load val+8(FP)
	I32AtomicRmwOr $0
	I64ExtendI32U
	Set R0
	MOVW R0, ret+16(FP)
	RET

TEXT ·Or64(SB), NOSPLIT, $0-24
	I32Load ptr+0(FP)
	I64Load val+8(FP)
	I64AtomicRmwOr $0
	Set R0
	MOVD R0, ret+16(FP)
	RET

TEXT ·Loadp(SB), NOSPLIT, $0-16
	JMP ·Load64(SB)

TEXT ·LoadAcq(SB), NOSPLIT, $0-12
	JMP ·Load(SB)

TEXT ·LoadAcq64(SB), NOSPLIT, $0-16
	JMP ·Load64(SB)

TEXT ·LoadAcquintptr(SB), NOSPLIT, $0-16
	JMP ·Load64(SB)

TEXT ·Loaduintptr(SB), NOSPLIT, $0-16
	JMP ·Load64(SB)

TEXT ·Loaduint(SB), NOSPLIT, $0-16
	JMP ·Load64(SB)

TEXT ·Loadint32(SB), NOSPLIT, $0-12
	JMP ·Load(SB)

TEXT ·Loadint64(SB), NOSPLIT, $0-16
	JMP ·Load64(SB)

TEXT ·StoreRel(SB), NOSPLIT, $0-12
	JMP ·Store(SB)

TEXT ·StoreRel64(SB), NOSPLIT, $0-16
	JMP ·Store64(SB)

TEXT ·StoreReluintptr(SB), NOSPLIT, $0-16
	JMP ·Store64(SB)

TEXT ·Storeuintptr(SB), NOSPLIT, $0-16
	JMP ·Store64(SB)

TEXT ·Storeint32(SB), NOSPLIT, $0-12
	JMP ·Store(SB)

TEXT ·Storeint64(SB), NOSPLIT, $0-16
	JMP ·Store64(SB)

TEXT ·StorepNoWB(SB), NOSPLIT, $0-16
	JMP ·Store64(SB)

TEXT ·Casp1(SB), NOSPLIT, $0-25
	JMP ·Cas64(SB)

TEXT ·Casint32(SB), NOSPLIT, $0-17
	JMP ·Cas(SB)

TEXT ·Casint64(SB), NOSPLIT, $0-25
	JMP ·Cas64(SB)

TEXT ·Casuintptr(SB), NOSPLIT, $0-25
	JMP ·Cas64(SB)

TEXT ·CasRel(SB), NOSPLIT, $0-17
	JMP ·Cas(SB)

TEXT ·Xadduintptr(SB), NOSPLIT, $0-24
	JMP ·Xadd64(SB)

TEXT ·Xaddint32(SB), NOSPLIT, $0-20
	JMP ·Xadd(SB)

TEXT ·Xaddint64(SB), NOSPLIT, $0-24
	JMP ·Xadd64(SB)

TEXT ·Xchguintptr(SB), NOSPLIT, $0-24
	JMP ·Xchg64(SB)

TEXT ·Xchgint32(SB), NOSPLIT, $0-20
	JMP ·Xchg(SB)

TEXT ·Xchgint64(SB), NOSPLIT, $0-24
	JMP ·Xchg64(SB)

TEXT ·Anduintptr(SB), NOSPLIT, $0-24
	JMP ·And64(SB)

TEXT ·Oruintptr(SB), NOSPLIT, $0-24
	JMP ·Or64(SB)

TEXT ·And(SB), NOSPLIT, $0-12
	I32Load ptr+0(FP)
	I32Load val+8(FP)
	I32AtomicRmwAnd $0
	Drop
	RET

TEXT ·Or(SB), NOSPLIT, $0-12
	I32Load ptr+0(FP)
	I32Load val+8(FP)
	I32AtomicRmwOr $0
	Drop
	RET
