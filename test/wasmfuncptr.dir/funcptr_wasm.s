// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "textflag.h"

FUNCPTR ·functionCode(SB), $·target(SB)
GLOBL ·functionCode(SB), RODATA|NOPTR, $8

TEXT ·functionValueCode(SB), NOSPLIT, $0-8
	MOVD $·functionCode(SB), ret+0(FP)
	RET
