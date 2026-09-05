// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build !wasm32

package runtime

//go:nosplit
func funcHandleToPC(handle uintptr) uintptr {
	return handle
}

//go:nosplit
func funcPCToHandle(pc uintptr) uintptr {
	return pc
}

//go:nosplit
func gobufSetPC(buf *gobuf, pc uintptr) {
	buf.pc = pc
	buf.lr = 0
}

//go:nosplit
func gobufSetPCWithHandle(buf *gobuf, pc, _ uintptr) {
	gobufSetPC(buf, pc)
}

//go:nosplit
func wasmGoexitHandle() uintptr { return 0 }

//go:nosplit
func gobufTracebackLR(buf *gobuf) uintptr {
	return buf.lr
}
