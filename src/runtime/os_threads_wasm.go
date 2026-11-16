// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm && wasm.threads

package runtime

type mOS struct {
	waitsema uint32
	wasmHost wasmHostState
}

//go:noescape
func wasmWait(addr *uint32, val uint32, ns int64) uint32

//go:noescape
func wasmNotify(addr *uint32, count uint32) uint32

// These helpers use raw WebAssembly signatures, not the Go ABI. They are
// called only by compiler-generated and assembly code.
func wasmMove()
func wasmZero()
func wasmLoad64()
func wasmStore64()

//go:nosplit
func futexsleep(addr *uint32, val uint32, ns int64) {
	if GOOS == "js" && getg().m == &m0 {
		wasmThreadWait(addr, val, ns)
		return
	}
	wasmWait(addr, val, ns)
}

//go:nosplit
func futexwakeup(addr *uint32, cnt uint32) {
	wasmNotify(addr, cnt)
}
