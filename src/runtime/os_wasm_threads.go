// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasip1 && wasm && wasm.threads

package runtime

import "unsafe"

// wasmThreadWait is used only by js/wasm, where the main agent cannot block.
func wasmThreadWait(addr *uint32, val uint32, ns int64) uint32 {
	throw("wasmThreadWait on WASI")
	return 0
}

// threadSpawn creates a new instance sharing this instance's linear memory.
// The host calls wasi_thread_start(tid, arg) on that instance. Unlike _start,
// that entry point must not initialize the shared data or the Go runtime.
//
//go:wasmimport wasi thread-spawn
func threadSpawn(arg uint32) int32

// May run with m.p==nil, so write barriers are not allowed.
//
//go:nowritebarrier
func newosproc(mp *m) {
	if tid := threadSpawn(uint32(uintptr(unsafe.Pointer(mp)))); tid < 0 {
		print("runtime: failed to create WebAssembly thread (errno=", -tid, ")\n")
		throw("newosproc")
	}
}

//go:wasmimport wasi_snapshot_preview1 sched_yield
func sched_yield() errno

//go:nosplit
func osyield() {
	sched_yield()
}
