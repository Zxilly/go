// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build js && wasm && wasm.threads

package runtime

import "unsafe"

//go:wasmimport gojs runtime.wasmThreadSpawn
func threadSpawn(arg uint32) int32

// May run with m.p==nil, so write barriers are not allowed.
//
//go:nowritebarrier
func newosproc(mp *m) {
	if tid := threadSpawn(uint32(uintptr(unsafe.Pointer(mp)))); tid < 0 {
		print("runtime: failed to create WebAssembly worker (errno=", -tid, ")\n")
		throw("newosproc")
	}
}

// The browser's main thread cannot execute memory.atomic.wait. The host
// spins there until the value changes or the timeout expires. Workers use
// the WebAssembly wait instruction directly.
//
//go:wasmimport gojs runtime.wasmThreadWait
//go:noescape
func wasmThreadWait(addr *uint32, val uint32, ns int64) uint32

//go:wasmimport gojs runtime.wasmThreadPoll
func wasmThreadPoll()

//go:nosplit
func osyield() {
	var word uint32
	futexsleep(&word, 0, 1e3) // 1 microsecond
}

// Calls from workers are marshalled to the JavaScript owner. Pin the
// goroutine to its instance while the host accesses its stack. The caller
// releases the P around the host call if this is a worker instance.
//
//go:linkname jsThreadEnter syscall/js.threadEnter
//go:nosplit
func jsThreadEnter() bool {
	lockOSThread()
	return getg().m != &m0
}

//go:linkname jsThreadExit syscall/js.threadExit
//go:nosplit
func jsThreadExit() {
	unlockOSThread()
}

func usleep(usec uint32) {
	var word uint32
	futexsleep(&word, 0, int64(usec)*1000)
}
