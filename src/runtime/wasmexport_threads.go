// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm && wasm.threads

package runtime

import "internal/runtime/sys"

// The anchor frame remains on the Go stack while native WebAssembly frames
// unwind to the host. Its offset from stack.hi survives stack growth during
// exported calls. Publishing that frame through reentersyscall lets the GC
// scan a suspended host goroutine while workers continue running.
type wasmHostState struct {
	pc        uintptr
	sp        stackOff
	depth     uint32 // number of nested exported calls
	suspended bool   // the host anchor is published as a syscall stack
	restore   bool   // the outermost export must restore the syscall state
	resumed   bool   // the last host entry resumed a suspended goroutine
}

func wasmHostPause()

//go:nosplit
func wasmHostSuspend() {
	gp := getg()
	if gp.m.wasmHost.pc == 0 {
		gp.m.wasmHost.pc = sys.GetCallerPC()
		gp.m.wasmHost.sp = gp.toStackOff(sys.GetCallerSP())
	}
	gp.m.wasmHost.suspended = true
	reentersyscall(gp.m.wasmHost.pc, gp.stack.hi-uintptr(gp.m.wasmHost.sp), 0)
}

//go:nosplit
func wasmHostResume() {
	gp := getg()
	gp.m.wasmHost.resumed = gp.m.wasmHost.suspended
	if gp.m.wasmHost.suspended {
		exitsyscall()
		getg().m.wasmHost.suspended = false
	}
}

//go:nosplit
func wasmHostWasPaused() bool {
	return getg().m.wasmHost.resumed
}

// Exported functions may block or grow their stacks. Keep their goroutine on
// the calling instance so the native export wrapper and any host import frames
// still belong to the M that resumes the Go call. Nested synchronous host
// callbacks participate in the same lock nesting.
//
//go:nosplit
func wasmExportEnter() {
	gp := getg()
	lockOSThread()
	if gp.m.wasmHost.depth == 0 {
		gp.m.wasmHost.restore = gp.m.wasmHost.suspended
	}
	gp.m.wasmHost.depth++
	wasmHostResume()
}

//go:nosplit
func wasmExportExit() {
	gp := getg()
	gp.m.wasmHost.depth--
	unlockOSThread()
	if gp.m.wasmHost.depth == 0 && gp.m.wasmHost.restore {
		gp.m.wasmHost.suspended = true
		reentersyscall(gp.m.wasmHost.pc, gp.stack.hi-uintptr(gp.m.wasmHost.sp), 0)
	}
}
