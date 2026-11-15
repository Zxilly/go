// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build js && wasm && wasm.threads

package runtime

import (
	"internal/runtime/sys"
	"unsafe"
)

// wasmStartMain moves package initialization and main onto a worker before
// either can lock the JavaScript owner in a blocking operation. Keep the
// initialization lock on the worker so an explicit LockOSThread in init
// continues to pin main to that same instance.
func wasmStartMain() {
	unlockOSThread()
	mcall(wasmStartMainM)
	lockOSThread()
}

//go:yeswritebarrierrec
func wasmStartMainM(gp *g) {
	if getg().m != &m0 || gp.lockedm != 0 {
		throw("wasmStartMain: unexpected thread")
	}
	fn := handleAsyncEvent
	newg := newproc1(*(**funcval)(unsafe.Pointer(&fn)), gp, sys.GetCallerPC(), false, waitReasonZero)
	trace := traceAcquire()
	if trace.ok() {
		trace.GoSched()
	}
	casgstatus(gp, _Grunning, _Grunnable)
	if trace.ok() {
		traceRelease(trace)
	}
	dropg()
	lock(&sched.lock)
	globrunqput(gp)
	unlock(&sched.lock)
	wakep()
	// The owner must run the event goroutine, never the main goroutine we
	// just queued. Its host pause lets a worker take the P even at GOMAXPROCS=1.
	execute(newg, false)
}

func beforeIdle(int64, int64) (*g, bool) {
	return nil, false
}

func handleAsyncEvent() {
	lockOSThread()
	if getg().m != &m0 {
		throw("JavaScript event loop on worker")
	}
	wasmThreadPoll()
	wasmHostPause()
	throw("JavaScript event loop resumed without event")
}

func checkTimeouts() {}

// The raw resume entry restores the host's syscall state before entering
// this function. A nested synchronous callback must return to the caller's
// import; an event arriving from the event loop returns to the idle anchor.
func handleEvent() {
	fromHost := wasmHostWasPaused()
	lockOSThread()
	if eventHandler != nil {
		eventHandler()
	}
	unlockOSThread()
	if fromHost {
		wasmThreadPoll()
		wasmHostPause()
	} else {
		pause(sys.GetCallerSP() - 16)
	}
}

var eventHandler func() bool

//go:linkname setEventHandler syscall/js.setEventHandler
func setEventHandler(fn func() bool) {
	eventHandler = fn
}
