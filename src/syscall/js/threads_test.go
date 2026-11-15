// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build js && wasm && wasm.threads

package js_test

import (
	"fmt"
	"runtime"
	"sync"
	"syscall/js"
	"testing"
	"time"
)

// Package initialization runs on a locked thread. Waiting here must still let
// the JavaScript owner process asynchronous callbacks, even with one P.
func init() {
	old := runtime.GOMAXPROCS(1)
	defer runtime.GOMAXPROCS(old)
	runtime.LockOSThread()
	defer runtime.UnlockOSThread()
	done := make(chan struct{})
	callback := js.FuncOf(func(js.Value, []js.Value) any {
		close(done)
		return nil
	})
	defer callback.Release()
	js.Global().Call("setTimeout", callback, 0)
	select {
	case <-done:
	case <-time.After(10 * time.Second):
		panic("JavaScript callback during package initialization timed out")
	}
}

func TestThreads(t *testing.T) {
	old := runtime.GOMAXPROCS(4)
	defer runtime.GOMAXPROCS(old)
	object := js.Global().Get("Object").New()
	inner := js.FuncOf(func(this js.Value, args []js.Value) any {
		return args[0].Int() + 1
	})
	defer inner.Release()
	outer := js.FuncOf(func(this js.Value, args []js.Value) any {
		// A callback can grow its stack before reentering JavaScript. The host
		// must update the main instance's SP without changing the worker's SP.
		sum := threadCallbackStack(64)
		return inner.Invoke(args[0].Int()).Int() + sum
	})
	defer outer.Release()
	var ready, wg sync.WaitGroup
	ready.Add(8)
	start := make(chan struct{})
	for i := 0; i < 8; i++ {
		wg.Go(func() {
			runtime.LockOSThread()
			defer runtime.UnlockOSThread()
			// Hold a distinct worker for each goroutine before starting the calls.
			ready.Done()
			<-start
			key := fmt.Sprint("worker", i)
			for n := 0; n < 32; n++ {
				object.Set(key, i*1000+n)
				if got := object.Get(key).Int(); got != i*1000+n {
					t.Errorf("worker %d: property = %d, want %d", i, got, i*1000+n)
					return
				}
				if got := outer.Invoke(n).Int(); got != n+1+64*65/2 {
					t.Errorf("worker %d: callback = %d, want %d", i, got, n+1+64*65/2)
					return
				}
				if n%8 == 0 {
					runtime.GC()
				}
			}
		})
	}
	ready.Wait()
	close(start)
	wg.Wait()
}

//go:noinline
func threadCallbackStack(n int) int {
	var data [128]byte
	data[0] = byte(n)
	if n == 0 {
		return 0
	}
	sum := threadCallbackStack(n - 1)
	runtime.KeepAlive(&data)
	return sum + int(data[0])
}
