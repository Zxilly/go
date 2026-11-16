// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm && wasm.threads

package runtime_test

import (
	"runtime"
	"sync"
	"testing"
	"time"
)

func TestWasmThreads(t *testing.T) {
	old := runtime.GOMAXPROCS(4)
	defer runtime.GOMAXPROCS(old)
	if got := runtime.GOMAXPROCS(0); got != 4 {
		t.Fatalf("GOMAXPROCS = %d, want 4", got)
	}
	// Holding each M until all workers have reported their host thread ID
	// prevents serial reuse of a single M from satisfying this test.
	ids := make(chan uint64, 4)
	release := make(chan struct{})
	var wg sync.WaitGroup
	for i := 0; i < 4; i++ {
		wg.Go(func() {
			runtime.LockOSThread()
			defer runtime.UnlockOSThread()
			id := runtime.WasmThreadID()
			ids <- id
			<-release
			if got := runtime.WasmThreadID(); got != id {
				t.Errorf("locked goroutine migrated from thread %d to %d", id, got)
			}
		})
	}
	seen := make(map[uint64]bool)
	for i := 0; i < 4; i++ {
		select {
		case id := <-ids:
			if seen[id] {
				t.Errorf("two locked goroutines share host thread %d", id)
			}
			seen[id] = true
		case <-time.After(10 * time.Second):
			t.Fatal("workers did not start")
		}
	}
	close(release)
	wg.Wait()
}

func TestWasmThreadsExit(t *testing.T) {
	old := runtime.GOMAXPROCS(4)
	defer runtime.GOMAXPROCS(old)
	for round := 0; round < 10; round++ {
		var wg sync.WaitGroup
		for i := 0; i < 8; i++ {
			wg.Go(func() {
				runtime.LockOSThread()
				// Exiting without UnlockOSThread retires this M. Later rounds
				// create new Ms and may reuse its stack after freeWait clears.
			})
		}
		wg.Wait()
		runtime.GC()
	}
}

//go:noinline
func wasmLargeStack(depth int) uint64 {
	var block [256]uint64
	for i := range block {
		block[i] = uint64(i + depth)
	}
	var sum uint64
	if depth != 0 {
		sum = wasmLargeStack(depth - 1)
	}
	for _, value := range block {
		sum += value
	}
	return sum
}

func TestWasmThreadsStackPreempt(t *testing.T) {
	old := runtime.GOMAXPROCS(4)
	defer runtime.GOMAXPROCS(old)
	var wg sync.WaitGroup
	for i := 0; i < 4; i++ {
		wg.Go(func() {
			// stackPreempt is near the top of the address space. Adding
			// a large frame to its low 32 bits must not wrap around and
			// let the frame bypass the stack check.
			runtime.WasmRequestPreemption()
			got := wasmLargeStack(96)
			var want uint64
			for depth := 0; depth <= 96; depth++ {
				want += 256*255/2 + uint64(256*depth)
			}
			if got != want {
				t.Errorf("large-frame recursion = %d, want %d", got, want)
			}
		})
	}
	wg.Wait()
}
