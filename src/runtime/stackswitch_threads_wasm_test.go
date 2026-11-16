// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm.stackswitch && wasm.threads

package runtime_test

import (
	"runtime"
	"sync"
	"testing"
)

func TestWasmContinuationThreadTables(t *testing.T) {
	defer runtime.GOMAXPROCS(runtime.GOMAXPROCS(4))
	const workers = 3
	type state struct {
		id   uint64
		size int32
	}
	before := make(chan state, workers)
	after := make(chan state, workers)
	grow := make(chan struct{})
	grown := make(chan struct{})
	release := make(chan struct{})
	defer close(release)
	var wg sync.WaitGroup
	for i := range workers {
		wg.Go(func() {
			runtime.LockOSThread()
			defer runtime.UnlockOSThread()
			id := runtime.WasmThreadID()
			size := runtime.WasmContTableSize()
			for _, slot := range []int32{runtime.WasmContSlot(), runtime.WasmContG0Slot()} {
				if slot < 0 || slot >= size {
					t.Errorf("thread %d: slot %d is outside table of size %d", id, slot, size)
				}
			}
			before <- state{id, size}
			if i == 0 {
				<-grow
				if old := runtime.WasmContTableGrow(17); old != size {
					t.Errorf("table.grow returned %d, want %d", old, size)
				}
				close(grown)
			} else {
				<-grown
			}
			after <- state{id, runtime.WasmContTableSize()}
			<-release
		})
	}
	initial := make(map[uint64]int32)
	for range workers {
		s := <-before
		if _, ok := initial[s.id]; ok {
			t.Errorf("two locked goroutines use the same thread %d", s.id)
		}
		initial[s.id] = s.size
	}
	close(grow)
	changed := 0
	for range workers {
		s := <-after
		delta := s.size - initial[s.id]
		if delta == 17 {
			changed++
		} else if delta != 0 {
			t.Errorf("thread %d: table size changed by %d, want 0 or 17", s.id, delta)
		}
	}
	if changed != 1 {
		t.Errorf("%d instance tables changed, want exactly one", changed)
	}
	// Let workers finish before the test changes GOMAXPROCS back.
	for range workers {
		release <- struct{}{}
	}
	wg.Wait()
}
