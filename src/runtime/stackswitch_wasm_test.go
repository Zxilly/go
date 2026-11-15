// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm.stackswitch

package runtime_test

import (
	"runtime"
	"sync"
	"testing"
)

func TestWasmContinuationTable(t *testing.T) {
	defer runtime.GOMAXPROCS(runtime.GOMAXPROCS(4))
	// Keep enough goroutines alive to exceed the initial table size. Create
	// them from several goroutines so slot allocation can run concurrently.
	const creators, perCreator = 4, 320
	slots := make(chan int32, creators*perCreator)
	done := make(chan struct{})
	defer close(done)
	var creatorsDone sync.WaitGroup
	for range creators {
		creatorsDone.Go(func() {
			for range perCreator {
				go func() {
					runtime.LockOSThread()
					slot := runtime.WasmContSlot()
					size := runtime.WasmContTableSize()
					if slot <= 0 || slot >= size {
						t.Errorf("goroutine slot %d is outside its current table of size %d", slot, size)
					}
					runtime.UnlockOSThread()
					slots <- slot
					<-done
				}()
			}
		})
	}
	creatorsDone.Wait()
	seen := make(map[int32]bool)
	for range creators * perCreator {
		slot := <-slots
		if seen[slot] {
			t.Errorf("goroutines share continuation slot %d", slot)
		}
		seen[slot] = true
	}
}
