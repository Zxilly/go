// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasip1 && wasm.threads

package syscall_test

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"sync"
	"syscall"
	"testing"
)

func TestWasmThreadedFileIO(t *testing.T) {
	defer runtime.GOMAXPROCS(runtime.GOMAXPROCS(4))
	dir := t.TempDir()
	done := make(chan struct{})
	gcDone := make(chan struct{})
	go func() {
		defer close(gcDone)
		for {
			select {
			case <-done:
				return
			default:
				runtime.GC()
			}
		}
	}()
	defer func() { close(done); <-gcDone }()
	var wg sync.WaitGroup
	for worker := range 4 {
		wg.Go(func() {
			path := filepath.Join(dir, fmt.Sprint(worker))
			f, err := os.Create(path)
			if err != nil {
				t.Error(err)
				return
			}
			defer f.Close()
			var want, got [4096]byte
			for round := range 16 {
				for i := range want {
					want[i] = byte(worker + round + i)
				}
				if _, err := f.WriteAt(want[:], 0); err != nil {
					t.Error(err)
					return
				}
				if _, err := f.Seek(0, io.SeekStart); err != nil {
					t.Error(err)
					return
				}
				if _, err := io.ReadFull(f, got[:]); err != nil {
					t.Error(err)
					return
				}
				if !bytes.Equal(want[:], got[:]) {
					t.Errorf("worker %d round %d: buffer changed during host I/O", worker, round)
					return
				}
			}
			if err := f.Sync(); err != nil {
				t.Error(err)
			}
			if _, err := f.Stat(); err != nil {
				t.Error(err)
			}
		})
	}
	wg.Wait()
}

func TestWasmThreadedChdir(t *testing.T) {
	defer runtime.GOMAXPROCS(runtime.GOMAXPROCS(4))
	old, err := syscall.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	paths := []string{filepath.Join(dir, "a"), filepath.Join(dir, "longer-directory-name")}
	for _, path := range paths {
		if err := os.Mkdir(path, 0700); err != nil {
			t.Fatal(err)
		}
	}
	defer func() {
		if err := syscall.Chdir(old); err != nil {
			t.Error(err)
		}
	}()
	var wg sync.WaitGroup
	for worker := range 4 {
		wg.Go(func() {
			for range 100 {
				if err := syscall.Chdir(paths[worker%2]); err != nil {
					t.Error(err)
					return
				}
				got, err := syscall.Getwd()
				if err != nil || got != paths[0] && got != paths[1] {
					t.Errorf("Getwd = %q, %v; want one of %q", got, err, paths)
					return
				}
				var st syscall.Stat_t
				if err := syscall.Stat(".", &st); err != nil {
					t.Error(err)
					return
				}
			}
		})
	}
	wg.Wait()
}
