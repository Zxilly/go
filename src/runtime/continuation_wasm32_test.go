// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm32

package runtime_test

import (
	"internal/abi"
	"reflect"
	"runtime"
	"strings"
	"testing"
	"unsafe"
)

type wasm32Adder interface {
	Add(int) int
}

type wasm32AddOne struct{}

//go:noinline
func (wasm32AddOne) Add(v int) int { return v + 1 }

//go:noinline
func wasm32GrowAndYield(depth int, fn func(int) int) int {
	var frame [256]byte
	frame[0] = byte(depth)
	if depth == 0 {
		runtime.Gosched()
		result := fn(40)
		runtime.KeepAlive(&frame)
		return result
	}
	result := wasm32GrowAndYield(depth-1, fn)
	runtime.KeepAlive(&frame)
	return result
}

//go:noinline
func wasm32RecoverAndContinue(fn func() int) (result int) {
	defer func() {
		if recover() == nil {
			result = -1
			return
		}
		runtime.Gosched()
		result = fn()
	}()
	panic("resume")
}

//go:noinline
func wasm32IndirectTarget(v int) int { return v + 2 }

func wasm32NamedDefer() {}

func TestWasm32ContinuationState(t *testing.T) {
	fn := wasm32IndirectTarget
	funcval := *(*unsafe.Pointer)(unsafe.Pointer(&fn))
	handle := *(*uintptr)(funcval)
	pc := abi.FuncPCABIInternal(fn)
	if handle == 0 || handle == pc {
		t.Fatalf("call handle %#x is not distinct from logical PC %#x", handle, pc)
	}
	if got := reflect.ValueOf(fn).Pointer(); got != pc {
		t.Fatalf("reflect function PC = %#x, want %#x", got, pc)
	}
	if f := runtime.FuncForPC(pc); f == nil {
		t.Fatalf("FuncForPC(%#x) returned nil", pc)
	}
	if got := runtime.WasmFuncName(wasm32NamedDefer); !strings.HasSuffix(got, ".wasm32NamedDefer") {
		t.Fatalf("defer function name = %q, want wasm32NamedDefer", got)
	}

	done := make(chan int, 1)
	go func() {
		var adder wasm32Adder = wasm32AddOne{}
		result := wasm32GrowAndYield(256, adder.Add)
		result += wasm32RecoverAndContinue(func() int { return 1 })
		done <- result
	}()

	if got := <-done; got != 42 {
		t.Fatalf("continuation result = %d, want 42", got)
	}

	pcs := make([]uintptr, 8)
	if n := runtime.Callers(0, pcs); n == 0 {
		t.Fatal("runtime.Callers returned no logical PCs")
	}
}
