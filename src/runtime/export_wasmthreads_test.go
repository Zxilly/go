// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm && wasm.threads

package runtime

func WasmThreadID() uint64 {
	return getg().m.procid
}

//go:nosplit
func WasmRequestPreemption() {
	gp := getg()
	gp.preempt = true
	gp.stackguard0 = stackPreempt
}
