// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build js && wasm && wasm.threads

package js

import _ "unsafe" // for go:linkname

//go:linkname runtime_entersyscallblock runtime.entersyscallblock
func runtime_entersyscallblock()

//go:linkname runtime_exitsyscall runtime.exitsyscall
func runtime_exitsyscall()

// threadEnter locks the calling goroutine to its thread and reports whether
// that thread is a Worker. threadExit releases the lock.
func threadEnter() bool
func threadExit()

// Worker imports wait for the main JavaScript thread to execute the call.
// Enter syscall state during that wait so that the runtime can schedule other
// goroutines and scan this goroutine's stack. The main thread can enter Go
// callbacks synchronously and must retain its P.
//
// These wrappers must call entersyscallblock and exitsyscall directly:
// entersyscallblock records its caller's frame. The wrappers must not split
// the stack between these calls.

//go:nosplit
func finalizeRef(r ref) {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	rawFinalizeRef(r)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
}

//go:nosplit
func stringVal(x string) ref {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	result := rawStringVal(x)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
	return result
}

//go:nosplit
func valueGet(v ref, p string) ref {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	result := rawValueGet(v, p)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
	return result
}

//go:nosplit
func valueSet(v ref, p string, x ref) {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	rawValueSet(v, p, x)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
}

//go:nosplit
func valueDelete(v ref, p string) {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	rawValueDelete(v, p)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
}

//go:nosplit
func valueIndex(v ref, i int) ref {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	result := rawValueIndex(v, i)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
	return result
}

//go:nosplit
func valueSetIndex(v ref, i int, x ref) {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	rawValueSetIndex(v, i, x)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
}

//go:nosplit
func valueLength(v ref) int {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	length := rawValueLength(v)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
	return length
}

//go:nosplit
func valueCall(v ref, m string, args []ref) (ref, bool) {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	result, ok := rawValueCall(v, m, args)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
	return result, ok
}

//go:nosplit
func valueInvoke(v ref, args []ref) (ref, bool) {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	result, ok := rawValueInvoke(v, args)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
	return result, ok
}

//go:nosplit
func valueNew(v ref, args []ref) (ref, bool) {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	result, ok := rawValueNew(v, args)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
	return result, ok
}

//go:nosplit
func valuePrepareString(v ref) (ref, int) {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	str, length := rawValuePrepareString(v)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
	return str, length
}

//go:nosplit
func valueLoadString(v ref, b []byte) {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	rawValueLoadString(v, b)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
}

//go:nosplit
func valueInstanceOf(v ref, t ref) bool {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	ok := rawValueInstanceOf(v, t)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
	return ok
}

//go:nosplit
func copyBytesToGo(dst []byte, src ref) (int, bool) {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	n, ok := rawCopyBytesToGo(dst, src)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
	return n, ok
}

//go:nosplit
func copyBytesToJS(dst ref, src []byte) (int, bool) {
	worker := threadEnter()
	if worker {
		runtime_entersyscallblock()
	}
	n, ok := rawCopyBytesToJS(dst, src)
	if worker {
		runtime_exitsyscall()
	}
	threadExit()
	return n, ok
}
