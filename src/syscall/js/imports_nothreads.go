// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build js && wasm && !wasm.threads

package js

//go:nosplit
func finalizeRef(r ref) {
	rawFinalizeRef(r)
}

//go:nosplit
func stringVal(x string) ref {
	return rawStringVal(x)
}

//go:nosplit
func valueGet(v ref, p string) ref {
	return rawValueGet(v, p)
}

//go:nosplit
func valueSet(v ref, p string, x ref) {
	rawValueSet(v, p, x)
}

//go:nosplit
func valueDelete(v ref, p string) {
	rawValueDelete(v, p)
}

//go:nosplit
func valueIndex(v ref, i int) ref {
	return rawValueIndex(v, i)
}

//go:nosplit
func valueSetIndex(v ref, i int, x ref) {
	rawValueSetIndex(v, i, x)
}

//go:nosplit
func valueLength(v ref) int {
	return rawValueLength(v)
}

//go:nosplit
func valueCall(v ref, m string, args []ref) (ref, bool) {
	return rawValueCall(v, m, args)
}

//go:nosplit
func valueInvoke(v ref, args []ref) (ref, bool) {
	return rawValueInvoke(v, args)
}

//go:nosplit
func valueNew(v ref, args []ref) (ref, bool) {
	return rawValueNew(v, args)
}

//go:nosplit
func valuePrepareString(v ref) (ref, int) {
	return rawValuePrepareString(v)
}

//go:nosplit
func valueLoadString(v ref, b []byte) {
	rawValueLoadString(v, b)
}

//go:nosplit
func valueInstanceOf(v ref, t ref) bool {
	return rawValueInstanceOf(v, t)
}

//go:nosplit
func copyBytesToGo(dst []byte, src ref) (int, bool) {
	return rawCopyBytesToGo(dst, src)
}

//go:nosplit
func copyBytesToJS(dst ref, src []byte) (int, bool) {
	return rawCopyBytesToJS(dst, src)
}
