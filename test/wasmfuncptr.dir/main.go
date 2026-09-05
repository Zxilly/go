// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import "unsafe"

var called bool

func target() {
	called = true
}

func functionValueCode() unsafe.Pointer

func main() {
	var f func()
	*(*unsafe.Pointer)(unsafe.Pointer(&f)) = functionValueCode()
	f()
	if !called {
		panic("assembly function value called the wrong target")
	}
}
