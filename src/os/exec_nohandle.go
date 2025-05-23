// Copyright 2024 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build !linux && !windows

package os

func (ph *processHandle) closeHandle() {
	panic("internal error: unexpected call to closeHandle")
}
