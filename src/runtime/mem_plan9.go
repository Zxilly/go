// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import "unsafe"

func sbrk(n uintptr) unsafe.Pointer {
	// Plan 9 sbrk from /sys/src/libc/9sys/sbrk.c
	bl := bloc
	end := bl + uint64(memRound(n))
	if end < bl || end > uint64(^uintptr(0)) {
		return nil
	}
	if end > blocMax {
		if brk_(unsafe.Pointer(uintptr(end))) < 0 {
			return nil
		}
		blocMax = end
	}
	bloc = end
	return unsafe.Pointer(uintptr(bl))
}
