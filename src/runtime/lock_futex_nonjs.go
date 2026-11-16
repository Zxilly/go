// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build dragonfly || freebsd || linux || (wasip1 && wasm.threads)

package runtime

func beforeIdle(int64, int64) (*g, bool) {
	return nil, false
}

func checkTimeouts() {}
