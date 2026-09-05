// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build js && wasm

package os_test

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestStatJSTime(t *testing.T) {
	name := filepath.Join(t.TempDir(), "stat")
	if err := os.WriteFile(name, nil, 0600); err != nil {
		t.Fatal(err)
	}
	// The JS host represents this as milliseconds, which exceed a 32-bit int.
	want := time.Unix(1700000000, 0)
	if err := os.Chtimes(name, want, want); err != nil {
		t.Fatal(err)
	}
	info, err := os.Stat(name)
	if err != nil {
		t.Fatal(err)
	}
	if got := info.ModTime(); !got.Equal(want) {
		t.Fatalf("ModTime = %v, want %v", got, want)
	}
}
