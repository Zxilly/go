// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build !purego

package sha512

import (
	"crypto/internal/impl"
	"internal/cpu"
)

var useSHA512 = cpu.S390X.HasSHA512

func init() {
	// CP Assist for Cryptographic Functions (CPACF)
	// https://www.ibm.com/docs/en/zos/3.1.0?topic=icsf-cp-assist-cryptographic-functions-cpacf
	impl.Register("crypto/sha512", "CPACF", &useSHA512)
}

//go:noescape
func blockS390X(dig *digest, p []byte)

func block(dig *digest, p []byte) {
	if useSHA512 {
		blockS390X(dig, p)
	} else {
		blockGeneric(dig, p)
	}
}
