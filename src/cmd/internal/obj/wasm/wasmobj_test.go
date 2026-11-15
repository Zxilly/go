// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package wasm

import (
	"bytes"
	"cmd/internal/obj"
	"testing"
)

// TestWriteOpcode checks one opcode from each encoding range. Vector
// instructions are the 0xFD prefix followed by a ULEB128-encoded opcode
// index, so the cases below bracket both points where that index grows a
// byte: 0x07F to 0x080 and 0x0FF to 0x100.
func TestWriteOpcode(t *testing.T) {
	tests := []struct {
		as   obj.As
		want []byte
	}{
		{AUnreachable, []byte{0x00}},
		{AEnd, []byte{0x0B}},
		{ADrop, []byte{0x1A}},
		{ALocalGet, []byte{0x20}},
		{AI32Load, []byte{0x28}},
		{ARefNull, []byte{0xD0}},
		{AContNew, []byte{0xE0}},
		{AI32TruncSatF32S, []byte{0xFC, 0x00}},

		{AV128Load, []byte{0xFD, 0x00}},                            // index 0x000
		{AI32x4ExtaddPairwiseI16x8U, []byte{0xFD, 0x7F}},           // index 0x07F
		{AI16x8Abs, []byte{0xFD, 0x80, 0x01}},                      // index 0x080
		{AF64x2ConvertLowI32x4U, []byte{0xFD, 0xFF, 0x01}},         // index 0x0FF
		{AI8x16RelaxedSwizzle, []byte{0xFD, 0x80, 0x02}},           // index 0x100
		{AI32x4RelaxedDotI8x16I7x16AddS, []byte{0xFD, 0x93, 0x02}}, // index 0x113

	}
	var buf bytes.Buffer
	for _, test := range tests {
		buf.Reset()
		writeOpcode(&buf, test.as)
		if got := buf.Bytes(); !bytes.Equal(got, test.want) {
			t.Errorf("writeOpcode(%v) = % x, want % x", test.as, got, test.want)
		}
	}
}

func TestAtomicMemoryArguments(t *testing.T) {
	tests := []struct {
		as   obj.As
		want []byte
	}{
		{AMemoryAtomicNotify, []byte{0xFE, 0x00, 2, 0x80, 1}},
		{AMemoryAtomicWait32, []byte{0xFE, 0x01, 2, 0x80, 1}},
		{AMemoryAtomicWait64, []byte{0xFE, 0x02, 3, 0x80, 1}},
		{AAtomicFence, []byte{0xFE, 0x03, 0}},
		{AI32AtomicLoad, []byte{0xFE, 0x10, 2, 0x80, 1}},
		{AI64AtomicLoad, []byte{0xFE, 0x11, 3, 0x80, 1}},
		{AI32AtomicLoad8U, []byte{0xFE, 0x12, 0, 0x80, 1}},
		{AI64AtomicLoad16U, []byte{0xFE, 0x15, 1, 0x80, 1}},
		{AI64AtomicStore, []byte{0xFE, 0x18, 3, 0x80, 1}},
		{AI32AtomicStore8, []byte{0xFE, 0x19, 0, 0x80, 1}},
		{AI32AtomicRmwAdd, []byte{0xFE, 0x1E, 2, 0x80, 1}},
		{AI64AtomicRmw32CmpxchgU, []byte{0xFE, 0x4E, 2, 0x80, 1}},
	}
	for _, test := range tests {
		t.Run(test.as.String(), func(t *testing.T) {
			ctxt := obj.Linknew(&Linkwasm)
			// Use a function with the raw WebAssembly calling convention so
			// there is no local SP initialization before the instruction.
			s := &obj.LSym{Name: "runtime.wasmDiv"}
			p := &obj.Prog{As: test.as}
			offset := obj.Addr{Type: obj.TYPE_CONST, Offset: 128}
			if unaryDst[test.as] {
				p.To = offset
			} else {
				p.From = offset
			}
			s.NewFuncInfo().Text = p
			assemble(ctxt, s, ctxt.NewProg)
			want := append([]byte{0}, test.want...) // no locals
			want = append(want, 0x0B)               // end
			if !bytes.Equal(s.P, want) {
				t.Fatalf("got % x, want % x", s.P, want)
			}
		})
	}
}
