// Copyright 2023 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasip1

package runtime

import (
	"unsafe"
)

// GOARCH=wasm currently has 64 bits pointers, but the WebAssembly host expects
// pointers to be 32 bits so we use this type alias to represent pointers in
// structs and arrays passed as arguments to WASI functions.
// GOARCH=wasm32 has 32 bit pointers but these values are still valid.
//
// Note that the use of an integer type prevents the compiler from tracking
// pointers passed to WASI functions, so we must use KeepAlive to explicitly
// retain the objects that could otherwise be reclaimed by the GC.
type uintptr32 = uint32

// https://github.com/WebAssembly/WASI/blob/a2b96e81c0586125cc4dc79a5be0b78d9a059925/legacy/preview1/docs.md#-size-u32
type size = uint32

// https://github.com/WebAssembly/WASI/blob/a2b96e81c0586125cc4dc79a5be0b78d9a059925/legacy/preview1/docs.md#-errno-variant
type errno = uint32

// https://github.com/WebAssembly/WASI/blob/a2b96e81c0586125cc4dc79a5be0b78d9a059925/legacy/preview1/docs.md#-filesize-u64
type filesize = uint64

// https://github.com/WebAssembly/WASI/blob/a2b96e81c0586125cc4dc79a5be0b78d9a059925/legacy/preview1/docs.md#-timestamp-u64
type timestamp = uint64

// https://github.com/WebAssembly/WASI/blob/a2b96e81c0586125cc4dc79a5be0b78d9a059925/legacy/preview1/docs.md#-clockid-variant
type clockid = uint32

const (
	clockRealtime  clockid = 0
	clockMonotonic clockid = 1
)

// https://github.com/WebAssembly/WASI/blob/a2b96e81c0586125cc4dc79a5be0b78d9a059925/legacy/preview1/docs.md#-iovec-record
type iovec struct {
	buf    uintptr32
	bufLen size
}

//go:wasmimport wasi_snapshot_preview1 proc_exit
func exit(code int32)

//go:wasmimport wasi_snapshot_preview1 args_get
//go:noescape
func args_get(argv, argvBuf unsafe.Pointer) errno

//go:wasmimport wasi_snapshot_preview1 args_sizes_get
//go:noescape
func args_sizes_get(argc, argvBufLen unsafe.Pointer) errno

//go:wasmimport wasi_snapshot_preview1 clock_time_get
//go:noescape
func clock_time_get(clock_id clockid, precision timestamp, time unsafe.Pointer) errno

//go:wasmimport wasi_snapshot_preview1 environ_get
//go:noescape
func environ_get(environ, environBuf unsafe.Pointer) errno

//go:wasmimport wasi_snapshot_preview1 environ_sizes_get
//go:noescape
func environ_sizes_get(environCount, environBufLen unsafe.Pointer) errno

//go:wasmimport wasi_snapshot_preview1 fd_write
//go:noescape
func fd_write(fd int32, iovs unsafe.Pointer, iovsLen size, nwritten unsafe.Pointer) errno

//go:wasmimport wasi_snapshot_preview1 random_get
//go:noescape
func random_get(buf unsafe.Pointer, bufLen size) errno

type eventtype = uint8

const (
	eventtypeClock   eventtype = 0
	eventtypeFdRead  eventtype = 1
	eventtypeFdWrite eventtype = 2
)

type eventrwflags = uint16

const (
	fdReadwriteHangup eventrwflags = 1 << iota
)

type userdata = uint64

// The go:wasmimport directive currently does not accept values of type uint16
// in arguments or returns of the function signature. Most WASI imports return
// an errno value, which we have to define as uint32 because of that limitation.
// However, the WASI errno type is intended to be a 16 bits integer, and in the
// event struct the error field should be of type errno. If we used the errno
// type for the error field it would result in a mismatching field alignment and
// struct size because errno is declared as a 32 bits type, so we declare the
// error field as a plain uint16.
type event struct {
	userdata    userdata
	error       uint16
	typ         eventtype
	fdReadwrite eventFdReadwrite
}

type eventFdReadwrite struct {
	nbytes filesize
	flags  eventrwflags
}

type subclockflags = uint16

const (
	subscriptionClockAbstime subclockflags = 1 << iota
)

type subscription [48]byte

func (u *subscription) setUserData(ud uint64) {
	le.PutUint64(u[0:], ud)
}

func (u *subscription) userData() uint64 {
	return le.Uint64(u[0:])
}

func (u *subscription) setBody(t uint64) []byte {
	le.PutUint64(u[8:], t)
	return u[16:]
}

var le littleEndian

func (u *subscription) setClock(cid clockid, timeout, prec uint64, flags uint16) {
	body := u.setBody(uint64(eventtypeClock))
	le.PutUint32(body[0:], cid)

	// it's 8 because it's using C, non-packed, alignment. Timeout is a
	// 64bit quantity, so there is 4 padding bytes before it to keep it aligned
	// on 8 byte boundaries.
	le.PutUint64(body[8:], timeout)
	le.PutUint64(body[16:], prec)
	le.PutUint16(body[24:], flags)
}

func (u *subscription) setFDRead(fd uint32) {
	body := u.setBody(uint64(eventtypeFdRead))
	le.PutUint32(body, fd)
}

func (u *subscription) setFDWrite(fd uint32) {
	body := u.setBody(uint64(eventtypeFdWrite))
	le.PutUint32(body, fd)
}

//go:wasmimport wasi_snapshot_preview1 poll_oneoff
//go:noescape
func poll_oneoff(in, out unsafe.Pointer, nsubscriptions size, nevents unsafe.Pointer) errno

func write1(fd uintptr, p unsafe.Pointer, n int32) int32 {
	iov := iovec{
		buf:    uintptr32(uintptr(p)),
		bufLen: size(n),
	}
	var nwritten size
	if fd_write(int32(fd), unsafe.Pointer(&iov), 1, unsafe.Pointer(&nwritten)) != 0 {
		throw("fd_write failed")
	}
	return int32(nwritten)
}

func usleep(usec uint32) {

	var buf [128]byte

	var slice []byte

	if uintptr(unsafe.Pointer(&buf))%8 != 0 {
		slice = buf[4:]
	} else {
		slice = buf[:]
	}

	in := subscription(slice[:48])
	out := slice[48:]
	var nevents size

	in.setClock(clockMonotonic, timestamp(usec)*1e3, 1e3, 0)

	if poll_oneoff(unsafe.Pointer(&in[0]), unsafe.Pointer(&out[0]), 1, unsafe.Pointer(&nevents)) != 0 {
		throw("wasi_snapshot_preview1.poll_oneoff")
	}
}

func readRandom(r []byte) int {
	if random_get(unsafe.Pointer(&r[0]), size(len(r))) != 0 {
		return 0
	}
	return len(r)
}

func goenvs() {
	// arguments
	var argc size
	var argvBufLen size
	if args_sizes_get(unsafe.Pointer(&argc), unsafe.Pointer(&argvBufLen)) != 0 {
		throw("args_sizes_get failed")
	}

	argslice = make([]string, argc)
	if argc > 0 {
		argv := make([]uintptr32, argc)
		argvBuf := make([]byte, argvBufLen)
		if args_get(unsafe.Pointer(&argv[0]), unsafe.Pointer(&argvBuf[0])) != 0 {
			throw("args_get failed")
		}

		for i := range argslice {
			start := argv[i] - uintptr32(uintptr(unsafe.Pointer(&argvBuf[0])))
			end := start
			for argvBuf[end] != 0 {
				end++
			}
			argslice[i] = string(argvBuf[start:end])
		}
	}

	// environment
	var environCount size
	var environBufLen size
	if environ_sizes_get(unsafe.Pointer(&environCount), unsafe.Pointer(&environBufLen)) != 0 {
		throw("environ_sizes_get failed")
	}

	envs = make([]string, environCount)
	if environCount > 0 {
		environ := make([]uintptr32, environCount)
		environBuf := make([]byte, environBufLen)
		if environ_get(unsafe.Pointer(&environ[0]), unsafe.Pointer(&environBuf[0])) != 0 {
			throw("environ_get failed")
		}

		for i := range envs {
			start := environ[i] - uintptr32(uintptr(unsafe.Pointer(&environBuf[0])))
			end := start
			for environBuf[end] != 0 {
				end++
			}
			envs[i] = string(environBuf[start:end])
		}
	}
}

func walltime() (sec int64, nsec int32) {
	return walltime1()
}

func walltime1() (sec int64, nsec int32) {
	timePtr := tmpUint64_1()

	if clock_time_get(clockRealtime, 0, unsafe.Pointer(timePtr)) != 0 {
		throw("clock_time_get failed")
	}

	time := timestamp(*timePtr)
	return int64(time / 1000000000), int32(time % 1000000000)
}

func nanotime1() int64 {
	timePtr := tmpUint64_1()

	if clock_time_get(clockMonotonic, 0, unsafe.Pointer(timePtr)) != 0 {
		throw("clock_time_get failed")
	}

	return int64(*timePtr)
}

// This is a bit of a hack because wasi expects 8 byte aligned pointers
// when the return value is a uint64, but the wasm32 port doesn't adher
// to managing the stack at 8 byte alignment. So because we don't have
// real threads anyway, we effectively use a set of globals as 8 byte
// aligned pointers, knowing that (at present) there is no chance that
// another goroutine can be running and reuse the same pointer. Note that
// the pointers are grab, passed to WASI, then read in the same function
// always.
type tmpStackS struct {
	_ [1024]byte
}

var tmpStack tmpStackS

func tmpUint64_1() *uint64 {
	ptr := uintptr(unsafe.Pointer(&tmpStack))

	if ptr%8 != 0 {
		ptr += 8 - (ptr % 8)
	}

	return (*uint64)(unsafe.Pointer(ptr))
}

func tmpUint64_2() *uint64 {
	ptr := uintptr(unsafe.Pointer(&tmpStack))

	ptr += 16

	if ptr%8 != 0 {
		ptr += 8 - (ptr % 8)
	}

	return (*uint64)(unsafe.Pointer(ptr))
}

// This is a weird one. We use generics to allocate various configurations
// of T to find one that is allocated on an 8 byte boundary. This is wasteful
// but it allows the GC to track the returned value correctly so we don't have
// to use a pool of already aligned values.
func NewAligned[T any](t **T) {
	v0 := new(T)
	if uintptr(unsafe.Pointer(v0))%8 == 0 {
		*t = v0
		return
	}

	v2 := new(struct {
		_ [2]byte
		t T
	})
	if uintptr(unsafe.Pointer(&v2.t))%8 == 0 {
		*t = &v2.t
		return
	}

	v4 := new(struct {
		_ [4]byte
		t T
	})
	if uintptr(unsafe.Pointer(&v4.t))%8 == 0 {
		*t = &v4.t
		return
	}

	v6 := new(struct {
		_ [6]byte
		t T
	})
	if uintptr(unsafe.Pointer(&v6.t))%8 == 0 {
		*t = &v6.t
		return
	}

	v8 := new(struct {
		_ [8]byte
		t T
	})
	if uintptr(unsafe.Pointer(&v8.t))%8 == 0 {
		*t = &v8.t
		return
	}

	println("0ptr= ", uintptr(unsafe.Pointer(v0)))
	println("2ptr= ", uintptr(unsafe.Pointer(&v2.t)))
	println("4ptr= ", uintptr(unsafe.Pointer(&v4.t)))
	println("6ptr= ", uintptr(unsafe.Pointer(&v6.t)))
	println("8ptr= ", uintptr(unsafe.Pointer(&v8.t)))

	throw("failed to allocate aligned value")
}

type littleEndian struct{}

func (littleEndian) Uint16(b []byte) uint16 {
	_ = b[1] // bounds check hint to compiler; see golang.org/issue/14808
	return uint16(b[0]) | uint16(b[1])<<8
}

func (littleEndian) PutUint16(b []byte, v uint16) {
	_ = b[1] // early bounds check to guarantee safety of writes below
	b[0] = byte(v)
	b[1] = byte(v >> 8)
}

func (littleEndian) Uint32(b []byte) uint32 {
	_ = b[3] // bounds check hint to compiler; see golang.org/issue/14808
	return uint32(b[0]) | uint32(b[1])<<8 | uint32(b[2])<<16 | uint32(b[3])<<24
}

func (littleEndian) PutUint32(b []byte, v uint32) {
	_ = b[3] // early bounds check to guarantee safety of writes below
	b[0] = byte(v)
	b[1] = byte(v >> 8)
	b[2] = byte(v >> 16)
	b[3] = byte(v >> 24)
}

func (littleEndian) Uint64(b []byte) uint64 {
	_ = b[7] // bounds check hint to compiler; see golang.org/issue/14808
	return uint64(b[0]) | uint64(b[1])<<8 | uint64(b[2])<<16 | uint64(b[3])<<24 |
		uint64(b[4])<<32 | uint64(b[5])<<40 | uint64(b[6])<<48 | uint64(b[7])<<56
}

func (littleEndian) PutUint64(b []byte, v uint64) {
	_ = b[7] // early bounds check to guarantee safety of writes below
	b[0] = byte(v)
	b[1] = byte(v >> 8)
	b[2] = byte(v >> 16)
	b[3] = byte(v >> 24)
	b[4] = byte(v >> 32)
	b[5] = byte(v >> 40)
	b[6] = byte(v >> 48)
	b[7] = byte(v >> 56)
}
