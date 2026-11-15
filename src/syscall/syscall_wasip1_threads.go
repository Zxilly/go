// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasip1 && wasm.threads

package syscall

import "unsafe"

const wasmThreads = true

// WASI calls may block even when the descriptor was opened non-blocking.
// Make the P available for sysmon to reclaim and the goroutine's stack
// available to the GC while the host is executing. These wrappers must not
// split the stack between entering and exiting the syscall. The raw
// wasmimport functions do not split either.
//
// entersyscall records its caller's PC and SP, so call it directly rather than
// through another Go wrapper.
//
//go:linkname runtime_entersyscall runtime.entersyscall
func runtime_entersyscall()

//go:linkname runtime_exitsyscall runtime.exitsyscall
func runtime_exitsyscall()

//go:nosplit
func fd_close(fd int32) Errno {
	runtime_entersyscall()
	errno := raw_fd_close(fd)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_filestat_set_size(fd int32, set_size filesize) Errno {
	runtime_entersyscall()
	errno := raw_fd_filestat_set_size(fd, set_size)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_pread(fd int32, iovs *iovec, iovsLen size, offset filesize, nread *size) Errno {
	runtime_entersyscall()
	errno := raw_fd_pread(fd, iovs, iovsLen, offset, nread)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_pwrite(fd int32, iovs *iovec, iovsLen size, offset filesize, nwritten *size) Errno {
	runtime_entersyscall()
	errno := raw_fd_pwrite(fd, iovs, iovsLen, offset, nwritten)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_read(fd int32, iovs *iovec, iovsLen size, nread *size) Errno {
	runtime_entersyscall()
	errno := raw_fd_read(fd, iovs, iovsLen, nread)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_readdir(fd int32, buf *byte, bufLen size, cookie dircookie, nwritten *size) Errno {
	runtime_entersyscall()
	errno := raw_fd_readdir(fd, buf, bufLen, cookie, nwritten)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_seek(fd int32, offset filedelta, whence uint32, newoffset *filesize) Errno {
	runtime_entersyscall()
	errno := raw_fd_seek(fd, offset, whence, newoffset)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_fdstat_set_rights(fd int32, rightsBase rights, rightsInheriting rights) Errno {
	runtime_entersyscall()
	errno := raw_fd_fdstat_set_rights(fd, rightsBase, rightsInheriting)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_filestat_get(fd int32, buf unsafe.Pointer) Errno {
	runtime_entersyscall()
	errno := raw_fd_filestat_get(fd, buf)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_write(fd int32, iovs *iovec, iovsLen size, nwritten *size) Errno {
	runtime_entersyscall()
	errno := raw_fd_write(fd, iovs, iovsLen, nwritten)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_sync(fd int32) Errno {
	runtime_entersyscall()
	errno := raw_fd_sync(fd)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func path_create_directory(fd int32, path *byte, pathLen size) Errno {
	runtime_entersyscall()
	errno := raw_path_create_directory(fd, path, pathLen)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func path_filestat_get(fd int32, flags lookupflags, path *byte, pathLen size, buf unsafe.Pointer) Errno {
	runtime_entersyscall()
	errno := raw_path_filestat_get(fd, flags, path, pathLen, buf)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func path_filestat_set_times(fd int32, flags lookupflags, path *byte, pathLen size, atim timestamp, mtim timestamp, fstflags fstflags) Errno {
	runtime_entersyscall()
	errno := raw_path_filestat_set_times(fd, flags, path, pathLen, atim, mtim, fstflags)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func path_link(oldFd int32, oldFlags lookupflags, oldPath *byte, oldPathLen size, newFd int32, newPath *byte, newPathLen size) Errno {
	runtime_entersyscall()
	errno := raw_path_link(oldFd, oldFlags, oldPath, oldPathLen, newFd, newPath, newPathLen)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func path_readlink(fd int32, path *byte, pathLen size, buf *byte, bufLen size, nwritten *size) Errno {
	runtime_entersyscall()
	errno := raw_path_readlink(fd, path, pathLen, buf, bufLen, nwritten)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func path_remove_directory(fd int32, path *byte, pathLen size) Errno {
	runtime_entersyscall()
	errno := raw_path_remove_directory(fd, path, pathLen)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func path_rename(oldFd int32, oldPath *byte, oldPathLen size, newFd int32, newPath *byte, newPathLen size) Errno {
	runtime_entersyscall()
	errno := raw_path_rename(oldFd, oldPath, oldPathLen, newFd, newPath, newPathLen)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func path_symlink(oldPath *byte, oldPathLen size, fd int32, newPath *byte, newPathLen size) Errno {
	runtime_entersyscall()
	errno := raw_path_symlink(oldPath, oldPathLen, fd, newPath, newPathLen)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func path_unlink_file(fd int32, path *byte, pathLen size) Errno {
	runtime_entersyscall()
	errno := raw_path_unlink_file(fd, path, pathLen)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func path_open(rootFD int32, dirflags lookupflags, path *byte, pathLen size, oflags oflags, fsRightsBase rights, fsRightsInheriting rights, fsFlags fdflags, fd *int32) Errno {
	runtime_entersyscall()
	errno := raw_path_open(rootFD, dirflags, path, pathLen, oflags, fsRightsBase, fsRightsInheriting, fsFlags, fd)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_fdstat_get(fd int32, buf *fdstat) Errno {
	runtime_entersyscall()
	errno := raw_fd_fdstat_get(fd, buf)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_fdstat_set_flags(fd int32, flags fdflags) Errno {
	runtime_entersyscall()
	errno := raw_fd_fdstat_set_flags(fd, flags)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_prestat_get(fd int32, prestat *prestat) Errno {
	runtime_entersyscall()
	errno := raw_fd_prestat_get(fd, prestat)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func fd_prestat_dir_name(fd int32, path *byte, pathLen size) Errno {
	runtime_entersyscall()
	errno := raw_fd_prestat_dir_name(fd, path, pathLen)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func random_get(buf *byte, bufLen size) Errno {
	runtime_entersyscall()
	errno := raw_random_get(buf, bufLen)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func sock_accept(fd int32, flags fdflags, newfd *int32) Errno {
	runtime_entersyscall()
	errno := raw_sock_accept(fd, flags, newfd)
	runtime_exitsyscall()
	return errno
}

//go:nosplit
func sock_shutdown(fd int32, flags sdflags) Errno {
	runtime_entersyscall()
	errno := raw_sock_shutdown(fd, flags)
	runtime_exitsyscall()
	return errno
}
