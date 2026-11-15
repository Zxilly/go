// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasip1 && !wasm.threads

package syscall

import "unsafe"

const wasmThreads = false

// The default WebAssembly port does not hand off its execution thread during
// a host call. These forwarding wrappers inline to the original wasm imports.

func fd_close(fd int32) Errno {
	return raw_fd_close(fd)
}

func fd_filestat_set_size(fd int32, set_size filesize) Errno {
	return raw_fd_filestat_set_size(fd, set_size)
}

func fd_pread(fd int32, iovs *iovec, iovsLen size, offset filesize, nread *size) Errno {
	return raw_fd_pread(fd, iovs, iovsLen, offset, nread)
}

func fd_pwrite(fd int32, iovs *iovec, iovsLen size, offset filesize, nwritten *size) Errno {
	return raw_fd_pwrite(fd, iovs, iovsLen, offset, nwritten)
}

func fd_read(fd int32, iovs *iovec, iovsLen size, nread *size) Errno {
	return raw_fd_read(fd, iovs, iovsLen, nread)
}

func fd_readdir(fd int32, buf *byte, bufLen size, cookie dircookie, nwritten *size) Errno {
	return raw_fd_readdir(fd, buf, bufLen, cookie, nwritten)
}

func fd_seek(fd int32, offset filedelta, whence uint32, newoffset *filesize) Errno {
	return raw_fd_seek(fd, offset, whence, newoffset)
}

func fd_fdstat_set_rights(fd int32, rightsBase rights, rightsInheriting rights) Errno {
	return raw_fd_fdstat_set_rights(fd, rightsBase, rightsInheriting)
}

func fd_filestat_get(fd int32, buf unsafe.Pointer) Errno {
	return raw_fd_filestat_get(fd, buf)
}

func fd_write(fd int32, iovs *iovec, iovsLen size, nwritten *size) Errno {
	return raw_fd_write(fd, iovs, iovsLen, nwritten)
}

func fd_sync(fd int32) Errno {
	return raw_fd_sync(fd)
}

func path_create_directory(fd int32, path *byte, pathLen size) Errno {
	return raw_path_create_directory(fd, path, pathLen)
}

func path_filestat_get(fd int32, flags lookupflags, path *byte, pathLen size, buf unsafe.Pointer) Errno {
	return raw_path_filestat_get(fd, flags, path, pathLen, buf)
}

func path_filestat_set_times(fd int32, flags lookupflags, path *byte, pathLen size, atim timestamp, mtim timestamp, fstflags fstflags) Errno {
	return raw_path_filestat_set_times(fd, flags, path, pathLen, atim, mtim, fstflags)
}

func path_link(oldFd int32, oldFlags lookupflags, oldPath *byte, oldPathLen size, newFd int32, newPath *byte, newPathLen size) Errno {
	return raw_path_link(oldFd, oldFlags, oldPath, oldPathLen, newFd, newPath, newPathLen)
}

func path_readlink(fd int32, path *byte, pathLen size, buf *byte, bufLen size, nwritten *size) Errno {
	return raw_path_readlink(fd, path, pathLen, buf, bufLen, nwritten)
}

func path_remove_directory(fd int32, path *byte, pathLen size) Errno {
	return raw_path_remove_directory(fd, path, pathLen)
}

func path_rename(oldFd int32, oldPath *byte, oldPathLen size, newFd int32, newPath *byte, newPathLen size) Errno {
	return raw_path_rename(oldFd, oldPath, oldPathLen, newFd, newPath, newPathLen)
}

func path_symlink(oldPath *byte, oldPathLen size, fd int32, newPath *byte, newPathLen size) Errno {
	return raw_path_symlink(oldPath, oldPathLen, fd, newPath, newPathLen)
}

func path_unlink_file(fd int32, path *byte, pathLen size) Errno {
	return raw_path_unlink_file(fd, path, pathLen)
}

func path_open(rootFD int32, dirflags lookupflags, path *byte, pathLen size, oflags oflags, fsRightsBase rights, fsRightsInheriting rights, fsFlags fdflags, fd *int32) Errno {
	return raw_path_open(rootFD, dirflags, path, pathLen, oflags, fsRightsBase, fsRightsInheriting, fsFlags, fd)
}

func fd_fdstat_get(fd int32, buf *fdstat) Errno {
	return raw_fd_fdstat_get(fd, buf)
}

func fd_fdstat_set_flags(fd int32, flags fdflags) Errno {
	return raw_fd_fdstat_set_flags(fd, flags)
}

func fd_prestat_get(fd int32, prestat *prestat) Errno {
	return raw_fd_prestat_get(fd, prestat)
}

func fd_prestat_dir_name(fd int32, path *byte, pathLen size) Errno {
	return raw_fd_prestat_dir_name(fd, path, pathLen)
}

func random_get(buf *byte, bufLen size) Errno {
	return raw_random_get(buf, bufLen)
}

func sock_accept(fd int32, flags fdflags, newfd *int32) Errno {
	return raw_sock_accept(fd, flags, newfd)
}

func sock_shutdown(fd int32, flags sdflags) Errno {
	return raw_sock_shutdown(fd, flags)
}
