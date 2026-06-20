// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

"use strict";

(() => {
	const enosys = () => {
		const err = new Error("not implemented");
		err.code = "ENOSYS";
		return err;
	};

	if (!globalThis.fs) {
		let outputBuf = "";
		globalThis.fs = {
			constants: { O_WRONLY: -1, O_RDWR: -1, O_CREAT: -1, O_TRUNC: -1, O_APPEND: -1, O_EXCL: -1, O_DIRECTORY: -1 }, // unused
			writeSync(fd, buf) {
				outputBuf += decoder.decode(buf);
				const nl = outputBuf.lastIndexOf("\n");
				if (nl != -1) {
					console.log(outputBuf.substring(0, nl));
					outputBuf = outputBuf.substring(nl + 1);
				}
				return buf.length;
			},
			write(fd, buf, offset, length, position, callback) {
				if (offset !== 0 || length !== buf.length || position !== null) {
					callback(enosys());
					return;
				}
				const n = this.writeSync(fd, buf);
				callback(null, n);
			},
			chmod(path, mode, callback) { callback(enosys()); },
			chown(path, uid, gid, callback) { callback(enosys()); },
			close(fd, callback) { callback(enosys()); },
			fchmod(fd, mode, callback) { callback(enosys()); },
			fchown(fd, uid, gid, callback) { callback(enosys()); },
			fstat(fd, callback) { callback(enosys()); },
			fsync(fd, callback) { callback(null); },
			ftruncate(fd, length, callback) { callback(enosys()); },
			lchown(path, uid, gid, callback) { callback(enosys()); },
			link(path, link, callback) { callback(enosys()); },
			lstat(path, callback) { callback(enosys()); },
			mkdir(path, perm, callback) { callback(enosys()); },
			open(path, flags, mode, callback) { callback(enosys()); },
			read(fd, buffer, offset, length, position, callback) { callback(enosys()); },
			readdir(path, callback) { callback(enosys()); },
			readlink(path, callback) { callback(enosys()); },
			rename(from, to, callback) { callback(enosys()); },
			rmdir(path, callback) { callback(enosys()); },
			stat(path, callback) { callback(enosys()); },
			symlink(path, link, callback) { callback(enosys()); },
			truncate(path, length, callback) { callback(enosys()); },
			unlink(path, callback) { callback(enosys()); },
			utimes(path, atime, mtime, callback) { callback(enosys()); },
		};
	}

	if (!globalThis.process) {
		globalThis.process = {
			getuid() { return -1; },
			getgid() { return -1; },
			geteuid() { return -1; },
			getegid() { return -1; },
			getgroups() { throw enosys(); },
			pid: -1,
			ppid: -1,
			umask() { throw enosys(); },
			cwd() { throw enosys(); },
			chdir() { throw enosys(); },
		}
	}

	if (!globalThis.path) {
		globalThis.path = {
			resolve(...pathSegments) {
				return pathSegments.join("/");
			}
		}
	}

	if (!globalThis.crypto) {
		throw new Error("globalThis.crypto is not available, polyfill required (crypto.getRandomValues only)");
	}

	if (!globalThis.performance) {
		throw new Error("globalThis.performance is not available, polyfill required (performance.now only)");
	}

	if (!globalThis.TextEncoder) {
		throw new Error("globalThis.TextEncoder is not available, polyfill required");
	}

	if (!globalThis.TextDecoder) {
		throw new Error("globalThis.TextDecoder is not available, polyfill required");
	}

	const encoder = new TextEncoder("utf-8");
	const decoder = new TextDecoder("utf-8");

	globalThis.Go = class {
		constructor(options) {
			const requestedPtrSize = options && options.ptrSize;
			if (requestedPtrSize !== undefined && requestedPtrSize !== 4 && requestedPtrSize !== 8) {
				throw new Error("Go: ptrSize must be 4 or 8");
			}
			this._ptrSize = 0; // selected from the module in run
			this.argv = ["js"];
			this.env = {};
			this.exit = (code) => {
				if (code !== 0) {
					console.warn("exit code:", code);
				}
			};
			this._exitPromise = new Promise((resolve) => {
				this._resolveExitPromise = resolve;
			});
			this._pendingEvent = null;
			this._scheduledTimeouts = new Map();
			this._nextCallbackTimeoutID = 1;

			const setInt64 = (addr, v) => {
				this.mem.setUint32(addr + 0, v, true);
				this.mem.setUint32(addr + 4, Math.floor(v / 4294967296), true);
			}

			const setInt32 = (addr, v) => {
				this.mem.setUint32(addr + 0, v, true);
			}

			const getInt64 = (addr) => {
				const low = this.mem.getUint32(addr + 0, true);
				const high = this.mem.getInt32(addr + 4, true);
				return low + high * 4294967296;
			}

			const getUint64 = (addr) => {
				const low = this.mem.getUint32(addr + 0, true);
				const high = this.mem.getUint32(addr + 4, true);
				return low + high * 4294967296;
			}

			const getUintPtr = (addr) => {
				if (this._ptrSize === 8) {
					return getUint64(addr);
				}
				return this.mem.getUint32(addr, true);
			}

			const getIntPtr = (addr) => {
				if (this._ptrSize === 8) {
					return getInt64(addr);
				}
				return this.mem.getInt32(addr, true);
			}

			const setIntPtr = (addr, v) => {
				if (this._ptrSize === 8) {
					setInt64(addr, v);
				} else {
					this.mem.setUint32(addr, v, true);
				}
			}

			const align = (n, a) => (n + a - 1) & ~(a - 1);

			// frame derives the sp-relative offsets of a host function's (args,
			// results) in its ABI0 stack frame. Args start at sp+8; results are
			// realigned to ptrSize. Field sizes: i32 (4); i64/f64/ref (8, ref is a
			// NaN-boxed js value); byte/bool (1); ptr/int/uint/uintptr (P); string
			// (2*P); slice (3*P).
			const frame = (P, args, results) => {
				const sizeAlign = (k) => {
					switch (k) {
						case "i32": return [4, 4];
						case "i64": case "f64": case "ref": return [8, 8];
						case "byte": case "bool": return [1, 1];
						case "ptr": case "int": case "uint": case "uintptr": return [P, P];
						case "string": return [2 * P, P];
						case "slice": return [3 * P, P];
						default: throw new Error("frame: unknown kind " + k);
					}
				};
				let off = 8;
				const offs = [];
				const place = (k) => {
					const [s, a] = sizeAlign(k);
					off = align(off, a);
					offs.push(off);
					off += s;
				};
				for (const k of args) place(k);
				off = align(off, P); // args/results boundary
				for (const k of results) place(k);
				return offs;
			}

			this._setPtrSize = (ptrSize) => {
				if (ptrSize !== 4 && ptrSize !== 8) {
					throw new Error("Go: module has invalid pointer size " + ptrSize);
				}
				if (requestedPtrSize !== undefined && requestedPtrSize !== ptrSize) {
					throw new Error("Go: configured ptrSize does not match WebAssembly module");
				}
				this._ptrSize = ptrSize;
				this._frames = {
					wasmWrite: frame(ptrSize, ["uintptr", "ptr", "i32"], []),
					stringVal: frame(ptrSize, ["string"], ["ref"]),
					valueGet: frame(ptrSize, ["ref", "string"], ["ref"]),
					valueSet: frame(ptrSize, ["ref", "string", "ref"], []),
					valueIndex: frame(ptrSize, ["ref", "int"], ["ref"]),
					valueSetIndex: frame(ptrSize, ["ref", "int", "ref"], []),
					valueCall: frame(ptrSize, ["ref", "string", "slice"], ["ref", "bool"]),
					valueInvoke: frame(ptrSize, ["ref", "slice"], ["ref", "bool"]),
					valueNew: frame(ptrSize, ["ref", "slice"], ["ref", "bool"]),
					valueLength: frame(ptrSize, ["ref"], ["int"]),
					valuePrepareString: frame(ptrSize, ["ref"], ["ref", "int"]),
					copyBytesToGo: frame(ptrSize, ["slice", "ref"], ["int", "bool"]),
					copyBytesToJS: frame(ptrSize, ["ref", "slice"], ["int", "bool"]),
				};
			};
			if (requestedPtrSize !== undefined) {
				this._setPtrSize(requestedPtrSize);
			}

			const loadValue = (addr) => {
				const f = this.mem.getFloat64(addr, true);
				if (f === 0) {
					return undefined;
				}
				if (!isNaN(f)) {
					return f;
				}

				const id = this.mem.getUint32(addr, true);
				return this._values[id];
			}

			const storeValue = (addr, v) => {
				const nanHead = 0x7FF80000;

				if (typeof v === "number" && v !== 0) {
					if (isNaN(v)) {
						this.mem.setUint32(addr + 4, nanHead, true);
						this.mem.setUint32(addr, 0, true);
						return;
					}
					this.mem.setFloat64(addr, v, true);
					return;
				}

				if (v === undefined) {
					this.mem.setFloat64(addr, 0, true);
					return;
				}

				let id = this._ids.get(v);
				if (id === undefined) {
					id = this._idPool.pop();
					if (id === undefined) {
						id = this._values.length;
					}
					this._values[id] = v;
					this._goRefCounts[id] = 0;
					this._ids.set(v, id);
				}
				this._goRefCounts[id]++;
				let typeFlag = 0;
				switch (typeof v) {
					case "object":
						if (v !== null) {
							typeFlag = 1;
						}
						break;
					case "string":
						typeFlag = 2;
						break;
					case "symbol":
						typeFlag = 3;
						break;
					case "function":
						typeFlag = 4;
						break;
				}
				this.mem.setUint32(addr + 4, nanHead | typeFlag, true);
				this.mem.setUint32(addr, id, true);
			}

			const loadSlice = (addr) => {
				const array = getUintPtr(addr + 0);
				const len = getUintPtr(addr + this._ptrSize);
				return new Uint8Array(this._inst.exports.mem.buffer, array, len);
			}

			const loadSliceOfValues = (addr) => {
				const array = getUintPtr(addr + 0);
				const len = getUintPtr(addr + this._ptrSize);
				const a = new Array(len);
				for (let i = 0; i < len; i++) {
					// Each element is a ref, always 8 bytes.
					a[i] = loadValue(array + i * 8);
				}
				return a;
			}

			const loadString = (addr) => {
				const saddr = getUintPtr(addr + 0);
				const len = getUintPtr(addr + this._ptrSize);
				return decoder.decode(new DataView(this._inst.exports.mem.buffer, saddr, len));
			}

			const testCallExport = (a, b) => {
				this._inst.exports.testExport0();
				return this._inst.exports.testExport(a, b);
			}

			const timeOrigin = Date.now() - performance.now();
			this.importObject = {
				_gotest: {
					add: (a, b) => a + b,
					callExport: testCallExport,
				},
				gojs: {
					// Go's SP does not change as long as no Go code is running. Some operations (e.g. calls, getters and setters)
					// may synchronously trigger a Go event handler. This makes Go code get executed in the middle of the imported
					// function. A goroutine can switch to a new stack if the current stack is too small (see morestack function).
					// This changes the SP, thus we have to update the SP used by the imported function.

					// func wasmExit(code int32)
					"runtime.wasmExit": (sp) => {
						sp >>>= 0;
						const code = this.mem.getInt32(sp + 8, true);
						this.exited = true;
						delete this._inst;
						delete this._values;
						delete this._goRefCounts;
						delete this._ids;
						delete this._idPool;
						this.exit(code);
					},

					// func wasmWrite(fd uintptr, p unsafe.Pointer, n int32)
					"runtime.wasmWrite": (sp) => {
						sp >>>= 0;
						// frame(["uintptr", "ptr", "i32"], [])
						const [fdOff, pOff, nOff] = this._frames.wasmWrite;
						const fd = getUintPtr(sp + fdOff);
						const p = getUintPtr(sp + pOff);
						const n = this.mem.getInt32(sp + nOff, true);
						fs.writeSync(fd, new Uint8Array(this._inst.exports.mem.buffer, p, n));
					},

					// func resetMemoryDataView()
					"runtime.resetMemoryDataView": (sp) => {
						sp >>>= 0;
						this.mem = new DataView(this._inst.exports.mem.buffer);
					},

					// func nanotime1() int64
					"runtime.nanotime1": (sp) => {
						sp >>>= 0;
						setInt64(sp + 8, (timeOrigin + performance.now()) * 1000000);
					},

					// func walltime() (sec int64, nsec int32)
					"runtime.walltime": (sp) => {
						sp >>>= 0;
						const msec = (new Date).getTime();
						setInt64(sp + 8, msec / 1000);
						this.mem.setInt32(sp + 16, (msec % 1000) * 1000000, true);
					},

					// func scheduleTimeoutEvent(delay int64) int32
					"runtime.scheduleTimeoutEvent": (sp) => {
						sp >>>= 0;
						const id = this._nextCallbackTimeoutID;
						this._nextCallbackTimeoutID++;
						this._scheduledTimeouts.set(id, setTimeout(
							() => {
								this._resume();
								while (this._scheduledTimeouts.has(id)) {
									// for some reason Go failed to register the timeout event, log and try again
									// (temporary workaround for https://github.com/golang/go/issues/28975)
									console.warn("scheduleTimeoutEvent: missed timeout event");
									this._resume();
								}
							},
							getInt64(sp + 8),
						));
						this.mem.setInt32(sp + 16, id, true);
					},

					// func clearTimeoutEvent(id int32)
					"runtime.clearTimeoutEvent": (sp) => {
						sp >>>= 0;
						const id = this.mem.getInt32(sp + 8, true);
						clearTimeout(this._scheduledTimeouts.get(id));
						this._scheduledTimeouts.delete(id);
					},

					// func getRandomData(r []byte)
					"runtime.getRandomData": (sp) => {
						sp >>>= 0;
						crypto.getRandomValues(loadSlice(sp + 8));
					},

					// func finalizeRef(v ref)
					"syscall/js.finalizeRef": (sp) => {
						sp >>>= 0;
						const id = this.mem.getUint32(sp + 8, true);
						this._goRefCounts[id]--;
						if (this._goRefCounts[id] === 0) {
							const v = this._values[id];
							this._values[id] = null;
							this._ids.delete(v);
							this._idPool.push(id);
						}
					},

					// func stringVal(value string) ref
					"syscall/js.stringVal": (sp) => {
						sp >>>= 0;
						// frame(["string"], ["ref"])
						const [valueOff, retOff] = this._frames.stringVal;
						storeValue(sp + retOff, loadString(sp + valueOff));
					},

					// func valueGet(v ref, p string) ref
					"syscall/js.valueGet": (sp) => {
						sp >>>= 0;
						// frame(["ref", "string"], ["ref"])
						const [vOff, pOff, retOff] = this._frames.valueGet;
						const result = Reflect.get(loadValue(sp + vOff), loadString(sp + pOff));
						sp = this._inst.exports.getsp() >>> 0; // see comment above
						storeValue(sp + retOff, result);
					},

					// func valueSet(v ref, p string, x ref)
					"syscall/js.valueSet": (sp) => {
						sp >>>= 0;
						// frame(["ref", "string", "ref"], [])
						const [vOff, pOff, xOff] = this._frames.valueSet;
						Reflect.set(loadValue(sp + vOff), loadString(sp + pOff), loadValue(sp + xOff));
					},

					// func valueDelete(v ref, p string)
					"syscall/js.valueDelete": (sp) => {
						sp >>>= 0;
						Reflect.deleteProperty(loadValue(sp + 8), loadString(sp + 16));
					},

					// func valueIndex(v ref, i int) ref
					"syscall/js.valueIndex": (sp) => {
						sp >>>= 0;
						// frame(["ref", "int"], ["ref"])
						const [vOff, iOff, retOff] = this._frames.valueIndex;
						storeValue(sp + retOff, Reflect.get(loadValue(sp + vOff), getIntPtr(sp + iOff)));
					},

					// valueSetIndex(v ref, i int, x ref)
					"syscall/js.valueSetIndex": (sp) => {
						sp >>>= 0;
						// frame(["ref", "int", "ref"], [])
						const [vOff, iOff, xOff] = this._frames.valueSetIndex;
						Reflect.set(loadValue(sp + vOff), getIntPtr(sp + iOff), loadValue(sp + xOff));
					},

					// func valueCall(v ref, m string, args []ref) (ref, bool)
					"syscall/js.valueCall": (sp) => {
						sp >>>= 0;
						// frame(["ref", "string", "slice"], ["ref", "bool"])
						const [vOff, mOff, argsOff, retOff, okOff] = this._frames.valueCall;
						try {
							const v = loadValue(sp + vOff);
							const m = Reflect.get(v, loadString(sp + mOff));
							const args = loadSliceOfValues(sp + argsOff);
							const result = Reflect.apply(m, v, args);
							sp = this._inst.exports.getsp() >>> 0; // see comment above
							storeValue(sp + retOff, result);
							this.mem.setUint8(sp + okOff, 1);
						} catch (err) {
							sp = this._inst.exports.getsp() >>> 0; // see comment above
							storeValue(sp + retOff, err);
							this.mem.setUint8(sp + okOff, 0);
						}
					},

					// func valueInvoke(v ref, args []ref) (ref, bool)
					"syscall/js.valueInvoke": (sp) => {
						sp >>>= 0;
						// frame(["ref", "slice"], ["ref", "bool"])
						const [vOff, argsOff, retOff, okOff] = this._frames.valueInvoke;
						try {
							const v = loadValue(sp + vOff);
							const args = loadSliceOfValues(sp + argsOff);
							const result = Reflect.apply(v, undefined, args);
							sp = this._inst.exports.getsp() >>> 0; // see comment above
							storeValue(sp + retOff, result);
							this.mem.setUint8(sp + okOff, 1);
						} catch (err) {
							sp = this._inst.exports.getsp() >>> 0; // see comment above
							storeValue(sp + retOff, err);
							this.mem.setUint8(sp + okOff, 0);
						}
					},

					// func valueNew(v ref, args []ref) (ref, bool)
					"syscall/js.valueNew": (sp) => {
						sp >>>= 0;
						// frame(["ref", "slice"], ["ref", "bool"])
						const [vOff, argsOff, retOff, okOff] = this._frames.valueNew;
						try {
							const v = loadValue(sp + vOff);
							const args = loadSliceOfValues(sp + argsOff);
							const result = Reflect.construct(v, args);
							sp = this._inst.exports.getsp() >>> 0; // see comment above
							storeValue(sp + retOff, result);
							this.mem.setUint8(sp + okOff, 1);
						} catch (err) {
							sp = this._inst.exports.getsp() >>> 0; // see comment above
							storeValue(sp + retOff, err);
							this.mem.setUint8(sp + okOff, 0);
						}
					},

					// func valueLength(v ref) int
					"syscall/js.valueLength": (sp) => {
						sp >>>= 0;
						// frame(["ref"], ["int"])
						const [vOff, retOff] = this._frames.valueLength;
						setIntPtr(sp + retOff, parseInt(loadValue(sp + vOff).length));
					},

					// valuePrepareString(v ref) (ref, int)
					"syscall/js.valuePrepareString": (sp) => {
						sp >>>= 0;
						// frame(["ref"], ["ref", "int"])
						const [vOff, strOff, lenOff] = this._frames.valuePrepareString;
						const str = encoder.encode(String(loadValue(sp + vOff)));
						storeValue(sp + strOff, str);
						setIntPtr(sp + lenOff, str.length);
					},

					// valueLoadString(v ref, b []byte)
					"syscall/js.valueLoadString": (sp) => {
						sp >>>= 0;
						const str = loadValue(sp + 8);
						loadSlice(sp + 16).set(str);
					},

					// func valueInstanceOf(v ref, t ref) bool
					"syscall/js.valueInstanceOf": (sp) => {
						sp >>>= 0;
						this.mem.setUint8(sp + 24, (loadValue(sp + 8) instanceof loadValue(sp + 16)) ? 1 : 0);
					},

					// func copyBytesToGo(dst []byte, src ref) (int, bool)
					"syscall/js.copyBytesToGo": (sp) => {
						sp >>>= 0;
						// frame(["slice", "ref"], ["int", "bool"])
						const [dstOff, srcOff, nOff, okOff] = this._frames.copyBytesToGo;
						const dst = loadSlice(sp + dstOff);
						const src = loadValue(sp + srcOff);
						if (!(src instanceof Uint8Array || src instanceof Uint8ClampedArray)) {
							this.mem.setUint8(sp + okOff, 0);
							return;
						}
						const toCopy = src.subarray(0, dst.length);
						dst.set(toCopy);
						setIntPtr(sp + nOff, toCopy.length);
						this.mem.setUint8(sp + okOff, 1);
					},

					// func copyBytesToJS(dst ref, src []byte) (int, bool)
					"syscall/js.copyBytesToJS": (sp) => {
						sp >>>= 0;
						// frame(["ref", "slice"], ["int", "bool"])
						const [dstOff, srcOff, nOff, okOff] = this._frames.copyBytesToJS;
						const dst = loadValue(sp + dstOff);
						const src = loadSlice(sp + srcOff);
						if (!(dst instanceof Uint8Array || dst instanceof Uint8ClampedArray)) {
							this.mem.setUint8(sp + okOff, 0);
							return;
						}
						const toCopy = src.subarray(0, dst.length);
						dst.set(toCopy);
						setIntPtr(sp + nOff, toCopy.length);
						this.mem.setUint8(sp + okOff, 1);
					},

					"debug": (value) => {
						console.log(value);
					},
				}
			};
		}

		async run(instance) {
			if (!(instance instanceof WebAssembly.Instance)) {
				throw new Error("Go.run: WebAssembly.Instance expected");
			}
			this._inst = instance;
			const ptrSizeExport = this._inst.exports["go:ptrsize"];
			if (ptrSizeExport !== undefined) {
				const ptrSize = typeof ptrSizeExport === "object" ? ptrSizeExport.value : ptrSizeExport;
				this._setPtrSize(Number(ptrSize));
			} else if (this._ptrSize === 0) {
				// Compatibility with modules produced by older toolchains.
				this._setPtrSize(8);
			}
			this.mem = new DataView(this._inst.exports.mem.buffer);
			this._values = [ // JS values that Go currently has references to, indexed by reference id
				NaN,
				0,
				null,
				true,
				false,
				globalThis,
				this,
			];
			this._goRefCounts = new Array(this._values.length).fill(Infinity); // number of references that Go has to a JS value, indexed by reference id
			this._ids = new Map([ // mapping from JS values to reference ids
				[0, 1],
				[null, 2],
				[true, 3],
				[false, 4],
				[globalThis, 5],
				[this, 6],
			]);
			this._idPool = [];   // unused ids that have been garbage collected
			this.exited = false; // whether the Go program has exited

			// Pass command line arguments and environment variables to WebAssembly by writing them to the linear memory.
			let offset = 4096;

			const strPtr = (str) => {
				const ptr = offset;
				const bytes = encoder.encode(str + "\0");
				new Uint8Array(this.mem.buffer, offset, bytes.length).set(bytes);
				offset += bytes.length;
				if (offset % 8 !== 0) {
					offset += 8 - (offset % 8);
				}
				return ptr;
			};

			const argc = this.argv.length;

			const argvPtrs = [];
			this.argv.forEach((arg) => {
				argvPtrs.push(strPtr(arg));
			});
			argvPtrs.push(0);

			const keys = Object.keys(this.env).sort();
			keys.forEach((key) => {
				argvPtrs.push(strPtr(`${key}=${this.env[key]}`));
			});
			argvPtrs.push(0);

			const argv = offset;
			argvPtrs.forEach((ptr) => {
				this.mem.setUint32(offset, ptr, true);
				if (this._ptrSize === 8) {
					this.mem.setUint32(offset + 4, 0, true);
				}
				offset += this._ptrSize;
			});

			// The linker guarantees global data starts from at least wasmMinDataAddr.
			// Keep in sync with cmd/link/internal/ld/data.go:wasmMinDataAddr.
			const wasmMinDataAddr = 4096 + 8192;
			if (offset >= wasmMinDataAddr) {
				throw new Error("total length of command line and environment variables exceeds limit");
			}

			this._inst.exports.run(argc, argv);
			if (this.exited) {
				this._resolveExitPromise();
			}
			await this._exitPromise;
		}

		_resume() {
			if (this.exited) {
				throw new Error("Go program has already exited");
			}
			this._inst.exports.resume();
			if (this.exited) {
				this._resolveExitPromise();
			}
		}

		_makeFuncWrapper(id) {
			const go = this;
			return function () {
				const event = { id: id, this: this, args: arguments };
				go._pendingEvent = event;
				go._resume();
				return event.result;
			};
		}
	}
})();
