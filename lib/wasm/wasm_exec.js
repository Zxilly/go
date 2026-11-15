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
				if (typeof SharedArrayBuffer !== "undefined" && buf.buffer instanceof SharedArrayBuffer) {
					buf = buf.slice();
				}
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
	const wasmExecURL = typeof document !== "undefined" && document.currentScript ? document.currentScript.src : undefined;

	globalThis.Go = class {
		constructor() {
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
				const array = getInt64(addr + 0);
				const len = getInt64(addr + 8);
				return new Uint8Array(this._inst.exports.mem.buffer, array, len);
			}

			const loadSliceOfValues = (addr) => {
				const array = getInt64(addr + 0);
				const len = getInt64(addr + 8);
				const a = new Array(len);
				for (let i = 0; i < len; i++) {
					a[i] = loadValue(array + i * 8);
				}
				return a;
			}

			const loadString = (addr) => {
				const saddr = getInt64(addr + 0);
				const len = getInt64(addr + 8);
				const bytes = new Uint8Array(this._inst.exports.mem.buffer, saddr, len);
				return decoder.decode(this._threadState ? bytes.slice() : bytes);
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
						this._finishExit(code);
					},

					// func wasmWrite(fd uintptr, p unsafe.Pointer, n int32)
					"runtime.wasmWrite": (sp) => {
						sp >>>= 0;
						const fd = getInt64(sp + 8);
						const p = getInt64(sp + 16);
						const n = this.mem.getInt32(sp + 24, true);
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
								if (this.exited || !this._resume()) {
									return;
								}
								while (this._scheduledTimeouts.has(id)) {
									// for some reason Go failed to register the timeout event, log and try again
									// (temporary workaround for https://github.com/golang/go/issues/28975)
									console.warn("scheduleTimeoutEvent: missed timeout event");
									if (this.exited || !this._resume()) {
										return;
									}
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
						storeValue(sp + 24, loadString(sp + 8));
					},

					// func valueGet(v ref, p string) ref
					"syscall/js.valueGet": (sp) => {
						sp >>>= 0;
						const result = Reflect.get(loadValue(sp + 8), loadString(sp + 16));
						sp = (this._proxySP ?? this._inst.exports.getsp()) >>> 0; // see comment above
						storeValue(sp + 32, result);
					},

					// func valueSet(v ref, p string, x ref)
					"syscall/js.valueSet": (sp) => {
						sp >>>= 0;
						Reflect.set(loadValue(sp + 8), loadString(sp + 16), loadValue(sp + 32));
					},

					// func valueDelete(v ref, p string)
					"syscall/js.valueDelete": (sp) => {
						sp >>>= 0;
						Reflect.deleteProperty(loadValue(sp + 8), loadString(sp + 16));
					},

					// func valueIndex(v ref, i int) ref
					"syscall/js.valueIndex": (sp) => {
						sp >>>= 0;
						storeValue(sp + 24, Reflect.get(loadValue(sp + 8), getInt64(sp + 16)));
					},

					// valueSetIndex(v ref, i int, x ref)
					"syscall/js.valueSetIndex": (sp) => {
						sp >>>= 0;
						Reflect.set(loadValue(sp + 8), getInt64(sp + 16), loadValue(sp + 24));
					},

					// func valueCall(v ref, m string, args []ref) (ref, bool)
					"syscall/js.valueCall": (sp) => {
						sp >>>= 0;
						try {
							const v = loadValue(sp + 8);
							const m = Reflect.get(v, loadString(sp + 16));
							const args = loadSliceOfValues(sp + 32);
							const result = Reflect.apply(m, v, args);
							sp = (this._proxySP ?? this._inst.exports.getsp()) >>> 0; // see comment above
							storeValue(sp + 56, result);
							this.mem.setUint8(sp + 64, 1);
						} catch (err) {
							sp = (this._proxySP ?? this._inst.exports.getsp()) >>> 0; // see comment above
							storeValue(sp + 56, err);
							this.mem.setUint8(sp + 64, 0);
						}
					},

					// func valueInvoke(v ref, args []ref) (ref, bool)
					"syscall/js.valueInvoke": (sp) => {
						sp >>>= 0;
						try {
							const v = loadValue(sp + 8);
							const args = loadSliceOfValues(sp + 16);
							const result = Reflect.apply(v, undefined, args);
							sp = (this._proxySP ?? this._inst.exports.getsp()) >>> 0; // see comment above
							storeValue(sp + 40, result);
							this.mem.setUint8(sp + 48, 1);
						} catch (err) {
							sp = (this._proxySP ?? this._inst.exports.getsp()) >>> 0; // see comment above
							storeValue(sp + 40, err);
							this.mem.setUint8(sp + 48, 0);
						}
					},

					// func valueNew(v ref, args []ref) (ref, bool)
					"syscall/js.valueNew": (sp) => {
						sp >>>= 0;
						try {
							const v = loadValue(sp + 8);
							const args = loadSliceOfValues(sp + 16);
							const result = Reflect.construct(v, args);
							sp = (this._proxySP ?? this._inst.exports.getsp()) >>> 0; // see comment above
							storeValue(sp + 40, result);
							this.mem.setUint8(sp + 48, 1);
						} catch (err) {
							sp = (this._proxySP ?? this._inst.exports.getsp()) >>> 0; // see comment above
							storeValue(sp + 40, err);
							this.mem.setUint8(sp + 48, 0);
						}
					},

					// func valueLength(v ref) int
					"syscall/js.valueLength": (sp) => {
						sp >>>= 0;
						setInt64(sp + 16, parseInt(loadValue(sp + 8).length));
					},

					// valuePrepareString(v ref) (ref, int)
					"syscall/js.valuePrepareString": (sp) => {
						sp >>>= 0;
						const str = encoder.encode(String(loadValue(sp + 8)));
						storeValue(sp + 16, str);
						setInt64(sp + 24, str.length);
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
						const dst = loadSlice(sp + 8);
						const src = loadValue(sp + 32);
						if (!(src instanceof Uint8Array || src instanceof Uint8ClampedArray)) {
							this.mem.setUint8(sp + 48, 0);
							return;
						}
						const toCopy = src.subarray(0, dst.length);
						dst.set(toCopy);
						setInt64(sp + 40, toCopy.length);
						this.mem.setUint8(sp + 48, 1);
					},

					// func copyBytesToJS(dst ref, src []byte) (int, bool)
					"syscall/js.copyBytesToJS": (sp) => {
						sp >>>= 0;
						const dst = loadValue(sp + 8);
						const src = loadSlice(sp + 16);
						if (!(dst instanceof Uint8Array || dst instanceof Uint8ClampedArray)) {
							this.mem.setUint8(sp + 48, 0);
							return;
						}
						const toCopy = src.subarray(0, dst.length);
						dst.set(toCopy);
						setInt64(sp + 40, toCopy.length);
						this.mem.setUint8(sp + 48, 1);
					},

					"debug": (value) => {
						console.log(value);
					},
				}
			};
		}

		// The control array has eight words per Worker, followed by a shutdown flag.
		// Each slot contains the thread state, start argument, RPC state, operation,
		// and caller SP; the remaining three words are reserved. Thread states are
		// idle (0), reserved (1), running (2), and stopped (-1). RPC states are idle
		// (0), pending (1), complete (2), failed (3), executing (4), and stopped (5).
		// Keep this layout in sync with wasm_exec_worker.js.
		//
		// The main JavaScript thread owns all syscall/js references. Workers pass
		// operation numbers and pointers into shared Go memory, not JavaScript
		// objects. A worker's stack is pinned until its RPC completes.
		_installThreads(state, slot = -1, post = null) {
			this._threadState = state;
			this._threadShutdown = new Error("Go threads stopped");
			const control = new Int32Array(state.control);
			const stride = 8;
			const imports = this.importObject.gojs;
			const names = Object.keys(imports).filter(name => name.startsWith("syscall/js.")).sort();
			this._threadOperations = names.map(name => imports[name]);
			const refresh = () => {
				this.mem = new DataView(state.memory.buffer);
			};
			const spawn = (arg) => {
				if (Atomics.load(control, state.workers * stride) !== 0) {
					throw this._threadShutdown;
				}
				for (let i = 0; i < state.workers; i++) {
					const base = i * stride;
					if (Atomics.compareExchange(control, base, 0, 1) !== 0) {
						continue;
					}
					Atomics.store(control, base + 1, arg);
					Atomics.store(control, base, 2);
					Atomics.notify(control, base);
					return i + 1;
				}
				return -11; // EAGAIN: all preinitialized workers are in use.
			};
			imports["runtime.wasmThreadSpawn"] = (sp) => {
				refresh();
				this.mem.setInt32((sp >>> 0) + 16, spawn(this.mem.getUint32((sp >>> 0) + 8, true)), true);
			};
			imports["runtime.wasmThreadPoll"] = () => {
				if (slot < 0) {
					this._drainThreadCalls();
				}
			};
			imports["runtime.wasmThreadWait"] = (sp) => {
				if (Atomics.load(control, state.workers * stride) !== 0) {
					throw this._threadShutdown;
				}
				refresh();
				sp >>>= 0;
				const addr = Number(this.mem.getBigUint64(sp + 8, true));
				const val = this.mem.getInt32(sp + 16, true);
				const ns = Number(this.mem.getBigInt64(sp + 24, true));
				const words = new Int32Array(state.memory.buffer);
				let result = 0;
				if (slot >= 0) {
					result = { ok: 0, "not-equal": 1, "timed-out": 2 }[Atomics.wait(words, addr / 4, val, ns < 0 ? Infinity : ns / 1e6)];
				} else if (Atomics.load(words, addr / 4) !== val) {
					result = 1;
				} else {
					// Browsers forbid Atomics.wait on the main agent. This import can
					// run on g0 with runtime locks held: do not execute proxy callbacks.
					// A negative timeout waits until the word changes or Go exits.
					const end = ns < 0 ? Infinity : performance.now() + ns / 1e6;
					while (Atomics.load(words, addr / 4) === val) {
						if (Atomics.load(control, state.workers * stride) !== 0) {
							throw this._threadShutdown;
						}
						if (performance.now() >= end) {
							result = 2;
							break;
						}
					}
				}
				refresh();
				this.mem.setUint32(sp + 32, result, true);
			};
			if (slot >= 0) {
				// Runtime output and termination can occur with runtime locks held.
				// Never wait for the JavaScript owner here. Messages from this worker
				// are ordered, so termination cannot overtake its diagnostic output.
				imports["runtime.wasmWrite"] = (sp) => {
					sp >>>= 0;
					const fd = Number(this.mem.getBigUint64(sp + 8, true));
					const addr = Number(this.mem.getBigUint64(sp + 16, true));
					const len = this.mem.getInt32(sp + 24, true);
					post({ type: "write", fd, bytes: new Uint8Array(state.memory.buffer, addr, len).slice() });
				};
				this._threadExit = new Error("Go worker exited");
				imports["runtime.wasmExit"] = (sp) => {
					post({ type: "exit", code: this.mem.getInt32((sp >>> 0) + 8, true) });
					this._signalThreadStop(1);
					throw this._threadExit;
				};
				const base = slot * stride;
				for (let op = 0; op < names.length; op++) {
					imports[names[op]] = (sp) => {
						Atomics.store(control, base + 3, op);
						Atomics.store(control, base + 4, sp);
						Atomics.store(control, base + 2, 1);
						post({ type: "rpc" });
						for (;;) {
							if (Atomics.load(control, state.workers * stride) !== 0) {
								throw this._threadShutdown;
							}
							const status = Atomics.load(control, base + 2);
							if (status === 5) {
								throw this._threadShutdown;
							}
							if (status === 2 || status === 3) {
								break;
							}
							Atomics.wait(control, base + 2, status);
						}
						if (Atomics.load(control, base + 2) === 3) {
							throw new Error("Go thread host operation failed");
						}
						Atomics.store(control, base + 2, 0);
					};
				}
			}
			imports["runtime.getRandomData"] = (sp) => {
				refresh();
				sp >>>= 0;
				const addr = Number(this.mem.getBigUint64(sp + 8, true));
				const len = Number(this.mem.getBigUint64(sp + 16, true));
				const bytes = new Uint8Array(len);
				crypto.getRandomValues(bytes);
				new Uint8Array(state.memory.buffer, addr, len).set(bytes);
			};
			imports["runtime.nanotime1"] = (sp) => {
				// Workers have different time origins. Convert each local clock to
				// the same time coordinate before returning it to the Go scheduler.
				const ns = (performance.timeOrigin - state.timeOrigin + performance.now()) * 1000000;
				this.mem.setUint32((sp >>> 0) + 8, ns, true);
				this.mem.setUint32((sp >>> 0) + 12, Math.floor(ns / 4294967296), true);
			};
			for (const [name, fn] of Object.entries(imports)) {
				imports[name] = (...args) => {
					refresh();
					return fn(...args);
				};
			}
			this.importObject.env = { memory: state.memory };
		}

		_signalThreadStop(status) {
			const state = this._threadState;
			if (!state) {
				return;
			}
			const control = new Int32Array(state.control);
			Atomics.compareExchange(control, state.workers * 8, 0, status);
			for (let slot = 0; slot < state.workers; slot++) {
				// Change the waited-on words before notification. A notification alone
				// can be lost between another agent's shutdown check and Atomics.wait.
				Atomics.store(control, slot * 8, -1);
				Atomics.store(control, slot * 8 + 2, 5);
				Atomics.notify(control, slot * 8);
				Atomics.notify(control, slot * 8 + 2);
			}
		}

		_drainThreadCalls() {
			if (this.exited) {
				return;
			}
			const state = this._threadState;
			const control = new Int32Array(state.control);
			if (Atomics.load(control, state.workers * 8) !== 0) {
				return;
			}
			for (let slot = 0; slot < state.workers; slot++) {
				const base = slot * 8;
				if (Atomics.compareExchange(control, base + 2, 1, 4) !== 1) {
					continue;
				}
				const saved = this._proxySP;
				try {
					this.mem = new DataView(state.memory.buffer);
					this._proxySP = Atomics.load(control, base + 4) >>> 0;
					this._threadOperations[Atomics.load(control, base + 3)](this._proxySP);
					if (this.exited) {
						this._resolveExitPromise();
					}
					Atomics.store(control, base + 2, 2);
				} catch (err) {
					if (err === this._threadShutdown) {
						Atomics.store(control, base + 2, 5);
						throw err;
					}
					Atomics.store(control, base + 2, 3);
					this._threadFailure(err);
				} finally {
					this._proxySP = saved;
					Atomics.notify(control, base + 2);
				}
				if (this.exited) {
					return;
				}
			}
		}

		// runThreads runs a module built with GOWASM=threads and terminates its
		// Worker pool on exit or failure. Set go.env.GOMAXPROCS for parallel Go
		// execution. Browsers require cross-origin isolation for shared memory.
		// options.workers sets the fixed pool size (1 through 1024, default 16).
		// options.workerURL selects the browser Worker script; by default it is
		// wasm_exec_worker.js next to wasm_exec.js. options.createWorker may
		// instead supply a factory returning a browser or Node.js Worker.
		async runThreads(module, options = {}) {
			if (!(module instanceof WebAssembly.Module)) {
				throw new Error("Go.runThreads: WebAssembly.Module expected");
			}
			const metadata = WebAssembly.Module.customSections(module, "go:threads");
			if (metadata.length !== 1 || metadata[0].byteLength !== 12) {
				throw new Error("Go.runThreads: missing go:threads memory metadata");
			}
			const limits = new DataView(metadata[0]);
			if (limits.getUint32(0, true) !== 1) {
				throw new Error("Go.runThreads: unsupported metadata version");
			}
			const count = options.workers ?? 16;
			if (!Number.isInteger(count) || count < 1 || count > 1024) {
				throw new Error("Go.runThreads: invalid worker count");
			}
			const state = {
				timeOrigin: performance.timeOrigin,
				memory: new WebAssembly.Memory({
					initial: limits.getUint32(4, true),
					maximum: limits.getUint32(8, true),
					shared: true,
				}),
				control: new SharedArrayBuffer((count * 8 + 1) * 4),
				workers: count,
			};
			const workers = [];
			let closing = false;
			let rejectFailure;
			const failure = new Promise((_, reject) => {
				rejectFailure = reject;
			});
			// Install the rejection handler before asynchronous worker startup.
			failure.catch(() => {});
			this._threadFailure = (err) => {
				this._signalThreadStop(2);
				rejectFailure(err);
			};
			this._installThreads(state);
			try {
				const ready = [];
				for (let slot = 0; slot < count; slot++) {
					const worker = options.createWorker ? options.createWorker() :
						new Worker(options.workerURL ?? new URL("wasm_exec_worker.js", wasmExecURL));
					workers.push(worker);
					ready.push(new Promise((resolve, reject) => {
						const message = (data) => {
							if (closing) {
								return;
							}
							try {
								if (data.type === "ready") {
									resolve();
								} else if (data.type === "rpc") {
									try {
										this._drainThreadCalls();
									} catch (err) {
										if (err !== this._threadShutdown) {
											this._threadFailure(err);
										}
									}
								} else if (data.type === "write") {
									fs.writeSync(data.fd, data.bytes);
								} else if (data.type === "exit") {
									this._finishExit(data.code);
								} else if (data.type === "error") {
									const err = new Error(data.error);
									reject(err);
									this._threadFailure(err);
								}
							} catch (err) {
								reject(err);
								this._threadFailure(err);
							}
						};
						const error = (err) => {
							reject(err);
							this._threadFailure(err);
						};
						if (worker.on) {
							worker.on("message", message);
							worker.on("error", error);
							worker.on("exit", (code) => {
								if (!closing && !this.exited && Atomics.load(new Int32Array(state.control), count * 8) === 0) {
									error(new Error(`Go worker exited unexpectedly (${code})`));
								}
							});
						} else {
							worker.onmessage = (event) => message(event.data);
							worker.onerror = error;
						}
						worker.postMessage({ type: "init", module, state, slot, execURL: wasmExecURL });
					}));
				}
				await Promise.race([Promise.all(ready), failure]);
				const instance = await WebAssembly.instantiate(module, this.importObject);
				try {
					await Promise.race([this.run(instance), failure]);
				} catch (err) {
					if (err !== this._threadShutdown) {
						throw err;
					}
					// A worker stopped while the main instance was blocked in Wasm.
					// Yield to deliver its preceding output and final exit/error message.
					await Promise.race([this._exitPromise, failure]);
				}
			} finally {
				closing = true;
				this._signalThreadStop(1);
				for (const timer of this._scheduledTimeouts.values()) {
					clearTimeout(timer);
				}
				this._scheduledTimeouts.clear();
				await Promise.all(workers.map(worker => worker.terminate()));
			}
		}

		_finishExit(code) {
			if (this.exited) {
				return;
			}
			this._signalThreadStop(1);
			this.exited = true;
			delete this._inst;
			delete this._values;
			delete this._goRefCounts;
			delete this._ids;
			delete this._idPool;
			this.exit(code);
			this._resolveExitPromise();
		}

		async run(instance) {
			if (!(instance instanceof WebAssembly.Instance)) {
				throw new Error("Go.run: WebAssembly.Instance expected");
			}
			this._inst = instance;
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
				this.mem.setUint32(offset + 4, 0, true);
				offset += 8;
			});

			// The linker guarantees global data starts from at least wasmMinDataAddr.
			// Keep in sync with cmd/link/internal/ld/data.go:wasmMinDataAddr.
			const wasmMinDataAddr = 4096 + 8192;
			if (offset >= wasmMinDataAddr) {
				throw new Error("total length of command line and environment variables exceeds limit");
			}

			this._wasmCallDepth = (this._wasmCallDepth || 0) + 1;
			try {
				this._inst.exports.run(argc, argv);
			} finally {
				this._wasmCallDepth--;
			}
			if (this.exited) {
				this._resolveExitPromise();
			}
			await this._exitPromise;
		}

		_resume() {
			if (this.exited) {
				throw new Error("Go program has already exited");
			}
			const nested = (this._wasmCallDepth || 0) !== 0;
			const state = this._threadState;
			if (state && Atomics.load(new Int32Array(state.control), state.workers * 8) !== 0) {
				if (nested) {
					throw this._threadShutdown;
				}
				return false;
			}
			this._wasmCallDepth = (this._wasmCallDepth || 0) + 1;
			try {
				this._inst.exports.resume();
			} catch (err) {
				// An asynchronous event is another outer Wasm entry, independent of
				// run(). Let its worker exit/error message finish the program. Nested
				// callbacks must unwind the enclosing Wasm invocation as well.
				if (!state || err !== this._threadShutdown || nested) {
					throw err;
				}
				return false;
			} finally {
				this._wasmCallDepth--;
			}
			if (this.exited) {
				this._resolveExitPromise();
			}
			return true;
		}

		_makeFuncWrapper(id) {
			const go = this;
			return function () {
				const event = { id: id, this: this, args: arguments };
				go._pendingEvent = event;
				const saved = go._proxySP;
				try {
					go._proxySP = undefined;
					go._resume();
				} finally {
					go._proxySP = saved;
				}
				return event.result;
			};
		}
	}
})();
