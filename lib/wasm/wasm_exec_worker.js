// Copyright 2026 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

"use strict";

(() => {
	const node = typeof process !== "undefined" && process.versions?.node;
	let post;
	let listen;
	if (node) {
		const { parentPort } = require("worker_threads");
		globalThis.fs = require("fs");
		globalThis.path = require("path");
		globalThis.crypto ??= require("crypto");
		post = (data) => parentPort.postMessage(data);
		listen = (fn) => parentPort.once("message", fn);
	} else {
		post = (data) => postMessage(data);
		listen = (fn) => {
			onmessage = (event) => {
				onmessage = null;
				fn(event.data);
			};
		};
	}
	listen(async (data) => {
		let go;
		try {
			if (data.type !== "init") {
				throw new Error("Go worker: expected initialization");
			}
			if (node) {
				require("./wasm_exec.js");
			} else {
				importScripts(data.execURL || "wasm_exec.js");
			}
			go = new Go();
			go._installThreads(data.state, data.slot, post);
			go._inst = await WebAssembly.instantiate(data.module, go.importObject);
			go.mem = new DataView(data.state.memory.buffer);
			const control = new Int32Array(data.state.control);
			const base = data.slot * 8;
			post({ type: "ready" });
			for (;;) {
				while (Atomics.load(control, base) !== 2) {
					if (Atomics.load(control, data.state.workers * 8) !== 0) {
						return;
					}
					const status = Atomics.load(control, base);
					if (status === -1) {
						return;
					}
					if (status !== 2) {
						Atomics.wait(control, base, status);
					}
				}
				if (Atomics.load(control, data.state.workers * 8) !== 0) {
					return;
				}
				const arg = Atomics.load(control, base + 1) >>> 0;
				go._inst.exports.go_thread_start(data.slot + 1, arg);
				// The Go M has exited. Its instance may be reused only after the
				// exported thread entry returns and all instance-local frames unwind.
				Atomics.store(control, base, 0);
			}
		} catch (err) {
			if (err === go?._threadExit || err === go?._threadShutdown) {
				return;
			}
			post({ type: "error", error: err?.stack || String(err) });
			go?._signalThreadStop(2);
		}
	});
})();
