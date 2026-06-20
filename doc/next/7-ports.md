## Ports {#ports}

### WebAssembly {#wasm32}

Go now supports `GOARCH=wasm32` with `GOOS=js` and `GOOS=wasip1`.
It uses 32-bit Go pointers and `int` values while sharing the WebAssembly
instruction backend with `GOARCH=wasm`. Function continuations use logical
PC tokens and a separate indirect-call handle, so program counters do not
reserve the high half of linear memory.
