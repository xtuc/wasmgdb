# wasmgdb

> gdb for WebAssembly

## Install

```
cargo install wasmgdb
```

## Usage

```
wasmgdb <coredump> <source.wasm>
```

### Commands

#### `bt`

Display the stack trace.

#### `f <#>`

Selects a stack frame and display informations.
