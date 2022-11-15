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

#### `p <var-name>`

Inspect the content of a variable.
Requires to select the frame with `f` first.

#### `p *<var-name>`

Inspect the content of a variable after dereferencing it.
Requires to select the frame with `f` first.
