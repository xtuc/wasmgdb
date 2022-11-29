# wasmgdb

> gdb for WebAssembly

## Install

```
cargo install wasmgdb
```

## Usage

Use [wasm-edit] to transform your module and, once WebAssembly traps, collect the
WebAssembly memory and analyze the coredump.

## Analyze a coredump

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

#### `p/s <var-name>`

Print the variable as string.
Requires to select the frame with `f` first.

#### `x/<number> hex-addr`

Examine the memory address with <number> length.

#### `x/<number>s hex-addr`

Examine the memory address and prints as string of <number> length.

[wasm-edit]: https://github.com/xtuc/wasm-edit#coredump-generation
