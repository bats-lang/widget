# widget

Virtual DOM widget types for [Bats](https://github.com/bats-lang) WASM applications.

## Features

- Rich widget type hierarchy: text, div, span, input, button, form, table, etc.
- CSS styling via the `css` package
- Event handling attributes
- Diff-based DOM updates (`AddChild`, `RemoveChild`, `ReplaceChild`, `SetAttribute`)
- HTML form types: input, select, textarea, checkbox, radio

## Usage

```bats
#use widget as W
#use css as C

val txt = $W.Text("Hello, world!")
val styled = $W.Div(
  $W.style_of($C.Color($C.Rgb(255, 0, 0))),
  $W.children_of(txt))
```

## API

See [docs/lib.md](docs/lib.md) for the full API reference.

## Safety

Safe library — `unsafe = false`.
