# widget

Virtual DOM widget types for [Bats](https://github.com/bats-lang) WASM applications.

## Features

- Rich widget type hierarchy: text, div, span, input, button, form, table, etc.
- CSS styling via the `css` package
- Event handling attributes
- Diff-based DOM updates (`AddChild`, `RemoveChild`, `SetAttribute`, ...)
- Linear tree: widgets, diffs and their values are freed by their owner
- HTML form types: input, select, textarea, checkbox, radio

## Usage

The widget tree is linear (there is no GC): every widget is consumed by
the operation that transforms it (which returns the new widget and the
diff to apply), by dom's `apply` (inside a diff), or by `widget_free`.

```bats
#use array as A
#use widget as W

val root = $W.Element($W.ElementNode($W.Root(), $W.Normal($W.Div()),
  $W.NoClass(), false, $W.NoneInt(), $W.NoneStr(), $W.WNil()))
val @(root, d) = $W.add_child(root, $W.Text($A.text_lit("hi"), 2))
val () = $D.apply(doc, d)        (* or $W.diff_free(d) *)
val () = $W.widget_free(root)
```

## API

See [docs/lib.md](docs/lib.md) for the full API reference.

## Safety

Safe library — `unsafe = false`.
