# Lean playground

> A tiny, reproducible place to learn Lean by building useful things.

This repository provides a **Lean 4** development environment using Nix flakes,
with `nixpkgs` pinned by [npins](https://github.com/andir/npins). The flake has
*no flake inputs*.

## Quick start

1. Run the hello-world program:

```console
just run Hello
```

Expected output:

```text
Hello, world!
```

2. Render this README in your terminal:

```console
just run Markdown README.md
```

## What's included

- [x] A pinned Lean development shell
- [x] A minimal `examples/Hello.lean` program
- [x] A single-file terminal Markdown renderer
- [x] A local, JSONL-backed message-to-self TUI
- [x] A self-paced Lean guide for Haskell programmers
- [ ] Your next Lean experiment

### Message yourself

Open the local journal:

```console
just run MessageSelf
```

Messages are appended immediately to `.messages.jsonl` and replayed when you
return. Pass another path to keep a separate journal:

```console
just run MessageSelf work-notes.jsonl
```

The full-screen TUI keeps its composer fixed while the message history scrolls.
Use ↑/↓ or Page Up/Page Down to navigate, and `/help`, `/clear`, or `/quit` for
commands. The JSONL file remains easy to inspect or process with other tools.

### Markdown renderer

`examples/Markdown.lean` understands **bold**, *italic*, `inline code`, links, headings,
quotes, lists, task lists, rules, and fenced code blocks. It accepts a filename
or reads from standard input.

```console
printf '# Hello\n\nBuilt with **Lean**.\n' |
  just run Markdown -
```

Use `--no-color`, or set the standard `NO_COLOR` environment variable, for
plain output.

---

Built to be *small enough to understand* and useful enough to keep extending.
