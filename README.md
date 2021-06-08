# Fromager

Fromager helps you format your codebase.

## Installation & Usage

If you have [Opam](https://opam.ocaml.org/), the package manger of OCaml, simply do:

```console
$ opam install fromager
$ fromager
```

It's that simple!

## Configuring Fromager

By default, fromager ignore any directory starting with an underscore (`_opam`, `_build`, `_coverage`, etc.).

You can optionally´ create a `fromage.toml` file at the root of your project:

```toml
[config]
ignored_files = []
ignored_dirs = ["some", "ignored", "directories]
```

## Inside a Github action

```yml
name: Run Fromager

on:
  [pull_request]

jobs:
  run_fromager:
    strategy:
      fail-fast: false
      matrix:
        os:
          - macos-latest
          - ubuntu-latest
        ocaml-version:
          - 4.07.1

    runs-on: ${{ matrix.os }}

    steps:
      - name: Get code
        uses: actions/checkout@v2

      - name: Use OCaml ${{ matrix.ocaml-version }}
        uses: avsm/setup-ocaml@v1
        with:
          ocaml-version: ${{ matrix.ocaml-version }}

      - name: Build
        run: |
          eval $(opam env)
          opam pin add . -y

      - name: Format
        run: |
          eval $(opam env)
          opam install fromager
          fromager
          git diff
```