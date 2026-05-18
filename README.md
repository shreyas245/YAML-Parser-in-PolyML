# Poly/ML YAML Parser

A YAML parser written for Poly/ML, built around a hand-rolled tokenizer and recursive-descent parser.

The tokenizer (`token.sml`) converts raw YAML text into a flat token stream, tracking indentation via explicit `Indent`/`Dedent` tokens to handle block structure. The parser (`parser.sml`) consumes that stream into a typed AST supporting nulls, booleans, integers, floats, strings, sequences, and mappings — both flow (`[]`, `{}`) and block styles. `yaml_parser.sml` wraps the parser with a clean high-level API including file I/O, recursive key search, and pretty printing.

## Requirements

- [Poly/ML 5.9+](https://www.polyml.org)

## Usage

```bash
poly --script test_parser.sml path/to/file.yaml
```
