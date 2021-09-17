# schema

A minimalist Natural Language Processing module in OCaml

## What is schema ?

Schema is a small OCaml module designed for the parsing and analysis of plain text. It features a lightweight method to declare "schemas" (language patterns similar to regular expressions) and generate efficient ocaml code to match these schemas.

This library includes a syntax extension (ppx) allowing one to generate the code of a schema parser at compil time (prior to the execution of the program declaring the schema parser).
