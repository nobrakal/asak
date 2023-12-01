# asak

[![CI](https://github.com/nobrakal/asak/actions/workflows/main.yml/badge.svg)](https://github.com/nobrakal/asak/actions/workflows/main.yml)

Asak is an OCaml library that allows to identify similar OCaml codes.

## Why ?

### For teaching

The module `Asak.Partition` offers a function `create` that produces a partition of codes implementing the same function, where two codes are in the same class if they are syntactically "close".

### For redundancy detection
The binary `anzad` can detect redundant definitions of an OCaml project built with `dune` and compare it with a database of previously analyzed projects.

To use it on a project with sources in `src/`, run:

```
dune build @check
anzad src/
```

## Documentation

The documentation of the API is available here: [https://nobrakal.github.io/asak/asak/](https://nobrakal.github.io/asak/asak/).

A man page is available for the binary `anzad`.

## How ?

The idea is to compare AST (Abstract Syntax Tree) of codes. However, the OCaml AST is too rich for our purpose (since, for example, `match x with ...` and `function ...` generate two different AST). We decided instead to use the Lambda language, an intermediate language in the OCaml compilation pipeline, where such syntactic sugar is optimized away.

To efficiently compare Lambda trees, we use the methodology of [Chilowicz et al.](http://igm.univ-mlv.fr/~chilowi/research/syntax_tree_fingerprinting/syntax_tree_fingerprinting_ICPC09.default_pdf.pdf) which consist in hashing recursively trees.

We then compare hashes and provide a clustering of the closest functions.

### But really, how ?

There is two cores:

* `Asak.Lambda_hash`, that defines a function
`hash_lambda : config -> threshold -> Lambda.lambda -> hash` which is hashing a Lambda expression
capturing the shape of the AST, with respect to the given configuration.
We recommend to first apply the functions from `Asak.Lambda_normalization` to normalize the Lambda expression.

* `Asak.Clustering`, that defines a function `cluster : ('a * Lambda_hash.hash) list -> ('a list) wtree list`
which is making a kind of complete-linkage clustering of a list of hashes.
The output is a [dendrogram](https://en.wikipedia.org/wiki/Dendrogram) where leaves are close in a tree
if they are similar. It is guaranteed that two codes in the same tree share at least a sub-AST.

### More details

A paper (in french) about asak was published in the proceedings of the JFLA (Journées Francophones des Langages Applicatifs) 2020. It can be found here: https://github.com/nobrakal/asak-paper/

#### The name

This tool is about making partitions. "Partition" is the word in french for "sheet music". Consequently, its name is about music: [asak](https://en.wikipedia.org/wiki/Tuareg_people#Music) is the name of Tuareg's traditional songs accompanied by a monochord violin.

This monochord violin is called an
[anzad](https://en.wikipedia.org/wiki/Imzad), which is the name of the
binary client of asak.

## Resources

[Inzad](https://gitlab.inria.fr/guesdon/inzad) is an experiment to
detect OCaml code similarities, based on Asak's ideas.

## License and copyright

Asak is released under the MIT license. The copyright is held by IRIF / OCaml Software Foundation.

### Authors

Asak is developed by Alexandre Moine.
