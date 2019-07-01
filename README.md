# asak

[![Build Status](https://travis-ci.com/nobrakal/asak.svg?branch=master)](https://travis-ci.com/nobrakal/asak)

Partitioning OCaml codes.

## Why ?

* For teaching: the module `Asak.Partition` offers a function `create` that produces a partition of codes implementing the same function, where two codes are in the same class if they are syntactically "close", and in the same sub-class if they are identical (up to alpha-renaming and some inlining).

* For redundancy detection: while not being totally implemented, the goal is to produce a module `Asak.Factorization` that will take a list of OCaml codes and detect codes that can be factorized.

## How ?

There is two cores:

* `Asak.Lambda_utils`, that defines a function `hash_lambda` that is hashing a lambda expression (an intermediate language in the OCaml compilation pipeline), capturing only the shape of the AST, ignoring for example variable names, function names, pattern-matching order…

* `Asak.Clustering`, that defines a function `cluster` which is making a complete-linkage clustering of a list of hashes.

For more details, see [this blog post](https://blog.nyarlathotep.one/2019/06/learnocaml-code-classification/).

#### The name

This tool is about making partitions. Consequently, its name is about music: [asak](https://en.wikipedia.org/wiki/Tuareg_people#Music) is the name of Tuareg's traditional songs accompanied by a monochord violin.
