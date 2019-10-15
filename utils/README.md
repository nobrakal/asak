# What ?

`build_all_opam`, `run_on_lambdas` and `inspect_db` are tools to run `asak` on OPAM libraries.

# How ?

## Build all opam packages

The simplest way is to use the provided `Dockerfile` in the root directory.

You will only need to have docker volume to store the produced lambda trees when running the image. Please mount it to `/mnt/asak`.

The entry-point takes optional arguments to specify on which package to begin and which package to stop (on alphabetical order). By default, all packages are built.

## Inspect these lambdas

Use the `run_on_lambdas` executable. It takes several arguments:

```
run_on_lambdas cores output_db.asak threshold [FILES]
```

where:

* `cores` is the number of cores of the computer to use.
* `output_db.asak` is a file to store the computed clustering. for further investigation.
* `threshold` is an integer threshold to keep sub-AST.
* `[FILES]` the collection of lambdas to inspect. Usually something like `copy_of_docker_volume/*`

It will run, store its result and print the whole clustering.

## Inspect this database

The whole clustering can be pretty big, so we offer another executable `inspect_db` which takes several arguments:

```
inepsect_db output_db.asak biggest_class number_of_repr prefix
```

where:

* `output_db.asak` is a file produced by `run_on_lambdas`
* `biggest_class` is an integer corresponding to the number of biggest classes to show.
* `number_of_repr` is an integer corresponding to the number of representative elements per class to show.
* `prefix` is a prefix to store a csv file containing more information.
