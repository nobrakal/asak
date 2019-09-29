# What ?

`run_on_lambdas` and `build_all_opam` are tools to run asak on opam libraries.

# How ?

First some variables:

```
OPAM_REPO=/tmp/opam
MODIFIED_OCAML=/tmp/ocaml
ASAK_REPO=/tmp/asak-repo
THRESHOLD=100
```

1. Download the [opam-repository][https://github.com/ocaml/opam-repository/]. It is used as a list of packages.

```
git clone --depth=1 https://github.com/ocaml/opam-repository.git $OPAM_REPO
```

2. Download the modified OCaml compiler that will dump intermediate lambda representation.

```
git clone --branch 4.08 --single-branch --depth=1 https://github.com/nobrakal/ocaml.git $MODIFIED_OCAML
```

3. Create an empty switch and install the modified compiler.

```
cd $MODIFIED_OCAML
opam switch create . --empty
eval $(opam env)
opam install .
```

4. Download this repository and build things

```
git clone https://github.com/nobrakal/asak.git $ASAK_REPO
cd $ASAK_REPO
dune build utils/build_all_opam.exe
```

4. Run `build_all_opam`

```
mkdir /tmp/asak # lambdas will be stored here
$ASAK_REPO/_build/default/utils/build_all_opam.exe $OPAM_REPO/packages $MODIFIED_OCAML
```

5. Run `run_on_lambdas`

```
$ASAK_REPO/_build/default/utils/run_on_lambdas.exe $THRESHOLD /tmp/asak/*
```
