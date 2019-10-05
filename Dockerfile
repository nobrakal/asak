FROM ocaml/opam2:4.08
# NB: opam-repositroy is here: /home/opam/opam-repository

RUN opam install dune

RUN git clone --branch 4.08 --single-branch --depth=1 https://github.com/nobrakal/ocaml.git /home/opam/ocaml

RUN mkdir /tmp/asak

RUN cd /home/opam/ocaml \
    && opam switch create . --empty \
    && eval $(opam env) \
    && opam install .

RUN sudo apt-get update
RUN sudo apt-get install -y \
    # for conf-m4
    m4 \
    pkg-config \
    # for ctypes
    libffi-dev

COPY . ./

RUN sudo chown -R opam:nogroup .

RUN eval $(opam env) \
    && dune build utils/build_all_opam.exe

VOLUME ["/tmp/asak"]

ENTRYPOINT ["_build/default/utils/build_all_opam.exe", "/home/opam/ocaml", "/home/opam/opam-repository/packages"]