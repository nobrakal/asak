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
    pkg-config \
    # for ctypes
    libffi-dev \
    # for conf-*
    m4 ncurses-dev libgmp-dev zlib1g-dev gnuplot-x11 \
    libgtksourceview2.0-dev cpio libzstd-dev libzmq3-dev \
    wget vim time libtidy-dev libgdbm-dev zlib1g-dev \
    libsnappy-dev libsqlite3-dev tcl-dev libtidy-dev \
    libsfml-dev libsdl-ttf2.0-dev libsdl2-dev libsdl2-image-dev \
    libsdl2-mixer-dev libsdl2-net-dev libelementary-dev coinor-csdp \
    cmake clang llvm-6.0-dev autoconf librocksdb-dev libaio-dev

COPY . ./

RUN sudo chown -R opam:nogroup .

RUN eval $(opam env) \
    && dune build utils/build_all_opam.exe

VOLUME ["/tmp/asak"]

ENTRYPOINT ["_build/default/utils/build_all_opam.exe", "/home/opam/ocaml", "/home/opam/opam-repository/packages"]