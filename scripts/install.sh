#!/usr/bin/env bash

(
    git clone https://github.com/Z3Prover/z3
    cd z3
    python scripts/mk_make.py
    cd build
    make
    sudo make install
)

(
    git clone https://github.com/ReedOei/CMips
    cd CMips
    stack install
)

