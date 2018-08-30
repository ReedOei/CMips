# CMips
[![Build Status](https://travis-ci.org/ReedOei/CMips.svg?branch=master)](https://travis-ci.org/ReedOei/CMips)

A C and Lisp compiler that outputs MIPS assembly code.

# Installation

If you're using some Linux distribution, you should be able to run the following command to do everything necessary:
```
curl -sSL https://raw.githubusercontent.com/ReedOei/CMips/master/scripts/install.sh | bash
```

To update, simply run inside the directory you download this repository to:
```
git pull
stack install
```

More detail instructions (which do exactly what the script from above does):

Get this repository and run `stack install`.

```
git clone https://github.com/ReedOei/CMips
cd CMips
stack install
```

If you wish to make use of the analyzer, you will also need [Z3](https://github.com/Z3Prover/z3).
Unfortunately, the version available via `apt` is outdated, so you will need to install it in some other way.
If you're on a reasonably Linux-y machine, you can run the following commands to do so.

```
git clone https://github.com/Z3Prover/z3
cd z3
python scripts/mk_make.py
cd build
make
sudo make install
```

# Usage

Write your code in C or Lisp (language is detected via extension), and then run:

```
CMips file.c --output out.s
```

# Non-standard features

- Can define a `mips` block. This assembly will be literally copied into the output file. This is useful for accessing memory mapped IO, as shown below, which retaining the ability to write most of the program in C. Note that the code is not checked for **anything**, including syntatic correctness. It is simply copied into the function body.
```
mips int getVelocity() {
    lw $v0, VELOCITY_ADDRESS
    jr $ra
}
```

- Java-style annotations. These are purely for use with the analyzer. For example, I can specify that some variable holds the length of an array.
```
void f() {
    int size = 10;

    @Length(size)
    int *x = malloc(size * sizeof(int));
}
```

# Known issues

- Currently, cannot parse the type "unsigned". Write "unsigned int" instead.
- Cannot parse accesses to a struct member after an array access. (e.g., write `(a[i]).b` instead of `a[i].b`).
- Local variable resolution looks for everything in the function, doesn't properly respect blocks.

# TODO

- Support more preprocessor directives. Currently only `#include` is supported.

