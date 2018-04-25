# CMips
[![Build Status](https://travis-ci.org/ReedOei/CMips.svg?branch=master)](https://travis-ci.org/ReedOei/CMips)

A C and Lisp compiler that outputs MIPS assembly code.

# Installation

Simply get this repository and run `stack install`.

```
git clone https://github.com/ReedOei/CMips
cd CMips
stack install
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

# Known issues

- Currently, cannot parse the type "unsigned". Write "unsigned int" instead.
- Cannot parse accesses to a struct member after an array access. (e.g., write `(a[i]).b` instead of `a[i].b`).
- Local variable resolution looks for everything in the function, doesn't properly respect blocks.

# TODO

- Support more preprocessor directives. Currently only `#include` is supported.

