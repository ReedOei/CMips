# CMips

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

