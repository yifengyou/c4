c5 - Compiler C to MIPS in five functions
====

Forked from [rswier/c4](https://github.com/rswier/c4). Modified to generate MIPS asm code.

And then convert asm code in MERL binary.

Try the following:

```
make
./c5 c5.c > c5.asm
./as -o c5.bin -m c5.asm

./c5 c5.c hello.c > combine.asm
./as -o combine.bin -m combine.asm
```
