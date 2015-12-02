c5 - Compiler C to MIPS in five functions
====

Forked from [rswier/c4](https://github.com/rswier/c4). Modified to generate MIPS asm code.

And then convert asm code in MERL binary.

Finally link multiple binaries into one.

Try the following:

```
make

./c5 -o c5.asm c5.c
./as -o c5.o -m c5.asm

./c5 -o other.asm <something else>.c 
./as -o other.o -m other.asm

./lk -o combine -m c5.o other.o
```

NOTE: `-m` in parameter meas output in MERL form, otherwise some error like `printf not found` would occurred when linking. All symbol must be resolved before running.
