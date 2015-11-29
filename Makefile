ALL : c5 as

.PHONY: ALL

c5 : c5.c
	cc -m32 -g -o c5 c5.c

as : as.c
	cc -m32 -g -o as as.c
