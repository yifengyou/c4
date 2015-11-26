#include <stdio.h>

char *
_memcpy(char *dst, char *src, int size)
{
    char *ret;
    ret = dst;
    while (size--) {
        *dst++ = *src++;
    }
    return ret;
}

