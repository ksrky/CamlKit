#include "stdio.h"

extern long writec(long x) {
    printf("%ld\n", x);
    return 0;
}

extern long readc() {
    long x;
    scanf("%ld", &x);
    return x;
}