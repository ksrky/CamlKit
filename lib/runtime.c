#include "stdio.h"

extern long printi(long x) {
    printf("%ld\n", x);
    return 0;
}

extern long readi() {
    long x;
    scanf("%ld", &x);
    return x;
}