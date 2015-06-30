#include <stdio.h>
#include <math.h>
#include <errno.h>


/*
This program is intended to find the system specific values of
"ovlog" and "unlog" to be inserted in the the FORTRAN function
"inversetransform" ( file functions.f, line 15)
*/


int main(void)
{
    double param, result;

    errno = 0;

    for (param = 1; ; param=param+1){
        result = exp (param);
        if (errno == ERANGE) {
            printf("exp(%f) overflows\n", param);
            break;
        }
    }

    errno = 0;

    for (param = -1; ; param=param-1){
        result = exp (param);
        if (errno == ERANGE) {
            printf("exp(%f) overflows\n", param);
            break;
        }
    }


    return 0;
}
