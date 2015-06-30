#include <cstdio>
#include <cstdlib>

extern "C" {
    double inversetransform_(double *time);
}

extern "C" {
    double flopt_(double *time);
}

int main(){

    double time = 10.1;
    double fexact;
    double fnumeric;


    fexact = flopt_( &time); //flopt is written in fortran
    fnumeric= inversetransform_(&time); //flopt is written in fortran

    printf( "   result:\n  %f\t%f\n", fexact, fnumeric);
    
    return 0;

}
