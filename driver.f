
      program main
          implicit none

          external modul1,modul2
          external flopt
          double precision flopt
          double precision fexact
          double precision inversetransform
          double precision inversetransformtest
          double precision time
          double precision fnumeric

          time = 10.1

          fexact = flopt(time)
          fnumeric =  inversetransform(time,fexact)

          write (*,44)
   44     format("   result:")
          write (*,*) fexact, fnumeric

          fnumeric =  inversetransform(time)

          write (*,*) fnumeric

      end








