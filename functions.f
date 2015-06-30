
      double precision function inversetransform(time,fexact)
          implicit none
          double precision time
          double precision fexact
          external flop
          integer ierr,mact,mtop,nstate
          double precision acoef,bvalue,epstol,errvec,fcalc,
     $ sigma, sigma0,sintab,work
          double precision  ovlog, unlog
          double precision diff,  reldif, pseudo, arg, temp
          integer i, k
          dimension acoef(1024), errvec(8), work(1024), sintab(256)

          data ovlog,unlog /709.0d0,708.0d0/

          sigma0 = 0.0
          sigma  = -222.2
          bvalue = -33.3
          epstol = 4.4d-4
          mtop   = 555

          write(*,3655)
 3655    format('#     --INPUT--- -----OUTPUT-----',3x,'----EXACT---',1x,
     $'-----ERROR  EVALUATION------')
          write(*,3656)
 3656    format('#       time   FUN(time) IFLO',4x,'FUN(time)',1x,
     $' RELATIVE    PSEUDO    INDEX'/)

         call modul1(sigma0,sigma,bvalue,epstol,mtop,flop,work,sintab,
     $   mact,acoef,errvec,nstate)
         call modul2(time,mact,acoef,sigma,bvalue,errvec,fcalc,ierr)

c       Calculate exact result, error, etc.


        diff = fcalc - fexact
        reldif = 0.0
        pseudo = 0.0

        if(diff.eq.0.0) go to 790
        if(fexact.ne.0.0) reldif = diff/fexact
        arg = sigma*time
        if(arg.le.ovlog) pseudo = diff/exp(arg)
        if(arg.le.ovlog) go to 790

c       Take care of remote possibility of overflow.
        temp = log(abs(diff)) - arg
        if(temp.lt.-unlog) go to 799
        pseudo = sign(exp(temp),diff)
 790    continue

        k = 0
        if(abs(pseudo).gt.errvec(1)) k = -1
 
         if(ierr.eq.0)
     $   write(*,5750) i,time,fcalc,ierr,fexact,reldif,pseudo,k
 5750    format(i4, f10.4, e14.6, i3, 3x, e13.6, e11.3, e11.3, i5)
         if(ierr.eq.0) go to 800    
 799     continue

         write(*,5751) i,time,fcalc,ierr,fexact
 5751    format(i4,1pd11.4,d13.6,i3,3x,d13.6)
    
 800     continue

         write(*,5850) errvec(1)
 5850    format('#',40x,'        errvec(1) =',1pd10.3/)


         inversetransform  = fcalc
         return
      end




      SUBROUTINE FLOP(ARGRE,ARGIM,RESRE,RESIM)
C     double complex function flop(z)
C     double complex z,z1,z2,z3
      DOUBLE PRECISION ARGRE,ARGIM,RESRE,RESIM
      EXTERNAL PYTHAG
      DOUBLE PRECISION PYTHAG,R,THETA,T1,T2
      DOUBLE PRECISION Z1R,Z1I,Z2R,Z2I,Z3R,Z3I
C     z1 = 1.0/(z+0.5)
      R = 1.0/PYTHAG(ARGRE+0.5,ARGIM)
      THETA = -ATAN2(ARGIM,ARGRE+0.5)
      Z1R = R*COS(THETA)
      Z1I = R*SIN(THETA)
C     z2 = 1.0/z**2
      R = 1.0/(ARGRE**2+ARGIM**2)
      THETA = -2*ATAN2(ARGIM,ARGRE)
      Z2R = R*COS(THETA)
      Z2I = R*SIN(THETA)
C     z3 = 1.0/(1.0+(z+0.2)**2)
      T1 = (ARGRE+0.2)**2 - ARGIM**2 + 1.0
      T2 = 2*(ARGRE+0.2)*ARGIM
      R = 1.0/PYTHAG(T1,T2)
      THETA = -ATAN2(T2,T1)
      Z3R = R*COS(THETA)
      Z3I = R*SIN(THETA)
C     flop = z1 + z2 + z3
      RESRE = Z1R + Z2R + Z3R
      RESIM = Z1I + Z2I + Z3I
      return
      end




      SUBROUTINE FLIP(ARGRE,ARGIM,RESRE,RESIM)
C     double complex function flip(z)
C     double complex z,z1,z2,z3,zeye
C     data zeye /(0.0,1.0)/
      DOUBLE PRECISION ARGRE,ARGIM,RESRE,RESIM
      EXTERNAL PYTHAG
      DOUBLE PRECISION PYTHAG,R,RN,RD,THETA,THETAN,THETAD,T1,T2
      DOUBLE PRECISION Z1R,Z1I,Z2R,Z2I,Z3R,Z3I
C     z1 = (z**2-1.0)/(z**2+1.0)**2
      T1 = ARGRE**2 - ARGIM**2 - 1.0
      T2 = 2*ARGRE*ARGIM
      RN = PYTHAG(T1,T2)
      THETAN = ATAN2(T2,T1)
      RD = (T1+2.0)**2 + T2**2
      THETAD = 2*ATAN2(T2,T1+2.0)
      R = RN/RD
      THETA = THETAN - THETAD
      Z1R = R*COS(THETA)
      Z1I = R*SIN(THETA)
C     z2 = exp(-4.0*z**0.5)
      T1 = SQRT(PYTHAG(ARGRE,ARGIM))
      T2 = 0.5*ATAN2(ARGIM,ARGRE)
      R = EXP(-4*T1*COS(T2))
      THETA = -4*T1*SIN(T2)
      Z2R = R*COS(THETA)
      Z2I = R*SIN(THETA)
C     z3 = -0.5*zeye*log((z+zeye)/(z-zeye))
      RN = PYTHAG(ARGRE,ARGIM+1.0)
      THETAN = ATAN2(ARGIM+1.0,ARGRE)
      RD = PYTHAG(ARGRE,ARGIM-1.0)
      THETAD = ATAN2(ARGIM-1.0,ARGRE)
      R = RN/RD
      THETA = THETAN - THETAD
      Z3R = 0.5*THETA
      Z3I = -0.5*LOG(R)
C     flip = z1 + z2 + z3
      RESRE = Z1R + Z2R + Z3R
      RESIM = Z1I + Z2I + Z3I
      return
      end






      double precision function flopt(tvalue)
c
c      This is the inverse Laplace transform of flop.
c
      double precision f1,f2,f3,tvalue
      f1 = exp(-0.5*tvalue)
      f2 = tvalue
      f3 = exp(-0.2*tvalue)*sin(tvalue)
      flopt = f1 + f2 + f3
      return
      end





      double precision function flipt(tvalue)
c
c      This is the inverse Laplace transform of flip.
c
      double precision f1,f2,f3,one,pi,tvalue
      data one /1.0d0/
      pi = 4.0*atan(one)
      f1 = tvalue*cos(tvalue)
      f2 = 0.0
      if(tvalue.gt.0.125)
     $   f2 = 2.0*exp(-4.0/tvalue)/sqrt(pi*tvalue**3)
      f3 = 1.0 - tvalue**2/6.0
      if(tvalue.gt.0.0001) f3 = sin(tvalue)/tvalue
      flipt = f1 + f2 + f3
      return
      end





      SUBROUTINE FUSERP(ARGRE,AR   GIM,RESRE,RESIM)
C     double complex function fuserp(z)
C     double complex z
      DOUBLE PRECISION ARGRE,ARGIM,RESRE,RESIM
      DOUBLE PRECISION R,THETA
c_OPTION The user should code his own function here (default example below).  He may
c_OPTION also code function fusert.
      R = 1.0/(ARGRE**2+ARGIM**2)
      THETA = -2*ATAN2(ARGIM,ARGRE)
      RESRE = R*COS(THETA)
      RESIM = R*SIN(THETA)
      return
      end




      double precision function fusert(tvalue)
      double precision tvalue
c_OPTION  fusert may be coded as the exact inverse Laplace transform of
c_OPTION  the function in fuserp, if known, or it may be left zero as
c_OPTION  coded below.
      fusert = sin( 20.0*tvalue )
      return
      end




