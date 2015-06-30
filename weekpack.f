










      subroutine modul1(sigma0,sigma,bvalue,epstol,mtop,f,
     $                  work,sintab,mact,acoef,errvec,nstate)
      integer mtop,mact,nstate
      double precision sigma0,sigma,bvalue,epstol
      double precision work(mtop),sintab(mtop/4),acoef(mtop),errvec(8)
      external entcre
      external f
c-----------------------------------------------------------------------
c  This subroutine is MODUL1.  Its principal purpose is to calculate,
c  to a user-provided absolute (pseudo) uniform accuracy, a set of
c  Laguerre coefficients  (acoef(j),j=1,mact)  which will be used
c  subsequently by MODUL2.  These coefficients are the Taylor
c  coefficients of the function defined in PHIFUN to which the user-
c  provided function F contributes.  MODUL1 calls ENTCRE, which in turn
c  calls HFCOF and PHIFUN.
c
c  INPUT arguments
c
c  sigma0 - real - the abscissa of convergence of the Laplace transform.
c  sigma  - real - the first parameter of the Laguerre expansion.
c                  If sigma is not greater than sigma0, it defaults and
c                  is reset to (sigma0 + 0.7).
c  bvalue - real - the second parameter of the Laguerre expansion.
c                  If bvalue is less than 2.0*(sigma - sigma0), it
c                  defaults and is reset to 2.5*(sigma - sigma0).
c  epstol - real - the required absolute uniform pseudo accuracy for the
c                  coefficients and inverse Laplace transform values.
c  mtop   - integer -  an upper limit on the number of coefficients to
c                      be computed.  mtop must be at least 8.  Note
c                      that the maximum number of Laplace transform
c                      evaluations is (mtop/2 + 2).
c  F      - procedure - name of subroutine subprogram for the complex
c                       valued Laplace transform to be inverted.  F must
c                       be declared EXTERNAL in the calling program.
c  work   - real(mtop)   - work array provided for ENTCRE.
c  sintab - real(mtop/4) - work array provided for ENTCRE.
c
c  OUTPUT arguments
c
c  mact   - integer - the number of coefficients actually computed.
c  acoef  - real(mtop) - the array of Laguerre coefficients.
c  errvec - real(8) - an 8-component vector of diagnostic information.
c          All components are functions of Laguerre coefficients acoef.
c          (1) = Overall estimate of the pseudo error = (2) + (3) + (4).
c                  Pseudo error = absolute error / exp(sigma*tvalue) .
c          (2) = Estimate of the pseudo discretisation error.
c          (3) = Estimate of the pseudo truncation error.
c          (4) = Estimate of the pseudo condition error on the basis
c                  of minimal noise levels in function values.
c          (5) = K - Coefficient of the decay function for acoef.
c          (6) = R - Base of the decay function for acoef.
c                  abs(acoef(j+1)) .le. K/R**j for j .ge. mact/2 .
c          (7) = ALPHA - Logarithm of the largest acoef.
c          (8) = BETA - Logarithm of the smallest nonzero acoef.
c  nstate - integer - the output state parameter, takes one of 7 values:
c           0 => Normal termination, estimated error less than epstol.
c           1 => Normal termination, but with estimated error bounds
c                slightly larger than epstol.  Note, however, that the
c                actual errors on the final results may be smaller than
c                epstol as bounds independent of tvalue are pessimistic.
c           2 => Normal calculation, terminated early at round off error
c                level estimate because this estimate exceeds the
c                required accuracy (usually due to overly optimistic
c                expectation by the user about attainable accuracy).
c          -1 => The decay rate of the coefficients is too small.
c                It may improve results to increase mtop.
c          -2 => The decay rate of the coefficients is too small.  In
c                addition, the round off error level is such that the
c                required accuracy cannot be attained.
c          -3 => No error bounds are returned as the behavior of the
c                coefficients does not enable reasonable prediction.
c                Results are probably wrong.  Check the value of sigma0.
c                In this case, (errvec(j),j=1,5) are each set to -1.0.
c         NOTE - When nstate is 2 or negative, changing bvalue and sigma
c                might help.  If not, the method should be abandoned.
c          -4 => mtop is not at least 8.  No calculation is done.
c-----------------------------------------------------------------------
      integer j,jb,jbot,jt,jtop,nb,ncode,nsbot,nsdiff,nstop,nt,ntab
      double precision bdef,ealfa,ebeta,epest,epmach,epsnoi,epreq,
     $                 fact,one,phisum,r1,r2,rcirc,rcircm,rest,restm,
     $                 sigdef,t1,t2,unlog,xkest,zero
      double precision ab(3),at(3),params(3)
      equivalence (phisum,params(3))
      external phifun
c
      data bdef,one,sigdef,zero /2.5d0,1.0d0,0.7d0,0.0d0/
c
c  ********** WARNING - The following constants are machine dependent
c                       (activated values are for IEEE machines):
c  epmach - the machine accuracy parameter.
c  unlog - the largest integer such that exp(-unlog) does not underflow.
c
c  Constants for some machines (activate by removing 'c' from column 1)
c  ---------------------------------------------------------------------
c  VAX 11/780
c     data epmach,unlog /1.38778d-17,88.0d0/
c  IEEE (Alliant FX/8, Encore Multimax, Sequent Balance, Sun, etc.)
      data epmach,unlog /2.22045d-16,708.0d0/
c  IBM 3033
c     data epmach,unlog /2.22045d-16,180.0d0/
c  CRAY
c     data epmach,unlog /2.52435d-29,5678.0d0/
c  ---------------------------------------------------------------------
c
c  Set parameters to default values and initialize function value sum.
c
      if (mtop .lt. 8) then
         nstate = -4
         return
         end if
      if (sigma .le. sigma0) sigma = sigma0 + sigdef
      params(1) = sigma
      if (bvalue .lt. 2*(sigma - sigma0)) bvalue = bdef*(sigma - sigma0)
      params(2) = bvalue
      phisum = 0.0
c-----------------------------------------------------------------------
c  Specify the circle radius for coefficient evaluation, the required
c  accuracy, and call the complex differentiation routine ENTCRE.
c
      rcirc = exp(-one/max(mtop,1024))
      epreq = 0.367879*epstol
      ncode = 0
      ntab = 0
      call entcre(phifun,zero,rcirc,epreq,epmach,mtop,ncode,
     $            epest,mact,acoef,work,ntab,sintab,params,f)
c
c  Unnormalize coefficients and compute parameters for use in MODUL2.
c
      fact = 1.0
      ealfa = 1.0
      ebeta = 1.0
c
      do 10 j = 1, mact
         acoef(j) = acoef(j)/fact
         fact = rcirc*fact
         if (acoef(j) .ne. 0.0) then
            ealfa = max(ealfa,abs(acoef(j)))
            ebeta = min(ebeta,abs(acoef(j)))
            end if
   10    continue
      errvec(7) = log(ealfa)
      errvec(8) = log(max(ebeta,epmach))
c-----------------------------------------------------------------------
c  The primary purpose of this routine, that of determining mact and
c  computing the coefficients acoef, has now been completed.  The rest
c  of the routine is devoted to calculating the error estimate array
c  errvec and the return code nstate.
c
c  Compute Cauchy inequality constants K=xkest and R=rest in
c  two stages: first from fit to coefficients acoef on their full
c  range, then from fit on the second half only of their range.
c  From these two fits estimate R and then use it to compute K.
c
      nsbot = 0
      nstop = mact - 1
c
   20 continue
c
c     nsbot is lowest s-index and nstop is highest s-index for fit.
c     Index s corresponds to acoef(s+1).
c
      do 30 j = 1, 3
         ab(j) = max(log(abs(acoef(nsbot+j))),-unlog)
         at(j) = max(log(abs(acoef(nstop-2+j))),-unlog)
   30    continue
c
c     Pivot selection - consider nine curves in a natural order.
c
      nsdiff = nstop - nsbot
      if (nsdiff .lt. 5) then
c
c        Default option when there are not six distinct points.
c
         nb = 1
         nt = 3
         go to 90
         end if
c
      do 80 nb = 1, 3
         do 70 nt = 1, 3
            nsdiff = nstop - 2 - nsbot + nt - nb
c
c     The (nb,nt) curve is fixed at nsbot-1+nb and nstop-3+nt.
c     In the following two loops, the ordinates of this curve at each
c     of the other four points (t1) are compared with the corresponding
c     actual ordinates (t2).  If this curve passes below one of the
c     actual ordinates it is abandoned and the next curve is considered.
c
            do 40 jb = 1, 3
               if (jb .ne. nb) then
                  t1 = (jb - nb)*at(nt) + (nsdiff - (jb - nb))*ab(nb)
                  t2 = nsdiff*ab(jb)
                  if (t1 .lt. t2) go to 60
                  end if
   40          continue
c
            do 50 jt = 1, 3
               if (jt .ne. nt) then
                  t1 = (nt - jt)*ab(nb) + (nsdiff - (nt - jt))*at(nt)
                  t2 = nsdiff*at(jt)
                  if (t1 .lt. t2) go to 60
                  end if
   50          continue
c
            go to 90
c
   60       continue
   70       continue
   80    continue
c
c     End of pivot selection.
c
   90 continue
      r2 = exp((ab(nb)-at(nt))/nsdiff)
c
      if (nsbot .eq. 0) then
         r1 = r2
         nsbot = mact/2 - 1
         go to 20
         end if
c
c     If both estimates of R are smaller than or too close to one,
c     return with no estimates of K or errors computed.
c
      if (r1 .lt. 1.0001 .and. r2. lt. 1.0001) then
         nstate = -3
         do 100 j = 1, 5
            errvec(j) = -1.0
  100       continue
         errvec(6) = r2
         return
         end if
c
      rest = max(r2,(r1+1)/2)
      xkest = abs(acoef(nsbot+nb))*rest**(nsbot-1+nb)
c
      jbot = nsbot + 3
      jtop = nstop - 3
      fact = rest**jbot
      do 110 j = jbot, jtop
         xkest = max(xkest,abs(acoef(j+1))*fact)
         fact = rest*fact
  110    continue
c-----------------------------------------------------------------------
c  Constants K and R are now available.  We may now calculate the
c  rest of the errvec array.
c
      restm = rest**mact
      errvec(3) = (rest/(rest - 1))*xkest/restm
      rcircm = rcirc**mact
      errvec(2) = errvec(3)*rcircm*(restm - 1)/(restm - rcircm)
c
c     Determine eps noise using the average of the
c     (mact/2 + 2) absolute values of phifun.
c
      epsnoi = 2*epmach*phisum/(mact + 4)
c
      errvec(4) = 0.577350*epsnoi*rcirc/rcircm*
     $                            sqrt((rcircm**2-1)/(rcirc**2-1))
      errvec(1) = errvec(2) + errvec(3) + errvec(4)
      errvec(5) = xkest
      errvec(6) = rest
c-----------------------------------------------------------------------
c  Finally, store the output state parameter nstate.
c
      nstate = 0
      if (errvec(1) .gt. epstol) nstate = ncode
c
      return
      end













      subroutine modul2(tvalue,mact,acoef,sigma,bvalue,errvec,finv,iflo)

      integer mact,iflo
      double precision tvalue,sigma,bvalue,finv
      double precision acoef(mact),errvec(8)
c  This subroutine is MODUL2.  It evaluates, for a specified nonnegative
c  tvalue, the inverse finv of the prescribed Laplace transform using
c  the series below, denoted as SUM, whose coefficients are provided by
c  MODUL1.  The values of sigma and bvalue are those returned by MODUL1.
c
c      finv = exp(sigma*tvalue)*SUM
c  where
c      SUM = summation of (acoef(j)*exp(-bt/2)*L(j,bt),j=1,mact),
c      bt = bvalue*tvalue,
c      L(j,bt) denotes the Laguerre polynomial of degree j-1,
c      exp(-bt/2)*L(j,bt) is the associated Laguerre function.
c
c  When tvalue is nonpositive, the evaluation approximates
c  the analytic continuation of the inverse Laplace transform,
c  becoming progressively poorer as tvalue becomes more negative.
c
c  Note that this routine is overflow/underflow(destructive) free
c  and can be used even when the value exp(sigma*tvalue) overflows
c  or exp(-bt/2) underflows.
c
c  INPUT arguments
c
c  tvalue - real - the point where the inverse Laplace transform
c                  is to be computed.
c  mact   - integer - the number of terms of the Laguerre expansion.
c  acoef  - real(mact) - the coefficients of the Laguerre expansion.
c  sigma  - real - the first parameter of the Laguerre expansion.
c                  It must have the same value as returned by MODUL1.
c  bvalue - real - the second parameter of the Laguerre expansion.
c                  It must have the same value as returned by MODUL1.
c  errvec - real(8) - the vector of diagnostic information from MODUL1.
c                     Only components 1,7 and 8 are used in MODUL2.
c                     (If MODUL2 is used independently of MODUL1, store
c                     ALPHA,BETA into (7),(8) and set errvec(1) = 0.0 .)
c
c  OUTPUT arguments
c
c  finv   - real - the calculated value of the inverse Laplace transform
c                  at tvalue, if iflo (see below) is zero.
c  iflo   - integer - the overflow/underflow indicator.
c           0 => Normal termination.
c           1 => The value of the inverse Laplace transform is found to
c                be too large to be representable - finv is set to 0.0.
c          -1 => The value of the inverse Laplace transform is found to
c                be too small to be representable - finv is set to 0.0.
c           2 => The value of the inverse Laplace transform is estimated
c                to be too large, even before the series expansion,
c                to be representable - finv is set to 0.0.
c          -2 => The value of the inverse Laplace transform is estimated
c                to be too small, even before the series expansion,
c                to be representable - finv is set to 0.0.
c-----------------------------------------------------------------------
      integer j
      double precision alfa,beta,blog,bt,bthalf,errlog,escale,expon,
     $                 one,ovlog,polnex,polnow,polpre,scale,slog,
     $                 sum,thresh,tlog,unlog,upexp,xmact

      data ovlog,unlog /709.0d0,708.0d0/
c
c  Initialize variables.
c
      alfa = errvec(7)
      beta = errvec(8)
      scale = unlog + beta
      escale = exp(-scale)
      upexp = 0.0
      finv = 0.0
c
      one = 1.0
      blog = log(max(abs(bvalue),one))
      slog = log(max(abs(sigma),one))
      tlog = log(max(abs(tvalue),one))
      iflo = 2
      if (blog+tlog .gt. ovlog .or. slog+tlog+1 .gt. ovlog) return
      if (errvec(1) .ne. 0.0) then
         errlog = log(abs(errvec(1)))
         if (sigma*tvalue + errlog .gt. ovlog) return
         iflo = -2
         if (sigma*tvalue + errlog + alfa - beta .lt. -unlog) return
         end if
c-----------------------------------------------------------------------
c  Compute sum.
c
      bt = bvalue*tvalue
      bthalf = 0.5*bt
      polpre = 0.0
      polnow = 1.0
      sum = acoef(1)
      xmact = mact
      thresh = ovlog - alfa - log(max(xmact,2+abs(bt)))
      if (abs(bthalf) .le. thresh) then
         do 10 j = 1, mact-1
            polnex = 2*polnow - polpre - ((1+bt)*polnow - polpre)/j
            sum = sum + acoef(j+1)*polnex
            polpre = polnow
            polnow = polnex
   10       continue
      else
         thresh = exp(ovlog-alfa)/(2+abs(bt))
         do 20 j = 1, mact-1
            if (abs(polnow) .gt. thresh) then
               polnow = escale*polnow
               polpre = escale*polpre
               sum = escale*sum
               upexp = upexp + scale
               end if
            polnex = 2*polnow - polpre - ((1+bt)*polnow - polpre)/j
            sum = sum + acoef(j+1)*polnex
            polpre = polnow
            polnow = polnex
   20       continue
         end if
c-----------------------------------------------------------------------
c  Compute finv.
c
      iflo = 0
      if (sum .eq. 0.0) return
      expon = sigma*tvalue - bthalf + upexp + log(abs(sum))
      if (expon .gt. ovlog) then
         iflo = 1
         return
         end if
      if (expon .lt. -unlog) then
         iflo = -1
         return
         end if
      finv = sign(exp(expon),sum)
c
      return
      end










      SUBROUTINE PHIFUN(ZVALRE,ZVALIM,FVALRE,FVALIM,PARAMS,F)
C     double complex function phifun(z,params,f)
C     double complex z,f
      DOUBLE PRECISION ZVALRE,ZVALIM,FVALRE,FVALIM
      double precision params(3)
      external f
c-----------------------------------------------------------------------
c  This function is PHIFUN, invoked from ENTCRE in the execution of
c  MODUL1.  It defines the function whose Taylor coefficients become
c  the coefficients of the series evaluated in MODUL2.  PHIFUN invokes
c  the user-provided transform function, denoted here by f.
c
      double precision bvalue,phisum,sigma
      DOUBLE PRECISION ARGRE,ARGIM,RESRE,RESIM,SQNORM,T1,T2
c
      sigma = params(1)
      bvalue = params(2)
      phisum = params(3)
c
      SQNORM = (1-ZVALRE)**2 + ZVALIM**2
      T1 = BVALUE*(1-ZVALRE)/SQNORM
      T2 = BVALUE*ZVALIM/SQNORM
C     phifun = (bvalue/(1 - z))*f((bvalue/(1-z))+sigma-bvalue/2)
      ARGRE = T1 + SIGMA - BVALUE/2
      ARGIM = T2
      CALL F(ARGRE,ARGIM,RESRE,RESIM)
      FVALRE = T1*RESRE - T2*RESIM
      FVALIM = T1*RESIM + T2*RESRE
C     phisum = phisum + abs(phifun)
      PHISUM = PHISUM + ABS(FVALRE) + ABS(FVALIM)
c-----------------------------------------------------------------------
c  phisum is used to estimate the condition error reported in
c  errvec(4) by MODUL1.
c
      params(3) = phisum
      return
      end









      subroutine entcre ( cfun, zeta, rcirc, epreq, epmach, nmax, ncode,
     . epest, ntcof, tcof, work, ntab, sintab, params, fentry )
c
c ** evaluation of normalised taylor coefficients of a real analytic fun
c
c
c     **  general purpose  **
c
c this routine evaluates a set of normalised taylor coefficients
c     tcof(j+1) = (rcirc**j) * (j-th derivative of cfun(z) at z = zeta)
c              divided by factorial(j)    . . . . j = 0,1,2,3....nmax-1.
c to a uniform absolute accuracy **epest** using function values of cfun
c points in the complex plane lying on the circle of radius **rcirc** wi
c center at z = zeta. this routine is a special version of entcaf for us
c zeta is real and also cfun(z) is real when z is real.
c
c
c     ** theoretical restrictions **
c
c rcirc must be smaller than the radius of convergence of the taylor ser
c the problem has to be reformulated should cfun(z) happen to be an odd
c of  (z - zeta)  , that is if the relation ** -cfun(-(z-zeta))=cfun(z-z
c is an identity.
c
c
c     ** requirements for calling program **
c
c calling program must contain control statements described in notes (3)
c below.  it must also assign values to input parameters.  the routine r
c two subprograms,  hfcof  (listed after entcre) and cfun (see note (4)
c
c
c     **input parameters**
c
c  (1)  cfun     name of complex function subprogram.
c  (2)  zeta     real point about which taylor expansion is required.
c  (3)  rcirc    radius (real).
c  (4)  epreq    the absolute accuracy (real) to which the normalised ta
c                coefficients, tcof(j), are required.
c  (5)  epmach   the machine accuracy parameter (real) (or an upper boun
c                relative accuracy of quantities likely to be encountere
c  (6)  nmax     physical upper limit on the size and length of the calc
c                the maximum number of coefficients calculated will be t
c                power of two less than or equal to nmax.  nmax is assum
c                be at least 8.  (see note(3) below.)
c  (7)  ncode    .ge.0  the routine will do as well as  it can.
c                .lt.0  the routine will abort at an early stage if the
c                accuracy cannot be attained because of round off error.
c  (12) ntab     in normal running, ntab should be set to zero before th
c                call to entcre, but left alone after that. ( for more s
c                isticated use, see output parameters (12) and (13)  and
c                below.)
c  (14) params   real parameter vector passed to cfun. (see note(4) belo
c  (15) fentry   entry point passed to cfun. (see note(4) below.)
c
c
c     ** output parameters **
c
c  (1),(2),(3),(4),(5),(6) identical with input values.
c  (7)  ncode    result status indicator. takes one of five values as fo
c                = +1. converged normally.
c                = -1. did not converge. no round off error trouble.
c                = +2. converged, but with a higher tolerance set by the
c                      off level. ( epest.gt.epreq )
c                = -2. did not converge in spite of higher tolerance set
c                      round off level.
c                =  0. run was aborted because epreq is unattainable due
c                      round off level and input ncode is negative.
c  (8)  epest    estimate of actual uniform absolute accuracy in all tco
c                except , if ncode.eq.0  estimate of round off level.
c  (9)  ntcof    number of nontrivial values of  tcof  actually calculat
c                they are based on  ntcof/2+2  calls of cfun (three call
c                were for purely real argument).
c  (10) tcof     real dimension (dim).  approximations to the normalised
c                coefficients, except when output ncode = 0. (see note(3
c  (11) work     internal working area of real dimension (dim) ( see not
c                below.  contents is identical with that of tcof.
c  (12) ntab     number of values of sintab available (see note(2) below
c  (13) sintab   real  dimension  (dim/4).   ( see notes (2) and (3) bel
c                sintab(j+1) = sin(pi*j/2*ntab) ,  j = 0,1,2,....ntab-1.
c                (a quarter cycle) other locations are empty.
c
c
c     ** notes on input/output parameters **
c
c  note(1)**  ncode is used both as input and output parameter.  normall
c     retains the value  +1  and need not be reset between normal runs.
c  note(2)**  the appearance of ntab and sintab in the calling sequence
c     the user to make use of - or to  precompute - these numbers in ano
c     part of the program should he so desire. ntab must be a power or t
c  note(3)**  the appearance of nmax,tcof,work and sintab in the calling
c     sequence allows the scope of the subprogram and the amount of stor
c     be assigned by the calling program, which should contain a control
c     ment to the following effect
c     real tcof(dim), work(dim), sintab(dim/4)
c     where  dim  is normally a power of two.  nmax is normally equal to
c     dim, but may be less than dim.
c  note(4)**  cfun(z,res,params,fentry) is a user provided subroutine su
c     with (pseudo) complex argument and result.  Possible parameters fo
c     and/or callable subprogram from cfun are communicated through para
c     fentry.  the calling program must contain control statements as fo
c     external cfun  external fentry
c
c
c     ** bookkeeping parameters for stage one **
c
c  nconv   1  convergence achieved.
c         -1  no convergence achieved.
c  nround  1  no round off trouble observed.
c          2  round off trouble observed.
c  nabort  0  update tolerance and continue on appearance of round off t
c          1  terminate when round off trouble observed.
c  exact   the exact value of tcof(1) which is cfun(zeta).
c  safety  this is a safety factor by which the routine avoids the round
c     level.  it is set to 10.0 and appears only in the combination (saf
c     epmach).  to alter this factor, or to remove the round off error g
c     completely, the user need only adjust the input parameter epmach
c     appropriately.
c
c
c     ** quantities calculated in stage three(a) **
c
c  this is the first part of iteration number ntcof. presently available
c                sintab(j+1) = sin(pi*j/2*ntab) , j = 0,1,2,...ntab-1.
c  we require the sequence  sin(pi*j/2*(ntcof/4)), j = 1,3,5,...(ntcof/4
c  if(ntcof.le.4*ntab) these numbers are already available in the sintab
c  spaced at an interval  2*nspace = 8*ntab/ntcof.
c  otherwise, ntcof = 8*ntab and the sintab table is updated. this invol
c  rearranging the ntab values available, calculating and storing ntab n
c  values and updating ntab to 2*ntab.
c
c
c     ** quantities calculated in stage three(b) **
c
c  iterations are numbered 8,16,32... at the end of iteration number ntc
c  ntcof/2 + 1 complex function values at abscissas regularly spaced on
c  half of circle are stored in the tcof vector as follows.
c     tcof(j+1)       =    real   part of cfun(z(j))   j=0,1,2,....ntcof
c     tcof(ntcof-j+1) = imaginary part of cfun(z(j))   j=1,2,...(ntcof/2
c  where
c     z(j)  =  zeta + rcirc*cexp(2*pi*eye*j/ntcof)
c  this involves  a rearrangement of the ntcof/4 + 1 function values ava
c  at the start of the iteration and the calculation of a further ntcof/
c  function values.  in addition fmax and approx are calculated.  these
c     fmax     maximum modulus of the function values so far encountered
c     approx   an approximation to tcof(1) based on these function value
c
c
c     ** quantities calculated at stage three(c) **
c
c  error1  current value of the error = abs(approx-exact).
c  error2,error3,error4  values of error at end of three previous iterat
c  epmach  machine accuracy parameter. (input parameter)
c  epreq   required accuracy. (input parameter)
c  epro    highest accuracy reasonably attainable in view of the size of
c          function values so far encountered. (=10.0*epmach*fmax)
c  epcof   currently required accuracy (=amax1(epreq,epro)).
c  epest   estimate of current accuracy. (the maximum of epro and a func
c          of errors 1,2,3 and 4 ) (output parameter)
c
c
c     ** convergence and termination checks in stage three(c) **
c
c  (1)  uses fmax to raise epcof above round off level.  if this is nece
c  and the input value of ncode is negative, it terminates setting ncode
c  (2)  uses approx to evaluate convergence of tcof(1) towards exact.  i
c  assign convergence and go to stage four(a) setting ncode = +1 or +2.
c  (3)  uses nmax to check physical limit.  if this has been reached, it
c  stage four(a) setting ncode = -1 or -2.
c  (4)  otherwise continues next iteration by going to stage three.
c
c
c     **  calculation of first ntcof taylor coefficients in stage four(a
c
c  a version of the fast fourier transform using a work array is used.
c  the array **work** is used only during this stage.  the work array al
c  the permuting of indicies associated with in-place fft@s to be suppre
c  the fft calculates the neccessary summations except for dividing by n
c
c
c     **  setting of remaining taylor coefficients in stage four(b)  **
c
c  the convergence criterion allows us to infer that the normalized tayl
c  coefficients of order greater then  ntcof  are zero to accuracy  epes
c  they are evaluated as being exactly zero.
c
c
      double precision zeta,rcirc,epreq,epmach,epest
      integer nmax,ncode,ntcof,ntab
      double precision tcof (*), work (*), sintab (*)
      double precision params(*)
      external hfcof
      external cfun
C     double complex cfun
      external fentry
C     double complex fentry
c
c
      integer nabort,nconv,ndisp,ndolim,nprev,nround,nspace
      integer j,jconj,jcos,jfrom,jrconj,jrefl,jsin,jto
      double precision approx,cosdif,epcof,epmin,epro,ep32,ep42
      double precision error1,error2,error3,error4,exact,fmax,fvalim
      double precision fvalre,rcos,rsin,safety,scale,supper,twopi
C     double complex fval,zval
c
      EXTERNAL PYTHAG
      double precision half,one,zero,PYTHAG
      data half,one,zero /0.5d0,1.0d0,0.0d0/
c
c            ***   stage one   ***
c            ---------------------
c            initialise bookkeeping parameters and exact value of tcof(1
c
      nround = 1
      nabort = 0
      if (ncode.lt.0) nabort = 1
      epcof = epreq
      safety = 10.0
C     zval = dcmplx(zeta,zero)
C     fval = cfun(zval,params,fentry)
C     fvalre = dreal(fval)
      CALL CFUN(ZETA,ZERO,FVALRE,FVALIM,PARAMS,FENTRY)
      exact = fvalre
c
c            ***   stage two   ***
c            ---------------------
c            first three iterations ( those with ntcof = 1,2,4 ).
c
C     zval = dcmplx(zeta+rcirc,zero)
C     fval = cfun(zval,params,fentry)
C     fvalre = dreal(fval)
      CALL CFUN(ZETA+RCIRC,ZERO,FVALRE,FVALIM,PARAMS,FENTRY)
      approx = fvalre
      fmax = abs(fvalre)
      tcof(1) = fvalre
      error3 = abs(approx-exact)
C     zval = dcmplx(zeta-rcirc,zero)
C     fval = cfun(zval,params,fentry)
C     fvalre = dreal(fval)
      CALL CFUN(ZETA-RCIRC,ZERO,FVALRE,FVALIM,PARAMS,FENTRY)
      approx = 0.5*(approx+fvalre)
      fmax = max(fmax,abs(fvalre))
      tcof(3) = fvalre
      error2 = abs(approx-exact)
C     zval = dcmplx(zeta,rcirc)
C     fval = cfun(zval,params,fentry)
C     fvalre = dreal(fval)
C     fvalim = dimag(fval)
      CALL CFUN(ZETA,RCIRC,FVALRE,FVALIM,PARAMS,FENTRY)
      approx = 0.5*(approx+fvalre)
      fmax = max(fmax,PYTHAG(FVALRE,FVALIM))
      tcof(2) = fvalre
      tcof(4) = fvalim
      error1 = abs(approx-exact)
      ntcof = 4
      epro = fmax*safety*epmach
      if (epro.lt.epcof) go to 300
        epcof = epro
        nround = 2
        if (nabort.eq.0) go to 300
        ncode = 0
        epest = epro
        go to 470
c
c            ***   stage three   ***
c            -----------------------
c            commence iteration number ntcof.
c
  300 continue
      nprev = ntcof
      ntcof = 2*ntcof
c
c            ***   stage three(a)   ***
c            --------------------------
c            update sintab table if necessary.
c
      if (4*ntab.ge.ntcof) go to 340
      if (ntab.ge.2) go to 310
        sintab(1) = 0.0
        sintab(2) = sqrt(half)
        ntab = 2
        go to 340
  310 continue
      ndolim = ntab-1
      do 320 j = 1,ndolim
        jfrom = ntab-j
        jto = 2*jfrom
        sintab(jto+1) = sintab(jfrom+1)
  320 continue
      ntab = 2*ntab
      twopi = 8.0*atan(one)
      cosdif = cos(twopi/(4*ntab))
      ndolim = ntab-3
      do 330 j = 1,ndolim,2
        sintab(j+1) = (0.5*sintab(j)+0.5*sintab(j+2))/cosdif
  330 continue
      sintab(ntab) = cosdif
  340 continue
c
c            ***   stage three(b)   ***
c            --------------------------
c            update list of function values in tcof, calculate fmax and
c
      ndolim = nprev-1
      do 350 j = 1,ndolim
        jfrom = nprev-j
        jto = 2*jfrom
        tcof(jto+1) = tcof(jfrom+1)
  350 continue
      supper = 0.0
      ndolim = (nprev/2)-1
      nspace = (4*ntab)/ntcof
      do 360 j = 1,ndolim,2
        jsin = j*nspace
        jcos = ntab-jsin
        rsin = rcirc*sintab(jsin+1)
        rcos = rcirc*sintab(jcos+1)
        jconj = ntcof-j
C       zval = dcmplx(zeta+rcos,rsin)
C       fval = cfun(zval,params,fentry)
C       fvalre = dreal(fval)
C       fvalim = dimag(fval)
        CALL CFUN(ZETA+RCOS,RSIN,FVALRE,FVALIM,PARAMS,FENTRY)
        supper = supper+fvalre
        fmax = max(fmax,PYTHAG(FVALRE,FVALIM))
        tcof(j+1) = fvalre
        tcof(jconj+1) = fvalim
        jrefl = nprev-j
        jrconj = ntcof-jrefl
C       zval = dcmplx(zeta-rcos,rsin)
C       fval = cfun(zval,params,fentry)
C       fvalre = dreal(fval)
C       fvalim = dimag(fval)
        CALL CFUN(ZETA-RCOS,RSIN,FVALRE,FVALIM,PARAMS,FENTRY)
        supper = supper+fvalre
        fmax = max(fmax,PYTHAG(FVALRE,FVALIM))
        tcof(jrefl+1) = fvalre
        tcof(jrconj+1) = fvalim
  360 continue
      approx = 0.5*approx+supper/nprev
c
c            ***   stage three(c)   ***
c            --------------------------
c            convergence and termination check.
c
      error4 = error3
      error3 = error2
      error2 = error1
      error1 = abs(approx-exact)
      epro = fmax*safety*epmach
      if (epro.lt.epcof) go to 370
        epcof = epro
        nround = 2
        if (nabort.eq.0) go to 370
        ncode = 0
        epest = epro
        go to 470
  370 continue
      error4 = max(error4,epro)
      error3 = max(error3,epro)
      ep42 = error2*((error2/error4)**(4.0/3.0))
      ep32 = error2*((error2/error3)**2)
      epmin = min(error2,ep32,ep42)
      epest = max(error1,epmin,epro)
      if (epest.gt.epcof) go to 380
        nconv = 1
        go to 400
  380 continue
      if (2*ntcof.le.nmax) go to 300
        nconv = -1
c
c            ***   stage four(a)   ***
c            -------------------------
c            calculation of first ntcof taylor coefficients using f.f.t.
c
  400 continue
      ncode = nconv*nround
      ndisp = ntcof
  410 continue
      ndisp = ndisp/2
      call hfcof (ntcof,ndisp,tcof,work,ntab,sintab)
      if (ndisp.gt.1) go to 430
      do 420 j = 1,ntcof
        tcof(j) = work(j)
  420 continue
      go to 440
  430 continue
      ndisp = ndisp/2
      call hfcof (ntcof,ndisp,work,tcof,ntab,sintab)
      if (ndisp.gt.1) go to 410
  440 continue
      scale = one/ntcof
      do 450 j = 1,ntcof
        tcof(j) = tcof(j)*scale
        work(j) = tcof(j)
  450 continue
c
c            ***   stage four(b)   ***
c            -------------------------
c            setting of remaining taylor coefficients.
c
      if (ntcof.ge.nmax) go to 470
      ndolim = ntcof+1
      do 460 j = ndolim,nmax
        tcof(j) = 0.0
        work(j) = 0.0
  460 continue
  470 continue
      return
c      end of entcre
      end












      subroutine hfcof ( ntcof, ndisp, tcof, work, ntab, sintab )
c
c ** hermitian fourier coefficients **
c
c
c     **  general purpose  **
c
c this routine does one pass of a fast fourier transform.  the indexing
c arranged so that the coefficients are in order at the end of the last
c this indexing requires the use of separate arrays for input and output
c the partial results.  this routine is called once for each pass.
c
c
c     **  input parameters  **
c
c  (1)  ntcof    number of coefficients to be processed.
c  (2)  ndisp    maximum value of displacement index.
c  (3)  tcof     (real) input array.
c  (5)  ntab     number of entries in sintab.
c  (6)  sintab   (real) table of values of sine.
c                sintab(j+1) = sin(pi*j/2*ntab), j =0,1,2,...ntab-1.
c
c
c     **  output parameters  **
c
c  (4)  work     (real) output array.
c
c
c     **  indexing of arrays  **
c
c  the two point fourier transform is applied to the points of tcof with
c  indicies
c     jdisp*nprev+jrepl  and  jdisp*nprev+jrepl+nhalf
c  the results are modified by the appropriate twiddle factor and stored
c  work with indicies
c     jdisp*nnext+jrepl  and  jdisp*nnext+jrepl+nprev
c  where
c     ndisp      product of remaining factors.
c     nprev      product of previous factors.
c     nnext      product of previous and current factors.
c     nhalf      product of previous and remaining factors.
c     jrepl      replication index = 1,2,...nprev.
c     jdisp      hermitian symmetry in this index results in three cases
c                1.) initial point - jdisp=0.  input points are purely r
c                and output points are purely real.
c                2.) middle point - jdisp=ndisp/2 - not always present.
c                input points are complex and output points are purely r
c                3.) intermediate points - jdisp=1,2,...(ndisp/2-1) -
c                not always present.  input points are complex and outpu
c                points are complex.
c
c  on input, the hermitian symmtery is in a block of length 2*ndisp, i.e
c  point conjugate to  jdisp  is  2*ndisp-jdisp.  on output, the hermiti
c  symmetry is in a block of length ndisp, i.e. the point conjugate to
c  is  ndisp-jdisp.  a hermitian symmetric block has real parts at the f
c  and imaginary parts (when they exist) at the conjugate positions at t
c
c  the twiddle factor  cexp(-pi*eye*j/ndisp), j=1,2,...(ndisp/2-1) is ob
c  as separate real and imaginary parts from the sintab table.  the imag
c  part  sin(pi*j/ndisp)  is found at a spacing of nspace=2*ntab/ndisp i
c  sintab.  the real part is found at a conjugate position in the table.
c
c
      integer ntcof,ndisp,ntab
      double precision tcof (*), work (*), sintab (*)
c
c
      double precision cs,is,iu,i0,i1,rs,ru,r0,r1,sn
      integer jconj,jcos,jdisp,jrepl,jsin,jt,jtc,jw,jwc,kt0,kt1
      integer kt2,kt3,kw0,kw1,kw2,kw3,nhalf,nmidl,nnext,nprev,nspace
c
c
      nhalf = ntcof/2
      nprev = ntcof/(2*ndisp)
      nnext = ntcof/ndisp
      nmidl = (ndisp-1)/2
      nspace = (2*ntab)/ndisp
c
c  initial points of blocks.
c
      do 100 jrepl = 1,nprev
        kt0 = jrepl
        kt1 = kt0+nhalf
        kw0 = jrepl
        kw1 = kw0+nprev
        r0 = tcof(kt0)
        r1 = tcof(kt1)
        work(kw0) = r0+r1
        work(kw1) = r0-r1
  100 continue
c
c  intermediate points of blocks.
c
      if (nmidl.lt.1) go to 400
      do 300 jdisp = 1,nmidl
        jconj = ndisp-jdisp
        jsin = jdisp*nspace
        jcos = ntab-jsin
        sn = sintab(jsin+1)
        cs = sintab(jcos+1)
        jt = jdisp*nprev
        jtc = jconj*nprev
        jw = jdisp*nnext
        jwc = jconj*nnext
        do 200 jrepl = 1,nprev
          kt0 = jt+jrepl
          kt1 = kt0+nhalf
          kt2 = jtc+jrepl
          kt3 = kt2+nhalf
          kw0 = jw+jrepl
          kw1 = kw0+nprev
          kw2 = jwc+jrepl
          kw3 = kw2+nprev
          r0 = tcof(kt0)
          i0 = tcof(kt3)
          r1 = tcof(kt2)
          i1 = -tcof(kt1)
          rs = r0+r1
          is = i0+i1
          ru = r0-r1
          iu = i0-i1
          work(kw0) = rs
          work(kw2) = is
          work(kw1) = ru*cs+iu*sn
          work(kw3) = iu*cs-ru*sn
  200   continue
  300 continue
  400 continue
c
c  middle points of blocks.
c
      if (ndisp.le.1) go to 600
      jt = (ndisp/2)*nprev
      jw = (ndisp/2)*nnext
      do 500 jrepl = 1,nprev
        kt0 = jt+jrepl
        kt1 = kt0+nhalf
        kw0 = jw+jrepl
        kw1 = kw0+nprev
        r0 = tcof(kt0)
        i0 = tcof(kt1)
        work(kw0) = 2.0*r0
        work(kw1) = 2.0*i0
  500 continue
  600 continue
      return
c      end of hfcof
      end








      double precision function pythag(a,b)
      double precision a,b
c
c     finds dsqrt(a**2+b**2) without overflow or destructive underflow
c
      double precision p,r,s,t,u
      p = dmax1(dabs(a),dabs(b))
      if (p .eq. 0.0d0) go to 20
      r = (dmin1(dabs(a),dabs(b))/p)**2
   10 continue
         t = 4.0d0 + r
         if (t .eq. 4.0d0) go to 20
         s = r/t
         u = 1.0d0 + 2.0d0*s
         p = u*p
         r = (s/u)**2 * r
      go to 10
   20 pythag = p
      return
      end

