
c-----------------------------------------------------------------------
      subroutine sxtab1(pval,gamma,xn,xk,nrows,ierret,kprint)
      implicit double precision (a-h,o-z)
c-----------------------------------------------------------------------
c
c   vector version of xtab1
c
      dimension pval(1),gamma(1),xn(1),xk(1)
      ierret=0
      do 22 irow=1,nrows
      call xtab1(pval(irow),gamma(irow),xn(irow),xk(irow),ier,kprint)
      if(ier.ge.0)ierret=ierret+ier
22    continue
      return
      end
c-----------------------------------------------------------------------
      subroutine xtab1(pval,gamma,xn,xk,ier,kprint)
c-----------------------------------------------------------------------
c
c  #compute factors g(      ) from table 1 of odeh and owen (1980)
c  #factors for one-sided tolerance intervals and sampling plans
c
c  #pval    percentile to be bounded
c  #gamma   confidence level
c  #xn       sample size (dof=n-1)
c  #xk      output factor from table
c
c  #following theory in odeh and owen with bit search
c  #find xk such that given n, pval, and gamma,
c  #
c  #pr(tf<k*dsqrt(n))
c  #
c  #where tf is a noncentral t r.v. with f=n-1 dof and
c  #ncp=kp*dsqrt(n) and kp is the kp percentile of n(0,1)
c  #
      implicit double precision (a-h,o-z)
      common/cxtab1/fval,sqrn,zdelta,xiif,kprinp
      double precision dble,wqm_fcdfn,zeroin
      external fxtab1
      data zero,one/0.0d00,1.0d00/
      data tol/1.00d-6/
      ier=0
      kprinp=kprint
      xiif=xn-1
      if(pval.le.zero.or.pval.ge.one.or.xn.lt.2.)go to 991
      zgval=wqm_quant(gamma,3)
      zpval=wqm_quant(pval,3)
      sqrn=dsqrt(xn)
      xinit=zpval+zgval/sqrn
c -noio-      if(kprint.ge.4)write(6,4433)xinit
4433  continue
c -noio-4433  format(" initial value=",d20.5)
c  #compute the noncentrality parameter
      zdelta=zpval*sqrn
      fval=gamma
      eps=1.0d-4
      call brack(xinit,0.0d00,x1,fx1,x2,fx2,fxtab1,kprint)
cxxx  call solve(xval,fval,x1,fx1,x2,fx2,eps,fxtab1,kprint)
cxxx      call solve(dval,zero,x1,fx1,x2,fx2,eps,fxtab7,kprint)
      maxfn=100
      nsig=3
cxxx      call zbrent(fxtab1,tol,nsig,x1,x2,maxfn,ier)
cxxx       xk=x2
      xk=zeroin(x1,x2,fxtab1,tol)
cxxx      xk=xval
      return
991   ier=1
      return
      end
c-----------------------------------------------------------------------
      function fxtab1(x)
c-----------------------------------------------------------------------c
c
c     function for root finder
c
      implicit double precision (a-h,o-z)
      common/cxtab1/fval,sqrn,zdelta,xiif,kprinp
      xkrsn=x*sqrn
      delta=zdelta
      call mdtnx(xkrsn,xiif,delta,hf,ier)
      if(hf.ge.1.0)go to 991
      if(hf.le.0.0)go to 990
      fxtab1=hf-fval
      go to 50
991   fxtab1=1.-fval
      go to 50
990   fxtab1=0.-fval
50    continue
c -noio-      if(kprinp.ge.4)write(6,42)x,xkrsn,xiif,delta,hf,fval,fxtab1
42    continue
c -noio-42    format(" ftab1**4**",3g12.5,4g12.5)
      return
      end


c-----------------------------------------------------------------------
      subroutine sxtab3(xn,p,q,fk,nrows,kprint)
      implicit double precision (a-h,o-z)
c-----------------------------------------------------------------------
c
c   vector version of xtab3
c
      dimension xn(1),p(1),q(1),fk(1)
      do 22 irow=1,nrows
      call xtab3(xn(irow),p(irow),q(irow),fk(irow),kprint)
22    continue
      return
      end


c-----------------------------------------------------------------------
      subroutine xtab3(xn,p,q,fk,kprint)
c-----------------------------------------------------------------------c
c
c  computes table 3 from Odeh and owen; algorithm by Bob Odeh
c
c     tolerance interval to control the center
c
c  sample size n
c  contain probability p
c  confidence level q
c  returns factor in fk
c
      implicit double precision (a-h,o-z)
      dimension rs(101)
c -noio-      if(kprint.ge.2)write(6,66) xn,p,q
 66   continue
c -noio- 66   format('xtab3 xn,p,q',3d24.16)
      d1=xn
      qq=1.d0-q
      d2=dsqrt(d1)
      d3=1.d0/d2
      df=xn-1
      xx=dsqrt(df/chisqd(df,qq))
      pp=wqm_pinv((1.d0-p)/2.d0)
c -noio-      if(kprint.ge.2)write(6,666) p,pp
 666  continue
c -noio- 666  format('xtab3 p, pp',2d24.16)
      s1=xx*rcomp(d3,p,pp)
      del=d2*pp
      call loadr(p,xn,rs)
      call inttab3(df,s1,s1y,rs)
c -noio-c      write(6,6666) s1,s1y
 6666 continue
c -noio- 6666 format('xtab3',1x,2d24.16)
      if (s1y.lt.qq) s2=s1-.002d0
      if( s1y.ge.qq) s2=s1+.002d0
      call inttab3(df,s2,s2y,rs)
c      write (6,66) s2,s2y
      s1y=s1y-qq
      s2y=s2y-qq
20    dif=s2y-s1y
      sb=s2y*s1/dif-s1y*s2/dif
      call inttab3(df,sb,ans,rs)
c -noio-c      write(6,66) sb,ans
      s1=s2
      s2=sb
      s1y=s2y
      s2y=ans-qq
      if (dabs(s1-s2).gt.1.d-7) go to 20
      fk=sb
      return
      end
c-----------------------------------------------------------------------
      subroutine loadr(pd,xn,rs)
c-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      dimension rs(101)
      fn=xn
      sn=dsqrt(fn)
c -noio-c      write(6,42)pd,n
 42   continue
c -noio- 42   format('loadr',f13.5,i5)
      rs(1)=wqm_pinv((1.d0-pd)/2.d0)
      do 10 jj=2,101
      yy=(jj-1)*0.0625d0/sn
10    rs(jj)=rcomp(yy,pd,rs(jj-1))
      return
      end
c-----------------------------------------------------------------------
      subroutine inttab3(df,d,sum,rs)
c-----------------------------------------------------------------------
      implicit double precision  (a-h,o-z)
      dimension rs(101)
      g1(x)=dexp(-x**2/2.d0)/dsqrt(2.d0*darcos(-1.d0))
      sum=0.d0
      con=1.d0
      do 100 l=1,101
      y=dfloat(l-1)*0.0625d0
      yd=df*(rs(l)/d)**2
      call gamain (yd/2.d0,df/2.d0,ans)
62    continue
c -noio-62    format (1x,'inttab3 ans',2d24.16)
      ans=ans*g1(y)
      sum=sum+con*ans
c       write (6,62) ans,sum
      con=4.d0
      if (l/2*2.eq.l) con=2.d0
      if (l.eq.101) con =1.d0
  100 continue
      sum=sum/24.d0
      return
      end
c-----------------------------------------------------------------------
      subroutine gamain(x,p,gammai)
c-----------------------------------------------------------------------
      double precision dlog10
      double precision dlog,dexp,dlgama,dabs
      double precision pn(6),x,p,gammai,acu,oflo,factor
      double precision gin,term,rn,a,b,an
      double precision dif,xp
      xp = p*dlog(x)-x-dlgama(p)
c -noio-c      write(6,422)x,p
c 422  format('gamain',2f12.5)
c     if (xp.gt.-180.d0) go to 1
c     gammai = 1.d0
c     return
  1   acu=1.d-12
      oflo = 1.0d30
      factor = dexp(xp)
      if (x.lt.1.d0) go to 61
      if (x.ge.p) go to 30
   61 gin = 1.d0
      term = 1.d0
      rn = p
   20 rn = rn + 1.d0
      term = term*x/rn
      gin = gin + term
      if (term.gt.acu) go to 20
      if(dlog10(gin)+dlog10(factor)-dlog10(p).lt.-50.d0) gin=0.d0
      gin = gin*factor/p
      go to 50
   30 a = 1.d0-p
      b = a + x + 1.d0
      term = 0.d0
      pn(1) = 1.d0
      pn(2) = x
      pn(3) = x + 1.d0
      pn(4) = x*b
      gin = pn(3)/pn(4)
   32 a = a + 1.d0
      b = b + 2.d0
      term = term + 1.d0
      an = a*term
      do 33 i = 1,2
   33 pn(i+4) = b*pn(i+2)-an*pn(i)
      if (pn(6).eq.0.d0) go to 35
      rn = pn(5)/pn(6)
      dif = dabs(gin-rn)
      if (dif.gt.acu) go to 34
      if (dif.le.acu*rn) go to 42
   34 gin = rn
   35 do 36 i = 1,4
   36 pn(i) = pn(i+2)
      if (dabs(pn(5)).lt.oflo) go to 32
      do 41 i = 1,4
   41 pn(i) = pn(i)/oflo
      go to 32
   42 if(dlog10(factor)+dlog10(gin).lt.-50.d0)  gin=0.d0
      gin = 1.d0-factor*gin
   50 gammai = gin
      return
      end
c-----------------------------------------------------------------------
      double precision function rcomp(xx,p,rr)
c-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      g(x)=dsign(1.d0,x)*derf(dabs(x)/dsqrt(2.d0))/2.d0
      g1(x)=dexp(-x*x/2.d0)/dsqrt(2.d0*darcos(-1.d0))
c     rcomp=wqm_pinv((1.d0-p)/2.d0)
       rcomp=rr
    1 r1=rcomp-(p-g(xx+rcomp)+g(xx-rcomp))/(-g1(xx+rcomp)-g1(xx-rcomp))
c      write (6,101) xx,rcomp,r1
c 101   format ('rcomp',1x,3d16.8)
      if(dabs(r1-rcomp).lt.1.d-12)go to 100
      rcomp=r1
      go to 1
 100  return
      end
c-----------------------------------------------------------------------
      double precision function  chisqd(df,p)
c-----------------------------------------------------------------------
c     real function chisqd lower percentage points of chi-square
c     requires wqm_pinv,
      implicit double precision  (a-h,o-z)
      dimension a(19)
      data a/1.264616d-2, -1.425296d-2, 1.400483d-2, -5.886090d-3,
     1 -1.091214d-2, -2.304527d-2, 3.135411d-3,  -2.728484d-4,
     2-9.699681d-3, 1.316872d-2, 2.618914d-2, -0.222222222222d0,
     3 5.406674d-5,  3.483789d-5, -7.274761d-4, 3.292181d-3,
     4 -8.729713d-3, 0.4714045,  1.d0 /
      if (df-2.d0) 4,5,6
4     chisqd=wqm_pinv(.5d0-p/2.d0)**2
      return
5     chisqd=-2.d0*dlog(1.d0-p)
      return
6     f1=1.d0/df
      t=wqm_pinv(dmin1(p,1.d0-p))
      if (p.lt..5d0) t=-t
      f2=dsqrt(f1)*t
      chisqd=(((a(1)+a(2)*f2)*f1+(((a(3)+a(4)*f2)*f2
     1+a(5))*f2+a(6)))*f1+(((((a(7)+a(8)*f2)*f2+a(9))*f2
     2 +a(10))*f2+a(11))*f2+a(12)))*f1  + (((((a(13)*f2
     3 +a(14))*f2+a(15))*f2+a(16))*f2+a(17))*f2*f2
     4 + a(18))*f2+a(19)
      chisqd=chisqd*chisqd*chisqd*df
      return
      end
c-----------------------------------------------------------------------
      subroutine sxtab7(eta,xn,xk,pval,nrows,ierret,kprint)
      implicit double precision (a-h,o-z)
c-----------------------------------------------------------------------
c
c   vector version of xtab7
c
      dimension eta(1),xn(1),xk(1),pval(1)
      ierret=0
      do 22 irow=1,nrows
      call xtab7(eta(irow),xn(irow),xk(irow),pval(irow),ier,kprint)
      if(ier.ge.0)ierret=ierret+ier
22    continue
      return
      end
c-----------------------------------------------------------------------
      subroutine xtab7(eta,xn,xk,pval,ier,kprint)
c-----------------------------------------------------------------------
c
c  #compute lower conf bnds  gl(      ) from table 7 of odeh and owen (1980)
c  #gl is a lcb on the probability that a normal rv y is
c  #less than or equal to specified l
c
c  #eta   confidence level
c  #xn       sample size (dof=xn-1)
c  #xk      (l-y)/s is the number of estimated standard deviations
c  #        that y is below the specified l
c  #pval    the lower cb on the pr(y<l)
c
c  #following theory in odeh and owen with bit search
c  #find pval such that given n, xk, and eta,
c  #
c  #pr(tf<k*dsqrt(n))=nu
c  #
c  #where tf is a noncentral t r.v. with f=n-1 dof and
c  #ncp=k(pval)*dsqrt(n) and k(pval) is the k(pval) percentile of n(0,1)
c  #
c  #fisrt solve for delta, then get pval from normal cdf
c  #
c  #
      implicit double precision (a-h,o-z)
      common/cxtab7/sqrn,zdelta,zeta,zxkrn,xiif,kprinp
      double precision dble,wqm_fcdfn,zeroin
cxxx      double precision dble,wqm_fcdfn,zeroin
      external fxtab7
      data zero,one/0.0d00,1.0d00/
      data tol/1.00d-5/
      kprinp=kprint
      ier=0
      xiif=xn-1.d00
      if(eta.le.zero.or.eta.ge.one.or.xn.lt.2)go to 991
      zval=wqm_quant(eta,3)
      sqrn=dsqrt(xn)
      phat=wqm_fcdfn(xk)
      pinit=phat-zval*dsqrt(phat*(1.0d00-phat)/xn)
      if(phat.le.0.0)pinit=1.0d-10
c -noio-      if(kprint.ge.2)write(6,4433)pinit
4433  continue
c -noio-4433  format(' initial value=',d20.5)
c  #compute the noncentrality parameter
      zxkrn=xk*sqrn
      zeta=eta
      fval=eta
      eps=1.0d-4
      call brack(pinit,zero,x1,fx1,x2,fx2,fxtab7,kprint)
cxxx      call solve(dval,zero,x1,fx1,x2,fx2,eps,fxtab7,kprint)
      maxfn=100
      nsig=3
cxxx  call zbrent(fxtab7,tol,nsig,x1,x2,maxfn,ier)
cxxx  dval= x2
      dval= zeroin(x1,x2,fxtab7,tol)
      pval=wqm_fcdfn(dval/sqrn)
      return
991   ier=1
      return
      end

c-----------------------------------------------------------------------
      function fxtab7(x)
c-----------------------------------------------------------------------
c
c     function for the root finder
c
      implicit double precision (a-h,o-z)
      common/cxtab7/sqrn,zdelta,zeta,zxkrn,xiif,kprinp
      xkrsn=zxkrn
      delta=x
      call mdtnx(xkrsn,xiif,delta,hf,ier)
      if(hf.ge.1.0)go to 991
      if(hf.le.1.0d-10)go to 990
      fxtab7=-zeta+hf
      go to 50
991   fxtab7=-zeta+1.
      go to 50
990   fxtab7=-zeta+0.
50    continue
c -noio-50    if(kprinp.ge.4)write(6,42)x,xkrsn,iif,delta,hf,fxtab7
42    continue
c -noio-42    format(' ftab7**4**',2g12.5,i7,3g12.5)
      return
      end
c-----------------------------------------------------------------------
      subroutine mdtnx(tval,xiif,d,p,ier)
c-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
c  #routine to look like imsl
c  #arguments are double precision
      call nctev(xiif,d,tval,p,der)
      return
      end
c-----------------------------------------------------------------------
      subroutine brack(xinit,fval,x1,fx1,x2,fx2,func,kprint)
c-----------------------------------------------------------------------
c***********************************************************************
c
c     find x1 and x1 such  that func(x1) and func(x2) bracket fval
c     inputs:
c              xinit     starting x value
c              fval      desired value for the function
c              sloper    guess for slope (input 0.0d00 for no guess)
c              func      function which must be declared as
c                        external in the calling program
c
c    outputs:
c              x1,x2     arguments of func such that function values
c                        bracket fval
c              fx1,fx2   corresponding function values
c
c                           latest version october 25, 1983   wqm
c
c***********************************************************************
      implicit double precision (a-h,o-z)
      external func
      data zero,small,onep5,two/0.0d00,0.05d00,1.5d00,2.0d00/
      ssgn=0.0d00
      ch=1.0
      x1=xinit
      fx1=func(x1)
      smaxx=dmax1(small,0.1d00*xinit)
      slope=(func(x1+smaxx)-fx1)/smaxx
      if(dabs(slope).lt.1.0d-3)go to 29
      delta=onep5*((fval-fx1)/slope)
c -noio-      if(kprint.ge.4)write(6,432)x1,fx1,xp1,fxp1,slope,delta
432   continue
c -noio-432   format(' x,f,x,f,sl,del=',6g13.4)
      go to 35
29    if(dabs(ssgn).gt.1.0d-10)go to 30
      ch=-1.
      ssgn=1.
      delta=.2
      go to 35
30    delta=0.2*ssgn
35    continue
      x2=x1
      fx2=fx1
      if(delta.eq.zero)return
      it=1
      if(kprint.lt.2) go to 20
c -noio-      write(6,42)it,x1,fx1,ssgnp,ssgn,ch,delta
42    continue
c -noio-42    format(' brack iteration no.',i4,2e13.4,4g10.2)
20    x2=x1+delta
      it=it+1
      fx2=func(x2)
      if(kprint.lt.2) go to 40
c -noio-      write(6,42)it,x2,fx2,ssgnp,ssgn,ch,delta
40    if((fx1-fval)*(fx2-fval).lt.zero)return
      x1=x2
      delta=ch*two*delta
      go to 20
      end


c-----------------------------------------------------------------------
      double precision function dlgama(x)
c-----------------------------------------------------------------------
c
c this routine calculates the log(gamma) function for a real argument
c     x.  computation is based on an algorithm outlined in references
c     1 and 2.  the program uses rational functions that approximate
c     log(gamma) to at least 18 significant decimal digits.  the
c     approximation for x .ge. 12 is from reference 3.  approximations
c     for x .lt. 12.0 are unpublished.  lower order approximations can
c     be substituted on machines with less precise arithmetic.
c
c
c explanation of machine-dependent constants
c
c xbig   - the largest argument for which ln(gamma(x)) is representable
c          in the machine, i.e., the solution to the equation
c                  ln(gamma(xbig)) = xinf.
c xinf   - the largest machine representable floating-point number.
c eps    - the smallest positive floating-point number such that
c          1.0+eps .gt. 1.0
c frtbig - rough estimate of the fourth root of xbig
c
c     approximate values for some important machines are:
c
c          ibm/370   cdc/7600   univac/110x      vax 11/780
c           (d.p.)  (s.p.,rndg)    (d.p.)      (s.p.)     (d.p.)
c
c xbig    4.293d+73  1.716e+319  1.280d+305  2.057e+36  2.057d+36
c xinf    7.230d+75  1.260e+322  8.980d+307  1.701e+38  1.701d+38
c eps     2.220d-16  3.550e-015  1.735d-018  5.960e-08  1.388d-17
c frtbig  2.500d+18  6.400e+079  1.800d+076  1.100e+09  1.100d+09
c
c
c error returns
c
c  the program returns the value xinf for singularities or
c     when overflow would occur.  the computation is believed
c     to be free of underflow and overflow.
c
c
c
c other subprograms required (single precision version)
c
c      alog,exp,float,ifix,sin
c
c other subprograms required (double precision version)
c
c     dble,dexp,dlog,dsin,float,ifix,sngl
c
c
c references:
c
c  1) w. j. cody and k. e. hillstrom, 'chebyshev approximations for
c     the natural logarithm of the gamma function,' math. comp. 21,
c     1967, pp. 198-203.
c
c  2) k. e. hillstrom, anl/amd program anlc366s, dgamma/dlgama, may,
c     1969.
c
c  3) hart, et. al., computer approximations, wiley and sons, new
c     york, 1968.
c
c
c  author: w. j. cody
c          argonne national  laboratory
c
c  latest modification: july 14, 1983
c
c----------------------------------------------------------------------
      integer i
      double precision c,corr,d1,d2,d4,eps,frtbig,four,half,one,pnt68,
     1     p1,p2,p4,q1,q2,q4,res,sqrtpi,thrhal,twelve,two,x,xbig,xden,
     2     xinf,xm1,xm2,xm4,xnum,y,ysq,zero
      dimension c(7),p1(8),p2(8),p4(8),q1(8),q2(8),q4(8)
c----------------------------------------------------------------------
c  mathematical constants
c----------------------------------------------------------------------
      data one,half,twelve,zero/1.0d0,0.5d0,12.0d0,0.0d0/
      data four,thrhal,two,pnt68/4.0d0,1.5d0,2.0d0,0.6796875d0/
      data sqrtpi/0.9189385332046727417803297d0/
c----------------------------------------------------------------------
c  machine dependent parameters
c----------------------------------------------------------------------
      data xbig,xinf,eps,frtbig/2.057d36,1.701d38,1.388d-17,1.1d9/
c----------------------------------------------------------------------
c  numerator and denominator coefficients for rational minimax
c     approximation over (0.5,1.5).
c----------------------------------------------------------------------
      data d1/-5.772156649015328605195174d-1/
      data p1/4.945235359296727046734888d0,2.018112620856775083915565d2,
     1        2.290838373831346393026739d3,1.131967205903380828685045d4,
     2        2.855724635671635335736389d4,3.848496228443793359990269d4,
     3        2.637748787624195437963534d4,7.225813979700288197698961d3/
      data q1/6.748212550303777196073036d1,1.113332393857199323513008d3,
     1        7.738757056935398733233834d3,2.763987074403340708898585d4,
     2        5.499310206226157329794414d4,6.161122180066002127833352d4,
     3        3.635127591501940507276287d4,8.785536302431013170870835d3/
c----------------------------------------------------------------------
c  numerator and denominator coefficients for rational minimax
c     approximation over (1.5,4.0).
c----------------------------------------------------------------------
      data d2/4.227843350984671393993777d-1/
      data p2/4.974607845568932035012064d0,5.424138599891070494101986d2,
     1        1.550693864978364947665077d4,1.847932904445632425417223d5,
     2        1.088204769468828767498470d6,3.338152967987029735917223d6,
     3        5.106661678927352456275255d6,3.074109054850539556250927d6/
      data q2/1.830328399370592604055942d2,7.765049321445005871323047d3,
     1        1.331903827966074194402448d5,1.136705821321969608938755d6,
     2        5.267964117437946917577538d6,1.346701454311101692290052d7,
     3        1.782736530353274213975932d7,9.533095591844353613395747d6/
c----------------------------------------------------------------------
c  numerator and denominator coefficients for rational minimax
c     approximation over (4.0,12.0).
c----------------------------------------------------------------------
      data d4/1.791759469228055000094023d0/
      data p4/1.474502166059939948905062d4,2.426813369486704502836312d6,
     1        1.214755574045093227939592d8,2.663432449630976949898078d9,
     2      2.940378956634553899906876d10,1.702665737765398868392998d11,
     3      4.926125793377430887588120d11,5.606251856223951465078242d11/
      data q4/2.690530175870899333379843d3,6.393885654300092398984238d5,
     2        4.135599930241388052042842d7,1.120872109616147941376570d9,
     3      1.488613728678813811542398d10,1.016803586272438228077304d11,
     4      3.417476345507377132798597d11,4.463158187419713286462081d11/
c----------------------------------------------------------------------
c  coefficients for minimax approximation over (12, inf).
c----------------------------------------------------------------------
      data c/-1.910444077728d-03,8.4171387781295d-04,
     1     -5.952379913043012d-04,7.93650793500350248d-04,
     2     -2.777777777777681622553d-03,8.333333333333333331554247d-02,
     3      5.7083835261d-03/
c----------------------------------------------------------------------
      y = x
      if ((y .le. zero) .or. (y .gt. xbig)) go to 700
      if (y .gt. twelve) go to 400
      if (y .gt. four) go to 300
      if (y .gt. thrhal) go to 200
      if (y .ge. pnt68) go to 100
      corr = -dlog(y)
      xm1 = y
      if (y .gt. eps) go to 120
      res = corr
      go to 900
c----------------------------------------------------------------------
c  0.5 .lt. x .le. 1.5
c----------------------------------------------------------------------
  100 corr = zero
      xm1 = (y - half) - half
  120 xden = one
      xnum = zero
      do 140 i = 1, 8
         xnum = xnum*xm1 + p1(i)
         xden = xden*xm1 + q1(i)
  140 continue
      res = corr + (xm1 * (d1 + xm1*(xnum/xden)))
      go to 900
c----------------------------------------------------------------------
c  1.5 .lt. x .le. 4.0
c----------------------------------------------------------------------
  200 xm2 = y - two
      xden = one
      xnum = zero
      do 240 i = 1, 8
         xnum = xnum*xm2 + p2(i)
         xden = xden*xm2 + q2(i)
  240 continue
      res = xm2 * (d2 + xm2*(xnum/xden))
      go to 900
c----------------------------------------------------------------------
c  4.0 .lt. x .le. 12.0
c----------------------------------------------------------------------
  300 xm4 = y - four
      xden = -one
      xnum = zero
      do 340 i = 1, 8
         xnum = xnum*xm4 + p4(i)
         xden = xden*xm4 + q4(i)
  340 continue
      res = d4 + xm4*(xnum/xden)
      go to 900
c----------------------------------------------------------------------
c  evaluate for argument .ge. 12.0,
c----------------------------------------------------------------------
  400 res = zero
      if (y .gt. frtbig) go to 460
      res = c(7)
      ysq = y * y
      do 450 i = 1, 6
         res = res / ysq + c(i)
  450 continue
  460 res = res/y
      corr = dlog(y)
      res = res + sqrtpi - half*corr
      res = res + y*(corr-one)
      go to 900
c----------------------------------------------------------------------
c  return for bad arguments
c----------------------------------------------------------------------
  700 res = xinf
c----------------------------------------------------------------------
c  final adjustments and return
c----------------------------------------------------------------------
  900 dlgama = res
      return
c ---------- last card of dlgama ----------
      end


c-----------------------------------------------------------------------
      double precision function derf(x)
c-----------------------------------------------------------------------
c  #
c
c  program to compute the error function
c
c   author - w. j. cody
c
c   mydate - january 8, 1985
c
c  #
      integer jint
      double precision x, result
c  #
      jint = 0
      call wqm_calerf(x,result,jint)
      derf = result
      return
c---------- last card of derf ----------
      end
c-----------------------------------------------------------------------
      double precision function derfc(x)
c-----------------------------------------------------------------------
c  #
c
c  program to compute the complimentary error function
c
c   author - w. j. cody
c
c   mydate - january 8, 1985
c
c  #
      integer jint
      double precision x, result
c  #
      jint = 1
      call wqm_calerf(x,result,jint)
      derfc = result
      return
c---------- last card of derfc ----------
      end



c-----------------------------------------------------------------------
      subroutine wqm_calerf(arg,result,jint)
c-----------------------------------------------------------------------
c  #
c
c this packet computes the error and complimentary error functions
c   for real arguments  arg.  it contains two function type
c   subprograms,  erf  and  erfc  (or  derf  and  wqm_dxerc),  and one
c   subroutine type subprogram,  wqm_calerf.  the calling statements
c   for the primary entries are
c
c                   y=erf(x)     (or   y=derf(x) )
c   and
c                   y=erfc(x)    (or   y=dxerc(x) ).
c
c   the routine  wqm_calerf  is intended for internal packet use only,
c   all computations within the packet being concentrated in this
c   routine.  the function subprograms invoke  calerf  with the
c   statement
c          call calerf(arg,result,jint)
c   where the parameter usage is as follows
c
c      function                     parameters for calerf
c       call              arg                  result          jint
c     erf(arg)      any real argument         erf(arg)          0
c     erfc(arg)     abs(arg) .lt. xmax        erfc(arg)         1
c
c   the main computation evaluates near minimax approximations
c   from 'rational chebyshev approximations for the error function'
c   by w. j. cody, math. comp., 1969, pp. 631-638.  this
c   transportable program uses rational functions that theoretically
c   approximate  erf(x)  and  erfc(x)  to at least 18 significant
c   decimal digits.  the accuracy achieved depends on the arithmetic
c   system, the compiler, the intrinsic functions, and proper
c   selection of the machine-dependent constants.
c
c*******************************************************************
c*******************************************************************
c
c explanation of machine-dependent constants
c
c   xsmall = argument below which erf(x) may be represented
c            by   2*x/sqrt(pi)  and above which  x*x  will
c            not underflow.  a conservative value is the
c            largest x such that   1.0 + x = 1.0   to machine
c            precision.
c   xmax   = largest argument acceptable to  erfc;  solution to
c            equation:  w(x) * (1-0.5/x**2) = xmin,  where
c            w(x) = exp(-x*x)/(x*sqrt(pi)),  and xmin is the
c            smallest positive machine number (see table below).
c
c     approximate values for some important machines are:
c
c                          xsmall     xmax     xmin
c
c    ibm 195     (d.p.)   1.39d-17   13.306   5.40d-79
c    cdc 7600    (s.p.)   7.11e-15   25.922   3.13e-294
c    cray-1      (s.p.)   7.11e-15   75.326   4.58e-2467
c    univac 1108 (d.p.)   1.73d-18   26.582   2.78d-309
c    vax 11/780  (s.p.)   5.96e-8     9.269   2.94e-39
c    vax 11/780  (d.p.)   1.39d-17    9.269   2.94d-39
c    ibm pc      (s.p.)   5.96e-8     9.194   1.18e-38
c    ibm pc      (d.p.)   1.11d-16   26.543   2.23d-308
c
c*******************************************************************
c*******************************************************************
c
c error returns
c
c  the program returns  erfc = 0  for  arg .gt. xmax.
c
c
c other subprograms required (single precision version)
c
c     abs, exp
c
c other subprograms required (double precision version)
c
c     dabs, dexp
c
c
c  author: w. j. cody
c          mathematics and computer science division
c          argonne national laboratory
c          argonne, il 60439
c
c  latest modification: january 8, 1985
c
c  #
      integer i,jint
      double precision a,arg,b,c,d,four,half,p,one,q,result,sqrpi,
     1               two,thresh,x,xmax,xden,xnum,xsmall,y,ysq,zero
      dimension a(5),b(4),c(9),d(8),p(6),q(5)
c  #
c  mathematical constants
c  #
      data four,one,half,two,zero/4.0d0,1.0d0,0.5d0,2.0d0,0.0d0/
      data sqrpi/5.6418958354775628695d-1/,thresh/0.46875d0/
c  #
c  machine-dependent parameters
c  #
      data xsmall/4.2d-16/, xmax/9.269d0/
c  #
c  coefficients for approximation to derf in first interval
c  #
      data a/3.16112374387056560d00,1.13864154151050156d02,
     1       3.77485237685302021d02,3.20937758913846947d03,
     2       1.85777706184603153d-1/
      data b/2.36012909523441209d01,2.44024637934444173d02,
     1       1.28261652607737228d03,2.84423683343917062d03/
c  #
c  coefficients for approximation to dxerc in second interval
c  #
      data c/5.64188496988670089d-1,8.88314979438837594d0,
     1       6.61191906371416295d01,2.98635138197400131d02,
     2       8.81952221241769090d02,1.71204761263407058d03,
     3       2.05107837782607147d03,1.23033935479799725d03,
     4       2.15311535474403846d-8/
      data d/1.57449261107098347d01,1.17693950891312499d02,
     1       5.37181101862009858d02,1.62138957456669019d03,
     2       3.29079923573345963d03,4.36261909014324716d03,
     3       3.43936767414372164d03,1.23033935480374942d03/
c  #
c  coefficients for approximation to dxerc in third interval
c  #
      data p/3.05326634961232344d-1,3.60344899949804439d-1,
     1       1.25781726111229246d-1,1.60837851487422766d-2,
     2       6.58749161529837803d-4,1.63153871373020978d-2/
      data q/2.56852019228982242d00,1.87295284992346047d00,
     1       5.27905102951428412d-1,6.05183413124413191d-2,
     2       2.33520497626869185d-3/
c  #
      x = arg
      y = dabs(x)
      if (y .gt. four) go to 200
      if (y .gt. thresh) go to 100
c  #
c  evaluate erf for abs(x) .le. 0.46875
c  #
      ysq = zero
      if (y .gt. xsmall) ysq = y * y
      xnum = a(5)*ysq
      xden = ysq
      do 20 i = 1, 3
         xnum = (xnum + a(i)) * ysq
         xden = (xden + b(i)) * ysq
   20 continue
      result = x * (xnum + a(4)) / (xden + b(4))
      if (jint .ne. 0) result = one - result
      go to 800
c  #
c  evaluate erfc for 0.46875 .lt. abs(x) .le. 4.0
c  #
  100 ysq = y * y
      xnum = c(9)*y
      xden = y
      do 120 i = 1, 7
         xnum = (xnum + c(i)) * y
         xden = (xden + d(i)) * y
  120 continue
      result = dexp(-ysq) * (xnum + c(8)) / (xden + d(8))
      go to 300
c  #
c  evaluate erfc for abs(x) .gt. 4.0
c  #
  200 result = zero
      if (y .ge. xmax) go to 300
  220 ysq = one / (y * y)
      xnum = p(6)*ysq
      xden = ysq
      do 240 i = 1, 4
         xnum = (xnum + p(i)) * ysq
         xden = (xden + q(i)) * ysq
  240 continue
      result = ysq *(xnum + p(5)) / (xden + q(5))
      result = (dexp(-y*y) / y) * (sqrpi - result)
c  #
c  fix up for neg. arg., erf, etc.
c  #
  300 if (jint .eq. 0) go to 350
      if (x .lt. zero) result = two - result
      go to 800
  350 result = (half - result) + half
      if (x .lt. zero) result = -result
c  #
  800 return
c---------- last card of wqm_calerf ----------
      end

c-----------------------------------------------------------------------
      double precision function wqm_pinv(p)
c-----------------------------------------------------------------------
c  #
c  #odeh's inverse normal with one iteration of newton for polish
c  #
      implicit double precision  (a-h,o-z)
      g(x)= wqm_dxerc(x/dsqrt(2.d0))/2.d0
      gp(x)= dexp(-x*x/2.d0)/dr
      data dr/0.2506628274631000502415765d+01/
      data a0,a1,a2,a3,a4,a5,a6/
     1 0.505300845045718d-1,0.6207981783784d0,1.d0,0.255758400033573d0,
     20.120012384541901d-1,0.274100549062517d-4,-0.109846617732390d-6/
      data b0,b1,b2,b3,b4,b5/
     4 0.129635913467631d-1,0.252771250307626d0,0.713433125153414d0 ,
     50.472899800222222d0,0.722351199288358d-1, 0.221326694412374d-2/
      wqm_pinv=0.d0
      if (p.eq..5d0) return
      y=dsqrt(-2.d0*dlog(p))
      x =  y -((((((y*a6+a5)*y+a4)*y+a3)* y+a2)*y+a1)* y+a0)/
     &     (((((y*b5+b4)*y+b3)*y+b2)*y+b1)*y+b0)
      wqm_pinv =x -(p-g(x))/gp(x)
      return
      end

c-----------------------------------------------------------------------
      double precision function wqm_dxerc(x)
c-----------------------------------------------------------------------
c  #
c
c  function to compute the complimentary error function
c
c   author - w. j. cody
c
c   date - january 8, 1985
c
c  #
      integer jint
      double precision x, result
c  #
      jint = 1
      call wqm_calerf(x,result,jint)
      wqm_dxerc = result
      return
c---------- last card of wqm_dxerc ----------
      end

c----------------------------------------------------------------------
      double precision  function wqm_quant(p,kdist)
c----------------------------------------------------------------------
c     #
c     #location-scale distribution quantiles
c     #
      implicit double precision (a-h,o-z)
      data half,one,smallp/0.5d00,1.0d00,1.0d-25/
      d=p
      if(p.lt.smallp)d=smallp
      if(p.ge.one)d=one-1.0d-15
      go to (1001,1001,1002,1002,1003,1003,1004,1004),kdist
c     #
c     #smallest extreme value distribution
c     #
 1001 wqm_quant = dlog(-dlog(one-d))
      return
c     #
c     #inverse of the normal distribution
c     #
 1002 if(d.lt.half) go to 9
      wqm_quant = wqm_pinv(one-d)
      return
 9    wqm_quant = -wqm_pinv(d)
      return
c     #
c     #logistic distribution
c     #
 1003 wqm_quant = -dlog(one/d-one)
      return
c     #
c     #largest extreme value distribution
c     #
 1004 wqm_quant = -dlog(-dlog(d))
      return
      end

c-----------------------------------------------------------------------
      double precision function zeroin(ax,bx,f,tol)
c-----------------------------------------------------------------------
      double precision ax,bx,f,tol
c
c      a zero of the function  f(x)  is computed in the interval ax,bx .
c
c  input..
c
c  ax     left endpoint of initial interval
c  bx     right endpoint of initial interval
c  f      function subprogram which evaluates f(x) for any x in
c         the interval  ax,bx
c  tol    desired length of the interval of uncertainty of the
c         final result (.ge.0.)
c
c  output..
c
c  zeroin abscissa approximating a zero of  f  in the interval ax,bx
c
c      it is assumed  that   f(ax)   and   f(bx)   have  opposite  signs
c  this is checked, and an error message is printed if this is not
c  satisfied.   zeroin  returns a zero  x  in the given interval
c  ax,bx  to within a tolerance  4*macheps*abs(x)+tol, where macheps  is
c  the  relative machine precision defined as the smallest representable
c  number such that  1.+macheps .gt. 1.
c      this function subprogram is a slightly  modified  translation  of
c  the algol 60 procedure  zero  given in  richard brent, algorithms for
c  minimization without derivatives, prentice-hall, inc. (1973).
c
c
c  modified 29 sept to return 0 in the error condition to avoid compile warn
c
      double precision  a,b,c,d,e,eps,fa,fb,fc,tol1,xm,p,q,r,s
      double precision zero,half,one,two
      double precision  dabs, d1mach
      data zero/0.0d00/,half/0.5d00/,one/1.0d00/,two/2.0d00/
   10 eps = d1mach(4)
      tol1 = eps+one
c
      a=ax
      b=bx
      fa=f(a)
      fb=f(b)
c     check that f(ax) and f(bx) have different signs
      if (fa .eq. zero .or. fb .eq. zero) go to 20
      if (fa * (fb/dabs(fb)) .le. zero) go to 20
c -noio-         write(6,2500)a,b,fa,fb
2500  continue
c -noio-2500     format(1x,'f(ax) and f(bx) do not have different signs,',
c -noio-     1             ' zeroin is aborting'/3x,4g14.4)
         zeroin = zero
         return
   20 c=a
      fc=fa
      d=b-a
      e=d
   30 if (dabs(fc).ge.dabs(fb)) go to 40
      a=b
      b=c
      c=a
      fa=fb
      fb=fc
      fc=fa
   40 tol1=two*eps*dabs(b)+half*tol
      xm = half*(c-b)
      if ((dabs(xm).le.tol1).or.(fb .eq. zero)) go to 150
c
c see if a bisection is forced
c
      if ((dabs(e).ge.tol1).and.(dabs(fa).gt.dabs(fb))) go to 50
      d=xm
      e=d
      go to 110
   50 s=fb/fa
      if (a.ne.c) go to 60
c
c linear interpolation
c
      p=two*xm*s
      q=one-s
      go to 70
c
c inverse quadratic interpolation
c
   60 q=fa/fc
      r=fb/fc
      p=s*(two*xm*q*(q-r)-(b-a)*(r-one))
      q=(q-one)*(r-one)*(s-one)
   70 if (p.le.zero) go to 80
      q=-q
      go to 90
   80 p=-p
   90 s=e
      e=d
      if (((two*p).ge.(3.0d0*xm*q-dabs(tol1*q))).or.(p.ge.
     *dabs(half*s*q))) go to 100
      d=p/q
      go to 110
  100 d=xm
      e=d
  110 a=b
      fa=fb
      if (dabs(d).le.tol1) go to 120
      b=b+d
      go to 140
  120 if (xm.le.zero) go to 130
      b=b+tol1
      go to 140
  130 b=b-tol1
  140 fb=f(b)
      if ((fb*(fc/dabs(fc))).gt.zero) go to 20
      go to 30
  150 zeroin=b
      return
      end

c-----------------------------------------------------------------------
      double precision function wqm_fcdfn(z)
c-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
c  #
c  #     standard normal cdf
c  #
      data half,root/0.5d00,.7071067811865475d00/
      zroot=z*root
      wqm_fcdfn=half*wqm_dxerc(-zroot)
      return
      end

c-----------------------------------------------------------------------
      double precision function d1mach(i)
c-----------------------------------------------------------------------
c version for DECstations etc.
c modified 4 dec 1997 to call sysabt instead of stop
c
c  double-precision machine constants
c
c  d1mach( 1) = b**(emin-1), the smallest positive magnitude.
c
c  d1mach( 2) = b**emax*(1 - b**(-t)), the largest magnitude.
c
c  d1mach( 3) = b**(-t), the smallest relative spacing.
c
c  d1mach( 4) = b**(1-t), the largest relative spacing.
c
c  d1mach( 5) = log10(b)
c
c  to alter this function for a particular environment,
c  the desired set of data statements should be activated by
c  removing the c from column 1.
c  on rare machines a static statement may need to be added.
c  (but probably more systems prohibit it than require it.)
c
c  for ieee-arithmetic machines (binary standard), one of the first
c  two sets of constants below should be appropriate.
c
c  where possible, decimal, octal or hexadecimal constants are used
c  to specify the constants exactly.  sometimes this requires using
c  equivalent integer arrays.  if your compiler uses half-word
c  integers by default (sometimes called integer*2), you may need to
c  change integer to integer*4 or otherwise instruct your compiler
c  to use full-word integers in the next 5 declarations.
c
      integer small(4)
      integer large(4)
      integer right(4)
      integer diver(4)
      integer log10(4)
      integer sc
c
      double precision dmach(5)
c
      equivalence (dmach(1),small(1))
      equivalence (dmach(2),large(1))
      equivalence (dmach(3),right(1))
      equivalence (dmach(4),diver(1))
      equivalence (dmach(5),log10(1))
c
c     machine constants for ieee arithmetic machines, such as the at&t
c     3b series and motorola 68000 based machines (e.g. sun 3 and at&t
c     pc 7300), in which the most significant byte is stored first.
c
c      data small(1),small(2) /    1048576,          0 /
c      data large(1),large(2) / 2146435071,         -1 /
c      data right(1),right(2) / 1017118720,          0 /
c      data diver(1),diver(2) / 1018167296,          0 /
c      data log10(1),log10(2) / 1070810131, 1352628735 /, sc/987/
c
c     machine constants for ieee arithmetic machines and 8087-based
c     micros, such as the ibm pc and at&t 6300, in which the least
c     significant byte is stored first.
c
       data small(1),small(2) /          0,    1048576 /
       data large(1),large(2) /         -1, 2146435071 /
       data right(1),right(2) /          0, 1017118720 /
       data diver(1),diver(2) /          0, 1018167296 /
       data log10(1),log10(2) / 1352628735, 1070810131 /, sc/987/
c
c  ***  issue stop 779 if all data statements are commented...
       d1mach=0
c       if (sc .ne. 987) call sysabt('d1mach 779$')
       if (i .lt. 1  .or.  i .gt. 5) goto 999
       d1mach = dmach(i)
       return
 999  continue
c -noio- 999   write(*,1999) i
 1999 continue
c -noio- 1999  format(' d1mach - i out of bounds',i10)
c       call sysabt('d1mach 7654$')
       return
       end


c-----------------------------------------------------------------------
      subroutine nctev(xndf,del,t,ans,der)
c-----------------------------------------------------------------------
c subroutine nctev(xndf,del,t,ans,der) evaluates the cumulative distribution of
c noncentrally t distributed random variable with noncentrality
c parameter del and degrees of freedom xndf
c ans gives the value of the cumulative at t .
c der gives value of derivative with respect to t
c ndf is the degrees of freedom.
      implicit double precision (a-h,o-z)
      double precision mo,m1,m2,m3
      external derf,derfc,dlgama
      g(x)=0.5d0+derf(x/dsqrt(2.0d00))/2.0d00
      gc(x)=derfc(x/dsqrt(2.0d00))/2.0d00
      gp(x)=dexp(-x*x/2.0d00)/dr
      dr=dsqrt(2.0d00*darcos(-1.0d00))
cxxx      call xuflow(0)
      f=xndf
      d1=(f+1.0d00)/2.0d00
      d2=f/2.0d00
      drat=dexp(dlgama(d1)-dlgama(d2))
      tf=dsqrt(2.0d00*f)*dr*drat
      if(t.eq.0.d0) go to 100
      l=f
      a=t/dsqrt(f)
      b=f/(f+t*t)
      sb=dsqrt(b)
      dsb=del*sb
      dasb=a*dsb
      s1=-(dsb*dsb)/2.0d00-dlog(dr)
      sf=dlog(sb)+s1
      sc=1.d-25
      scale=1.0d00
      if (sf.gt.-170.) go to 1
      iss=sf
      sc=-iss-160
1      mo=a*dexp(sf+sc)*g(dasb)
      sx=0.d0
      if (dabs(del).lt.18.d0) sx=gp(del)
      m1=b*(del*a*mo+a*sx/dr)
      if ((l/2)*2.ne.l) go to 15
      sum=mo
      m2=0.5d0*(del*a*m1+mo)*b
      if (f.eq.2) der=m2*dexp(-sc)*f/t*dr
c     derd=-m1*dexp(-sc)*drat*4/t
      if(f.eq.2.0d00) go to 7
      sum=sum+m2
c      if(f.eq.4.d0) go to 7
      m=f/2.0d00
c      m=m-2
      m=m-1
      fk=2.0d00
      a2=1.0d00
      do 5 k=1,m
      do 6 j=1,2
      fk=fk+1
      a3=1.0d00/((fk-2.0d00)*a2)
      m3=(fk-1.0d00)/fk*b*(a*a3*del*m2+m1)
      m1=m2
      m2=m3
    6 a2=a3
      if (m1.lt.10.d+56) go to 5
      div=10.d+10
      m1=m1/div
      m2=m2/div
      sum=sum/div
      m3=m3/div
      sc=sc-dlog(div)
    5 if (k.lt.m) sum=sum+m3
7     xx=0.d0
      if (del.gt.18.d0) xx=0.d0
      if (del.lt.-18.d0) xx=1.0d00
      if (dabs(del).lt.18) xx=gc(del)
      if (sum.gt.0.d0) ans=xx+dr*dexp(dlog(sum)-sc)
      if (sum.le.0.d0) ans=xx+dr*sum*dexp(-sc)
      if (f.gt.2)  der=m3*dexp(-sc)*f/t*dr
c     if (f.gt.2) derd =-m1*dexp(-sc)*drat/t
c     if (f.gt.2) derd=derd*2.0d00*dsqrt(2.0d00*f)
      return
  100 ans=g(-del)
      der=gp(del)
c     derd=-der
      return
   15 continue
      sum=2.0d00*t1(dsb,a)
   50 if (f.eq.1.0d00) go to 70
      sum=sum+2.0d00*m1
c      if (f.eq.3.d0) go to 70
      m=(f-1.0d00)/2.0d00
c      m=m-1
      a2=1.0d00
      fk=2.0d00
      do 51 k=1,m
      do 61 j=1,2
      m2=(fk-1.0d00)/fk*b*(a2*del*a*m1+mo)
      fk=fk+1.0d00
      a2=1.0d00/((fk-2.0d00)*a2)
      mo=m1
      m1=m2
61    continue
      if (dabs(mo).lt.10.d+46 ) go to 51
      aa=dlog(10.d+10)
      mo=mo/10.d+10
      m1=m1/10.d+10
      sc=sc-aa
      sum=sum/10.d+10
      m2=m2/10.d+10
   51 if (k.lt.m) sum=sum+2.0d00*m2
70     if (dsb.lt.-18.d0) ss=1.0d00
      if (dsb.gt.18.d0) ss=0.d0
      if (dabs(dsb).lt.18.d0) ss=gc(dsb)
      if (sum.gt.0.d0) ans=ss+dexp(dlog(sum)-sc)
      if (sum.le.0.d0) ans=ss+sum*dexp(-sc)
      der=m1*dexp(-sc)*2.*f/t
c     derd=-1.0d00*mo*dexp(-sc)/t
c     derd=derd*tf
      return
      end
c-----------------------------------------------------------------------
      double precision function t1 (h,a)
c-----------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      double precision c(7),i2k(7)
      external derf
      data c/0.9999936d0,   -0.9992989d0,   0.9872976d0, -0.9109973d0,
     1 0.6829098d0, -0.3360210d0,   0.07612251d0   /
      p(x)=(1.0d00+derf(x/dsqrt(2.0d00)))*0.5d0
      fo(w)=con*derf(w)
      pitms2=darcos(-1.0d00)*2.0d00
      con=dsqrt(darcos(-1.0d00))/2.0d00
      sumout=0.d0
      t1=0.d0
      if (dabs(h).gt.5.2d0.and.dabs(a).lt.1.0d00) return
      sa=a
      ht=h
      h=dabs(h)
      a=dabs(a)
      at=a
      if(a.le.1.0d00)  go to 1
      a=1.0d00/a
      h=at*h
      if (h.lt.5.2d0) go to 1
      go to 40
    1 if (a.gt..3d0.and.h.gt.1.6d0) go to 39
      sumout=datan(a)/pitms2
      hsq=h*h/2.0d00
      exph=dexp(-hsq)
      tol=1.d-10
      sign=1.0d00
      j=0
   10 j=j+1
      jm1=j-1
      apower=a**(2*jm1+1)
      sumin=1.0d00
      if(j.eq.1) go to 30
      facti=1.0d00
      do 20 i=1,jm1
      facti=facti*i
      rnum=h**(2*i)
      denom=2.0d00**i*facti
      if(rnum.lt.denom*1.d-30) go to 30
   20 sumin=sumin+rnum/denom
   30 cj=sign/(2*jm1+1)*(1.0d00-exph*sumin)
      term=(cj*apower)/pitms2
      sign=sign*(-1.0d00)
      sumout=sumout-term
      if(dabs(term).gt.tol.and.j.lt.100) go to 10
      t1=sumout
      if(at.gt.1.0d00) go to 40
      a=sa
      h=ht
      if (a.lt.0.d0) t1=-t1
      return
   39 w=h*a/dsqrt(2.0d00)
      hds2=h/dsqrt(2.0d00)
      ew=dexp(-w*w)
      i2k(1)=fo(w)
      do 50 l=2,7
      lp=2*(l-1)-1
   50 i2k(l)=.5d0*(lp*i2k(l-1)-w**lp*ew)
      sum=0.d0
      do 60 l=1,7
   60 sum=sum+c(l)*i2k(l)/hds2**(2*l-1)
      sumout=dexp(-h*h/2.0d00)*sum/pitms2
      if (at.gt.1.0d00) go to 40
      t1=sumout
      h=ht
      a=sa
      if (a.lt.0.d0) t1=-t1
      return
   40 continue
      xh=dabs(ht)
      y=xh*dabs(sa)
      t1=0.5d0*(p(xh)+p(y))-p(xh)*p(y)-sumout
      h=ht
      a=sa
      if (a.lt.0.d0) t1=-t1
      return
      end
c-----------------------------------------------------------------------
      double precision function darcos(x)
c-----------------------------------------------------------------------
      double precision x,acos
      darcos=acos(x)
      return
      end
