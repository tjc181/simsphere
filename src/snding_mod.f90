module snding_mod
  implicit none
  private
  public :: splint, spline

  contains

    real pure function splint(XA,YA,Y2A,n,x)
      integer, intent(in) :: n, x
      real, intent(in) :: XA(50), YA(50), Y2A(n)
    
      real :: h, a, b
      integer :: klo, khi, k
    
      klo=1
      khi=n
    
      do 
        if (khi-klo .le. 1) exit
        if (khi-klo .gt. 1) then
          k=(khi+klo)/2
          if (XA(k) .gt. x) then
            khi=k
          else
            klo=k
          end if
        end if
      end do
    
      H=XA(KHI)-XA(KLO)
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      splint=A*YA(KLO)+B*YA(KHI)+((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
    
    end function


    pure function spline(X,Y,N,YP1,YPN)
      integer, parameter :: NMAX=100
      real :: U(NMAX)
      real :: UN,QN,P,SIG
      integer, intent(in) :: N
      integer :: i, k
      real, intent(in) :: X(N), Y(N), YP1, YPN
      real :: spline(N)
    
      IF (YP1.GT..99E30) THEN
        spline(1)=0.
        U(1)=0.
      ELSE
        spline(1)=-0.5
        U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      end if
      do I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*spline(I-1)+2.
        spline(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))                 &
             /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
      end do
      IF (YPN.GT..99E30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      end if
      spline(N)=(UN-QN*U(N-1))/(QN*spline(N-1)+1.)
      do K=N-1,1,-1
        spline(K)=spline(K)*spline(K+1)+U(K)
      end do
      return
    end function

end module snding_mod
