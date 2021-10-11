!     Last change:  EF   19 Jul 2005    9:13 am

MODULE adapt

   USE typePrecision
   USE typeMalla
   USE typeArreglo3D
   USE propiedades
   USE typeBorde

   REAL (nP) :: amu

   contains

   SUBROUTINE begin(u,v,w,t)

   TYPE(arreglo3D) :: u,v,w,t

   INTENT(IN OUT)  :: u,v,w,t
   t%value(:,:,1)           = 1.0D0
   u%value(1,:,:)           = 0.0D0
   u%value(objmalla%l1,:,:) = 0.0D0
   v%value(:,1,:) 	    = 0.0D0
   v%value(:,objMalla%m1,:) = 0.0D0
   w%value(:,:,1) 	    = 1.0D0
   w%value(:,:,objMalla%n1) = 0.5D0

   rho%value(:,:,:) = 1.0D0

   kbc_I1  = "WALL"
   kbc_L1  = "WALL"
   kbc_J1  = "WALL"
   kbc_M1  = "WALL"
   kbc_K1  = "INFLOW"
   kbc_N1  = "OUTFLOW"

END SUBROUTINE begin

!SUBROUTINE bound(f,nf,iter)

!   USE typePrecision
!   USE typeMalla
!   USE typeArreglo3D
!   USE UVWT
!   USE typeBound

   !INTEGER          :: iter

   !TYPE(arreglo4D)  :: f(nfmax)

   !INTENT (IN OUT)  :: f

!END SUBROUTINE bound

SUBROUTINE phi(nf,timer,u,v,w,t)

   USE Coeficientes

   INTEGER  :: i,j,k,nf
   REAL(nP) :: TIMER

   TYPE(arreglo3D) :: u,v,w,t

   INTENT (IN)          :: nf,timer
   INTENT (IN OUT)      :: u,v,w,t

   amu = 1.0D0

   IF (nf.EQ.0) THEN
   gamX%value(:,:,:) = amu
   END IF

   IF (NF.EQ.5) THEN
   KBCN1=2
   END IF
END SUBROUTINE phi
   !  Sc = con
   !  Sp = ap
! ------------------------

END MODULE adapt
