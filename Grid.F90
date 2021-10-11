!     Last change:  DAN  14 Apr 2018    4:04 pm

MODULE typePrecision
!Define la precision de las variables reales en el programa
!(4 u 8 bits), tambien un valor infinitesimal para evitar la
!división entre cero en algunos casos. Y el numero maximo de
!variables dependientes a calcular.

   integer, parameter   :: nP = 8
   real(nP), parameter  :: SMALL = 1.0D-20
   INTEGER, parameter   :: nfmax = 6
END MODULE typePrecision


MODULE typeMalla
!Contiene y calcula todas las variables correspondientes
!a la geometria de la malla.

   USE typePrecision

   INTEGER      :: ni,nj,nk        !Numero de nodos totales en x,y,z

   TYPE malla
     INTEGER 		:: ncvx,ncvy,ncvz    !Numero de volumenes de control
     INTEGER		:: l1,m1,n1,l2,m2,n2 !Contiene el valor de los nodos en los bordes
     REAL(nP), POINTER  :: x(:),y(:),z(:)    !Almacena la ubicacion de cada nodo
     REAL(nP), POINTER  :: xu(:),yv(:),zw(:) !Almacena la ubicacion de cada cara
     REAL(nP), POINTER  :: dx(:),dy(:),dz(:) !Anchos de cara para cada volumen de control
     REAL(nP), POINTER  :: areaX(:,:),areaY(:,:),areaZ(:,:) !Area de cada cara del volumen de control
     REAL(nP), POINTER  :: vol(:,:,:)        !Volumen de cada volumen de control
     REAL(nP), POINTER  :: fx(:),fxm(:),fy(:),fym(:),fz(:),fzm(:) !Factores de interpolacion
   END TYPE malla

!La declaracion TYPE se utiliza para declarar estructuras de datos
!las cuales guardan una relacion entre si.
   TYPE(malla)     :: objMalla

   contains

      real(nP) function InterLin(varP,varE,n,iCoord)
      !Funcion de interpolacion lineal
         use typePrecision
         integer     :: n,iCoord
         real(nP)    :: varP,varE,wm,wp
         intent(in)  :: varP,varE,n,iCoord

         !phie = phiE * wm + phiP * wp
         !
         !        (wm)   (wp)
         !   (P)--------|----(E)
         !             (e)

         wm = 0.0D0 !(minus)
         wp = 0.0D0 !(plus)

         select case (iCoord)
            case (1)
               wm = objMalla%fx(n+1)
               wp = objMalla%fxm(n+1)
            case (2)
               wm = objMalla%fy(n+1)
               wp = objMalla%fym(n+1)
            case (3)
               wm = objMalla%fz(n+1)
               wp = objMalla%fzm(n+1)
         end select

         if (wm+wp.lt.1.0D0) WRITE (*,*) 'ERROR'
          InterLin = varE * wm + varP * wp
         return
      end function InterLin

      REAL(8) FUNCTION InterArmonica(gamm,gamp,dxm,dxp)
         USE typePrecision
         REAL(nP) :: gamm,gamp
         REAL(nP) :: dxm,dxp

         InterArmonica = 2.0D0*gamm*gamp/(dxm*gamp+dxp*gamm+SMALL)        !Intepolacion media armonica
         !InterArmonica = gamm*gamp*(dxm + dxp)/(dxm*gamp+dxp*gamm+SMALL)   !Interpolacion armonica
      END function InterArmonica

      SUBROUTINE setGridDimensions()
      !Dimensiona las variables gemotrica de la malla

         ALLOCATE(objMalla%dx(ni))
         ALLOCATE(objMalla%dy(nj))
         ALLOCATE(objMalla%dz(nk))
         ALLOCATE(objMalla%areaX(nj,nk))
         ALLOCATE(objMalla%areaY(ni,nk))
         ALLOCATE(objMalla%areaZ(ni,nj))
         ALLOCATE(objMalla%vol(ni,nj,nk))
         ALLOCATE(objMalla%fx(ni))
         ALLOCATE(objMalla%fxm(ni))
         ALLOCATE(objMalla%fy(nj))
         ALLOCATE(objMalla%fym(nj))
         ALLOCATE(objMalla%fz(nk))
         ALLOCATE(objMalla%fzm(nk))

      END SUBROUTINE setGridDimensions

      subroutine makeGrid()
      !Realiza los calculos de la geometria de la malla
         integer     :: i,j,k

         !Establece el número de volumenes de control
         objMalla%ncvx = ni-2
         objMalla%ncvy = nj-2
         objMalla%ncvz = nk-2

         !Establece los nodos en los bordes y adyacentes
         !al borde
         objMalla%l1 = objMalla%ncvx+2
         objMalla%m1 = objMalla%ncvy+2
         objMalla%n1 = objMalla%ncvz+2

         objMalla%l2 = objMalla%ncvx+1
         objMalla%m2 = objMalla%ncvy+1
         objMalla%n2 = objMalla%ncvz+1

         ! Longitud de los arreglos en los bordes
         objMalla%dx(1) = 0.0D0
         objMalla%dy(1) = 0.0D0
         objMalla%dz(1) = 0.0D0

         objMalla%dx(objMalla%l1) = 0.0D0
         objMalla%dy(objMalla%m1) = 0.0D0
         objMalla%dz(objMalla%n1) = 0.0D0

         ! Cálculo de la ubicacion de los nodos en los bordes
         objMalla%x(1) = 0.0D0
         objMalla%y(1) = 0.0D0
         objMalla%z(1) = 0.0D0

         objMalla%x(objMalla%l1) = objMalla%xu(objMalla%l2)
         objMalla%y(objMalla%m1) = objMalla%yv(objMalla%m2)
         objMalla%z(objMalla%n1) = objMalla%zw(objMalla%n2)

         ! Calculo de los anchos de cara
         DO i = 2,objMalla%l2
            objMalla%dx(i) = objMalla%xu(i) - objMalla%xu(i-1)
         END DO

    	 DO j = 2,objMalla%m2
      	    objMalla%dy(j) = objMalla%yv(j) - objMalla%yv(j-1)
   	 END DO

   	 DO k = 2,objMalla%n2
      	    objMalla%dz(k) = objMalla%zw(k) - objMalla%zw(k-1)
   	 END DO

         DO i = 1,objMalla%l1
         DO j = 1,objMalla%m1
         DO k = 1,objMalla%n1

            ! Calculo de las areas (areaX, areaY, areaZ)
            objMalla%areaX(j,k) = objMalla%dy(j)*objMalla%dz(k)
            objMalla%areaY(i,k) = objMalla%dx(i)*objMalla%dz(k)
            objMalla%areaZ(i,j) = objMalla%dx(i)*objMalla%dy(j)

            ! Calculo del volumen
            objMalla%vol(i,j,k) = objMalla%dx(i)*objMalla%dy(j)*objMalla%dz(k)

         END DO
         END DO
         END DO

         ! Calculo de los factores de interpolación
         ! Dirección X
         objMalla%fx(1)  = 0.0D0
         objMalla%fxm(1) = 0.0D0

         DO i = 2,objMalla%l1
            objMalla%fxm(i) = (objMalla%x(i)-objmalla%xu(i-1))/(objMalla%x(i)-objMalla%x(i-1))
            objMalla%fx(i)  = 1.0D0-objMalla%fxm(i)
         END DO

         ! Dirección Y
         objMalla%fy(1)  = 0.0D0
         objMalla%fym(1) = 0.0D0

         DO j = 2,objMalla%m1
            objMalla%fym(j) = (objMalla%y(j)-objmalla%yv(j-1))/(objMalla%y(j)-objMalla%y(j-1))
            objMalla%fy(j)  = 1.0D0-objMalla%fym(j)
         END DO

         ! Dirección Z
         objMalla%fz(1)  = 0.0D0
         objMalla%fzm(1) = 0.0D0

         DO k = 2,objMalla%n1
            objMalla%fzm(k) = (objMalla%z(k)-objmalla%zw(k-1))/(objMalla%z(k)-objMalla%z(k-1))
            objMalla%fz(k)  = 1.0D0-objMalla%fzm(k)
         END DO

         return

      end subroutine makeGrid

      subroutine writeMalla()
      !Escribe los valores de los nodos en cada direccion en
      !archivo de texto externo
         integer  :: iunit,iend,ibeg,i,jend,jbeg,j,kend,kbeg,k

         CHARACTER(len = 16)  :: nomArchivo

         nomArchivo = "VTCout.txt"
         iunit = 10

         open (iunit,file = nomarchivo)

         ! Crea la salida para la Malla
         1 FORMAT(/1x,6(1h*),3x,"Malla             ",3x,6(1h*)/9x,20(1h-))

         iend=0

         2 FORMAT(/,'I =',2x,8(i4,5x))
         4 FORMAT('X =',1p8e9.2)

         6 FORMAT(/,'J =',2x,8(i4,5x))
         8 FORMAT('Y =',1p8e9.2)

         10 FORMAT(/,'K =',2x,8(i4,5x))
         12 FORMAT('Z =',1p8e9.2)

         DO WHILE (iend.NE.objMalla%l1)
            ibeg=iend+1
            iend=iend+8
            iend=MIN(iend,objMalla%l1)
            WRITE(iunit,2) (i,i=ibeg,iend)
            WRITE(iunit,4) (objMalla%x(i),i=ibeg,iend)
         END DO

         DO WHILE (jend.NE.objMalla%m1)
            jbeg=jend+1
            jend=jend+8
            jend=MIN(jend,objMalla%m1)
            WRITE(iunit,6) (j,j=jbeg,jend)
            WRITE(iunit,8) (objMalla%y(j),j=jbeg,jend)
         END DO

         DO WHILE (kend.NE.objMalla%n1)
            kbeg=kend+1
            kend=kend+8
            kend=MIN(kend,objMalla%n1)
            WRITE(iunit,10) (k,k=kbeg,kend)
            WRITE(iunit,12) (objMalla%z(k),k=kbeg,kend)
         END DO
      
      end subroutine writeMalla

end module typeMalla
