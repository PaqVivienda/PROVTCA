!     Last change:  R     3 Jun 2004    9:09 am

MODULE OUTPUT
!Contiene todas las subrutinas correspondientes a la
!salida de datos

   USE LeerData
   USE typeMalla
   USE typeArreglo3D

   contains

      SUBROUTINE OutScreen(iter,numTimeStep,timer)
      !Subrutina para la salida de datos por pantalla

         USE typePrecision
         USE PRESION
         USE UVWT

	 INTEGER         :: iter,numTimeStep
	 REAL(nP)        :: timer

	 INTENT(IN)  ::  iter,numTimeStep,timer

         IF (iter.EQ.0.AND.numTimeStep.EQ.0.0D0) THEN
      	    WRITE(*,*) "                              		      "
      	    WRITE(*,*) " -----------------------------------------        "
  	    WRITE(*,*) " Programa ProVTC                                  "
  	    WRITE(*,*) " Procesador para el estudio de Viviendas            "
  	    WRITE(*,*) " Termicamente Confortables                        "
  	    WRITE(*,*) " Proyecto LUZ-FONACIT                             "
  	    WRITE(*,*) " Facultad de Ingenieria                           "
  	    WRITE(*,*) " Universidad del Zulia                            "
  	    WRITE(*,*) " Maracaibo - Venezuela                            "
  	    WRITE(*,*) " -----------------------------------------        "
  	    WRITE(*,*) " Realizado  por: Ing. Javier E. Romero V.          "
            WRITE(*,*) " Modificado por: Ing. Javier V. Goicochea P.      "
            WRITE(*,*) " All Rigths Reserved                              "
  	    WRITE(*,*) " Copyright 2002-2004                              "
  	    WRITE(*,*) " -----------------------------------------        "
            WRITE(*,*) "                           			      "
      	    WRITE(*,*) " 	      RESULTADOS DEL ProVTC                "
      	    WRITE(*,*) "                                       	      "
         END IF

         IF (iter.EQ.0) THEN
            WRITE(*,*)  '					 '
            WRITE(*,*)  'Pasos en el Tiempo =',numTimeStep,'      Tiempo de simulacion =',CEILING(TIMER/60),'min.'
            WRITE(*,*)  '					 '
            WRITE(*,*) 'Imonitoreo=',Imon,',Jmonitoreo=',Jmon,',Kmonitoreo=',Kmon
            WRITE(*,*) 'Iter       velU     velV   	velW       Temp      smax       ssum'
            WRITE(*,*) '					                            '
         END IF

         IF (iter.EQ.1.OR.MOD(iter,1).EQ.0) THEN
  	     WRITE(*,10) iter,u%value(Imon,Jmon,kmon),v%value(Imon,Jmon,kmon),&
  	        	 w%value(Imon,Jmon,kmon),t%value(Imon,Jmon,kmon),smax,ssum
         END IF

  10 FORMAT(1X,I4,2x,1P8E11.3,4X,1P8E11.3,4X,1P8E11.3,4X,1P8E11.3,4X,1P8E11.3,4X,1P8E11.3)

         RETURN

      END SUBROUTINE OutScreen

      subroutine writeOutputFiles(u,v,w,T,visc,p)

         type(arreglo3D), intent(in)	:: u,v,w,T,visc,p
         type(arreglo3D)    		:: f(nfmax)

         ! Copia los valores de las variables a imprimir
         f(1)  = u
         f(2)  = v
         f(3)  = w
         f(4)  = p
         f(5)  = T
         f(6)  = visc

         ! Llama al modulo Grid y escribe la malla en el archivo
         CALL writeMalla()

         ! Escribe el campo de solución para u,v,w,T
         call writeFileArreglos3D(f)

         ! Escribe el campo de solución para u,v,w,T en el archivo plotf
         call plotFileArreglos(f)

      END SUBROUTINE writeOutputFiles

      subroutine writeFileArreglos3D(f)
      !Crea el archivo de datos de texto con los valores de todas las
      !variables estudiadas (f(nfmax)), para cada nodo

         type(arreglo3D), intent(in)   :: f(nfmax)
         character(len=15)             :: nomArchivo
         integer                       :: i,j,k,nf,iunit,ibeg,iend

         nomArchivo = "VTCout.txt"

         iunit = 10

         open (iunit,file = nomarchivo, status = 'unknown')

         ! crea la salida para las variables dependientes f
         14 format(//1x,6(1h*),3x,a24,3x,6(1h*)/9x,26(1h-))
         16 format(/'  i =',i6,6i9)
         18 format(1x,i2,3x,1p7e9.2)
         20 format(/'  k =',i6)
         22 format(1x,20a,1x,20(1h-))

         do nf=1,nfmax

            write (iunit,14) f(nf)%title

            do k = 1,nk

               ibeg = 1
               iend = ibeg + 6

               write(iunit,20) k

               if (ni.gt.iend) then

		            do while (iend.ne.ni)

                     iend = ibeg + 6
                     iend = min(iend,ni)

                     write(iunit,16) (i,i=ibeg,iend)
                     write(iunit,*) ' j ='

                     do j=nj,1,-1
                        write(iunit,18) j,(f(nf)%value(i,j,k),i=ibeg,iend)
                     end do

                     ibeg = iend + 1

                  end do

               else

                  iend = ibeg + 6
                  iend = min(iend,ni)

                  write(iunit,16) (i,i=ibeg,iend)
                  write(iunit,*) ' j ='

		            do j=nj,1,-1
                     write(iunit,18) j,(f(nf)%value(i,j,k),i=ibeg,iend)
                  end do

                  ibeg = iend + 1

               end if

            end do
         end do

         close (iunit)

      end subroutine writeFileArreglos3D

      SUBROUTINE plotFileArreglos(f)
      !Crea el archivo de salida grafico

         type(arreglo3D), intent(in)	:: f(nfmax)

         nZone = 1

         iunit      = 10

         4 FORMAT('  C=BLACK, F=POINT, I=',I5,3X, 'J=',I5,3X,'K=',I5)
         6 FORMAT(18(E16.6))

         IF (nZone.EQ.1) THEN
            OPEN (iunit,FILE= plotf, ACCESS='SEQUENTIAL',FORM='FORMATTED')
            WRITE (iunit,*) 'TITLE = "',title,'"'
	    WRITE (iunit,*) 'VARIABLES ='
            WRITE (iunit,*) ' "X           "'
            WRITE (iunit,*) ' "Y           "'
	    WRITE (iunit,*) ' "Z           "'
	    WRITE (iunit,*) ' "Velocidad U "'
	    WRITE (iunit,*) ' "Velocidad V "'
	    WRITE (iunit,*) ' "Velocidad W "'
	    WRITE (iunit,*) ' "Presión     "'
	    WRITE (iunit,*) ' "Temperatura "'
	    WRITE (iunit,*) ' "Viscosidad  "'

          ELSE
      	    OPEN (iunit,FILE= plotf, ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN' &
                                     ,POSITION='APPEND')
            REWIND(100)
          END IF

          WRITE (iunit,*) ' ZONE T = "',nZone,'"'
          WRITE (iunit,4) ni,nj,nk

   	  DO k = 1,nk
   	  DO j = 1,nj
	  DO i = 1,ni

   	     WRITE (iunit,6) objMalla%x(i),objMalla%y(j),objMalla%z(k),(f(nf)%value(i,j,k),nf=1,nfmax)

   	  END DO
   	  END DO
   	  END DO

   	  CLOSE(iunit)

      END SUBROUTINE plotFileArreglos

END MODULE OUTPUT
