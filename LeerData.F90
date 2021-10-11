!     Last change:  F    30 Jun 2004   10:25 pm

MODULE LeerData
!Realiza la lectura de todos los datos iniciales necesarios
!para la corrida del programa

   USE typePrecision

   INTEGER              :: IndAlgSol
   CHARACTER(LEN = 12)  :: plotf
   CHARACTER(LEN = 120) :: title

   REAL(nP)     :: relaxUVW,relaxP,relaxT  ! Factores de relajacion

   INTEGER      :: Imon,Jmon,Kmon          ! Punto de Monitoreo

   LOGICAL      :: lcaluvw,lcalP,lcalT

   TYPE arreglo1D
     SEQUENCE
     REAL(nP), POINTER :: value(:)
   END TYPE

   TYPE(arreglo1D) :: relax

   CONTAINS

      SUBROUTINE DatosEntrada(numIter,runTime,timeStep)

         USE typeMalla

         CHARACTER(LEN = 20)  :: fileName,TEMP

  	 ! RUNOPTIONS
  	 INTEGER              :: numIter
  	 REAL(nP)             :: timeStep,runTime

  	 INTENT (IN OUT) :: numIter,runTime,timeStep

  	 ! GRID
  	 INTEGER               :: ncx,ncy,ncz      ! numero de caras
  	 INTEGER               :: ncvx,ncvy,ncvz   ! numero de volumenes de control

         !Lectura de los datos iniciales de la simulacion
  	 fileName = 'salida.dat'
	 OPEN (UNIT = 1, FILE = fileName, STATUS ='OLD')
            READ (1,*)
            READ (1,*) title
            READ (1,*)
            READ (1,*) numIter,IndAlgSol
            READ (1,*)
            READ (1,*) plotf
            READ (1,*)
            READ (1,*) runtime,timeStep
            READ (1,*)
            READ (1,*)
            READ (1,*) lcalUVW,lcalP,lcalT
            READ (1,*)
            READ (1,*)
            READ (1,*) relaxUVW,relaxP,relaxT
            READ (1,*)
            READ (1,*)
            READ (1,*) Imon,Jmon,Kmon

  	    !transforma de minutos a segundos el tiempo
            !total de la corrida y el paso de tiempo
  	    runTime = runTime * 60
  	    timeStep = timeStep * 60

            !Lectura de la geometria de la malla
            !LECTURA DE LAS CARAS DE VOLUMENES DE CONTROL
    	    READ (1,*)
    	    READ (1,*) TEMP,ncx
    	    READ (1,*) TEMP,ncy
    	    READ (1,*) TEMP,ncz
    	    ALLOCATE(objMalla%xu(ncx),objMalla%yv(ncy),objMalla%zw(ncz))
    	    READ (1,*) (objmalla%xu(I),I=1,ncx)
    	    READ (1,*) (objmalla%yv(I),I=1,ncy)
    	    READ (1,*) (objmalla%zw(I),I=1,ncz)
  	    !LECTURA DE LOS NODOS
    	    READ (1,*)
    	    READ (1,*) TEMP,ncvx
    	    READ (1,*) TEMP,ncvy
    	    READ (1,*) TEMP,ncvz
    	    ni = ncvx + 2
    	    nj = ncvy + 2
    	    nk = ncvz + 2
    	    ALLOCATE(objMalla%x(ni),objMalla%y(nj),objMalla%z(nk))
    	    READ (1,*) (objmalla%x(I+1),I=1,ncvx)
    	    READ (1,*) (objmalla%y(I+1),I=1,ncvy)
    	    READ (1,*) (objmalla%z(I+1),I=1,ncvz)
	 CLOSE(1)
  !======================================

  	 ! nf = 1 -----> Velocidad U
  	 ! nf = 2 -----> Velocidad V
  	 ! nf = 3 -----> Velocidad W
  	 ! nf = 4 -----> Presión
  	 ! nf = 5 -----> Temperatura
  	 ! nf = 6 -----> Viscosidad

  	 ! Dimensiona dinámicamente a relax
   	 ALLOCATE(relax%value(nfmax))

  	 ! Factores de relajación para cada variable
   	 relax%value(1:3)     = relaxUVW
  	 relax%value(4)       = relaxP
   	 relax%value(5)       = relaxT

     END SUBROUTINE DatosEntrada

END MODULE LeerData
