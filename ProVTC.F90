   !     Last change:  EF   23 Jun 2005    3:40 pm
    PROGRAM VTC

   ! ----------------------------------------
   ! Programa VTC
   ! Programa para el estudio de viviendas
   ! térmicamente confortables
   ! Proyecto LUZ-FONACIT
   ! Facultad de Ingenieria
   ! Universidad del Zulia
   ! Maracaibo - Venezuela
   ! ----------------------------------------
   ! Realizado por: Ing. Javier E. Romero V.
   ! Modificado por: Ing. Javier V. Goicochea P.
   ! All Rigths Reserved
   ! Copyright 2002-2003
   !-----------------------------------------


   USE typePrecision
   USE UVWT
   USE presion
   USE output
   USE Propiedades
   USE LeerData

   !Declarar las variables que no estan definidas en ningun modulo
   INTEGER  :: nf,iter,numIter,numTimeStep
   REAL(nP) :: timeStep,runTime,Tol,timer

   ! Llama a la subrutina DatosEntrada y lee los nodos y caras
   ! de la malla, para establecer el tamaño de los arreglos.
   ! La subroutina regresa los valores de ni,nj,nk.
   ! Tambien el archivo de salida y regresa los valores
   ! correspondientes a:
   ! 1) Dimensiones de la malla (x,y,z,xu,yv,zw)
   ! 2) Intervalos de Tiempo
   ! 3) Datos de la Simulacion
   CALL DatosEntrada(numIter,runTime,timeStep)

   !Dimensiona e inicializa todos los arreglos utilizados en el programa
   CALL InicioProb()

   numTimeStep = 0.0D0
   Tol = 1.0E-12         !Define el criterio de convergencia
   TIMER = 0.0D0         !Inicia el tiempo en cero

   ! Inicializa el campo de velocidades en las caras (flujo en las caras)
   CALL initVelFace()

   DO WHILE (TIMER.LE.runTime)

      iter = 0
      smax = 1.0E20

      DO WHILE ((smax.GE.Tol).AND.(iter.LT.numIter))
      	! Imprime las variables en pantalla
      	CALL OutScreen(iter,numTimeStep,timer)

       	! Construye el lazo para la solución de u, v, w
       	IF (lcaluvw) CALL calUVW(timer,TimeStep)

        ! Calculo de la temperatura
       	IF (lcalT)  CALL calT(TimeStep,timer)

        ! Siguiente iteracion
        iter = iter + 1

      END DO

     	CALL OutScreen(iter,numTimeStep,timer)

      !Siguiente intervalo de tiempo
      numTimeStep = numTimeStep + 1
      TIMER = TIMER + timestep

   END DO
   ! Escribe archivos de salida para el graficador
   CALL writeOutputFiles(u,v,w,T,visc,p)
   pause

END PROGRAM VTC

SUBROUTINE InicioProb()

   USE Coeficientes
   USE typeBorde
   USE Propiedades
   USE UVWT
   USE presion
   USE typeMalla
   USE solvers
   USE adapt

 !Hace la llamada para las distintas subrutinas necesarias para establecer tanto
 !Condiciones Iniciales(KBC, etc),  como  variables  para
 !hacer calculos referentes a CM, Vel, etc.

   CALL setCoef()             !Inicializa coeficientes
   CALL setProp()             !Inicializa propiedades
   CALL setUVWT()             !Inicializa velocidades y temperatura
   CALL setPresion()          !Presiones y flujos masicos
   call setGridDimensions()   !Dimensiones de la malla
   !CALL setFlux()

   ! Crea los arreglos IA y JA necesarios para emplear los algoritmos de
   ! solución de la libreria ITpack
   CALL setupSolver()

   ! Regresa todos los valores correspondientes de la Malla
   ! dx,dy,dz - areaX,areaY,areaZ - vol - Factores de Relajacion
   CALL makeGrid()

   ! Llama a la subrutina BEGIN del Adapt, Lee el archivo de
   ! las propiedades Densidad, Gamma, Gammat, Viscosidad
   CALL begin(u,v,w,t)

   ! Inicializa las condiciones de borde de la velocidad
   call setBC()

END SUBROUTINE InicioProb

