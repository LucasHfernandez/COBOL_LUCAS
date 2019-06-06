      *****************************************************************
      * Author:      FERNANDEZ LUCAS IVAN
      * Date:        04 DE JUNIO 2019.
      * Purpose:     EJERCICIO 2 - VECTORES - CALCULO PROMEDIO DE NOTAS
      * Tectonics:   cobc
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   PROG02_V3-09-FL.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
      ******************************************************************
      * 01 VARIABLES: VARIABLES QUE TOMARAN LA RESPUESTA DEL USUARIO   *
      *               SI DESEA SEGUIR OPERANDO O NO.                   *
      *                                                                *
      *    WSV-RESPUESTACANT: SI DESEA INGRESAR OTRO ALUMNO            *
      *    WSV-RESPALUMNO: TOMA INDICE PARA MOSTRAR ALUMNO             *
      *    WSV-RESPUESTA:  SI DESEA SEGUIR MOSTRANDO ALUMNOS           *
      *    WSV-RESPUESTAMAT: PARA ASIGNAR PROMEDIO A MATERIA           *
      *    WSV-RESPUESTANOTA: SI DESEA SEGUIR INGRESANDO NOTA          *
      ******************************************************************
       01 VARIABLES.
          05 WSV-RESPUESTACANT    PIC 9(01).
          05 WSV-RESPALUMNO       PIC 9(01).
          05 WSV-RESPUESTA        PIC 9(01).
          05 WSV-RESPUESTAMAT     PIC 9(01).
          05 WSV-RESPUESTANOTA    PIC 9(01).

       01 CONSTANTES.
          05 WSC-0                PIC 9(01)       VALUE 0.
          05 WSC-3                PIC 9(01)       VALUE 3.
          05 WSC-6                PIC 9(01)       VALUE 6.
          05 WSC-10               PIC 9(02)       VALUE 10.

       01 INDICES.
          05 WSI-I                PIC 9(01).
          05 WSJ-J                PIC 9(01).

       01 WST-NOT-NOTA1     PIC 9(02)            VALUE 0.
       01 WST-NOT-NOTA2     PIC 9(02)            VALUE 0.
       01 WST-NOT-NOTATP    PIC 9(02)            VALUE 0.
       01 WSV-PROMEDIO      PIC 9(02)V9(02)      VALUE 0.

      ******************************************************************
      * 01 TABLA_ALUMNOS: MATRIZ DE ALUMNOS CON SUS RESPECTIVAS MATERIAS
      *                   Y SUS PROMEDIOS POR CADA UNA DE ELLAS.       *
      ******************************************************************
       01 TABLA_ALUMNOS.
          05 WST-ALUMNO           OCCURS 100 TIMES.
             10 WST-ALU-NOMBRE    PIC A(20).
             10 WST-ALU-APELLIDO  PIC A(20).
             10 WST-ALU-INDICE    PIC 9(01).
          05 WST-MATERIA             OCCURS 100 TIMES.
             10 WST-MAT-MATEMATICA   PIC 9(02).
             10 WST-MAT-PROGRAMACION PIC 9(02).
             10 WST-MAT-INGLES       PIC 9(02).
             10 WST-MAT-METODOLOGIA  PIC 9(02).
             10 WST-MAT-LITERATURA   PIC 9(02).
             10 WST-MAT-FISICA       PIC 9(02).
             10 WST-MAT-TOTAL        PIC 9(02)V9(02).
             10 WST-MAT-PROMEDIOGEN  PIC 9(02)V9(02).
             10 WSC-CONTADOR         PIC 9(01).

       01 ACUMULADORES.
          05 WSA-ACUMULADOR       PIC 9(02)V9(02).


       PROCEDURE DIVISION.

       000000-CONTROL.
           PERFORM 100000-INICIO
           PERFORM 200000-PROCESO
           PERFORM 300000-FINAL
           STOP RUN.

       100000-INICIO.
      ******************************************************************
      * LA TABLA DE ALUMNOS CONTARA CON 100 ESPACIOS, CADA ALUMNO      *
      * CONTARA CON UN INDICE QUE SE INICIALIZARA EN 0, QUE ES VACIO,  *
      * UNA VEZ IGRESADO LOS DATOS DEL ALUMNO CON SUS NOTAS, SE LE     *
      * AÑADIRA UN 1 A SU INDICE DICIENDO QUE ESTA CARGADO Y ESE ESPACIO
      * DENTRO DE LA TABLA ESTARA LLENO.                               *
      ******************************************************************

           INITIALIZE VARIABLES
                      TABLA_ALUMNOS

              PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL WSI-I > 100

                ADD 0 TO WSC-CONTADOR(WSI-I)

      ******************************************************************
      * INGRESO DE DATOS DEL ALUMNO (NOMBRE Y APELLIDO) Y GUARDADOS EN *
      * LA CORREPONDIENTE SECCION DE LA TABLA SEGUN INDICE.            *
      ******************************************************************

                DISPLAY '**************************************'
                DISPLAY 'INGRESE NOMBRE ALUMNO NRO ' WSI-I
                DISPLAY '**************************************'
                DISPLAY "RESPUESTA: "
                ACCEPT WST-ALU-NOMBRE(WSI-I)

                DISPLAY '**************************************'
                DISPLAY 'INGRESE APELLIDO ALUMNO NRO ' WSI-I
                DISPLAY '**************************************'
                DISPLAY "RESPUESTA: "
                ACCEPT WST-ALU-APELLIDO(WSI-I)

      ******************************************************************

      ******************************************************************
      * INGRESO DE LAS 3 NOTAS POR MATERIA DEL ALUMNO.                 *
      ******************************************************************

               PERFORM VARYING WSJ-J FROM 1 BY 1 UNTIL WSJ-J > 6

                DISPLAY '**************************************'
                DISPLAY 'INGRESE NOTA PRIMER PARCIAL'
                DISPLAY '**************************************'
                DISPLAY "RESPUESTA: "
                ACCEPT WST-NOT-NOTA1

                PERFORM UNTIL WST-NOT-NOTA1> WSC-0
                          AND WST-NOT-NOTA1<= WSC-10

                   DISPLAY '**************************************'
                   DISPLAY 'DATO INVALIDO.'
                   DISPLAY 'INGRESE UNA NOTA NUMERICA'
                   DISPLAY 'ENTRE 1 Y 10.'
                   DISPLAY '**************************************'
                   ACCEPT WST-NOT-NOTA1

                END-PERFORM
      ******************************************************************
                DISPLAY '**************************************'
                DISPLAY 'INGRESE NOTA SEGUNDO PARCIAL'
                DISPLAY '**************************************'
                DISPLAY "RESPUESTA: "
                ACCEPT WST-NOT-NOTA2

                PERFORM UNTIL WST-NOT-NOTA2> WSC-0
                          AND WST-NOT-NOTA2<= WSC-10

                    DISPLAY '**************************************'
                    DISPLAY 'DATO INVALIDO.'
                    DISPLAY 'INGRESE UNA NOTA NUMERICA'
                    DISPLAY 'ENTRE 1 Y 10.'
                    DISPLAY '**************************************'
                    ACCEPT WST-NOT-NOTA2

                END-PERFORM
      ******************************************************************
                DISPLAY '**************************************'
                DISPLAY 'INGRESE NOTA TP'
                DISPLAY '**************************************'
                DISPLAY "RESPUESTA: "
                ACCEPT WST-NOT-NOTATP

                PERFORM UNTIL WST-NOT-NOTATP> WSC-0
                          AND WST-NOT-NOTATP<= WSC-10

                    DISPLAY '**************************************'
                    DISPLAY 'DATO INVALIDO.'
                    DISPLAY 'INGRESE UNA NOTA NUMERICA'
                    DISPLAY 'ENTRE 1 Y 10.'
                    DISPLAY '**************************************'
                    ACCEPT WST-NOT-NOTATP

                END-PERFORM

      ******************************************************************
      * UNA VEZ INGRESADA LAS 3 NOTAS DEL ALUMNO, SE CALCULARA EL      *
      * PROMEDIO DEL TOTAL DE LAS NOTAS PARA LUEGO ASIGNARLAS A UNA    *
      * MATERIA.                                                       *
      ******************************************************************

                COMPUTE WSA-ACUMULADOR= WST-NOT-NOTA1
                                        + WST-NOT-NOTA2
                                        + WST-NOT-NOTATP

                COMPUTE WSV-PROMEDIO = WSA-ACUMULADOR / WSC-3

                DISPLAY '**************************************'
                DISPLAY "DONDE DESEA ASIGNIAR EL PROMEDIO?"
                DISPLAY "01-MATEMATICA" " "
                        "02- PROGRAMACION" " "
                        "03-INGLES" " "
                        "04-METODOLOGIA" " "
                        "05-LITERATURA" " "
                        "06-FISICA"
                DISPLAY " "
                DISPLAY "RESPUESTA: "
                ACCEPT WSV-RESPUESTAMAT
                DISPLAY '**************************************'

                PERFORM UNTIL WSV-RESPUESTAMAT> WSC-0
                          AND WSV-RESPUESTAMAT<= WSC-6

                    DISPLAY '**************************************'
                    DISPLAY 'DATO INVALIDO.'
                    DISPLAY 'INGRESE MATERIA ASIGNADA'
                    DISPLAY 'ENTRE 1 Y 6.'
                    DISPLAY '**************************************'
                    ACCEPT WSV-RESPUESTAMAT

                END-PERFORM

      ******************************************************************
      * ASIGNACION DEL PROMEDIO DE LAS 3 NOTAS A UNA MATERIA SEGUN LA  *
      * RESPUESTA DEL USUARIO.                                         *
      ******************************************************************

                EVALUATE(WSV-RESPUESTAMAT)
                WHEN 1
                   MOVE WSV-PROMEDIO TO WST-MAT-MATEMATICA(WSI-I)
                WHEN 2
                   MOVE WSV-PROMEDIO TO WST-MAT-PROGRAMACION(WSI-I)
                WHEN 3
                   MOVE WSV-PROMEDIO TO WST-MAT-INGLES(WSI-I)
                WHEN 4
                   MOVE WSV-PROMEDIO TO WST-MAT-METODOLOGIA(WSI-I)
                WHEN 5
                   MOVE WSV-PROMEDIO TO WST-MAT-LITERATURA(WSI-I)
                WHEN 6
                   MOVE WSV-PROMEDIO TO WST-MAT-FISICA(WSI-I)
                END-EVALUATE

      ******************************************************************
      * WSC-CONTADOR ES UN CONTADOR QUE SE IRA SUMANDO EN 1 POR CADA   *
      * MATERIA INGRESADA, INDICANDO CUANTAS MAERIAS POSEE EL ALUMNO,  *
      * PARA LUEGO USARLO PARA EL CALCULO DEL PROMEDIO GENERAL.        *
      ******************************************************************

                ADD 1 TO WSC-CONTADOR(WSI-I)
                ADD 0 TO WSA-ACUMULADOR

                DISPLAY '**************************************'
                DISPLAY "DESEA SEGUIR INGRESANDO NOTAS? SI = 1 NO = 2"
                DISPLAY "RESPUESTA: "
                ACCEPT WSV-RESPUESTANOTA
                DISPLAY '**************************************'

                IF WSV-RESPUESTANOTA = 1
                    CONTINUE
                ELSE
                    EXIT PERFORM
                END-IF

             END-PERFORM
      ******************************************************************

      ******************************************************************
      * UNA VEZ QUE EL USUARIO TERMINO DE INGRESAR LOS DATOS DEL ALUMNO*
      * Y SUS NOTAS, SE LE ASIGNARA 1 AL INDICE PARA INDICAR QUE EL    *
      * ESPACIO DE LA TABLA ESTA LLENO.                                *
      ******************************************************************

                ADD 1 TO WST-ALU-INDICE(WSI-I)

                DISPLAY '**************************************'
                DISPLAY "DESEA SEGUIR INGRESANDO ALUMNO? SI = 1 NO = 2"
                DISPLAY "RESPUESTA: "
                ACCEPT WSV-RESPUESTACANT
                DISPLAY '**************************************'

                IF WSV-RESPUESTACANT = 2
                    EXIT PERFORM
                END-IF

           END-PERFORM.

           IF WSI-I = 100
               DISPLAY "NO HAY MAS ESPACIO PARA INGRESAR NUEVO ALUMNO"
           END-IF.

       200000-PROCESO.

      ******************************************************************
      * SE SUMARA TODOS LOS PROMEDIOS DE LAS MATERIAS DEL ALUMNO Y SE  *
      * DIVIDIRA POR LA CANTIDAD DE MATERIAS CURSADAS PARA DAR CON EL  *
      * PROMEDIO GENERAL DEL ALUMNO.                                   *
      ******************************************************************

           PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL
                                             WST-ALU-INDICE(WSI-I) = 0

            COMPUTE WST-MAT-TOTAL(WSI-I) = WST-MAT-MATEMATICA(WSI-I) +
                                           WST-MAT-PROGRAMACION(WSI-I) +
                                           WST-MAT-INGLES(WSI-I) +
                                           WST-MAT-METODOLOGIA(WSI-I) +
                                           WST-MAT-LITERATURA(WSI-I) +
                                           WST-MAT-FISICA(WSI-I)


             COMPUTE WST-MAT-PROMEDIOGEN(WSI-I) = WST-MAT-TOTAL(WSI-I)
                                                  / WSC-CONTADOR(WSI-I)


           END-PERFORM.

       300000-FINAL.

           DISPLAY '**************************************'
           DISPLAY 'Author:   FERNANDEZ LUCAS IVAN'
           DISPLAY 'Purpose:  EJERCICIO 2 - ARCHIVOS'
           DISPLAY 'Programme:PROG02_V3-09-FL'.
           DISPLAY '**************************************'.

           PERFORM UNTIL WSV-RESPUESTA = 2

           DISPLAY '**************************************'

           DISPLAY "INGRESE NRO DE ALUMNO."
           DISPLAY "RESPUESTA: "
           ACCEPT WSV-RESPALUMNO

      *****************************************************************
      * EL USUARIO INGRESARA EL INDICE DEL ALUMNO Y SI ESTE EXISTE, SE
      * MOSTRARA TODOS LOS DATOS DEL ALUMNO Y SUS MATERIAS, CON SUS
      * PROMEDIOS Y EL PROMEDIO GENERAL (SI LA MATERIA TIENE UN 0,
      * SIGNIFICA QUE NO SE LE ASIGNO LA MATERIA). SI NO EXISTE EL
      * INDICE SE LE INFORMARA AL USUARIO QUE NO EXISTE EL ALUMNO.
      *****************************************************************

           PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL
                                             WST-ALU-INDICE(WSI-I) = 0
              IF WSV-RESPALUMNO = WSI-I
                 DISPLAY "ALUMNO: "
                         FUNCTION TRIM (WST-ALU-NOMBRE(WSI-I)) " "
                         FUNCTION TRIM (WST-ALU-APELLIDO(WSI-I))

                 DISPLAY "***MATERIAS***"
                 DISPLAY " "
                 IF WST-MAT-MATEMATICA(WSI-I) = 0
                     DISPLAY "MATEMATICA: NO ASIGNADO"
                 ELSE
                     DISPLAY 'MATEMATICA: ' WST-MAT-MATEMATICA(WSI-I)
                 END-IF

                 IF WST-MAT-PROGRAMACION(WSI-I) = 0
                     DISPLAY "PROGRAMACION: NO ASIGNADO"
                 ELSE
                    DISPLAY 'PROGRAMACION: ' WST-MAT-PROGRAMACION(WSI-I)
                 END-IF

                 IF WST-MAT-INGLES(WSI-I) = 0
                     DISPLAY "INGLES: NO ASIGNADO"
                 ELSE
                     DISPLAY 'INGLES: ' WST-MAT-INGLES(WSI-I)
                 END-IF

                 IF WST-MAT-METODOLOGIA(WSI-I) = 0
                     DISPLAY "METODOLOGIA: NO ASIGNADO"
                 ELSE
                    DISPLAY 'METODOLOGIA: ' WST-MAT-METODOLOGIA(WSI-I)
                 END-IF

                 IF WST-MAT-LITERATURA(WSI-I) = 0
                     DISPLAY "LITERATURA: NO ASIGNADO"
                 ELSE
                     DISPLAY "LITERATURA: " WST-MAT-LITERATURA(WSI-I)
                 END-IF

                 IF WST-MAT-FISICA(WSI-I) = 0
                     DISPLAY "FISICA: NO ASIGNADO"
                 ELSE
                     DISPLAY "FISICA: " WST-MAT-FISICA(WSI-I)
                 END-IF

                 DISPLAY " "
                 DISPLAY "PROMEDIO GENERAL: " WST-MAT-PROMEDIOGEN(WSI-I)

                 EXIT PERFORM
              END-IF

           END-PERFORM

           IF WST-ALU-INDICE(WSI-I) = 0
                 DISPLAY "EL ALUMNO NO EXISTE."
           END-IF

           DISPLAY '**************************************'

           DISPLAY "DESEA SEGUIR OPERANDO? SI = 1 NO = 2"
           DISPLAY "RESPUESTA: "
           ACCEPT WSV-RESPUESTA

           DISPLAY '**************************************'

           END-PERFORM.

           DISPLAY "ADIOS! ^^".

       END-RUN.
