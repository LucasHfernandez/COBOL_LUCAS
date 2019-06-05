      *****************************************************************
      * Author:      FERNANDEZ LUCAS IVAN
      * Date:        04 DE JUNIO 2019.
      * Purpose:     EJERCICIO 2 - VECTORES - CALCULO PROMEDIO DE NOTAS
      * Tectonics:   cobc
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   PROG02_V2-09-FL.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       01 VARIABLES.
          05 WSV-RESPUESTACANT    PIC 9(01).
          05 WSV-RESPALUMNO       PIC 9(01).
          05 WSV-RESPUESTA        PIC 9(01).

       01 CONSTANTES.
          05 WSC-0                PIC 9(01)       VALUE 0.
          05 WSC-1                PIC 9(01)       VALUE 1.
          05 WSC-3                PIC 9(01)       VALUE 3.
          05 WSC-7                PIC 9(02)V9(02) VALUE 07,00.
          05 WSC-10               PIC 9(02)       VALUE 10.
          05 WSC-APROBADO         PIC X(08)       VALUE 'APROBADO'.
          05 WSC-DESAPROBADO      PIC X(11)       VALUE 'DESAPROBADO'.

       01 INDICES.
          05 WSI-I                PIC 9(01).

       01 TABLA_ALUMNOS.
          05 WST-ALUMNO           OCCURS 100 TIMES.
             10 WST-ALU-NOMBRE    PIC A(20).
             10 WST-ALU-APELLIDO  PIC A(20).
             10 WST-ALU-INDICE    PIC 9(01).
          05 WST-NOTAS            OCCURS 100 TIMES.
             10 WST-NOT-NOTA1     PIC 9(02).
             10 WST-NOT-NOTA2     PIC 9(02).
             10 WST-NOT-NOTATP    PIC 9(02).
             10 WSV-PROMEDIO      PIC 9(02)V9(02).

       01 ACUMULADORES.
          05 WSA-ACUMULADOR       PIC 9(02)V9(02).


       PROCEDURE DIVISION.

       000000-CONTROL.
           PERFORM 100000-INICIO
           PERFORM 200000-PROCESO
           PERFORM 300000-FINAL
           STOP RUN.

       100000-INICIO.

           INITIALIZE VARIABLES
                      TABLA_ALUMNOS

              PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL WSI-I > 100

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
                DISPLAY '**************************************'
                DISPLAY 'INGRESE NOTA PRIMER PARCIAL'
                DISPLAY '**************************************'
                DISPLAY "RESPUESTA: "
                ACCEPT WST-NOT-NOTA1(WSI-I)

                PERFORM UNTIL WST-NOT-NOTA1(WSI-I)> WSC-0
                          AND WST-NOT-NOTA1(WSI-I)<= WSC-10

                   DISPLAY '**************************************'
                   DISPLAY 'DATO INVALIDO.'
                   DISPLAY 'INGRESE UNA NOTA NUMERICA'
                   DISPLAY 'ENTRE 1 Y 10.'
                   DISPLAY '**************************************'
                   ACCEPT WST-NOT-NOTA1(WSI-I)

                END-PERFORM
      ******************************************************************
                DISPLAY '**************************************'
                DISPLAY 'INGRESE NOTA SEGUNDO PARCIAL'
                DISPLAY '**************************************'
                DISPLAY "RESPUESTA: "
                ACCEPT WST-NOT-NOTA2(WSI-I)

                PERFORM UNTIL WST-NOT-NOTA2(WSI-I)> WSC-0
                          AND WST-NOT-NOTA2(WSI-I)<= WSC-10

                    DISPLAY '**************************************'
                    DISPLAY 'DATO INVALIDO.'
                    DISPLAY 'INGRESE UNA NOTA NUMERICA'
                    DISPLAY 'ENTRE 1 Y 10.'
                    DISPLAY '**************************************'
                    ACCEPT WST-NOT-NOTA2(WSI-I)

                END-PERFORM
      ******************************************************************
                DISPLAY '**************************************'
                DISPLAY 'INGRESE NOTA TP'
                DISPLAY '**************************************'
                DISPLAY "RESPUESTA: "
                ACCEPT WST-NOT-NOTATP(WSI-I)

                PERFORM UNTIL WST-NOT-NOTATP(WSI-I)> WSC-0
                          AND WST-NOT-NOTATP(WSI-I)<= WSC-10

                    DISPLAY '**************************************'
                    DISPLAY 'DATO INVALIDO.'
                    DISPLAY 'INGRESE UNA NOTA NUMERICA'
                    DISPLAY 'ENTRE 1 Y 10.'
                    DISPLAY '**************************************'
                    ACCEPT WST-NOT-NOTATP(WSI-I)

                END-PERFORM
      ******************************************************************

                ADD 1 TO WST-ALU-INDICE(WSI-I)

                DISPLAY '**************************************'
                DISPLAY "DESEA SEGUIR INGRESANDO? SI = 1 NO = 2"
                DISPLAY "RESPUESTA: "
                ACCEPT WSV-RESPUESTACANT
                DISPLAY '**************************************'

                IF WSV-RESPUESTACANT = 2
                    EXIT PERFORM
                END-IF

           END-PERFORM.

       200000-PROCESO.

           PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL
                                             WST-ALU-INDICE(WSI-I) = 0

           COMPUTE WSA-ACUMULADOR= WST-NOT-NOTA1(WSI-I)
                                   + WST-NOT-NOTA2(WSI-I)
                                   + WST-NOT-NOTATP(WSI-I)

           COMPUTE WSV-PROMEDIO(WSI-I) = WSA-ACUMULADOR / WSC-3

           END-PERFORM.

       300000-FINAL.

           DISPLAY '**************************************'
           DISPLAY 'Author:   FERNANDEZ LUCAS IVAN'
           DISPLAY 'Purpose:  EJERCICIO 2 - ARCHIVOS'
           DISPLAY 'Programme:PROG02_V2-09-FL'.
           DISPLAY '**************************************'.

           PERFORM UNTIL WSV-RESPUESTA = 2

           DISPLAY '**************************************'

           DISPLAY "INGRESE NRO DE ALUMNO."
           DISPLAY "RESPUESTA: "
           ACCEPT WSV-RESPALUMNO

           PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL
                                             WST-ALU-INDICE(WSI-I) = 0
              IF WSV-RESPALUMNO = WSI-I
                 DISPLAY "ALUMNO: "
                         FUNCTION TRIM (WST-ALU-NOMBRE(WSI-I)) " "
                         FUNCTION TRIM (WST-ALU-APELLIDO(WSI-I))
                 DISPLAY " "
                 DISPLAY 'PROMEDIO: ' WSV-PROMEDIO(WSI-I)

                 IF WSV-PROMEDIO(WSI-I) < WSC-7
                    DISPLAY WSC-DESAPROBADO " DEBE RENDIR FINAL"
                    ELSE
                       DISPLAY WSC-APROBADO
                 END-IF

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

       END-RUN.
