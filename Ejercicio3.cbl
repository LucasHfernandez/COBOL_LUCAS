      ******************************************************************
      * Author: FERNANDEZ LUCAS IVAN
      * Date: 12/06/2019
      * Purpose: EJERCICIO 3 SERIE 3
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG03-09-FL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       01 VARIABLES.
           05 WSV-CONTADOR     PIC 9(02)           VALUE 0.
           05 WSV-SUMADOR      PIC 9(03)           VALUE 0.
           05 WSV-BANDERA      PIC 9               VALUE 0.
           05 WSV-MAXIMO       PIC 99              VALUE 0.
           05 WSV-MINIMO       PIC 99              VALUE 0.
           05 WSV-PROMEDIO     PIC 9(03)V9(02)     VALUE 0.

       01 INDICES.
           05 WSI-I            PIC 9(02).
           05 WSI-J            PIC 9(02).
           05 WSI-K            PIC 9(02).

       01 NUMEROS.
           05 WSN-AUXNUMERO1   PIC 99              VALUE 0.
           05 WSN-AUXNUMERO2   PIC 99              VALUE 0.

       01 NOTAS.
           05 WSN-ALUMNOS          OCCURS 10 TIMES.
               10 WSN-ALU-LEGAJO   PIC 9(02)           VALUE 0.
               10 WSN-ALU-NOTA     PIC 9(02)           VALUE 0.


       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
      *****************************************************************
      * VALORES FORZADOS.
      *****************************************************************
           ADD 1 TO WSN-ALU-LEGAJO(1).
           ADD 9 TO WSN-ALU-NOTA(1).
           ADD 3 TO WSN-ALU-LEGAJO(2).
           ADD 8 TO WSN-ALU-NOTA(2).
           ADD 2 TO WSN-ALU-LEGAJO(3).
           ADD 10 TO WSN-ALU-NOTA(3).
           ADD 5 TO WSN-ALU-LEGAJO(4).
           ADD 7 TO WSN-ALU-NOTA(4).
           ADD 9 TO WSN-ALU-LEGAJO(5).
           ADD 5 TO WSN-ALU-NOTA(5).
           ADD 2 TO WSN-ALU-LEGAJO(6).
           ADD 8 TO WSN-ALU-NOTA(6).
           ADD 4 TO WSN-ALU-LEGAJO(7).
           ADD 6 TO WSN-ALU-NOTA(7).
           ADD 3 TO WSN-ALU-LEGAJO(8).
           ADD 9 TO WSN-ALU-NOTA(8).
           ADD 6 TO WSN-ALU-LEGAJO(9).
           ADD 8 TO WSN-ALU-NOTA(9).
           ADD 1 TO WSN-ALU-LEGAJO(10).
           ADD 7 TO WSN-ALU-NOTA(10).
      *****************************************************************
      *      PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL WSI-I > 10
      *         DISPLAY "INGRESE LEGAJO: "
      *         ACCEPT WSN-ALU-LEGAJO(WSI-I)
      *         DISPLAY "INGRESE LA NOTA: "
      *         ACCEPT WSN-ALU-NOTA(WSI-I)
      *      END-PERFORM.
      *****************************************************************
      * ORDENAMIENTO DEL VECTOR.
      *****************************************************************
            PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL WSI-I > 10
              PERFORM VARYING WSI-J FROM 1 BY 1 UNTIL WSI-J > 10 - WSI-I
                   IF WSN-ALU-LEGAJO(WSI-J) > WSN-ALU-LEGAJO(WSI-J + 1)
                       MOVE WSN-ALU-LEGAJO(WSI-J) TO WSN-AUXNUMERO1
                       MOVE WSN-ALU-LEGAJO(WSI-J + 1) TO
                                                   WSN-ALU-LEGAJO(WSI-J)
                       MOVE WSN-AUXNUMERO1 TO WSN-ALU-LEGAJO(WSI-J + 1)
                       MOVE WSN-ALU-NOTA(WSI-J) TO WSN-AUXNUMERO2
                       MOVE WSN-ALU-NOTA(WSI-J + 1) TO
                                                   WSN-ALU-NOTA(WSI-J)
                       MOVE WSN-AUXNUMERO2 TO WSN-ALU-NOTA(WSI-J + 1)
                   END-IF
              END-PERFORM

               PERFORM VARYING WSI-K FROM 1 BY 1 UNTIL WSI-K > 10
                   DISPLAY WSN-ALU-LEGAJO(WSI-K) "    "
                           WSN-ALU-NOTA(WSI-K)
               END-PERFORM
            END-PERFORM.
      *****************************************************************
      * PROCESAMIENTO DE LAS NOTAS DEL VECTOR.
      *****************************************************************
           PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL WSI-I > 10

              IF NOT WSN-ALU-LEGAJO(WSI-I) = WSN-ALU-LEGAJO(WSI-I - 1)

              PERFORM VARYING WSI-J FROM WSI-I BY 1 UNTIL NOT
                           WSN-ALU-LEGAJO(WSI-J) = WSN-ALU-LEGAJO(WSI-I)

              ADD 1 TO WSV-CONTADOR
              COMPUTE WSV-SUMADOR = WSV-SUMADOR + WSN-ALU-NOTA(WSI-J)

              IF WSV-BANDERA = 0
                 MOVE WSN-ALU-NOTA(WSI-J) TO WSV-MAXIMO
                 MOVE WSN-ALU-NOTA(WSI-J) TO WSV-MINIMO
                 ADD 1 TO WSV-BANDERA
                 DISPLAY "ENTRE"
              END-IF

              IF WSN-ALU-NOTA(WSI-J) > WSV-MAXIMO
                 MOVE WSN-ALU-NOTA(WSI-J) TO WSV-MAXIMO
              END-IF

              IF WSN-ALU-NOTA(WSI-J) < WSV-MINIMO
                 MOVE WSN-ALU-NOTA(WSI-J) TO WSV-MINIMO
              END-IF

              COMPUTE WSV-PROMEDIO = WSV-SUMADOR / WSV-CONTADOR

              END-PERFORM
      *****************************************************************
      * MUESTREO POR PANTALLA DEL VECTOR.
      *****************************************************************
              DISPLAY "LEGAJO NRO: " WSN-ALU-LEGAJO(WSI-I)
              DISPLAY "SU PROMEDIO DE NOTAS: " WSV-PROMEDIO
              DISPLAY "LA NOTA MAXIMA: " WSV-MAXIMO
              DISPLAY "LA NOTA MINIMA: " WSV-MINIMO

              END-IF
           END-PERFORM.

            STOP RUN.

       END PROGRAM PROG03-09-FL.
