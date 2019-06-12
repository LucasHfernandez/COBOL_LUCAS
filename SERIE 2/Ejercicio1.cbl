      ******************************************************************
      * Author: FERNANDEZ LUCAS IVAN
      * Date: 08/06/2019
      * Purpose: EJERCICIO 1 SERIE 2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG01-09-FL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       01 VARIABLES.
      *     05 WS-NUMERO            PIC 9(02)       VALUE 0.
           05 WS-SUMA              PIC 9(04)       VALUE 0.
           05 WS-TOTAL             PIC 9(04)       VALUE 0.

       01 CONSTANTES.
           05 WSC-5                PIC 9(01)       VALUE 5.
           05 WSC-10               PIC 9(02)       VALUE 10.

       01 INDICES.
           05 WSI-F               PIC 9(01).
           05 WSI-C               PIC 9(01)        VALUE 1.
           05 WSI-NUM             PIC 9(01).

       01 TABLA_NUMERICA.
           05 WST-FILAS          OCCURS 5 TIMES.
               10 WST-FI-FILA1    PIC 9(02).
               10 WST-FI-FILA2    PIC 9(02).
               10 WST-FI-FILA3    PIC 9(02).
               10 WST-FI-FILA4    PIC 9(02).
               10 WST-FI-FILA5    PIC 9(02).





       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

            PERFORM VARYING WSI-F FROM 1 BY 1 UNTIL WSI-F > 25

               IF WSI-F <= 5
                   MOVE WSC-5 TO WST-FI-FILA1(WSI-F)
               ELSE
                   IF WSI-F >= 6 AND WSI-F <= 10
                       MOVE WSC-10 TO WST-FI-FILA2(WSI-F)
                   ELSE
                       IF WSI-F >= 11 AND WSI-F <= 15
                           MOVE WSC-5 TO WST-FI-FILA3(WSI-F)
                       ELSE
                           IF WSI-F >= 16 AND WSI-F <= 20
                              MOVE WSC-10 TO WST-FI-FILA4(WSI-F)
                           ELSE
                               IF WSI-F >= 21 AND WSI-F <= 25
                                 MOVE WSC-5 TO WST-FI-FILA5(WSI-F)

            END-PERFORM.

            PERFORM VARYING WSI-F FROM 1 BY 1 UNTIL WSI-F > 25

            DISPLAY WST-FI-FILA1(WSI-F)
            DISPLAY WST-FI-FILA2(WSI-F)
            DISPLAY WST-FI-FILA3(WSI-F)
            DISPLAY WST-FI-FILA4(WSI-F)
            DISPLAY WST-FI-FILA5(WSI-F)

            END-PERFORM.


            STOP RUN.

       END PROGRAM PROG01-09-FL.
