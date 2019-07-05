      ******************************************************************
      * Author: FERNANDEZ LUCAS IVAN
      * Date: 05/07/2019
      * Purpose: EJERCICIO 1 EDD.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO1-EDD.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

           01 VARIABLES                 OCCURS 2 TIMES.
               05 WSV-NUMERO            PIC 9(02)      VALUE 0.

           01 INDICES.
               05 WSI-I                 PIC 9(02).
               05 WSI-AUX               PIC 9(02).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL WSI-I > 2

               DISPLAY "INGRESE UN NUMERO: "
               ACCEPT WSV-NUMERO(WSI-I)

           END-PERFORM.

               IF WSV-NUMERO(WSI-I - 1) > WSV-NUMERO(WSI-I)
                   MOVE WSV-NUMERO(WSI-I) TO WSI-AUX
                   MOVE WSV-NUMERO(WSI-I - 1) TO WSV-NUMERO(WSI-I)
                   MOVE WSI-AUX TO WSV-NUMERO(WSI-I - 1)
               END-IF.

               DISPLAY "NUMEROS: " WSV-NUMERO(WSI-I - 1)
               DISPLAY             WSV-NUMERO(WSI-I)


            STOP RUN.

       END PROGRAM EJERCICIO1-EDD.
