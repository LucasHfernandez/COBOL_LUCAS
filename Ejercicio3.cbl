      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Ejercicio3.
      *****************************************************************
      *EJERCICIO RESUELTO EN "EJERCICIO1".
      *****************************************************************
       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
           01 WS-FECHA.
               05 WS-DIA   PIC 9(2)    VALUE 0.
               05 FILLER   PIC X       VALUE "/".
               05 WS-MES   PIC 9(2)    VALUE 0.
               05 FILLER   PIC X       VALUE "/".
               05 WS-SIGLO PIC 9(2)    VALUE 0.
               05 WS-AÑO   PIC 9(2)    VALUE 0.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.

       END PROGRAM Ejercicio3.
