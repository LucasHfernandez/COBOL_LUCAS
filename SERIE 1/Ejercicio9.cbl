      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG09-09-FL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
           01 WS-NOMBRE            PIC A(30).
           01 WS-APELLIDO          PIC A(30).
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            DISPLAY "INGRESE SU NOMBRE: "
            ACCEPT WS-NOMBRE.
            DISPLAY "INGRESE SU APELLIDO: "
            ACCEPT WS-APELLIDO.
            DISPLAY " "
            DISPLAY "SU NOMBRE ES: " WS-NOMBRE WS-APELLIDO.

            STOP RUN.

       END PROGRAM PROG09-09-FL.
