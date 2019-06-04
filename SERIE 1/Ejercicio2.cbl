      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG02-09-FL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
           01 WS-FECHA.
               05 WS-DIA       PIC 9(2)    VALUE 0.
               05 FILLER       PIC X       VALUE "-".
               05 WS-MES       PIC 9(2)    VALUE 0.
               05 FILLER       PIC X       VALUE "-".
               05 WS-AÑO       PIC 9(4)    VALUE 0.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            DISPLAY "INGRESE LA FECHA DESEADA (DD/MM/AAAA)".
            DISPLAY " ".
            DISPLAY "INGRESE EL DIA: "
            ACCEPT WS-DIA
            DISPLAY "INGRESE EL MES: "
            ACCEPT WS-MES
            DISPLAY "INGRESE EL AÑO: "
            ACCEPT WS-AÑO
            DISPLAY "LA FECHA DEL DIA ES: "WS-FECHA.

            STOP RUN.

       END PROGRAM PROG02-09-FL.
