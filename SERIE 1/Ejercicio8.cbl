      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG08-09-FL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
           77 WS-NUMERO            PIC 9(6)         VALUE 0.
           77 WS-MIN               PIC S9(5)        VALUE -45834.
           77 WS-MAX               PIC 9(5)         VALUE 49234.
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            DISPLAY "INGRESE UN NUMERO: "
            ACCEPT WS-NUMERO.

            IF WS-NUMERO > WS-MIN AND WS-NUMERO < WS-MAX
                DISPLAY "SU NUMERO ES: "WS-NUMERO
                ELSE
                    DISPLAY "SU NUMERO SUPERA EL RANGO".

            STOP RUN.

       END PROGRAM PROG08-09-FL.
