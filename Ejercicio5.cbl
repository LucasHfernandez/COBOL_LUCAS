      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
           77 RIESGO           PIC 9(1).
           88 WS-NORIESGO                  VALUE 1 2.
           88 WS-RESPUESTA                 VALUE "SI" "NO".

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            DISPLAY "INGRESE UN VALOR DE RIESGO: "
            ACCEPT RIESGO.

            IF WS-NORIESGO
                DISPLAY "ACEPTADO, NO HAY RIESGO"
                ELSE
                     DISPLAY "NO ACEPTADO, HAY RIESGOS"

            STOP RUN.

       END PROGRAM YOUR-PROGRAM-NAME.
