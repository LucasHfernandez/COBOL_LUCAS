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
           77 ESTADO-CIVIL     PIC X.
           88 WS-CASADO                 VALUE 'C' 'c'.
           88 WS-SOLTERO                VALUE 'S' 's'.
           88 WS-VIUDO                  VALUE 'V' 'v'.
           88 WS-DIVORCIADO             VALUE 'D' 'd'.
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            DISPLAY "INGRESE SU ESTADO CIVIL"
            DISPLAY "CASADO = C"
            DISPLAY "SOLTERO = S"
            DISPLAY "VIUDO = V"
            DISPLAY "DIVORCIADO = D"
            DISPLAY " "
            DISPLAY "RESPUESTA: "

            ACCEPT ESTADO-CIVIL
            IF WS-CASADO
                DISPLAY "SU ESTADO CIVIL [CASADO]"
            ELSE
            IF WS-SOLTERO
                DISPLAY "SU ESTADO CIVIL [SOLTERO]"
            ELSE
            IF WS-VIUDO
                DISPLAY "SU ESTADO CIVIL [VIUDO]"
            ELSE
            IF WS-DIVORCIADO
               DISPLAY "SU ESTADO CIVIL [DIVORCIADO]"
            ELSE
                DISPLAY "ERROR, NO EXISTE VALOR"

            STOP RUN.

       END PROGRAM YOUR-PROGRAM-NAME.
