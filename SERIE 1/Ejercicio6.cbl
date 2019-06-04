      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG06-09-FL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
           01 WS-NUMERO            PIC 9(2)    VALUE 0.
           01 WS-RESPUESTA         PIC A(1).
           88 WS-AFIRMATIVO                    VALUE 'S' 's'.
           88 WS-NEGATIVO                      VALUE 'N' 'n'.
           88 WS-RESPUESTA-VALIDA              VALUE 'S' 'N' 's' 'n'.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            DISPLAY "INGRESE UN NUMERO: ".
            ACCEPT WS-NUMERO

            DISPLAY "NUMERO INGRESADO: "WS-NUMERO
            DISPLAY "CONFIRMAR OPERACION (S = SI - N = NO)".
            DISPLAY " ".
            DISPLAY "RESPUESTA: ".
            ACCEPT WS-RESPUESTA.

            PERFORM UNTIL WS-RESPUESTA-VALIDA
               DISPLAY "VALOR NO VALIDO, INGRESE NUEVAMENTE"
               DISPLAY " "
               DISPLAY "RESPUESTA: "
               ACCEPT WS-RESPUESTA

            END-PERFORM.

            IF WS-AFIRMATIVO
                DISPLAY "NUMERO GUARDADO CORRECTAMENTE".

            IF WS-NEGATIVO
                DISPLAY "NUMERO DESCARTADO".

            STOP RUN.

       END PROGRAM PROG06-09-FL.
