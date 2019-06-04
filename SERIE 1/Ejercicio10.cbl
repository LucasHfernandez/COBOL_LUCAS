      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG10-09-FL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
           01 WS-MONTO             PIC 9(20)      VALUE 0.
           01 WS-IVA               PIC 9(5)v99   VALUE 0.
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            DISPLAY "INGRESE MONTO DESEADO: "
            ACCEPT WS-MONTO.

            MULTIPLY WS-MONTO BY 0.21 GIVING WS-IVA.

            DISPLAY "EL IVA DEL MONTO ES: " WS-IVA.

            STOP RUN.

       END PROGRAM PROG10-09-FL.
