      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Ejercicio1.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
      *****************************************************************
      *SECCION QUE GUARDARA LA FECHA TOMADA DEL SISTEMA.
      *****************************************************************
           01 WS-DATOS.
               05 WS-AUXAÑO    PIC 99      VALUE 0.
               05 WS-AUXMES    PIC 99      VALUE 0.
               05 WS-AUXDIA    PIC 99      VALUE 0.
      *****************************************************************
      *SECCION QUE GUARDARA TODOS LOS DATOS DE LA FECHA DEL SISTEMA.
      *****************************************************************
           01 WS-FECHA.
               05 WS-DIA       PIC 9(2)    VALUE 0.
               05 FILLER       PIC X       VALUE "/".
               05 WS-MES       PIC 9(2)    VALUE 0.
               05 FILLER       PIC X       VALUE "/".
               05 WS-SIGLO     PIC 9(2)    VALUE 0.
               05 WS-AÑO       PIC 9(2)    VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            ACCEPT WS-DATOS FROM DATE.

            IF WS-AUXAÑO < 20
                MOVE 20 TO WS-SIGLO.

            MOVE WS-AUXDIA TO WS-DIA.
            MOVE WS-AUXMES TO WS-MES.
            MOVE WS-AUXAÑO TO WS-AÑO.

            DISPLAY WS-FECHA.

            STOP RUN.

       END PROGRAM Ejercicio1.
