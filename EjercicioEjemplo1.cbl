      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EjercicioEjemplo1.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
           77  NUMERO          PIC 9(3)          VALUE 99.

           01  WS-TABLE.
                05 WS-FILA OCCURS 5 TIMES  INDEXED BY WS-I.
                     10 WS-ITEM-FILA       PIC 9(3).
                     10 WS-COLUMNA OCCURS 5 TIMES  INDEXED BY WS-J.
                         15  WS-ITEM-COLUMNA     PIC 9(3).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            PERFORM UNTIL NUMERO > 125

            PERFORM 1000-ARMO-FILA VARYING WS-I FROM 1 BY 1 UNTIL
                                                            WS-I > 5


            END-PERFORM.

      *      DISPLAY WS-TABLE.

            STOP RUN.


            1000-ARMO-FILA.

            PERFORM 2000-ARMO-COLUMNA VARYING WS-J FROM 1 BY 1 UNTIL
                                                               WS-J > 5.

            2000-ARMO-COLUMNA.

            ADD 1 TO NUMERO.
            MOVE NUMERO TO WS-ITEM-COLUMNA (WS-I WS-J).
            DISPLAY "FILA: " WS-I.
            DISPLAY "COLUMNA: " WS-J.
            DISPLAY "NUMERO: " NUMERO.

       END PROGRAM EjercicioEjemplo1.
