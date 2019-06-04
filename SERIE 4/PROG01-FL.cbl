      *****************************************************************
      * Author:      FABRICA DE SOFTWARE - SISTEMAS ACTIVOS S.R.L.
      * Date:        01 DE JUNIO 2019.
      * Purpose:     EJERCICIO 1 - Estructuras Básicas
      * Tectonics:   cobc
      *****************************************************************
       IDENTIFICATION DIVISION.
      *****************************************************************
      *****El nombre de programa se conforma de la sig. manera      ***
      *****PROG01 donde 01 corresponde al nro de ejerc de 2 digitos ***
      *****       si el ejercicio fuera 1-b se agrega la letra      ***
      *****       a continuación ej. PROG01B                        ***
      *****-00-   indica el cod. de usuario s/lista curso           ***
      *****-SA    indica las iniciales del usuario                  ***
      *****************************************************************
       PROGRAM-ID.   PROG01-09-FL.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       01 VARIABLES.
          05 WSV-IMP-CUOTA        PIC 9(04)V9(02).
          05 WSV-NUMERO-CUOTA     PIC 9(01).
          05 WSV-MONTOTOTAL       PIC 9(04)V9(02).
          05 WSV-TOTIVA           PIC 9(04)V9(02).
          05 WSV-PROMEDIO         PIC 9(04)V9(02).
          05 WSV-NOMBRE           PIC A(20).
          05 WSV-APELLIDO         PIC A(20).
          05 WSV-RESPUESTA        PIC 9.



       01 CONSTANTES.
          05 WSC-0                PIC 9(01)       VALUE 0.
          05 WSC-1                PIC 9(01)       VALUE 1.
          05 WSC-5                PIC 9(01)       VALUE 5.
          05 WSC-PORCIVA          PIC 99          VALUE 21.



       01 ACUMULADORES.
          05 WSA-ACUM-IMPORTE     PIC 9(04)V9(02).
          05 WSA-CONT             PIC 9.
          05 WSA-CONTPROMEDIO     PIC 9.


       77 WS-MASCARAVALOR         PIC ZZZZ.V99.
       77 WS-MASCARAIVA           PIC ZZZZ.V99.
       77 WS-MASCARAPROMEDIO      PIC ZZZZ.V99.

       PROCEDURE DIVISION.

       000000-CONTROL.
           PERFORM 100000-INICIO
           PERFORM 200000-PROCESO
               PERFORM 210000-CARGA
               PERFORM 220000-CALCULO
               PERFORM 230000-RESULTADO
           STOP RUN.

       100000-INICIO.

           INITIALIZE VARIABLES
                      ACUMULADORES.

       200000-PROCESO.

           210000-CARGA.

              DISPLAY "INGRESE SU NOMBRE: "
              ACCEPT WSV-NOMBRE.
              DISPLAY "INGRESE SU APELLIDO: "
              ACCEPT WSV-APELLIDO.

              PERFORM UNTIL WSV-RESPUESTA = 2
              ADD 1 TO WSV-NUMERO-CUOTA
              DISPLAY '**************************************'
              DISPLAY 'INGRESE IMPORTE CUOTA ' WSV-NUMERO-CUOTA
              DISPLAY '**************************************'
              ACCEPT WSV-IMP-CUOTA
              COMPUTE WSA-ACUM-IMPORTE=WSA-ACUM-IMPORTE + WSV-IMP-CUOTA
              ADD 1 TO WSA-CONTPROMEDIO

              DISPLAY "DESEA CONTINUAR? (SI = 1 - NO = 2)"
              DISPLAY "RESPUESTA: "
              ACCEPT WSV-RESPUESTA

              END-PERFORM.

              MOVE WSA-ACUM-IMPORTE TO WS-MASCARAVALOR.

           220000-CALCULO.

               COMPUTE WSV-TOTIVA = WSA-ACUM-IMPORTE * WSC-PORCIVA / 100.
               COMPUTE WSV-MONTOTOTAL = WSA-ACUM-IMPORTE + WSV-TOTIVA.
               COMPUTE WSV-PROMEDIO = WSV-MONTOTOTAL / WSA-CONTPROMEDIO.

               MOVE WSV-TOTIVA TO WS-MASCARAIVA.
               MOVE WSV-PROMEDIO TO WS-MASCARAPROMEDIO.

           230000-RESULTADO.

           DISPLAY '**************************************'
           DISPLAY 'Author:   SISTEMAS ACTIVOS S.R.L.'
           DISPLAY 'Purpose:  EJERCICIO 1 - Estr.Basicas'
           DISPLAY 'Programme:PROG01-00-SA'.
           DISPLAY '**************************************'.

           DISPLAY '**************************************'
           DISPLAY "USUARIO: " FUNCTION TRIM (WSV-NOMBRE) " "
               FUNCTION TRIM (WSV-APELLIDO).
           DISPLAY '**************************************'.

           DISPLAY '**************************************'
           DISPLAY "PAGOS TOTALES SIN IVA: " WSA-ACUM-IMPORTE
           DISPLAY "MONTO IVA: " WS-MASCARAIVA
           DISPLAY "PAGOS TOTALES CON IVA: " WSV-MONTOTOTAL
           DISPLAY " "
           DISPLAY "PROMEDIO: " WS-MASCARAPROMEDIO.
           DISPLAY '**************************************'.

       END-RUN.
