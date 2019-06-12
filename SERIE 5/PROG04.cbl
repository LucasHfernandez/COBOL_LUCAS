      *****************************************************************
      * Author:      FABRICA DE SOFTWARE - SISTEMAS ACTIVOS S.R.L.
      * Date:        01 DE JUNIO 2019.
      * Purpose:     EJERCICIO 4 - ARCHIVOS - CREACION DE ARCHIVO DE
      *              SALIDA A IMAGEN DEL DE ENTRADA
      * Tectonics:   cobc
      *****************************************************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID.  PROG04-00-SA.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA            ASSIGN TO DISK 'CLIENTES.DAT'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS WSS-FS-ENTRADA.

           SELECT SALIDA             ASSIGN TO DISK 'CLIENTES1.TXT'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS WSS-FS-SALIDA.
       DATA DIVISION.

       FILE SECTION.
       FD ENTRADA.
       01 REG-ENTRADA.
         05 WSV-NUMCLIEN               PIC X(08).
         05 WSV-OFIAPE                 PIC X(03).
         05 WSV-IMPORTE                PIC 9(08)V99.

       FD SALIDA.
       01 REG-SALIDA.
         05 WSV-SALIDA-NUMCLIEN        PIC X(08).
         05 WSV-SALIDA-OFIAPE          PIC X(03).
         05 WSV-SALIDA-IMPORTE         PIC 9(08)V99.

       WORKING-STORAGE SECTION.

       01 SWITCHES.
         05 WSS-FS-ENTRADA             PIC X(02).
           88 WSS-FS-ENTRADA-OK                     VALUE '00'.
           88 WSS-FS-ENTRADA-EOF                    VALUE '10'.

         05 WSS-FS-SALIDA              PIC X(02).
           88 WSS-FS-SALIDA-OK                      VALUE '00'.
           88 WSS-FS-SALIDA-EOF                     VALUE '10'.

       01 Constantes.
        05 WSC-1                       PIC 9(01)    VALUE 1.

       01 Acumuladores.
        05 WSA-LEI-ENTRADA             PIC 9(09).
        05 WSA-GRAB-SALIDA              PIC 9(09).

       PROCEDURE DIVISION.

       000000-CONTROL.
                 PERFORM 100000-INICIO
                 PERFORM 200000-PROCESO
                                UNTIL WSS-FS-ENTRADA-EOF
                 PERFORM 300000-FINAL.

       100000-INICIO.
           INITIALIZE SWITCHES
                      ACUMULADORES

           PERFORM 110000-ABRIR-ENTRADA

           PERFORM 120000-ABRIR-SALIDA

           PERFORM 130000-PRIMER-LECTURA.


       110000-ABRIR-ENTRADA.
           OPEN INPUT ENTRADA
           IF NOT WSS-FS-ENTRADA-OK
             DISPLAY 'ERROR DE ARCHIVO DE ENTRADA'
             DISPLAY 'FILE STATUS' WSS-FS-ENTRADA
             PERFORM 300000-FINAL
           END-IF.

       120000-ABRIR-SALIDA.
           OPEN OUTPUT SALIDA
           IF NOT WSS-FS-SALIDA-OK
             DISPLAY 'ERROR DE ARCHIVO DE SALIDA'
             DISPLAY 'FILE STATUS' WSS-FS-SALIDA
             PERFORM 300000-FINAL
           END-IF.

       130000-PRIMER-LECTURA.
           PERFORM 131000-LEER-ENTRADA
           IF WSS-FS-ENTRADA-EOF
             DISPLAY 'ARCHIVO VACIO'
             PERFORM 300000-FINAL
           END-IF.

       131000-LEER-ENTRADA.
           READ ENTRADA
              EVALUATE TRUE
                 WHEN WSS-FS-ENTRADA-OK
                      MOVE REG-ENTRADA         TO REG-SALIDA
                      CONTINUE
                 WHEN WSS-FS-ENTRADA-EOF
                      CONTINUE
                 WHEN OTHER
                      DISPLAY 'FILE STATUS' WSS-FS-ENTRADA
                      PERFORM 300000-FINAL.


       200000-PROCESO.
           ADD WSC-1                       TO WSA-LEI-ENTRADA
           ADD WSC-1                       TO WSA-GRAB-SALIDA
           PERFORM 210000-GRABAR-SALIDA
           PERFORM 131000-LEER-ENTRADA.

       210000-GRABAR-SALIDA.
           WRITE REG-SALIDA.


       300000-FINAL.
           PERFORM 310000-TITULOS
           PERFORM 320000-TOTALES-DE-CONTROL
           PERFORM 330000-CERRAR-ENTRADA
           PERFORM 340000-CERRAR-SALIDA
           STOP RUN.

       310000-TITULOS.
           DISPLAY '**************************************'
           DISPLAY 'Author:   SISTEMAS ACTIVOS S.R.L.'
           DISPLAY 'Purpose:  EJERCICIO 4 - ARCHIVOS'
           DISPLAY 'Programme:PROG04-00-SA'
           DISPLAY '**************************************'.

       320000-TOTALES-DE-CONTROL.
           DISPLAY '**************************************'
           DISPLAY 'TOTAL LEIDOS =' WSA-LEI-ENTRADA
           DISPLAY '**************************************'.

           DISPLAY '**************************************'
           DISPLAY 'TOTAL GRABADOS =' WSA-GRAB-SALIDA
           DISPLAY '**************************************'.
.
       330000-CERRAR-ENTRADA.
           CLOSE ENTRADA
           IF NOT WSS-FS-ENTRADA-OK
              DISPLAY 'ERROR DE ARCHIVO DE ENTRADA'
            DISPLAY 'FILE STATUS' WSS-FS-ENTRADA
           END-IF.

       340000-CERRAR-SALIDA.
           CLOSE SALIDA
           IF NOT WSS-FS-SALIDA-OK
              DISPLAY 'ERROR DE ARCHIVO DE ENTRADA'
            DISPLAY 'FILE STATUS' WSS-FS-SALIDA
           END-IF.
