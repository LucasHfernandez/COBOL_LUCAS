      ******************************************************************
      * Author: FERNANDEZ LUCAS IVAN
      * Date: 11/06/2019
      * Purpose: EJERCICIO 4 SERIE 6
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG04-09-FL.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA
           ASSIGN TO DISK'D:\EjerciciosCobol\Serie6\CUENTAS.DAT'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS WSS-FS-ENTRADA.

       DATA DIVISION.

       FILE SECTION.
       FD ENTRADA.
       01 REG-ENTRADA.
           05 FSE-NROCUENTA            PIC X(08).
           05 FSE-CODIGOCLTE           PIC 9(08).
           05 FSE-MONTOCUENTA          PIC 9(15)V9(02).

       WORKING-STORAGE SECTION.

       01 INDICES.
           05 WSI-I                    PIC 9           VALUE 0.

       01 ACUMULADOR.
           05 WSC-ACUMULADOR           PIC 9(03)       VALUE 0.

       01 SWITCHES.
           05 WSS-FS-ENTRADA           PIC X(02).
             88 WSS-FS-ENTRADA-OK                      VALUE '00'.
             88 WSS-FS-ENTRADA-EOF                     VALUE '10'.

       01 MASCARAS                    OCCURS 100 TIMES.
           05 WSM-MASCARAMONTO        PIC Z(15),99.

       01 CUADRO_CUENTA_FILA.
           05 FILLER    PIC X(19) VALUE '*-----------------*'.
           05 FILLER    PIC X(13) VALUE '------------*'.
           05 FILLER    PIC X(28) VALUE '---------------------------*'.


       01 CUADRO_CUENTA_TITULO.
           05 TITULO_NROCUENTA.
               10 FILLER             PIC X(14)   VALUE '|   NRO.CUENTA'.
           05 TITULO_CODIGO.
               10 FILLER             PIC X(16)   VALUE
                                                     '    |  COD.CLI.'.
           05 TITULO_MONTO.
               10 FILLER             PIC X(24)   VALUE
                                             ' |     DISPONIBLE CUENTA'.
               10 FILLER             PIC X(06)   VALUE '     |'.

       01 CUADRO_CUENTA_DATOS        OCCURS 100 TIMES.
           05 NROCLIENTE.
               10 WSC-AUXNROCUENTA   PIC X(08).
           05 CODIGO.
               10 WSC-AUXCODCLIENT   PIC 9(08).
           05 MONTO.
               10 WSC-AUXMONTO       PIC 9(15)V9(02).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

       000000-CONTROL.

           PERFORM 100000-ABRIR.
           PERFORM 200000-INGRESO.
           PERFORM 300000-IMPRIMIR.

       100000-ABRIR.
           OPEN INPUT ENTRADA
           IF NOT WSS-FS-ENTRADA-OK
             DISPLAY 'ERROR EN ARCHIVO DE ENTRADA!!'
             DISPLAY " "
             IF WSS-FS-ENTRADA = 35
               DISPLAY 'NO SE PUDO ABRIR EL ARCHIVO ESPECIFICADO X.X'
               DISPLAY " "
               DISPLAY 'EL ARCHIVO NO EXISTE O NO SE ENCUENTRA :S'
               PERFORM 300000-IMPRIMIR
             END-IF
           END-IF.



       200000-INGRESO.

           PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL WSS-FS-ENTRADA-EOF
               READ ENTRADA
               IF WSS-FS-ENTRADA-OK
                   MOVE REG-ENTRADA TO CUADRO_CUENTA_DATOS(WSI-I)
                   MOVE WSC-AUXMONTO(WSI-I) TO WSM-MASCARAMONTO(WSI-I)
                   ADD 1 TO WSC-ACUMULADOR

               IF NOT WSS-FS-ENTRADA-OK AND WSS-FS-ENTRADA-EOF
                   DISPLAY 'FILE STATUS ' WSS-FS-ENTRADA
                   EXIT PERFORM

           END-PERFORM.

           IF WSS-FS-ENTRADA-EOF AND WSI-I = 1
               DISPLAY "EL ARCHIVO ESTA VACIO."
               PERFORM 300000-IMPRIMIR
           END-IF.




       300000-IMPRIMIR.
           IF WSS-FS-ENTRADA-OK OR WSS-FS-ENTRADA-EOF
               DISPLAY "CUADRO DE CUENTAS"
               DISPLAY " "

               DISPLAY CUADRO_CUENTA_FILA
               DISPLAY CUADRO_CUENTA_TITULO
               DISPLAY CUADRO_CUENTA_FILA

               PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL
                                                 WSI-I > WSC-ACUMULADOR

               DISPLAY '|    ' WSC-AUXNROCUENTA(WSI-I)
                       '     |  ' WSC-AUXCODCLIENT(WSI-I)
                       '  |     ' WSM-MASCARAMONTO(WSI-I) '$'
                       '   |'

               END-PERFORM

               DISPLAY CUADRO_CUENTA_FILA

           END-IF.
           PERFORM 310000-SALIR.


       310000-SALIR.

           CLOSE ENTRADA
           IF NOT WSS-FS-ENTRADA-OK
              DISPLAY " "
              DISPLAY " "
              DISPLAY 'ERROR EN ARCHIVO DE ENTRADA!!'
              IF WSS-FS-ENTRADA = 42
               DISPLAY " "
               DISPLAY 'NO SE PUDO CERRAR EL ARCHIVO ESPECIFICADO U_U'
               DISPLAY " "
               DISPLAY '***FALLA NO CONTEMPLADA***'
           END-IF.
            STOP RUN.

       END PROGRAM PROG04-09-FL.
