      ******************************************************************
      * Author: FERNANDEZ LUCAS IVAN
      * Date: 11/06/2019
      * Purpose: EJERCICIO 3 SERIE 6
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG03-09-FL.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA
           ASSIGN TO DISK'D:\EjerciciosCobol\Serie6\SERVICIO.DAT'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS WSS-FS-ENTRADA.

       DATA DIVISION.

       FILE SECTION.
       FD ENTRADA.
       01 REG-ENTRADA.
           05 FSE-CODIGOSERVICIO       PIC X(03).
           05 FSE-NROCUENTA            PIC 9(08).
           05 FSE-DESCRIPCION          PIC X(30).
           05 FSE-PERIODO.
               10 FSE-PER-AÑO          PIC X(04).
               10 FSE-PER-MES          PIC X(02).
           05 FSE-MONTOFACTURA         PIC 9(05)V9(02).

       WORKING-STORAGE SECTION.

       01 INDICES.
           05 WSI-I                    PIC 9           VALUE 0.

       01 ACUMULADOR.
           05 WSC-CANTREG              PIC 9(03)       VALUE 0.

       01 SWITCHES.
           05 WSS-FS-ENTRADA           PIC X(02).
             88 WSS-FS-ENTRADA-OK                      VALUE '00'.
             88 WSS-FS-ENTRADA-EOF                     VALUE '10'.

       01 CUADRO_SERVICIO_FILA.
           05 FILLER                 PIC X(07)   VALUE '*-----*'.
           05 FILLER                 PIC X(14)   VALUE '-------------*'.
           05 FILLER                 PIC X(48)   VALUE
                     '-----------------------------------------------*'.
           05 FILLER                 PIC X(13)   VALUE '------------*'.
           05 FILLER                 PIC X(13)   VALUE '------------*'.

       01 CUADRO_SERVICIO_TITULO.
           05 TITULO_CODIGO.
               10 FILLER             PIC X(06)   VALUE '|SER. '.
           05 TITULO_CUENTA.
               10 FILLER             PIC X(11)   VALUE '|   CUENTA '.
           05 TITULO_DESCRIPCION.
               10 FILLER             PIC X(51)   VALUE
                  '   |                 DESCRIPCION                   '.
           05 TITULO_PERIODO.
               10 FILLER             PIC X(13)   VALUE
                                                  '|  PERIODO   '.
           05 TITULO_MONTO.
               10 FILLER             PIC X(13)   VALUE '|    MONTO   '.
               10 FILLER             PIC X       VALUE '|'.

       01 CUADRO_SERVICIO_DATOS      OCCURS 100 TIMES.
           05 CODIGO.
               10 WSC-AUXCODIGO      PIC X(03).
           05 CUENTA.
               10 WSC-AUXNROCUENTA   PIC 9(08).
           05 DESCRIPCION.
               10 WSC-AUXDESCRIPCION PIC X(30).
           05 PERIODO.
               10 WSC-AUXAÑO         PIC X(04).
               10 WSC-AUXMES         PIC X(02).
           05 MONTO.
               10 WSC-AUXMONTO       PIC 9(05)V9(02).

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
                   MOVE REG-ENTRADA TO CUADRO_SERVICIO_DATOS(WSI-I)
                   ADD 1 TO WSC-CANTREG

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
               DISPLAY "CUADRO DE SERVICIOS"
               DISPLAY " "

               DISPLAY CUADRO_SERVICIO_FILA
               DISPLAY CUADRO_SERVICIO_TITULO
               DISPLAY CUADRO_SERVICIO_FILA
               PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL
                                                 WSI-I > WSC-CANTREG

                   DISPLAY '| ' WSC-AUXCODIGO(WSI-I)
                           ' |  ' WSC-AUXNROCUENTA(WSI-I)
                           '   |' WSC-AUXDESCRIPCION(WSI-I)
           '                 |  ' WSC-AUXAÑO(WSI-I)'/'WSC-AUXMES(WSI-I)
                           '   |  ' WSC-AUXMONTO(WSI-I) '$'
                           ' |'

               END-PERFORM

               DISPLAY CUADRO_SERVICIO_FILA

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

       END PROGRAM PROG03-09-FL.
