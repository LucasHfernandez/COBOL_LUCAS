      ******************************************************************
      * Author: FERNADEZ LUCAS IVAN
      * Date: 10/06/2019
      * Purpose: EJERCICIO 1 SERIE 6
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG01-09-FL.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALIDA
           ASSIGN TO DISK'D:\EjerciciosCobol\Serie6\SERVICIO.DAT'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS WSS-FS-SALIDA.

       DATA DIVISION.

       FILE SECTION.
       FD SALIDA.
       01 REG-SALIDA.
           05 FSS-CODIGOSERVICIO       PIC X(03).
           05 FSS-NROCUENTA            PIC 9(08).
           05 FSS-DESCRIPCION          PIC X(30).
           05 FSS-PERIODO.
               10 FSS-PER-AÑO          PIC X(04).
               10 FSS-PER-MES          PIC X(02).
           05 FSS-MONTOFACTURA         PIC 9(05)V9(02).

       WORKING-STORAGE SECTION.

       01 SWITCHES.
           05 WSS-FS-SALIDA              PIC X(02).
             88 WSS-FS-SALIDA-OK                      VALUE '00'.
             88 WSS-FS-SALIDA-EOF                     VALUE '10'.

       01 VARIABLES.
           05 WSV-RESPUESTA              PIC 9(01)    VALUE 0.


       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

       000000-CONTROL.
           PERFORM 100000-ABRIR
           PERFORM 200000-TOMADEDATOS
           PERFORM 300000-CERRAR.


       100000-ABRIR.

           OPEN OUTPUT SALIDA
           IF NOT WSS-FS-SALIDA-OK
             DISPLAY 'ERROR AL GENERAR ARCHIVO DE SALIDA'
             DISPLAY 'FILE STATUS' WSS-FS-SALIDA
             DISPLAY " "
             PERFORM 300000-CERRAR
           END-IF.

       200000-TOMADEDATOS.

           PERFORM UNTIL WSV-RESPUESTA = 2

           DISPLAY "INGRESE CODIGO DEL SERVICIO (3 DIGITOS)"
           DISPLAY "RESPUESTA: "
           ACCEPT FSS-CODIGOSERVICIO

           DISPLAY "INGRESE NUMERO DE CUENTA (8 DIGITOS)"
           DISPLAY "RESPUESTA: "
           ACCEPT FSS-NROCUENTA

           DISPLAY "INGRESE DESCRIPCION DEL SERVICIO (HASTA 30 CARACT.)"
           DISPLAY "RESPUESTA: "
           ACCEPT FSS-DESCRIPCION

           DISPLAY "INGRESE PERIODO DE FACTURACION."
           DISPLAY "INGRESE AÑO: "
           ACCEPT FSS-PER-AÑO
           DISPLAY " "
           DISPLAY "INGRESE MES: "
           ACCEPT FSS-PER-MES

           DISPLAY "INGRESE MONTO DE FACTURA (5 DIGITOS)"
           DISPLAY "RESPUESTA: "
           ACCEPT FSS-MONTOFACTURA

           WRITE REG-SALIDA

           DISPLAY "DESEA INGRESAR MAS DATOS? (SI = 1 NO = 2)"
           DISPLAY "RESPUESTA: "
           ACCEPT WSV-RESPUESTA

           END-PERFORM.

       300000-CERRAR.
           CLOSE SALIDA
           IF NOT WSS-FS-SALIDA-OK
              DISPLAY " "
              DISPLAY 'ERROR AL CERRAR EL ARCHIVO'
              DISPLAY 'FILE STATUS' WSS-FS-SALIDA
           END-IF.
            STOP RUN.

       END PROGRAM PROG01-09-FL.
