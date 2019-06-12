      ******************************************************************
      * Author: FERNANDEZ LUCAS IVAN
      * Date: 11/06/2019
      * Purpose: EJERCICIO 7 SERIE 6
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG07-09-FL.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA1
           ASSIGN TO DISK'D:\EjerciciosCobol\Serie6\SERVICIO.DAT'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS WSS-FS-ENTRADA.

           SELECT ENTRADA2
           ASSIGN TO DISK'D:\EjerciciosCobol\Serie6\CUENTAS.DAT'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS WSS-FS-ENTRADA.

           SELECT SALIDA
           ASSIGN TO DISK'D:\EjerciciosCobol\Serie6\SALDOS.DAT'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS WSS-FS-SALIDA.


       DATA DIVISION.

       FILE SECTION.
       FD ENTRADA1.
       01 REG-ENTRADA-SERVICIO.
           05 FSE-CODIGOSERVICIO       PIC X(03).
           05 FSE-NROCUENTA-SERVICIO   PIC 9(08).
           05 FSE-DESCRIPCION          PIC X(30).
           05 FSE-PERIODO.
               10 FSE-PER-AÑO          PIC X(04).
               10 FSE-PER-MES          PIC X(02).
           05 FSE-MONTOFACTURA         PIC 9(05)V9(02).

       FD ENTRADA2.
       01 REG-ENTRADA-CUENTA.
           05 FSE-NROCUENTA-CUENTA     PIC X(08).
           05 FSE-CODIGOCLTE           PIC 9(08).
           05 FSE-MONTOCUENTA          PIC 9(15)V9(02).

       FD SALIDA.
       01 REG-SALIDA.
           05 FSS-CLIENTE              PIC 9(08).
           05 FSS-MONTOCUENTA          PIC 9(15)V9(02).

       WORKING-STORAGE SECTION.

       01 VARIABLES.
           05 WSV-AUXNROCUENTA         PIC 9(08).

       01 INDICES.
           05 WSI-I                    PIC 9           VALUE 0.
           05 WSI-J                    PIC 9           VALUE 0.

       01 SWITCHES-ENTRADA.
           05 WSS-FS-ENTRADA           PIC X(02).
             88 WSS-FS-ENTRADA-OK                      VALUE '00'.
             88 WSS-FS-ENTRADA-EOF                     VALUE '10'.

       01 SWITCHES-SALIDA.
           05 WSS-FS-SALIDA            PIC X(02).
             88 WSS-FS-SALIDA-OK                      VALUE '00'.
             88 WSS-FS-SALIDA-EOF                     VALUE '10'.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
       000000-INICIO.

           PERFORM 100000-ABRIR_ENTRADA.
           PERFORM 200000-ABRIR_SALIDA.
           PERFORM 300000-PROCESO.
           PERFORM 400000-CERRAR_ENTRADA.
           PERFORM 500000-CERRAR_SALIDA.


       100000-ABRIR_ENTRADA.
           OPEN INPUT ENTRADA1
           IF NOT WSS-FS-ENTRADA-OK
             DISPLAY 'ERROR EN ARCHIVO DE ENTRADA!!'
             DISPLAY " "
             IF WSS-FS-ENTRADA = 35
               DISPLAY 'NO SE PUDO ABRIR EL ARCHIVO ESPECIFICADO X.X'
               DISPLAY " "
               DISPLAY 'EL ARCHIVO NO EXISTE O NO SE ENCUENTRA :S'
             END-IF
           END-IF.

           OPEN INPUT ENTRADA2
           IF NOT WSS-FS-ENTRADA-OK
             DISPLAY 'ERROR EN ARCHIVO DE ENTRADA!!'
             DISPLAY " "
             IF WSS-FS-ENTRADA = 35
               DISPLAY 'NO SE PUDO ABRIR EL ARCHIVO ESPECIFICADO X.X'
               DISPLAY " "
               DISPLAY 'EL ARCHIVO NO EXISTE O NO SE ENCUENTRA :S'
             END-IF
           END-IF.



       200000-ABRIR_SALIDA.
           OPEN OUTPUT SALIDA
           IF NOT WSS-FS-SALIDA-OK
             DISPLAY 'ERROR EN ARCHIVO DE SALIDA!!'
             DISPLAY " "
             IF WSS-FS-SALIDA = 35
               DISPLAY 'NO SE PUDO GENERAR EL ARCHIVO ESPECIFICADO X.X'
               DISPLAY " "
               DISPLAY 'FALLA EN LA CREACION O MODIFICACION! :S'
             END-IF
           END-IF.



       300000-PROCESO.
           PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL WSS-FS-ENTRADA-EOF
               READ ENTRADA1
               IF WSS-FS-ENTRADA-OK
                   PERFORM VARYING WSI-J FROM 1 BY 1 UNTIL
                                                     WSS-FS-ENTRADA-EOF
                   READ ENTRADA2
                   IF WSS-FS-ENTRADA-OK
                       IF FSE-NROCUENTA-SERVICIO = FSE-NROCUENTA-CUENTA
                           COMPUTE FSS-MONTOCUENTA = FSE-MONTOCUENTA -
                                                     FSE-MONTOFACTURA
                           MOVE FSE-NROCUENTA-CUENTA TO FSS-CLIENTE
                           WRITE REG-SALIDA
                           EXIT PERFORM
                       END-IF
                   END-IF

                   END-PERFORM
               END-IF

           END-PERFORM.

           IF WSS-FS-ENTRADA-EOF AND WSI-I = 1
               DISPLAY "EL ARCHIVO ESTA VACIO."
           END-IF.



       400000-CERRAR_ENTRADA.

       CLOSE ENTRADA1
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

       CLOSE ENTRADA2
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




       500000-CERRAR_SALIDA.

       CLOSE SALIDA
           IF NOT WSS-FS-ENTRADA-OK
              DISPLAY " "
              DISPLAY " "
              DISPLAY 'ERROR EN ARCHIVO DE SALIDA!!'
              IF WSS-FS-ENTRADA = 42
               DISPLAY " "
               DISPLAY 'NO SE PUDO CERRAR EL ARCHIVO ESPECIFICADO U_U'
               DISPLAY " "
               DISPLAY '***FALLA NO CONTEMPLADA***'
           END-IF.

            STOP RUN.

       END PROGRAM PROG07-09-FL.
