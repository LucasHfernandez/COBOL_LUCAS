      ******************************************************************
      * Author: FERNANDEZ LUCAS IVAN
      * Date: 05/07/2019
      * Purpose: EJERCICIO 2 EDD.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO2-EDD.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

           01 CONSTANTES.
               05 WSC-MSGAPTO          PIC X(04)     VALUE "APTO".
               05 WSC-MSGNO_APTO       PIC X(07)     VALUE "NO APTO".

           01 VARIABLES                OCCURS 5 TIMES.
               05 WSV-AUXALTURA        PIC ZZ,ZZ.
               05 WSV-AUXPESO          PIC ZZZ,ZZ.
               05 WSV-NOMBRE           PIC X(30).
               05 WSV-EDAD             PIC 9(02)     VALUE 0.
               05 WSV-SEXO             PIC X.

           01 INDICE.
               05 WSI-I                PIC 9(02).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

      ******************************************************************
      * FORZADA DE DATOS.
      ******************************************************************

           MOVE "M" TO WSV-SEXO(1)
           MOVE "RAUL" TO WSV-NOMBRE(1)
           MOVE 28 TO WSV-EDAD(1)
           MOVE 1,75 TO WSV-AUXALTURA(1)
           MOVE 75 TO WSV-AUXPESO(1)
           MOVE "F" TO WSV-SEXO(2)
           MOVE "CARLA" TO WSV-NOMBRE(2)
           MOVE 19 TO WSV-EDAD(2)
           MOVE 1,58 TO WSV-AUXALTURA(2)
           MOVE 62 TO WSV-AUXPESO(2)
           MOVE "M" TO WSV-SEXO(3)
           MOVE "PABLO" TO WSV-NOMBRE(3)
           MOVE 42 TO WSV-EDAD(3)
           MOVE 1,60 TO WSV-AUXALTURA(3)
           MOVE 94 TO WSV-AUXPESO(3)
           MOVE "F" TO WSV-SEXO(4)
           MOVE "MIRIAM" TO WSV-NOMBRE(4)
           MOVE 22 TO WSV-EDAD(4)
           MOVE 1,69 TO WSV-AUXALTURA(4)
           MOVE 55 TO WSV-AUXPESO(4)
      ******************************************************************
      ******************************************************************

           PERFORM VARYING WSI-I FROM 5 BY 1 UNTIL WSI-I > 5

               DISPLAY "INGRESE NOMBRE:"
               ACCEPT WSV-NOMBRE(WSI-I)
               DISPLAY "INGRESE EDAD: "
               ACCEPT WSV-EDAD(WSI-I)
               DISPLAY "INGRESE SEXO (M / F)"
               ACCEPT WSV-SEXO(WSI-I)
               DISPLAY "INGRESE ALTURA: "
               ACCEPT WSV-AUXALTURA(WSI-I)
               DISPLAY "INGRSE PESO: "
               ACCEPT WSV-AUXPESO(WSI-I)

           END-PERFORM.

           PERFORM VARYING WSI-I FROM 1 BY 1 UNTIL WSI-I > 5

               DISPLAY "POSTULANTE " WSI-I
               DISPLAY " "
               DISPLAY "SEXO: " WSV-SEXO(WSI-I)
               DISPLAY "NOMBRE: " WSV-NOMBRE(WSI-I)
               DISPLAY "EDAD: " WSV-EDAD(WSI-I)
               DISPLAY "ALTURA: " WSV-AUXALTURA(WSI-I)
               DISPLAY "PESO: " WSV-AUXPESO(WSI-I)

               IF WSV-SEXO(WSI-I) = "M"
                   IF WSV-AUXALTURA(WSI-I) >= 1,70 AND
                                                WSV-AUXPESO(WSI-I) <= 90
                       DISPLAY "CALIFICACION: "WSC-MSGAPTO
                   ELSE
                       DISPLAY "CALIFICACION: "WSC-MSGNO_APTO
                   END-IF
               ELSE
               IF WSV-SEXO(WSI-I) = "F"
                   IF WSV-AUXALTURA(WSI-I) >= 1,60 AND
                                                WSV-AUXPESO(WSI-I) <= 60
                       DISPLAY "CALIFICACION: "WSC-MSGAPTO
                   ELSE
                       DISPLAY "CALIFICACION: "WSC-MSGNO_APTO
                   END-IF
               END-IF


           END-PERFORM.


            STOP RUN.

       END PROGRAM EJERCICIO2-EDD.
