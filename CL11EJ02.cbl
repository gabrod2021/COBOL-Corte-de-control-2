      ******************************************************************
      * Author: GABRIELA RODRIGUEZ
      * Date: 07/09/2023
      * Purpose: CORTE DE CONTROL ANIDADO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL11EJ02.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ENTRADA
           ASSIGN TO '../PARCIAL2023.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ENTRADA.
      *-----------------------------------------------------------------*
       SELECT SALIDA
           ASSIGN TO '../TOTALES2023.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SALIDA.
      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENTRADA.
       01 ENT-ARCHIVO.
          05 ENT-FECHA                      PIC X(10).
          05 ENT-ID-EMPLEADO                PIC 9(05).
          05 ENT-NOMBRE-APELLIDO            PIC X(40).
          05 ENT-CATEGORIA                  PIC X(20).
          05 ENT-IMPORTE                    PIC 9(8)V9(2).

       FD SALIDA.
       01 SAL-ARCHIVO.
          05 SAL-linea                      PIC X(42).




       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-ENTRADA                      PIC X(2).
             88 FS-ENTRADA-OK                    VALUE '00'.
             88 FS-ENTRADA-EOF                   VALUE '10'.
             88 FS-ENTRADA-NFD                   VALUE '35'.

          05 FS-SALIDA                      PIC X(2).
             88 FS-SALIDA-OK                    VALUE '00'.
             88 FS-SALIDA-EOF                   VALUE '10'.
             88 FS-SALIDA-NFD                   VALUE '35'.

       01 WS-CONTADORES.
          05 WS-CONT-REG-ENTRADA             PIC 9(5) VALUE 0.
          05 WS-SALIDA-CANT-REG              PIC 9(05) VALUE 0.

       01 WS-CORTE-CONTROL.
          05 WS-CC-FECHA-ANT                PIC X(10).
          05 WS-CC-CATEGORIA-ANT            PIC X(20).

       01 WS-ACUMULADORES.
          05 WS-CC-IMPORTE-ACUM             PIC 9(8)V9(2).
          05 WS-CC-CANT-VENTAS-ACUM         PIC 9(05).
          05 WS-CC-IMPORTE-ACUM-DIA         PIC 9(8)V9(2).
          05 WS-CC-CANT-VENT-ACUM-DIA       PIC 9(05).
          05 WS-TOTAL-IMPORTE               PIC 9(9)V9(2).
          05 WS-TOTAL-VENTAS                PIC 9(05).

       01 WS-LISTADO.
          05 WS-LIS-SEPARADOR-1             PIC X(41) VALUE ALL '-'.
          05 WS-LIS-SEPARADOR-2             PIC X(41) VALUE ALL '-'.
          05 WS-LIS-SEPARADOR-3             PIC X(41) VALUE ALL '#'.
          05 WS-LIS-SEPARADOR-4             PIC X(41) VALUE ALL '#'.

          05 WS-LIS-HEADER.
             10 FILLER                      PIC X(12) VALUE 'CATEGORIA'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(08) VALUE 'CANTIDAD'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(15) VALUE 'IMPORTE'.
          05 WS-LIS-DETALLE.
             10 WS-LIS-D-CATEGORIA          PIC X(12).
             10 FILLER                      PIC X(07) VALUE ' |     '.
             10 WS-LIS-D-CANTIDAD           PIC ZZZ9.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-LIS-D-IMPORTE            PIC ZZ.ZZZ.ZZ9,99.
          05 WS-LIS-TOTALES.
             10 WS-LIS-D-TOTAL              PIC X(01).
             10 FILLER                      PIC X(11) VALUE 'TOTAL'.
             10 FILLER                      PIC X(08) VALUE ' |'.
             10 WS-LIS-D-CANT-TOT           PIC ZZZ9.
             10 FILLER                      PIC X(03) VALUE '| '.
             10 WS-LIS-D-IMP-TOT            PIC ZZ.ZZZ.ZZ9,99.
             10 FILLER                      PIC X(05) VALUE '| '.
          05 WS-LIS-TOTAL-GENERAL.
             10 WS-LIS-D-TOTALTOTAL         PIC X(01).
             10 FILLER               PIC X(11) VALUE 'TOTAL GRAL'.
             10 FILLER                      PIC X(07) VALUE ' | '.
             10 WS-LIS-D-CANT-TOTTOT        PIC ZZZ99.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-LIS-D-IMP-TOTTOT         PIC ZZZ.ZZZ.ZZ9,99.
             10 FILLER                      PIC X(05) VALUE '| '.



      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-ENTRADA-OK

              PERFORM 2000-PROCESAR-PROGRAMA
                 THRU 2000-PROCESAR-PROGRAMA-FIN
                UNTIL FS-ENTRADA-EOF

           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

            STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-ARCHIVO
              THRU 1100-ABRIR-ARCHIVO-FIN.

           PERFORM 1300-ABRIR-ARCHIVO-SALIDA
              THRU 1300-ABRIR-ARCHIVO-SALIDA-EXIT.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVO.

           OPEN INPUT ENTRADA.

           EVALUATE TRUE
               WHEN FS-ENTRADA-OK
                    PERFORM 1500-LEER-ARCHIVO
                       THRU 1500-LEER-ARCHIVO-EXIT
               WHEN FS-ENTRADA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1100-ABRIR-ARCHIVO-FIN.
           EXIT.
      *----------------------------------------------------------------*
      *    PROCESO PARA ABRIR ARCHIVO DE SALIDA                        *
      *----------------------------------------------------------------*
       1300-ABRIR-ARCHIVO-SALIDA.

      *    Abro archivo  de salida  : SALIDA
           OPEN OUTPUT SALIDA.

           EVALUATE TRUE
               WHEN FS-SALIDA-OK
                    CONTINUE
               WHEN FS-SALIDA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
           END-EVALUATE.

       1300-ABRIR-ARCHIVO-SALIDA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA LEER ARCHIVO                        *
      *----------------------------------------------------------------*

       1500-LEER-ARCHIVO.

           READ ENTRADA.

            EVALUATE TRUE
               WHEN FS-ENTRADA-OK
                    ADD 1                   TO WS-CONT-REG-ENTRADA
               WHEN FS-ENTRADA-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1500-LEER-ARCHIVO-EXIT.
            EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.



           MOVE ZEROS                       TO WS-CC-IMPORTE-ACUM.
           MOVE ZEROS                       TO WS-CC-CANT-VENTAS-ACUM.
           MOVE ZEROS                       TO WS-CC-IMPORTE-ACUM-DIA.
           MOVE ZEROS                   TO WS-CC-CANT-VENT-ACUM-DIA.



           MOVE ENT-FECHA                   TO WS-CC-FECHA-ANT.

           DISPLAY 'FECHA: ' WS-CC-FECHA-ANT

           MOVE WS-CC-FECHA-ANT TO SAL-linea.

           PERFORM 2500-GRABAR-ARCHIVO-SAL
           THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.

           DISPLAY WS-LIS-HEADER.

           MOVE WS-LIS-HEADER TO SAL-linea.

           PERFORM 2500-GRABAR-ARCHIVO-SAL
           THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.

           DISPLAY WS-LIS-SEPARADOR-1.

           MOVE WS-LIS-SEPARADOR-1 TO SAL-linea.

           PERFORM 2500-GRABAR-ARCHIVO-SAL
           THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.



            PERFORM 2100-PROCESAR-CORTE-X-DIA
              THRU 2100-PROCESAR-CORTE-X-DIA
             UNTIL FS-ENTRADA-EOF
                OR ENT-FECHA NOT EQUAL WS-CC-FECHA-ANT.




           DISPLAY WS-LIS-SEPARADOR-2.
           MOVE WS-LIS-SEPARADOR-2 TO SAL-linea.

           PERFORM 2500-GRABAR-ARCHIVO-SAL
           THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.

           DISPLAY WS-LIS-TOTALES.
           MOVE WS-LIS-TOTALES TO SAL-linea.



           PERFORM 2500-GRABAR-ARCHIVO-SAL
           THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.

           DISPLAY WS-LIS-SEPARADOR-3.
           MOVE WS-LIS-SEPARADOR-3 TO SAL-linea.

           PERFORM 2500-GRABAR-ARCHIVO-SAL
           THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.



           ADD WS-CC-IMPORTE-ACUM-DIA TO WS-TOTAL-IMPORTE.
           ADD WS-CC-CANT-VENT-ACUM-DIA TO WS-TOTAL-VENTAS.
           MOVE WS-TOTAL-IMPORTE        TO WS-LIS-D-IMP-TOTTOT.
           MOVE WS-TOTAL-VENTAS         TO WS-LIS-D-CANT-TOTTOT.



       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2100-PROCESAR-CORTE-X-DIA.


             MOVE ZEROS                       TO WS-CC-IMPORTE-ACUM.
             MOVE ZEROS                       TO WS-CC-CANT-VENTAS-ACUM.


           MOVE ENT-CATEGORIA               TO WS-CC-CATEGORIA-ANT.


           PERFORM 2200-PROCESAR-CORTE-X-CATEG
              THRU 2200-PROCESAR-CORTE-X-CATEG-FIN
             UNTIL FS-ENTRADA-EOF
                OR ENT-FECHA NOT EQUAL WS-CC-FECHA-ANT
                OR ENT-CATEGORIA NOT EQUAL WS-CC-CATEGORIA-ANT.

           ADD WS-CC-CANT-VENTAS-ACUM TO WS-CC-CANT-VENT-ACUM-DIA.
           ADD WS-CC-IMPORTE-ACUM TO WS-CC-IMPORTE-ACUM-DIA.



           MOVE WS-CC-CATEGORIA-ANT         TO WS-LIS-D-CATEGORIA.
           MOVE WS-CC-CANT-VENTAS-ACUM      TO WS-LIS-D-CANTIDAD.
           MOVE WS-CC-IMPORTE-ACUM          TO WS-LIS-D-IMPORTE.
           MOVE WS-CC-IMPORTE-ACUM-DIA      TO WS-LIS-D-IMP-TOT.
           MOVE WS-CC-CANT-VENT-ACUM-DIA    TO WS-LIS-D-CANT-TOT.

           DISPLAY WS-LIS-DETALLE.
           MOVE WS-LIS-DETALLE TO SAL-linea.

           PERFORM 2500-GRABAR-ARCHIVO-SAL
           THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.



       2100-PROCESAR-CORTE-X-DIA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-PROCESAR-CORTE-X-CATEG.


           ADD ENT-IMPORTE                  TO WS-CC-IMPORTE-ACUM.
           ADD 1                            TO WS-CC-CANT-VENTAS-ACUM.


           MOVE ENT-FECHA                   TO WS-CC-FECHA-ANT.
           MOVE ENT-CATEGORIA               TO WS-CC-CATEGORIA-ANT.




           PERFORM 1500-LEER-ARCHIVO
              THRU 1500-LEER-ARCHIVO-EXIT.


       2200-PROCESAR-CORTE-X-CATEG-FIN.
           EXIT.



      *----------------------------------------------------------------*
      *    PROCESO PARA ESCRIBIR  ARCHIVO DE SALIDA                    *
      *----------------------------------------------------------------*
           2500-GRABAR-ARCHIVO-SAL.

             WRITE SAL-ARCHIVO.

            EVALUATE FS-SALIDA
               WHEN '00'
                     ADD 1 TO WS-SALIDA-CANT-REG
                WHEN OTHER
                      DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE SALIDA'
                      DISPLAY 'FILE STATUS: ' FS-SALIDA
            END-EVALUATE.

       2500-GRABAR-ARCHIVO-SAL-EXIT.
             EXIT.


      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.


           DISPLAY WS-LIS-TOTAL-GENERAL.
           MOVE WS-LIS-TOTAL-GENERAL TO SAL-linea.

           PERFORM 2500-GRABAR-ARCHIVO-SAL
           THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.

           DISPLAY WS-LIS-SEPARADOR-4.

           MOVE WS-LIS-SEPARADOR-4 TO SAL-linea.


           PERFORM 2500-GRABAR-ARCHIVO-SAL
           THRU 2500-GRABAR-ARCHIVO-SAL-EXIT.

           DISPLAY 'CANTIDAD DE REGISTROS LEIDOS: ' WS-CONT-REG-ENTRADA.

           PERFORM 3200-CERRAR-ARCHIVO
              THRU 3200-CERRAR-ARCHIVO-FIN.

           PERFORM 3300-CERRAR-ARCHIVO-SALIDA
              THRU 3300-CERRAR-ARCHIVO-SALIDA-FIN.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVO.

           CLOSE ENTRADA.

           IF NOT FS-ENTRADA-OK
              DISPLAY 'ERROR AL CERRAR ARCHUIVO ENTRADA: ' FS-ENTRADA
           END-IF.

       3200-CERRAR-ARCHIVO-FIN.
           EXIT.
      *----------------------------------------------------------------*

       3300-CERRAR-ARCHIVO-SALIDA.

           CLOSE SALIDA.

           IF NOT FS-SALIDA-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO SALIDA: ' FS-SALIDA
           END-IF.

       3300-CERRAR-ARCHIVO-SALIDA-FIN.
           EXIT.
      *----------------------------------------------------------------*



       END PROGRAM CL11EJ02.
