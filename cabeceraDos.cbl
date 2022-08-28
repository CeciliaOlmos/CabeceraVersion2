      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CECILIA_OLMOS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCH-FILIALES ASSIGN TO "../transacciones.txt"
               ORGANIZATION is line sequential.

           SELECT arch-sort ASSIGN to "sortwork".
           SELECT Trans-Act ASSIGN TO "..\TRANS-ACT.txt".
           SELECT LISTADO
           ASSIGN TO PRINTER,
           "..\impTRANSACC.dat".

       DATA DIVISION.
       FILE SECTION.
       FD  ARCH-FILIALES.
       01  tr-cab1-reg.
           03 tr1-cab1-tipo1 pic x.
           03 tr1-cab1-filial1 pic 9.
       01  tr-cab2-reg1.
           03 tr-cab2-tipo1 pic x.
           03 tr-cab2-fecha1 pic 9(8).
       01  tr-det-reg1.
           03 tr-det-tipo1 pic x.
           03 tr-det-socio1 pic 9(4).
           03 tr-det-importe1 pic s9(7)v99.

       SD  arch-sort.
       01  srt-reg.
           03 srt-cod-soc pic 9(4).
           03 srt-importe pic S9(7)V99.

       FD  Trans-Act.
       01  tra-reg.
           03 tra-socio pic 9(4).
           03 tra-importe pic S9(8)V99.

       WORKING-STORAGE SECTION.
       01 cabecera0.
           03 filler       pic x(25).
           03 filler       pic x(24) value "LISTADO DE TRANSACCIONES".
           03 filler       pic x(12)  value spaces.
       01  cabecera1.
           03  lin-titulo-soc.
               05 filler pic x(25) value spaces.
               05 FILLER pic x(5) value "SOCIO".
               05 filler  pic x(10) value space.
               05 FILLER pic x(15) value "IMPORTE".
       01  guarda.
           03 filler pic x(80) value all "*".

       01  guarda2.
           03 filler pic x(80) value all ".".

       01  detalle1.
           03  lin-det-soc.
               05 filler pic x(25) value spaces.
               05 l-cod-soc pic x(5).
               05 filler pic x(8) value spaces.
               05 l-importe pic z(8),zz value spaces.

       01  w-flag-fil pic 9.
       01  w-flag-sort pic 9.
       01  i pic 9.
       01  w-fecha pic 9(8).
       01  w-flag-act pic 9.
       01  w-srt-soc-ant pic 9(4).
       01  w-srt-acum-imp pic S9(8)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            SORT arch-sort ASCENDING srt-cod-soc
            INPUT PROCEDURE is DATOS-ENTRADA
            OUTPUT PROCEDURE IS DATOS-SALIDA.

            STOP RUN.

       DATOS-ENTRADA.
           PERFORM 100-INICIO.
           PERFORM 200-LEER-FILIALES.
           PERFORM 300-PEDIR-FECHA.

           PERFORM VARYING tr1-cab1-filial1 from 1 by 1
           UNTIL tr1-cab1-filial1> 4
               PERFORM UNTIL w-flag-fil =1 OR tr-cab2-fecha1=w-fecha
                       PERFORM 200-LEER-FILIALES
               END-PERFORM
               PERFORM 500-CARGAR-SORT
           END-PERFORM.
           PERFORM 700-CERRAR-ARCHIVOS.

       100-INICIO.
           PERFORM 120-ABRIR-ARCHIVOS.

       120-ABRIR-ARCHIVOS.
           OPEN INPUT ARCH-FILIALES.

       200-LEER-FILIALES.
           READ ARCH-FILIALES AT END MOVE 1 TO w-flag-fil.

       300-PEDIR-FECHA.
           DISPLAY "Ingrese una fecha de transaccion AAAAMMDD".
           ACCEPT w-fecha.

       400-DESAGOTAR-FECHA.
           IF tr1-cab1-tipo1="C"
               PERFORM 200-LEER-FILIALES.

       450-DESAGOTAR-DETALLE.
           IF tr-cab2-fecha1="F" or tr-cab2-fecha1=w-fecha
               PERFORM 200-LEER-FILIALES.

       500-CARGAR-SORT.
           PERFORM 450-DESAGOTAR-DETALLE.
           PERFORM UNTIL w-flag-fil=1 or tr1-cab1-tipo1="C"
           OR tr-cab2-tipo1="F" OR tr-cab2-fecha1=w-fecha
                PERFORM 550-GRABAR-SORT
                PERFORM 200-LEER-FILIALES
           END-PERFORM.

       550-GRABAR-SORT.
            MOVE tr-det-socio1 TO srt-cod-soc.
            MOVE tr-det-importe1 TO srt-importe.
            RELEASE srt-reg.

       700-CERRAR-ARCHIVOS.
           CLOSE ARCH-FILIALES.

       DATOS-SALIDA.
           PERFORM 800-INICIO-ACT.
           PERFORM 900-LEER-SORT.
           PERFORM UNTIL w-flag-sort =1
           PERFORM 950-INICIO-ACT
               PERFORM UNTIL w-flag-sort=1
               OR w-srt-soc-ant IS NOT = srt-cod-soc
                  PERFORM 1000-PROCESO-ACT
                   PERFORM 900-LEER-SORT
               END-PERFORM
           PERFORM 1100-FIN-ACT
           END-PERFORM.
           PERFORM 1300-CERRAR-ACT.

       800-INICIO-ACT.
           PERFORM 830-ABRIR-ARCH-ACT.
           PERFORM 850-MOSTRAR-CABECERA.

       830-ABRIR-ARCH-ACT.
           OPEN OUTPUT Trans-Act.

       850-MOSTRAR-CABECERA.
           DISPLAY guarda.
           DISPLAY cabecera0.
           DISPLAY guarda.
           DISPLAY cabecera1.
           DISPLAY guarda2.

       900-LEER-SORT.
           RETURN arch-sort at end move 1 to  w-flag-sort.

       950-INICIO-ACT.
           MOVE srt-cod-soc TO w-srt-soc-ant.
           MOVE ZERO TO w-srt-acum-imp.

       1000-PROCESO-ACT.
           ADD srt-importe TO w-srt-acum-imp.

       1100-FIN-ACT.
           PERFORM 1150-MOVER-A-ACT.
           PERFORM 1170-GRABAR-ACT.
           PERFORM 1180-MOSTRAR-SOCIO.

       1150-MOVER-A-ACT.
           MOVE w-srt-soc-ant TO tra-socio.
           MOVE w-srt-acum-imp TO tra-importe.

       1170-GRABAR-ACT.
           WRITE tra-reg.

       1180-MOSTRAR-SOCIO.
           MOVE tra-socio to l-cod-soc.
           MOVE tra-importe to l-importe.
           DISPLAY lin-det-soc.

       1300-CERRAR-ACT.
           DISPLAY guarda2.
           CLOSE Trans-Act.
       END PROGRAM CECILIA-OLMOS.
