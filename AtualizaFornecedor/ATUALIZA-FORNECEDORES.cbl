      ******************************************************************
      * Author: VICTOR MONTEIRO ARNONI
      * Date: 12/09/2021
      * Purpose: ESTE PROGRAMA REALIZA A LEITURA DOS ARQUIVOS SEQUEN-
      * CIAIS "FORNECE" E "ATUALIZ". A PARTIR DA LEITURA, É CRIADO
      * UM TERCEIRO ARQUIVO CHAMADO "FORNNEW" QUE CONTÉM OS DADOS ATUA-
      * LIZADOS DOS FORNECEDORES, DE ACORDO COM AS INSTRUÇÕES DO ARQUIVO
      * "ATUALIZ".
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATUALIZAFORN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQFORN ASSIGN TO "FORNECE.DAT"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ST-FRN.
           SELECT ARQMOV ASSIGN TO "ATUALIZ.DAT"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ST-ATU.
           SELECT ARQNEW ASSIGN TO "FORNNEW.DAT"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ST-NEW.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQFORN.
       01  REG-FORN.
           03 CD-FORNF      PIC 9(03).
           03 NM-FORNF      PIC X(15).
           03 NM-CIDADEF    PIC X(14).
       FD  ARQMOV.
       01  REG-MOV.
           03 CD-FORNM      PIC 9(03).
           03 NM-FORNM      PIC X(15).
           03 NM-CIDADEM    PIC X(14).
           03 SG-MOVIM      PIC X(01).
       FD  ARQNEW.
       01  REG-NEW.
           03 CD-FORNN      PIC 9(03).
           03 NM-FORNN      PIC X(15).
           03 NM-CIDADEN    PIC X(14).
       WORKING-STORAGE SECTION.
       01  ST-FRN           PIC X(02).
       01  ST-ATU           PIC X(02).
       01  ST-NEW           PIC X(02).
       01  WS-FIM-FORN      PIC X(01) VALUE 'N'.
       01  WS-FIM-MOV       PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       INICIO.
           PERFORM ABRE-ARQ.
           PERFORM LER-REG-FORN.
           PERFORM LER-REG-MOV.
           PERFORM PROCESSO UNTIL WS-FIM-FORN ='S' AND WS-FIM-MOV = 'S'.
           PERFORM FINALIZA.
           STOP RUN.
       ABRE-ARQ.
           OPEN INPUT ARQFORN.
           IF ST-FRN NOT EQUAL '00'
               DISPLAY "ERRO ABERTURA ARQFORN" ST-FRN
               STOP RUN.
      *
           OPEN INPUT ARQMOV.
           IF ST-ATU NOT EQUAL '00'
               DISPLAY "ERRO ABERTURA ARQMOV" ST-ATU
               STOP RUN.
      *
           OPEN OUTPUT ARQNEW
           IF ST-NEW NOT EQUAL '00'
               DISPLAY "ERRO ABERTURA ARQNEW" ST-NEW
               STOP RUN.
           CLOSE ARQNEW.
           OPEN EXTEND ARQNEW.
       PROCESSO.
           IF CD-FORNF < CD-FORNM
               PERFORM GRAVA-REG-NEW
               PERFORM LER-REG-FORN
           ELSE
               IF SG-MOVIM = 'I'
                   IF CD-FORNF = CD-FORNM
                       DISPLAY "ERRO DE INCLUDE, CODIGOS IGUAIS"
                       PERFORM LER-REG-FORN
                       PERFORM LER-REG-MOV
                   ELSE
                       PERFORM GRAVA-REG-NEW
                       PERFORM LER-REG-MOV
                   END-IF
               ELSE
                   IF SG-MOVIM = 'A'
                       IF CD-FORNF = CD-FORNM
                           PERFORM GRAVA-REG-NEW
                           PERFORM LER-REG-FORN
                           PERFORM LER-REG-MOV
                       ELSE
                           DISPLAY "ERRO DE ALTERA, CODIGOS DIFERENTES"
                           PERFORM LER-REG-MOV
                       END-IF
                   ELSE
                       IF CD-FORNF = CD-FORNM
                           PERFORM LER-REG-FORN
                           PERFORM LER-REG-MOV
                       ELSE
                           DISPLAY "ERRO DE EXCLUI, CODIGOS DIFERENTES"
                           PERFORM LER-REG-MOV
                       END-IF
                   END-IF
               END-IF
           END-IF.
       LER-REG-FORN.
           READ ARQFORN AT END MOVE 'S' TO WS-FIM-FORN.
       LER-REG-MOV.
           READ ARQMOV
               AT END
                   MOVE 'S' TO WS-FIM-MOV
                   MOVE 999 TO CD-FORNM
           END-READ.
       GRAVA-REG-NEW.
           IF CD-FORNF < CD-FORNM
               MOVE CD-FORNF TO CD-FORNN
               MOVE NM-FORNF TO NM-FORNN
               MOVE NM-CIDADEF TO NM-CIDADEN
               WRITE REG-NEW
           ELSE
               MOVE CD-FORNM TO CD-FORNN
               MOVE NM-FORNM TO NM-FORNN
               MOVE NM-CIDADEM TO NM-CIDADEN
               WRITE REG-NEW.
       FINALIZA.
           CLOSE ARQFORN.
           CLOSE ARQMOV.
           CLOSE ARQNEW.
