      ******************************************************************
      * Author: VICTOR MONTEIRO ARNONI
      * Date: 12/09/2021
      * Purpose: ESTE PROGRAMA CRIA DOIS ARQUIVOS SEQUENCIAIS E REALIZA
      * A GRAVAÇÃO DOS SEUS RESPECTIVOS REGISTROS. O ARQUIVO "FORNECE"
      * CONTEM INFORMAÇÕES SOBRE FORNECEDORES DE PRODUTOS, O ARQUIVO
      * "ATUALIZ" CONTEM UMA LISTA DE ATUALIZAÇÕES A SEREM FEITAS NOS
      * DADOS DO ARQUIVO "FORNECE".
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRIAFORN.
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
       WORKING-STORAGE SECTION.
       01  ST-FRN           PIC X(02).
       01  ST-ATU           PIC X(02).
       PROCEDURE DIVISION.
       INICIO.
           PERFORM ABRE-ARQ.
           PERFORM PROCESSO.
           PERFORM FINALIZA.
           STOP RUN.
       ABRE-ARQ.
           OPEN OUTPUT ARQFORN.
           IF ST-FRN NOT EQUAL '00'
               DISPLAY "ERRO DE ABERTURA DO ARQFORN" ST-FRN
               STOP RUN.
           CLOSE ARQFORN.
           OPEN EXTEND ARQFORN.
      *
           OPEN OUTPUT ARQMOV.
           IF ST-ATU NOT EQUAL '00'
               DISPLAY "ERRO DE ABERTURA DO ARQMOV" ST-ATU
               STOP RUN.
           CLOSE ARQMOV.
           OPEN EXTEND ARQMOV.
       PROCESSO.
           MOVE 001 TO CD-FORNF.
           MOVE "ISM            " TO NM-FORNF.
           MOVE "SAO PAULO     " TO NM-CIDADEF.
           WRITE REG-FORN.
      *
           MOVE 013 TO CD-FORNF.
           MOVE "DECATRON       " TO NM-FORNF.
           MOVE "RIO DE JANEIRO" TO NM-CIDADEF.
           WRITE REG-FORN.
      *
           MOVE 026 TO CD-FORNF.
           MOVE "SES SYSTEMS    " TO NM-FORNF.
           MOVE "SANTOS        " TO NM-CIDADEF.
           WRITE REG-FORN.
      *
           MOVE 048 TO CD-FORNF.
           MOVE "ENTERDATA      " TO NM-FORNF.
           MOVE "SANTOS        " TO NM-CIDADEF.
           WRITE REG-FORN.
      *
           MOVE 191 TO CD-FORNF.
           MOVE "DIGITAL        " TO NM-FORNF.
           MOVE "RIO DE JANEIRO" TO NM-CIDADEF.
           WRITE REG-FORN.
      *
           MOVE 234 TO CD-FORNF.
           MOVE "NETDB          " TO NM-FORNF.
           MOVE "SANTOS        " TO NM-CIDADEF.
           WRITE REG-FORN.
      *
           MOVE 420 TO CD-FORNF.
           MOVE "CENTERSOFT     " TO NM-FORNF.
           MOVE "SANTOS        " TO NM-CIDADEF.
           WRITE REG-FORN.
      *
           MOVE 518 TO CD-FORNF.
           MOVE "TRTEC          " TO NM-FORNF.
           MOVE "CAMPINAS      " TO NM-CIDADEF.
           WRITE REG-FORN.
      *
           MOVE 001 TO CD-FORNM.
           MOVE "ISM            " TO NM-FORNM.
           MOVE "SAO PAULO     "  TO NM-CIDADEM.
           MOVE 'E' TO SG-MOVIM.
           WRITE REG-MOV.
      *
           MOVE 006 TO CD-FORNM.
           MOVE "M.A INFORMATICA" TO NM-FORNM.
           MOVE "RIO DE JANEIRO"  TO NM-CIDADEM.
           MOVE 'I' TO SG-MOVIM.
           WRITE REG-MOV.
      *
           MOVE 013 TO CD-FORNM.
           MOVE "DECATRON       " TO NM-FORNM.
           MOVE "SAO PAULO     "  TO NM-CIDADEM.
           MOVE 'A' TO SG-MOVIM.
           WRITE REG-MOV.
      *
           MOVE 026 TO CD-FORNM.
           MOVE "SES SYSTEMS    " TO NM-FORNM.
           MOVE "SANTOS        "  TO NM-CIDADEM.
           MOVE 'I' TO SG-MOVIM.
           WRITE REG-MOV.
      *
           MOVE 048 TO CD-FORNM.
           MOVE "ENTERDATA      " TO NM-FORNM.
           MOVE "RIO DE JANEIRO"  TO NM-CIDADEM.
           MOVE 'A' TO SG-MOVIM.
           WRITE REG-MOV.
      *
           MOVE 132 TO CD-FORNM.
           MOVE "SISGRAPH       " TO NM-FORNM.
           MOVE "RIO DE JANEIRO"  TO NM-CIDADEM.
           MOVE 'I' TO SG-MOVIM.
           WRITE REG-MOV.
       FINALIZA.
           CLOSE ARQFORN.
           CLOSE ARQMOV.
