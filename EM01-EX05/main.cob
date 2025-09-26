       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EX05.
       AUTHOR.       ERIKA ROCHA.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 22-08-2025.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.      LE OS REGISTROS DO ARQUIVO DE ENTRADA
      *              CADALU, CALCULA A MEDIA E GRAVA NO ARQUIVO 
      *              DE SAÍDA CADAPR SOMENTE OS REGISTROS
      *              COM MEDIA MAIOR OU IGUAL A 7 E NUMERO
      *              DE FALTAS MENOR OU IGUAL A 18.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU   ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADAPR   ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD CADALU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADALU.DAT".
      
       01 REG-ENT.
           02 NUM-ENT   PIC 9(05).
           02 NOME-ENT  PIC X(20).
           02 NOTA01    PIC 9(02).
           02 NOTA02    PIC 9(02).
           02 FALTA-ENT PIC 9(02).

       FD CADAPR
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADAPR.DAT".
           
       01 REG-SAI.
           02 NUM-SAI   PIC 9(05).
           02 NOME-SAI  PIC X(20).
           02 MEDIA     PIC 9(02).
           02 FALTA-SAI PIC 9(02).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ    PIC X(03) VALUE "NAO".
       77 SOMA       PIC 9(04) VALUE 0.
       77 MEDIA-CALC PIC 9(02) VALUE 0.

       PROCEDURE DIVISION.
       
       EXEMPLO.
           
           PERFORM INICIO.
           
           PERFORM PRINCIPAL 
               UNTIL FIM-ARQ EQUAL "SIM".
           
       PERFORM TERMINO.
       
       STOP RUN.
       
       INICIO. 

           OPEN INPUT CADALU OUTPUT CADAPR.
           PERFORM LEITURA.
           
       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.
           
       GRAVACAO.
           MOVE  NUM-ENT  TO NUM-SAI
           PERFORM CALCULAMEDIA
           IF MEDIA-CALC NOT < 7 AND FALTA-ENT NOT > 18 THEN
               MOVE NUM-ENT    TO NUM-SAI
               MOVE NOME-ENT   TO NOME-SAI
               MOVE MEDIA-CALC TO MEDIA
               MOVE FALTA-ENT  TO FALTA-SAI
           WRITE REG-SAI.
           
       CALCULAMEDIA.
           ADD NOTA01, NOTA02 GIVING SOMA
           DIVIDE SOMA BY 2 GIVING MEDIA-CALC.
           
       LEITURA.
           READ CADALU AT END 
               MOVE "SIM" TO FIM-ARQ.
                
       TERMINO. 
           CLOSE CADALU CADAPR.

         