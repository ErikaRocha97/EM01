       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EX03.
       AUTHOR.       ERIKA ROCHA.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 21-08-2025.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.      LE OS REGISTROS DO ARQUIVO DE ENTRADA
      *              CADALU (NUMERO, NOME, NOTA01, NOTA02,
      *              NOTA03), CALCULA A MEDIA E GRAVA NO 
      *              ARQUIVO DE SAIDA CADATU.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU  ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADATU  ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD CADALU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADALU.DAT".
      
       01 REG-ENT.
           02 NUM-ENT   PIC 9(05).
           02 NOME-ENT  PIC X(20).
           02 NOTA01    PIC 9(04).
           02 NOTA02    PIC 9(04).
           02 NOTA03    PIC 9(04).
       
       FD CADATU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".
           
       01 REG-SAI.
           02 NUM-SAI   PIC 9(05).
           02 MEDIA     PIC 9(04).

       WORKING-STORAGE SECTION.
       
       77 FIM-ARQ       PIC X(03) VALUE "NAO".
       77 SOMA          PIC 9(04) VALUE 0.
       77 MEDIA-CALC    PIC 9(04) VALUE 0.

       PROCEDURE DIVISION.
       
       EXEMPLO.
           
           PERFORM INICIO.
           
           PERFORM PRINCIPAL 
                UNTIL FIM-ARQ EQUAL "SIM".
           
       PERFORM TERMINO.
       
       STOP RUN.
       
       INICIO. 
           OPEN INPUT CADALU OUTPUT CADATU.
           PERFORM LEITURA.
           
       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.
           
       GRAVACAO.
           MOVE  NUM-ENT  TO NUM-SAI
           PERFORM CALCULAMEDIA
           MOVE MEDIA-CALC TO MEDIA
           WRITE REG-SAI.
           
       CALCULAMEDIA.
           ADD NOTA01, NOTA02, NOTA03 GIVING SOMA
           DIVIDE SOMA BY 3 GIVING MEDIA-CALC.
           
       LEITURA.
           READ CADALU AT END 
                MOVE "SIM" TO FIM-ARQ.
                
       TERMINO. 
           CLOSE CADALU CADATU.
         