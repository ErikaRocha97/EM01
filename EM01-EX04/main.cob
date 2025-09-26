       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EX04.
       AUTHOR.       ERIKA ROCHA.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 21-08-2025.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.      LE OS REGISTROS DO ARQUIVO DE ENTRADA
      *              CADCLI1 E GRAVA NO ARQUIVO DE SAÍDA 
      *              CADCLI2 SOMENTE OS REGISTRO DO SEXO M 

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.
           SELECT CADCLI1  ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADCLI2  ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD CADCLI1
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADCLI1.DAT".
      
       01 REG-ENT.
           02 COD-ENT   PIC 9(05).
           02 NOME-ENT  PIC X(20).
           02 SEXO      PIC X(01).

       FD CADCLI2
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADCLI2.DAT".
           
       01 REG-SAI.
           02 COD-SAI   PIC 9(05).
           02 NOME-SAI  PIC X(20).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ    PIC X(03) VALUE "NAO".
       77 MASCULINO  PIC X(01) VALUE "M".

       PROCEDURE DIVISION.
       
       EXEMPLO.
           
           PERFORM INICIO.
           
           PERFORM PRINCIPAL 
                UNTIL FIM-ARQ EQUAL "SIM".
           
       PERFORM TERMINO.
       
       STOP RUN.
       
       INICIO. 

           OPEN INPUT CADCLI1 OUTPUT CADCLI2.
           PERFORM LEITURA.
           
       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.
           
       GRAVACAO.
           IF SEXO = MASCULINO THEN 
                MOVE  COD-ENT  TO COD-SAI
                MOVE  NOME-ENT TO NOME-SAI
           
           WRITE REG-SAI.
           
       LEITURA.
           READ CADCLI1 AT END 
                MOVE "SIM" TO FIM-ARQ.
                
       TERMINO. 
           CLOSE CADCLI1 CADCLI2.

         