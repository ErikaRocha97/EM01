       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EX06.
       AUTHOR.       ERIKA ROCHA.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 22-08-2025.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.      LE OS REGISTROS DO ARQUIVO DE ENTRADA 
      *              CADENT E SELECIONA PARA GRAVACAO NO ARQUIVO 
      *              CADSAI SOMENTE OS REGISTROS QUE TIVEREM 
      *              SALARIO BRUTO MAIOR QUE 3000.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADENT   ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADSAI   ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD CADENT
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADENT.DAT".
      
       01 REG-ENT.
           02 MATR-ENT   PIC 9(05).
           02 NOME-ENT   PIC X(20).
           02 SABR-ENT   PIC 9(05).

       FD CADSAI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADSAI.DAT".
           
       01 REG-SAI.
           02 MATR-SAI   PIC 9(05).
           02 NOME-SAI   PIC X(20).
           02 SABR-SAI   PIC 9(05).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ       PIC X(03) VALUE "NAO".

       PROCEDURE DIVISION.
       
       EXEMPLO.
           PERFORM INICIO.
           
           PERFORM PRINCIPAL 
               UNTIL FIM-ARQ EQUAL "SIM".
           
       PERFORM TERMINO.
       
       STOP RUN.
       
       INICIO. 
           OPEN INPUT CADENT OUTPUT CADSAI.
           PERFORM LEITURA.
           
       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.
           
       GRAVACAO.
           IF SABR-ENT > 3000 THEN
               MOVE MATR-ENT TO MATR-SAI
               MOVE NOME-ENT TO NOME-SAI
               MOVE SABR-ENT TO SABR-SAI
           WRITE REG-SAI.
           
       LEITURA.
           READ CADENT AT END 
               MOVE "SIM" TO FIM-ARQ.
                
       TERMINO. 
           CLOSE CADENT CADSAI.

         