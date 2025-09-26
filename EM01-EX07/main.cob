       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EX07.
       AUTHOR.       ERIKA ROCHA.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 25-08-2025.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.      LE TODOS OS REGISTROS DO ARQUIVO DE ENTRADA 
      *              CADALU, CALCULA A MEDIA DAS NOTAS NOTA1, 
      *              NOTA2, NOTA3 E NOTA4 E SELECIONA PARA 
      *              GRAVAÇÃO NO ARQUIVO DE SAIDA CADATU APENAS 
      *              OS REGISTROS DO SEXO FEMININO.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU   ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADATU   ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD CADALU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADALU.DAT".
      
       01 REG-ENT.
           02 NUM-ENT       PIC 9(05).
           02 NOME-ENT      PIC X(20).
           02 NOTA1         PIC 9(02)V99.
           02 NOTA2         PIC 9(02)V99.
           02 NOTA3         PIC 9(02)V99.
           02 NOTA4         PIC 9(02)V99.
           02 SEXO-ENT      PIC X(01).

       FD CADATU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".
           
       01 REG-SAI.
           02 NUM-SAI       PIC 9(05).
           02 NOME-SAI      PIC X(20).
           02 MEDIA         PIC 9(02)V99.
           02 SEXO-SAI      PIC X(01).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ       PIC X(03) VALUE "NAO".
       77 SOMA          PIC 9(02)V99 VALUE 0.
       77 MEDIA-CALC    PIC 9(02)V99 VALUE 0.

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
           IF SEXO-ENT = "F" THEN
               PERFORM CALCULAMEDIA
               PERFORM GRAVACAO.
           PERFORM LEITURA.
           PERFORM LIMPAVARIAVEIS.
           
       GRAVACAO.
           MOVE NUM-ENT    TO NUM-SAI
           MOVE NOME-ENT   TO NOME-SAI
           MOVE MEDIA-CALC TO MEDIA
           MOVE SEXO-ENT   TO SEXO-SAI
           WRITE REG-SAI.
           
       CALCULAMEDIA.
           ADD NOTA1, NOTA2, NOTA3, NOTA4 TO SOMA.
           DIVIDE SOMA BY 4 GIVING MEDIA-CALC.
           
       LIMPAVARIAVEIS.
               MOVE 0 TO SOMA
               MOVE 0 TO MEDIA-CALC.
       
       LEITURA.
           READ CADALU AT END 
               MOVE "SIM" TO FIM-ARQ.
                
       TERMINO. 
           CLOSE CADALU CADATU.

         