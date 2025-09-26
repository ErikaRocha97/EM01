       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EX00.
       AUTHOR.       ERIKA.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 12-08-2025.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.      LE OS REGISTROS DO ARQUIVO DE ENTRADA
      *              CADALU (NUMERO, NOME, SEXO, DATA NASCIMENTO)
      *              E GRAVA NO ARQUIVO DE SAIDA CADATU.

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
       
      *              Descrição de arquivos lógicos.
      *              FD: file description.
      *              O número na frente é referente ao NÍVEL
      
       FILE SECTION.

       FD CADALU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADALU.DAT".
      
       01 REG-ENT.
           02 NUM-ENT   PIC 9(05).
           02 NOME-ENT  PIC X(20).
           02 SEXO-ENT  PIC X(01).
       02 DATANASCIMENTO.
           03 DD-ENT    PIC 9(02).
           03 MM-ENT    PIC 9(02).
           03 AA-ENT    PIC 9(04).
       
       FD CADATU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".
           
       01 REG-SAI.
           02 NUM-SAI   PIC 9(05).
           02 NOME-SAI  PIC X(20).
       02 DATANASCIMENTO.
           03 DD-SAI    PIC 9(02).
           03 MM-SAI    PIC 9(02).
           03 AA-SAI    PIC 9(04).

       WORKING-STORAGE SECTION.
       
       77 FIM-ARQ       PIC X(03) VALUE "NAO".
       
      *------------->CÓDIGO EXECUTAVEL (INSTRUÇÕES).
      
      *              Nome de rotina inicia na margem a(8);
      *              Comando inicia na margem b(12).
       
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
           
       LEITURA.
           READ CADALU AT END 
                MOVE "SIM" TO FIM-ARQ.
                
       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.
           
       GRAVACAO.
           MOVE  NUM-ENT  TO NUM-SAI.
           MOVE  NOME-ENT TO NOME-SAI.
           MOVE  DD-ENT TO DD-SAI.
           MOVE  MM-ENT TO MM-SAI.
           MOVE  AA-ENT TO AA-SAI.
           WRITE REG-SAI.
           
       TERMINO. 
           CLOSE CADALU CADATU.

         