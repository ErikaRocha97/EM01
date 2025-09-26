       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EX08.
       AUTHOR.       ERIKA ROCHA.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 25-08-2025.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.      LE O ARQUIVO DE ENTRADA CADFUN, CALCULA O 
      *              REAJUSTE DE ACORDO COM A FAIXA SALARIAL E 
      *              GRAVA OS REGISTROS DE SAIDA NO ARQUIVO 
      *              CADSAI.

       ENVIRONMENT DIVISION.
       
      *              Descrição do ambiente de execução e compilação.
      *              Detalhes do computador/linguagem/dispositivos E/S.
      *              - SOURCE-COMPUTER: computador/ambiente onde o 
      *              codigo foi escrito.
      *              - OBJECT-COMPUTER: especifica o computador onde o 
      *              programa compilado (objeto) será executado.  
      *                  - IBM-PC: microcomputador padrão IBM-PC.
      *              - SPECIAL-NAMES: cláusula usada para definir nomes 
      *              especiais e convenções que afetam a forma como o 
      *              COBOL interpreta certos símbolos.
      *                  - DECIMAL-POINT IS COMMA significa que, neste 
      *              programa, o separador decimal será a vírgula (,) 
      *              em vez do ponto (.).
      
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
       
      *              Descrição de arquivos (de onde vem, pra onde 
      *              vão). 
      *              ASSIGN TO DISK indica que esse arquivo está 
      *              fisicamente em disco.
      *              LINE SEQUENTIALindica que o arquivo é um texto 
      *              comum, onde cada linha corresponde a um registro 
      *              (separados por quebra de linha). Ou seja: o COBOL 
      *              lê/grava linha a linha.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADFUN   ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADSAI   ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       
      *              Descrição de arquivos lógicos.
      *              FD: file description.
      *              O número na frente é referente ao NÍVEL
      
       FILE SECTION.
       FD CADFUN
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADFUN.DAT".
      
       01 REG-ENT.
           02 COD-ENT       PIC 9(05).
           02 NOME-ENT      PIC X(20).
           02 SALARIO-BRU   PIC 9(05)V99.

       FD CADSAI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADSAI.DAT".
           
       01 REG-SAI.
           02 COD-SAI       PIC 9(05).
           02 NOME-SAI      PIC X(20).
           02 SALARIO-SAI   PIC 9(05)V99.

       WORKING-STORAGE SECTION.
       77 FIM-ARQ       PIC X(03)    VALUE "NAO".
       77 PERCENTUAL    PIC 9(02)    VALUE 0.
       77 AJUSTE        PIC 9(05)V99 VALUE 0.
       77 SALARIO-REA   PIC 9(05)V99 VALUE 0.

      *------------->CÓDIGO EXECUTAVEL (INSTRUÇÕES).
      
      *              Nome de rotina inicia na margem a(8);
      *              Comando inicia na margem b(12).
       
       PROCEDURE DIVISION.
       
       EXECUTAR.
           PERFORM INICIO.
           PERFORM PRINCIPAL 
               UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM TERMINO.
           STOP RUN.
       
       INICIO. 
           OPEN INPUT CADFUN 
           OPEN OUTPUT CADSAI
           PERFORM LEITURA.
           PERFORM PRINCIPAL
               UNTIL FIM-ARQ = "SIM".
           PERFORM TERMINO.
           STOP RUN.
           
       LEITURA.
           READ CADFUN 
               AT END MOVE "SIM" TO FIM-ARQ.
           
       PRINCIPAL.
           PERFORM CALCULAREAJUSTE.
           PERFORM GRAVACAO.
           PERFORM LEITURA.
           PERFORM LIMPAVARIAVEIS.
           
       GRAVACAO.
           MOVE COD-ENT     TO COD-SAI
           MOVE NOME-ENT    TO NOME-SAI
           MOVE SALARIO-REA TO SALARIO-SAI
           WRITE REG-SAI.
      
       CALCULAREAJUSTE.
           PERFORM DEFINEPERCENTUAL.
           MULTIPLY SALARIO-BRU BY PERCENTUAL  GIVING AJUSTE.
           DIVIDE   AJUSTE      BY 100         GIVING AJUSTE.
           ADD      AJUSTE      TO SALARIO-BRU GIVING SALARIO-REA.
       
       DEFINEPERCENTUAL.
           IF SALARIO-BRU <= 1000 THEN MOVE 12 TO PERCENTUAL
           ELSE 
               IF SALARIO-BRU <= 2000 
                   THEN MOVE 11 TO PERCENTUAL
               ELSE MOVE 10 TO PERCENTUAL
               END-IF
           END-IF.
           
       LIMPAVARIAVEIS.
           MOVE 0 TO PERCENTUAL.
           MOVE 0 TO AJUSTE.
           MOVE 0 TO SALARIO-REA.

       TERMINO. 
           CLOSE CADFUN.
           CLOSE CADSAI.

         