      ******************************************************************
      * PROGRAM      : RAILMAIN
      * SYSTEM       : INDIAN RAILWAY RESERVATION SYSTEM (IRRS)
      * DESCRIPTION  : MAIN APPLICATION PROGRAM. COVERS USER LOGIN AND
      *                REGISTRATION, PROFILE MANAGEMENT, TRAIN SEARCH
      *                AND ENQUIRY (AVAILABILITY / FARE / SCHEDULE),
      *                TICKET BOOKING WITH QUOTA HANDLING, PNR
      *                GENERATION AND STATUS TRACKING, SIMULATED
      *                PAYMENT PROCESSING, TICKET CANCELLATION WITH
      *                TIERED REFUND CALCULATION, AND ADMINISTRATOR
      *                FUNCTIONS FOR TRAIN / FARE / QUOTA MAINTENANCE
      *                AND BOOKING / REVENUE REPORTING.
      * FILES        : TRAINPF  - TRAIN MASTER          (KEYED TRNNO)
      *                BOOKPF   - BOOKING LEDGER         (KEYED PNRNO)
      *                USERPF   - USER / CUSTOMER MASTER (KEYED USRID)
      *                PAYPF    - PAYMENT LEDGER         (KEYED TXNNO)
      *                RAILDSPF - DISPLAY FILE (13 RECORD FORMATS)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RAILMAIN.
       AUTHOR.        IRRS-CASE-STUDY.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-ISERIES.
       OBJECT-COMPUTER.  IBM-ISERIES.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRAINPF ASSIGN TO DATABASE-TRAINPF
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TP-TRNNO
               FILE STATUS IS WS-TRN-STATUS.

           SELECT BOOKPF ASSIGN TO DATABASE-BOOKPF
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BP-PNRNO
               FILE STATUS IS WS-BOK-STATUS.

           SELECT USERPF ASSIGN TO DATABASE-USERPF
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UP-USRID
               FILE STATUS IS WS-USR-STATUS.

           SELECT PAYPF ASSIGN TO DATABASE-PAYPF
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PP-TXNNO
               FILE STATUS IS WS-PAY-STATUS.

           SELECT RAILDSPF ASSIGN TO WORKSTATION-RAILDSPF
               ORGANIZATION IS TRANSACTION
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-DSP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRAINPF
           LABEL RECORDS ARE STANDARD.
       01  TRAINPF-REC.
           05  TP-TRNNO           PIC 9(5).
           05  TP-TRNNAM          PIC X(30).
           05  TP-SRCSTN          PIC X(20).
           05  TP-SRCCOD          PIC X(4).
           05  TP-DSTSTN          PIC X(20).
           05  TP-DSTCOD          PIC X(4).
           05  TP-DEPTIM          PIC 9(4).
           05  TP-ARRTIM          PIC 9(4).
           05  TP-RUNDAY          PIC X(7).
           05  TP-TOTSEAT         PIC 9(4).
           05  TP-AVLSEAT         PIC 9(4).
           05  TP-GENSEAT         PIC 9(4).
           05  TP-LADSEAT         PIC 9(4).
           05  TP-SCTSEAT         PIC 9(4).
           05  TP-FARE            PIC S9(5)V9(2) PACKED-DECIMAL.
           05  TP-TRNSTS          PIC X(1).

       FD  BOOKPF
           LABEL RECORDS ARE STANDARD.
       01  BOOKPF-REC.
           05  BP-PNRNO           PIC 9(10).
           05  BP-TRNNO           PIC 9(5).
           05  BP-JRNDAT          PIC 9(8).
           05  BP-BOOKDT          PIC 9(8).
           05  BP-BOOKTM          PIC 9(6).
           05  BP-USRID           PIC X(10).
           05  BP-PSGNAM          PIC X(30).
           05  BP-PSGAGE          PIC 9(3).
           05  BP-PSGSEX          PIC X(1).
           05  BP-QUOTA           PIC X(2).
           05  BP-BERTHTY         PIC X(2).
           05  BP-BERTHNO         PIC 9(3).
           05  BP-FARAMT          PIC S9(5)V9(2) PACKED-DECIMAL.
           05  BP-BOOKSTS         PIC X(1).
           05  BP-CANDT           PIC 9(8).
           05  BP-CANTM           PIC 9(6).
           05  BP-REFAMT          PIC S9(5)V9(2) PACKED-DECIMAL.
           05  BP-TXNNO           PIC X(12).

       FD  USERPF
           LABEL RECORDS ARE STANDARD.
       01  USERPF-REC.
           05  UP-USRID           PIC X(10).
           05  UP-USRPWD          PIC X(10).
           05  UP-USRNAM          PIC X(30).
           05  UP-USRAGE          PIC 9(3).
           05  UP-USRSEX          PIC X(1).
           05  UP-USREML          PIC X(40).
           05  UP-USRPHN          PIC X(12).
           05  UP-USRADR          PIC X(60).
           05  UP-USRTYP          PIC X(1).
           05  UP-REGDT           PIC 9(8).
           05  UP-USRSTS          PIC X(1).

       FD  PAYPF
           LABEL RECORDS ARE STANDARD.
       01  PAYPF-REC.
           05  PP-TXNNO           PIC X(12).
           05  PP-PNRNO           PIC 9(10).
           05  PP-USRID           PIC X(10).
           05  PP-AMOUNT          PIC S9(5)V9(2) PACKED-DECIMAL.
           05  PP-PAYMOD          PIC X(2).
           05  PP-PAYSTS          PIC X(1).
           05  PP-PAYDT           PIC 9(8).
           05  PP-PAYTM           PIC 9(6).
           05  PP-REFNO           PIC X(16).

       FD  RAILDSPF
           LABEL RECORDS ARE STANDARD.
       01  SGNFMT.
           05  SGUSRID            PIC X(10).
           05  SGUSRPWD           PIC X(10).
           05  SGMSG1             PIC X(60).
           05  SGNFMT-IND-AREA.
               10  SGNFMT-IND       PIC X OCCURS 12.
       01  REGFMT.
           05  RGUSRID            PIC X(10).
           05  RGUSRPWD           PIC X(10).
           05  RGUSRNAM           PIC X(30).
           05  RGUSRAGE           PIC 9(3).
           05  RGUSRSEX           PIC X(1).
           05  RGUSREML           PIC X(40).
           05  RGUSRPHN           PIC X(12).
           05  RGUSRADR           PIC X(60).
           05  RGMSG1             PIC X(60).
           05  REGFMT-IND-AREA.
               10  REGFMT-IND       PIC X OCCURS 12.
       01  PROFFMT.
           05  PRUSRID            PIC X(10).
           05  PRUSRPWD           PIC X(10).
           05  PRUSRNAM           PIC X(30).
           05  PRUSRAGE           PIC 9(3).
           05  PRUSRSEX           PIC X(1).
           05  PRUSREML           PIC X(40).
           05  PRUSRPHN           PIC X(12).
           05  PRUSRADR           PIC X(60).
           05  PRMSG1             PIC X(60).
           05  PROFFMT-IND-AREA.
               10  PROFFMT-IND      PIC X OCCURS 12.
       01  MENFMT.
           05  MNUSRNAM           PIC X(30).
           05  MNOPTION           PIC 9(1).
           05  MNMSG1             PIC X(60).
           05  MENFMT-IND-AREA.
               10  MENFMT-IND       PIC X OCCURS 12.
       01  SRHFMT.
           05  SRTRNNO            PIC 9(5).
           05  SRSRCCOD           PIC X(4).
           05  SRDSTCOD           PIC X(4).
           05  SRJRNDAT           PIC 9(8).
           05  SRMSG1             PIC X(60).
           05  SRHFMT-IND-AREA.
               10  SRHFMT-IND       PIC X OCCURS 12.
       01  SRLFMT.
           05  SLIDX              PIC 9(3).
           05  SLTOT              PIC 9(3).
           05  SLTRNNO            PIC 9(5).
           05  SLTRNAM            PIC X(30).
           05  SLSRCST            PIC X(20).
           05  SLDSTST            PIC X(20).
           05  SLDEPTM            PIC 9(4).
           05  SLARRTM            PIC 9(4).
           05  SLTOTST            PIC 9(4).
           05  SLAVLST            PIC 9(4).
           05  SLGENST            PIC 9(4).
           05  SLLADST            PIC 9(4).
           05  SLSCTST            PIC 9(4).
           05  SLFARE             PIC S9(5)V9(2) PACKED-DECIMAL.
           05  SLBKTRN            PIC 9(5).
           05  SLMSG1             PIC X(60).
           05  SRLFMT-IND-AREA.
               10  SRLFMT-IND       PIC X OCCURS 12.
       01  BOKFMT.
           05  BKTRNNO            PIC 9(5).
           05  BKTRNAM            PIC X(25).
           05  BKSRCST            PIC X(16).
           05  BKDSTST            PIC X(16).
           05  BKDEPTM            PIC 9(4).
           05  BKARRTM            PIC 9(4).
           05  BKAVLST            PIC 9(4).
           05  BKFARE             PIC S9(5)V9(2) PACKED-DECIMAL.
           05  BKPSGNAM           PIC X(30).
           05  BKPSGAGE           PIC 9(3).
           05  BKPSGSEX           PIC X(1).
           05  BKQUOTA            PIC X(2).
           05  BKBRTHTY           PIC X(2).
           05  BKJRNDAT           PIC 9(8).
           05  BKMSG1             PIC X(60).
           05  BOKFMT-IND-AREA.
               10  BOKFMT-IND       PIC X OCCURS 12.
       01  CNFFMT.
           05  CNTRNAM            PIC X(30).
           05  CNPSGNAM           PIC X(30).
           05  CNQUOTA            PIC X(2).
           05  CNBRTHTY           PIC X(2).
           05  CNFARAMT           PIC S9(5)V9(2) PACKED-DECIMAL.
           05  CNPNRNO            PIC 9(10).
           05  CNPAYMOD           PIC X(2).
           05  CNPAYREF           PIC X(16).
           05  CNMSG1             PIC X(60).
           05  CNFFMT-IND-AREA.
               10  CNFFMT-IND       PIC X OCCURS 12.
       01  PNRFMT.
           05  PNPNRNO            PIC 9(10).
           05  PNTRNAM            PIC X(30).
           05  PNSRCST            PIC X(16).
           05  PNDSTST            PIC X(16).
           05  PNJRNDAT           PIC 9(8).
           05  PNPSGNAM           PIC X(30).
           05  PNPSGAGE           PIC 9(3).
           05  PNPSGSEX           PIC X(1).
           05  PNQUOTA            PIC X(2).
           05  PNBRTHTY           PIC X(2).
           05  PNBRTHNO           PIC 9(3).
           05  PNFARAMT           PIC S9(5)V9(2) PACKED-DECIMAL.
           05  PNBOKSTS           PIC X(1).
           05  PNREFAMT           PIC S9(5)V9(2) PACKED-DECIMAL.
           05  PNMSG1             PIC X(60).
           05  PNRFMT-IND-AREA.
               10  PNRFMT-IND       PIC X OCCURS 12.
       01  CANFMT.
           05  CXPNRNO            PIC 9(10).
           05  CXTRNAM            PIC X(30).
           05  CXPSGNAM           PIC X(30).
           05  CXJRNDAT           PIC 9(8).
           05  CXQUOTA            PIC X(2).
           05  CXFARAMT           PIC S9(5)V9(2) PACKED-DECIMAL.
           05  CXBOKSTS           PIC X(1).
           05  CXREFAMT           PIC S9(5)V9(2) PACKED-DECIMAL.
           05  CXMSG1             PIC X(60).
           05  CANFMT-IND-AREA.
               10  CANFMT-IND       PIC X OCCURS 12.
       01  ADMMNU.
           05  AMOPTION           PIC 9(1).
           05  AMMSG1             PIC X(60).
           05  ADMMNU-IND-AREA.
               10  ADMMNU-IND       PIC X OCCURS 12.
       01  ADMTRN.
           05  ATTRNNO            PIC 9(5).
           05  ATTRNAM            PIC X(30).
           05  ATSRCST            PIC X(20).
           05  ATSRCCOD           PIC X(4).
           05  ATDSTST            PIC X(20).
           05  ATDSTCOD           PIC X(4).
           05  ATDEPTIM           PIC 9(4).
           05  ATARRTIM           PIC 9(4).
           05  ATRUNDAY           PIC X(7).
           05  ATTOTSET           PIC 9(4).
           05  ATGENSET           PIC 9(4).
           05  ATLADSET           PIC 9(4).
           05  ATSCTSET           PIC 9(4).
           05  ATFARE             PIC S9(5)V9(2) PACKED-DECIMAL.
           05  ATTRNSTS           PIC X(1).
           05  ATMSG1             PIC X(60).
           05  ADMTRN-IND-AREA.
               10  ADMTRN-IND       PIC X OCCURS 12.
       01  RPTFMT.
           05  RPTRNNO            PIC 9(5).
           05  RPTOTBK            PIC 9(6).
           05  RPACTBK            PIC 9(6).
           05  RPCANBK            PIC 9(6).
           05  RPTOTREV           PIC S9(7)V9(2) PACKED-DECIMAL.
           05  RPTOTREF           PIC S9(7)V9(2) PACKED-DECIMAL.
           05  RPUSRCNT           PIC 9(6).
           05  RPMSG1             PIC X(60).
           05  RPTFMT-IND-AREA.
               10  RPTFMT-IND       PIC X OCCURS 12.

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS.
           05  WS-TRN-STATUS      PIC X(2) VALUE '00'.
           05  WS-BOK-STATUS      PIC X(2) VALUE '00'.
           05  WS-USR-STATUS      PIC X(2) VALUE '00'.
           05  WS-PAY-STATUS      PIC X(2) VALUE '00'.
           05  WS-DSP-STATUS      PIC X(2) VALUE '00'.

       01  WS-FLAGS.
           05  WS-EXIT-PGM        PIC X VALUE 'N'.
           05  WS-BACK-TO-MENU    PIC X VALUE 'N'.
           05  WS-FOUND           PIC X VALUE 'N'.
           05  WS-EOF             PIC X VALUE 'N'.
           05  WS-BOOKING-OK      PIC X VALUE 'N'.
           05  WS-DONE-REG        PIC X VALUE 'N'.
           05  WS-DONE-PROF       PIC X VALUE 'N'.
           05  WS-DONE-SRH        PIC X VALUE 'N'.
           05  WS-DONE-BRW        PIC X VALUE 'N'.
           05  WS-DONE-BOK        PIC X VALUE 'N'.
           05  WS-DONE-PAY        PIC X VALUE 'N'.
           05  WS-DONE-PNR        PIC X VALUE 'N'.
           05  WS-DONE-CAN        PIC X VALUE 'N'.
           05  WS-DONE-ADM        PIC X VALUE 'N'.
           05  WS-DONE-ATM        PIC X VALUE 'N'.
           05  WS-DONE-RPT        PIC X VALUE 'N'.

       01  WS-SESSION.
           05  WS-LOGIN-USRID     PIC X(10).
           05  WS-LOGIN-USRNAM    PIC X(30).
           05  WS-LOGIN-USRTYP    PIC X(1).
           05  WS-LOGIN-TRIES     PIC 9 VALUE 0.

       01  WS-CURR-DATE-TIME      PIC X(21).
       01  WS-CURR-DATE           PIC 9(8).
       01  WS-CURR-TIME           PIC 9(6).

       01  WS-PNR-TXN.
           05  WS-NEW-PNR         PIC 9(10).
           05  WS-NEW-PNR-X       PIC X(10).
           05  WS-NEW-TXN         PIC X(12).

       01  WS-SEARCH.
           05  WS-MATCH-COUNT     PIC 9(3) VALUE 0.
           05  WS-MATCH-IDX       PIC 9(3) VALUE 0.
           05  WS-MATCH-LIST OCCURS 50 TIMES PIC 9(5).

       01  WS-BOOKING-WORK.
           05  WS-SELECTED-TRNNO  PIC 9(5).
           05  WS-COMPUTED-FARE   PIC S9(7)V99.
           05  WS-BOOKED-COUNT    PIC 9(5).

       01  WS-REFUND-WORK.
           05  WS-JRN-INT         PIC S9(9).
           05  WS-CUR-INT         PIC S9(9).
           05  WS-DAYS-DIFF       PIC S9(9).
           05  WS-REFUND-AMT      PIC S9(7)V99.

       01  WS-REPORT-WORK.
           05  WS-RPT-TOTBK       PIC 9(6) VALUE 0.
           05  WS-RPT-ACTBK       PIC 9(6) VALUE 0.
           05  WS-RPT-CANBK       PIC 9(6) VALUE 0.
           05  WS-RPT-REV         PIC S9(9)V99 VALUE 0.
           05  WS-RPT-REF         PIC S9(9)V99 VALUE 0.
           05  WS-RPT-USRCNT      PIC 9(6) VALUE 0.

       PROCEDURE DIVISION.

       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE.
           IF WS-EXIT-PGM = 'N'
               PERFORM 2000-SIGNON-PROCESS
               IF WS-EXIT-PGM = 'N'
                   PERFORM 3000-MAIN-MENU-PROCESS
                       UNTIL WS-EXIT-PGM = 'Y'
               END-IF
           END-IF
           PERFORM 9900-TERMINATE.
           STOP RUN.

      ******************************************************************
      * INITIALIZATION
      ******************************************************************
       1000-INITIALIZE.
           MOVE 'N' TO WS-EXIT-PGM.
           OPEN I-O TRAINPF.
           IF WS-TRN-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING TRAINPF - STATUS ' WS-TRN-STATUS
               MOVE 'Y' TO WS-EXIT-PGM
           END-IF.
           OPEN I-O BOOKPF.
           IF WS-BOK-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING BOOKPF - STATUS ' WS-BOK-STATUS
               MOVE 'Y' TO WS-EXIT-PGM
           END-IF.
           OPEN I-O USERPF.
           IF WS-USR-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING USERPF - STATUS ' WS-USR-STATUS
               MOVE 'Y' TO WS-EXIT-PGM
           END-IF.
           OPEN I-O PAYPF.
           IF WS-PAY-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING PAYPF - STATUS ' WS-PAY-STATUS
               MOVE 'Y' TO WS-EXIT-PGM
           END-IF.
           OPEN I-O RAILDSPF.
           IF WS-DSP-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING RAILDSPF - STATUS ' WS-DSP-STATUS
               MOVE 'Y' TO WS-EXIT-PGM
           END-IF.
           MOVE FUNCTION CURRENT-DATE TO WS-CURR-DATE-TIME.
           MOVE WS-CURR-DATE-TIME(1:8) TO WS-CURR-DATE.
           MOVE WS-CURR-DATE-TIME(9:6) TO WS-CURR-TIME.

      ******************************************************************
      * 1. USER MANAGEMENT - SIGN ON / REGISTRATION / PROFILE
      ******************************************************************
       2000-SIGNON-PROCESS.
           MOVE 0 TO WS-LOGIN-TRIES.
           MOVE SPACES TO SGMSG1.
           PERFORM UNTIL WS-EXIT-PGM = 'Y'
               MOVE SPACES TO SGUSRID SGUSRPWD
               MOVE ZEROS TO SGNFMT-IND-AREA
               WRITE SGNFMT
               READ RAILDSPF RECORD
               EVALUATE TRUE
                   WHEN SGNFMT-IND(3) = '1'
                       MOVE 'Y' TO WS-EXIT-PGM
                   WHEN SGNFMT-IND(6) = '1'
                       PERFORM 2200-REGISTER-NEW-USER
                   WHEN OTHER
                       PERFORM 2100-VALIDATE-LOGIN
                       IF WS-LOGIN-USRID NOT = SPACES
                           MOVE 'Y' TO WS-EXIT-PGM
                       END-IF
               END-EVALUATE
           END-PERFORM.
           IF WS-LOGIN-USRID NOT = SPACES
               MOVE 'N' TO WS-EXIT-PGM
           END-IF.

       2100-VALIDATE-LOGIN.
           MOVE SGUSRID TO UP-USRID.
           READ USERPF
               INVALID KEY
                   MOVE 'N' TO WS-FOUND
               NOT INVALID KEY
                   MOVE 'Y' TO WS-FOUND
           END-READ.
           IF WS-FOUND = 'Y' AND UP-USRPWD = SGUSRPWD
                   AND UP-USRSTS = 'A'
               MOVE UP-USRID  TO WS-LOGIN-USRID
               MOVE UP-USRNAM TO WS-LOGIN-USRNAM
               MOVE UP-USRTYP TO WS-LOGIN-USRTYP
           ELSE
               ADD 1 TO WS-LOGIN-TRIES
               IF WS-LOGIN-TRIES >= 3
                   MOVE 'TOO MANY FAILED ATTEMPTS - PROGRAM ENDING'
                       TO SGMSG1
                   MOVE ZEROS TO SGNFMT-IND-AREA
                   WRITE SGNFMT
                   MOVE 'Y' TO WS-EXIT-PGM
               ELSE
                   MOVE 'INVALID USER ID OR PASSWORD - RE-ENTER'
                       TO SGMSG1
               END-IF
           END-IF.

       2200-REGISTER-NEW-USER.
           MOVE SPACES TO RGUSRID RGUSRPWD RGUSRNAM RGUSREML.
           MOVE SPACES TO RGUSRPHN RGUSRADR RGMSG1.
           MOVE 0 TO RGUSRAGE.
           MOVE SPACE TO RGUSRSEX.
           MOVE 'N' TO WS-DONE-REG.
           PERFORM UNTIL WS-DONE-REG = 'Y' OR WS-EXIT-PGM = 'Y'
               MOVE ZEROS TO REGFMT-IND-AREA
               WRITE REGFMT
               READ RAILDSPF RECORD
               EVALUATE TRUE
                   WHEN REGFMT-IND(3) = '1'
                       MOVE 'Y' TO WS-DONE-REG
                       MOVE 'Y' TO WS-EXIT-PGM
                   WHEN REGFMT-IND(12) = '1'
                       MOVE 'Y' TO WS-DONE-REG
                   WHEN OTHER
                       MOVE RGUSRID TO UP-USRID
                       READ USERPF
                           INVALID KEY MOVE 'N' TO WS-FOUND
                           NOT INVALID KEY MOVE 'Y' TO WS-FOUND
                       END-READ
                       IF WS-FOUND = 'Y'
                           MOVE 'USER ID ALREADY EXISTS'
                               TO RGMSG1
                       ELSE
                           MOVE RGUSRID  TO UP-USRID
                           MOVE RGUSRPWD TO UP-USRPWD
                           MOVE RGUSRNAM TO UP-USRNAM
                           MOVE RGUSRAGE TO UP-USRAGE
                           MOVE RGUSRSEX TO UP-USRSEX
                           MOVE RGUSREML TO UP-USREML
                           MOVE RGUSRPHN TO UP-USRPHN
                           MOVE RGUSRADR TO UP-USRADR
                           MOVE 'C'      TO UP-USRTYP
                           MOVE WS-CURR-DATE TO UP-REGDT
                           MOVE 'A'      TO UP-USRSTS
                           WRITE USERPF-REC
                           IF WS-USR-STATUS = '00'
                               MOVE 'REGISTRATION OK - SIGN ON BELOW'
                                   TO SGMSG1
                               MOVE 'Y' TO WS-DONE-REG
                           ELSE
                               MOVE 'ERROR SAVING REGISTRATION'
                                   TO RGMSG1
                           END-IF
                       END-IF
               END-EVALUATE
           END-PERFORM.

       3100-PROFILE-MANAGEMENT.
           MOVE WS-LOGIN-USRID TO UP-USRID.
           READ USERPF
               INVALID KEY CONTINUE
           END-READ.
           MOVE UP-USRID  TO PRUSRID.
           MOVE SPACES    TO PRUSRPWD.
           MOVE UP-USRNAM TO PRUSRNAM.
           MOVE UP-USRAGE TO PRUSRAGE.
           MOVE UP-USRSEX TO PRUSRSEX.
           MOVE UP-USREML TO PRUSREML.
           MOVE UP-USRPHN TO PRUSRPHN.
           MOVE UP-USRADR TO PRUSRADR.
           MOVE SPACES    TO PRMSG1.
           MOVE 'N' TO WS-DONE-PROF.
           PERFORM UNTIL WS-DONE-PROF = 'Y' OR WS-EXIT-PGM = 'Y'
               MOVE ZEROS TO PROFFMT-IND-AREA
               WRITE PROFFMT
               READ RAILDSPF RECORD
               EVALUATE TRUE
                   WHEN PROFFMT-IND(3) = '1'
                       MOVE 'Y' TO WS-DONE-PROF
                       MOVE 'Y' TO WS-EXIT-PGM
                   WHEN PROFFMT-IND(12) = '1'
                       MOVE 'Y' TO WS-DONE-PROF
                   WHEN OTHER
                       MOVE WS-LOGIN-USRID TO UP-USRID
                       READ USERPF
                           INVALID KEY CONTINUE
                       END-READ
                       MOVE PRUSRNAM TO UP-USRNAM
                       MOVE PRUSRAGE TO UP-USRAGE
                       MOVE PRUSRSEX TO UP-USRSEX
                       MOVE PRUSREML TO UP-USREML
                       MOVE PRUSRPHN TO UP-USRPHN
                       MOVE PRUSRADR TO UP-USRADR
                       IF PRUSRPWD NOT = SPACES
                           MOVE PRUSRPWD TO UP-USRPWD
                       END-IF
                       REWRITE USERPF-REC
                       IF WS-USR-STATUS = '00'
                           MOVE PRUSRNAM TO WS-LOGIN-USRNAM
                           MOVE 'PROFILE UPDATED SUCCESSFULLY' TO PRMSG1
                       ELSE
                           MOVE 'ERROR UPDATING PROFILE - TRY AGAIN'
                               TO PRMSG1
                       END-IF
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * MAIN MENU DISPATCH
      ******************************************************************
       3000-MAIN-MENU-PROCESS.
           MOVE WS-LOGIN-USRNAM TO MNUSRNAM.
           MOVE SPACES TO MNMSG1.
           MOVE ZEROS TO MENFMT-IND-AREA
           WRITE MENFMT.
           READ RAILDSPF RECORD.
           EVALUATE TRUE
               WHEN MENFMT-IND(3) = '1'
                   MOVE 'Y' TO WS-EXIT-PGM
               WHEN MNOPTION = 1
                   PERFORM 4000-SEARCH-TRAINS
               WHEN MNOPTION = 2
                   PERFORM 4000-SEARCH-TRAINS
               WHEN MNOPTION = 3
                   PERFORM 7000-PNR-STATUS
               WHEN MNOPTION = 4
                   PERFORM 8000-CANCEL-TICKET
               WHEN MNOPTION = 5
                   PERFORM 3100-PROFILE-MANAGEMENT
               WHEN MNOPTION = 6 AND WS-LOGIN-USRTYP = 'A'
                   PERFORM 9000-ADMIN-MENU
               WHEN MNOPTION = 6
                   MOVE 'ACCESS DENIED - ADMIN OPTION ONLY' TO MNMSG1
                   MOVE ZEROS TO MENFMT-IND-AREA
                   WRITE MENFMT
                   READ RAILDSPF RECORD
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      ******************************************************************
      * 2. TRAIN SEARCH & ENQUIRY (AVAILABILITY / FARE / SCHEDULE)
      ******************************************************************
       4000-SEARCH-TRAINS.
           MOVE 'N' TO WS-BACK-TO-MENU.
           MOVE 0 TO SRTRNNO SRJRNDAT.
           MOVE SPACES TO SRSRCCOD SRDSTCOD SRMSG1.
           MOVE 'N' TO WS-DONE-SRH.
           PERFORM UNTIL WS-DONE-SRH = 'Y' OR WS-EXIT-PGM = 'Y'
                   OR WS-BACK-TO-MENU = 'Y'
               MOVE ZEROS TO SRHFMT-IND-AREA
               WRITE SRHFMT
               READ RAILDSPF RECORD
               EVALUATE TRUE
                   WHEN SRHFMT-IND(3) = '1'
                       MOVE 'Y' TO WS-DONE-SRH
                       MOVE 'Y' TO WS-EXIT-PGM
                   WHEN SRHFMT-IND(12) = '1'
                       MOVE 'Y' TO WS-DONE-SRH
                   WHEN SRTRNNO = 0 AND
                          (SRSRCCOD = SPACES OR SRDSTCOD = SPACES)
                       MOVE 'ENTER A TRAIN NUMBER OR BOTH STATION CODES'
                           TO SRMSG1
                   WHEN OTHER
                       PERFORM 4100-BUILD-MATCH-LIST
                       IF WS-MATCH-COUNT > 0
                           MOVE 1 TO WS-MATCH-IDX
                           PERFORM 4300-BROWSE-RESULTS
                       ELSE
                           MOVE 'NO MATCHING TRAINS FOUND - TRY AGAIN'
                               TO SRMSG1
                       END-IF
               END-EVALUATE
           END-PERFORM.

       4100-BUILD-MATCH-LIST.
           MOVE 0 TO WS-MATCH-COUNT.
           IF SRTRNNO NOT = 0
               MOVE SRTRNNO TO TP-TRNNO
               READ TRAINPF
                   INVALID KEY CONTINUE
                   NOT INVALID KEY
                       IF TP-TRNSTS = 'A'
                           MOVE 1 TO WS-MATCH-COUNT
                           MOVE TP-TRNNO TO WS-MATCH-LIST(1)
                       END-IF
               END-READ
           ELSE
               MOVE 0 TO TP-TRNNO
               START TRAINPF KEY IS NOT LESS THAN TP-TRNNO
                   INVALID KEY MOVE 'N' TO WS-FOUND
                   NOT INVALID KEY MOVE 'Y' TO WS-FOUND
               END-START
               IF WS-FOUND = 'Y'
                   MOVE 'N' TO WS-EOF
                   PERFORM UNTIL WS-EOF = 'Y'
                       READ TRAINPF NEXT RECORD
                           AT END
                               MOVE 'Y' TO WS-EOF
                           NOT AT END
                               IF TP-SRCCOD = SRSRCCOD
                                       AND TP-DSTCOD = SRDSTCOD
                                       AND TP-TRNSTS = 'A'
                                       AND WS-MATCH-COUNT < 50
                                   ADD 1 TO WS-MATCH-COUNT
                                   MOVE TP-TRNNO
                                       TO WS-MATCH-LIST(WS-MATCH-COUNT)
                               END-IF
                       END-READ
                   END-PERFORM
               END-IF
           END-IF.

       4200-DISPLAY-MATCH.
           MOVE WS-MATCH-LIST(WS-MATCH-IDX) TO TP-TRNNO.
           READ TRAINPF
               INVALID KEY CONTINUE
           END-READ.
           MOVE WS-MATCH-IDX   TO SLIDX.
           MOVE WS-MATCH-COUNT TO SLTOT.
           MOVE TP-TRNNO    TO SLTRNNO.
           MOVE TP-TRNNAM   TO SLTRNAM.
           MOVE TP-SRCSTN   TO SLSRCST.
           MOVE TP-DSTSTN   TO SLDSTST.
           MOVE TP-DEPTIM   TO SLDEPTM.
           MOVE TP-ARRTIM   TO SLARRTM.
           MOVE TP-TOTSEAT  TO SLTOTST.
           MOVE TP-AVLSEAT  TO SLAVLST.
           MOVE TP-GENSEAT  TO SLGENST.
           MOVE TP-LADSEAT  TO SLLADST.
           MOVE TP-SCTSEAT  TO SLSCTST.
           MOVE TP-FARE     TO SLFARE.
           MOVE 0           TO SLBKTRN.
           MOVE SPACES      TO SLMSG1.

       4300-BROWSE-RESULTS.
           MOVE 'N' TO WS-DONE-BRW.
           PERFORM UNTIL WS-DONE-BRW = 'Y' OR WS-EXIT-PGM = 'Y'
                   OR WS-BACK-TO-MENU = 'Y'
               PERFORM 4200-DISPLAY-MATCH
               MOVE ZEROS TO SRLFMT-IND-AREA
               WRITE SRLFMT
               READ RAILDSPF RECORD
               EVALUATE TRUE
                   WHEN SRLFMT-IND(3) = '1'
                       MOVE 'Y' TO WS-DONE-BRW
                       MOVE 'Y' TO WS-EXIT-PGM
                   WHEN SRLFMT-IND(12) = '1'
                       MOVE 'Y' TO WS-DONE-BRW
                   WHEN SRLFMT-IND(7) = '1'
                       IF WS-MATCH-IDX > 1
                           SUBTRACT 1 FROM WS-MATCH-IDX
                       END-IF
                   WHEN SRLFMT-IND(8) = '1'
                       IF WS-MATCH-IDX < WS-MATCH-COUNT
                           ADD 1 TO WS-MATCH-IDX
                       END-IF
                   WHEN SLBKTRN = 0
                       CONTINUE
                   WHEN OTHER
                       MOVE SLBKTRN TO TP-TRNNO
                       READ TRAINPF
                           INVALID KEY CONTINUE
                       END-READ
                       IF WS-TRN-STATUS NOT = '00'
                           MOVE 'TRAIN NUMBER NOT FOUND' TO SLMSG1
                       ELSE
                           IF TP-TRNSTS NOT = 'A'
                               MOVE 'TRAIN IS NOT IN ACTIVE SERVICE'
                                   TO SLMSG1
                           ELSE
                               IF TP-AVLSEAT = 0
                                   MOVE 'NO SEATS AVAILABLE'
                                       TO SLMSG1
                               ELSE
                                   MOVE SLBKTRN TO WS-SELECTED-TRNNO
                                   PERFORM 5000-BOOK-TICKET
                               END-IF
                           END-IF
                       END-IF
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 3. TICKET BOOKING / RESERVATION (PASSENGER/QUOTA/BOOKING LOGIC)
      ******************************************************************
       5000-BOOK-TICKET.
           MOVE WS-SELECTED-TRNNO TO TP-TRNNO.
           READ TRAINPF
               INVALID KEY CONTINUE
           END-READ.
           IF WS-TRN-STATUS = '00' AND TP-AVLSEAT > 0
               MOVE TP-TRNNO    TO BKTRNNO
               MOVE TP-TRNNAM   TO BKTRNAM
               MOVE TP-SRCSTN   TO BKSRCST
               MOVE TP-DSTSTN   TO BKDSTST
               MOVE TP-DEPTIM   TO BKDEPTM
               MOVE TP-ARRTIM   TO BKARRTM
               MOVE TP-AVLSEAT  TO BKAVLST
               MOVE TP-FARE     TO BKFARE
               MOVE SPACES TO BKPSGNAM BKQUOTA BKBRTHTY BKMSG1
               MOVE 0 TO BKPSGAGE BKJRNDAT
               MOVE SPACE TO BKPSGSEX
               MOVE 'N' TO WS-DONE-BOK
               PERFORM UNTIL WS-DONE-BOK = 'Y' OR WS-EXIT-PGM = 'Y'
                       OR WS-BACK-TO-MENU = 'Y'
                   MOVE ZEROS TO BOKFMT-IND-AREA
                   WRITE BOKFMT
                   READ RAILDSPF RECORD
                   EVALUATE TRUE
                       WHEN BOKFMT-IND(3) = '1'
                           MOVE 'Y' TO WS-DONE-BOK
                           MOVE 'Y' TO WS-EXIT-PGM
                       WHEN BOKFMT-IND(12) = '1'
                           MOVE 'Y' TO WS-DONE-BOK
                       WHEN OTHER
                           PERFORM 5100-VALIDATE-PASSENGER-INPUT
                           IF BKMSG1 = SPACES
                               PERFORM 5200-CONFIRM-AND-PAY
                               IF WS-BACK-TO-MENU = 'Y'
                                   MOVE 'Y' TO WS-DONE-BOK
                               END-IF
                           END-IF
                   END-EVALUATE
               END-PERFORM
           END-IF.

       5100-VALIDATE-PASSENGER-INPUT.
           MOVE SPACES TO BKMSG1.
           IF BKQUOTA = 'LD' AND BKPSGSEX NOT = 'F'
               MOVE 'LADIES QUOTA REQUIRES PASSENGER SEX = F' TO BKMSG1
           END-IF.
           IF BKMSG1 = SPACES AND BKQUOTA = 'SC' AND BKPSGAGE < 60
               MOVE 'SR.CITIZEN QUOTA REQUIRES AGE 60 OR ABOVE'
                   TO BKMSG1
           END-IF.
           IF BKMSG1 = SPACES
               COMPUTE WS-JRN-INT = FUNCTION INTEGER-OF-DATE(BKJRNDAT)
               COMPUTE WS-CUR-INT =
                   FUNCTION INTEGER-OF-DATE(WS-CURR-DATE)
               IF WS-JRN-INT < WS-CUR-INT
                   MOVE 'JOURNEY DATE CANNOT BE IN THE PAST' TO BKMSG1
               END-IF
           END-IF.
           IF BKMSG1 = SPACES
               MOVE WS-SELECTED-TRNNO TO TP-TRNNO
               READ TRAINPF
                   INVALID KEY CONTINUE
               END-READ
               EVALUATE TRUE
                   WHEN TP-AVLSEAT = 0
                       MOVE 'NO SEATS AVAILABLE - BOOKING NOT POSSIBLE'
                           TO BKMSG1
                   WHEN BKQUOTA = 'GN' AND TP-GENSEAT = 0
                       MOVE 'GENERAL QUOTA IS FULL FOR THIS TRAIN'
                           TO BKMSG1
                   WHEN BKQUOTA = 'LD' AND TP-LADSEAT = 0
                       MOVE 'LADIES QUOTA IS FULL FOR THIS TRAIN'
                           TO BKMSG1
                   WHEN BKQUOTA = 'SC' AND TP-SCTSEAT = 0
                       MOVE 'SR.CITIZEN QUOTA IS FULL FOR THIS TRAIN'
                           TO BKMSG1
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF.

       5150-COMPUTE-FARE.
           EVALUATE BKQUOTA
               WHEN 'SC'
                   COMPUTE WS-COMPUTED-FARE ROUNDED = BKFARE * 0.90
               WHEN 'TQ'
                   COMPUTE WS-COMPUTED-FARE ROUNDED = BKFARE * 1.20
               WHEN OTHER
                   MOVE BKFARE TO WS-COMPUTED-FARE
           END-EVALUATE.

       5200-CONFIRM-AND-PAY.
           MOVE BKTRNAM  TO CNTRNAM.
           MOVE BKPSGNAM TO CNPSGNAM.
           MOVE BKQUOTA  TO CNQUOTA.
           MOVE BKBRTHTY TO CNBRTHTY.
           PERFORM 5150-COMPUTE-FARE.
           MOVE WS-COMPUTED-FARE TO CNFARAMT.
           MOVE 0 TO CNPNRNO.
           MOVE SPACES TO CNPAYMOD CNPAYREF CNMSG1.
           MOVE 'N' TO WS-DONE-PAY.
           PERFORM UNTIL WS-DONE-PAY = 'Y' OR WS-EXIT-PGM = 'Y'
                   OR WS-BACK-TO-MENU = 'Y'
               MOVE ZEROS TO CNFFMT-IND-AREA
               WRITE CNFFMT
               READ RAILDSPF RECORD
               EVALUATE TRUE
                   WHEN CNFFMT-IND(3) = '1'
                       MOVE 'Y' TO WS-DONE-PAY
                       MOVE 'Y' TO WS-EXIT-PGM
                   WHEN CNFFMT-IND(12) = '1'
                       MOVE 'Y' TO WS-DONE-PAY
                   WHEN OTHER
                       PERFORM 5300-FINALIZE-BOOKING
                       IF WS-BOOKING-OK = 'Y'
                           MOVE 'BOOKING CONFIRMED - PNR SHOWN ABOVE'
                               TO CNMSG1
                           MOVE ZEROS TO CNFFMT-IND-AREA
                           WRITE CNFFMT
                           READ RAILDSPF RECORD
                           MOVE 'Y' TO WS-DONE-PAY
                           MOVE 'Y' TO WS-BACK-TO-MENU
                       END-IF
               END-EVALUATE
           END-PERFORM.

       5300-FINALIZE-BOOKING.
           MOVE 'N' TO WS-BOOKING-OK.
           IF CNPAYMOD = SPACES OR CNPAYREF = SPACES
               MOVE 'PAYMENT MODE AND CARD/UPI/REF NUMBER ARE REQUIRED'
                   TO CNMSG1
           ELSE
               MOVE WS-SELECTED-TRNNO TO TP-TRNNO
               READ TRAINPF
                   INVALID KEY CONTINUE
               END-READ
               IF TP-AVLSEAT = 0
                   MOVE 'SEATS SOLD OUT - TRY ANOTHER TRAIN'
                       TO CNMSG1
               ELSE
                   COMPUTE WS-BOOKED-COUNT = TP-TOTSEAT - TP-AVLSEAT + 1
                   SUBTRACT 1 FROM TP-AVLSEAT
                   EVALUATE BKQUOTA
                       WHEN 'GN' SUBTRACT 1 FROM TP-GENSEAT
                       WHEN 'LD' SUBTRACT 1 FROM TP-LADSEAT
                       WHEN 'SC' SUBTRACT 1 FROM TP-SCTSEAT
                       WHEN OTHER CONTINUE
                   END-EVALUATE
                   REWRITE TRAINPF-REC
                   IF WS-TRN-STATUS NOT = '00'
                       MOVE 'ERROR UPDATING SEATS - TRY AGAIN'
                           TO CNMSG1
                   ELSE
                       PERFORM 5400-GENERATE-PNR
                       PERFORM 5500-GENERATE-TXN
                       MOVE WS-NEW-PNR     TO BP-PNRNO
                       MOVE WS-SELECTED-TRNNO TO BP-TRNNO
                       MOVE BKJRNDAT       TO BP-JRNDAT
                       MOVE WS-CURR-DATE   TO BP-BOOKDT
                       MOVE WS-CURR-TIME   TO BP-BOOKTM
                       MOVE WS-LOGIN-USRID TO BP-USRID
                       MOVE BKPSGNAM       TO BP-PSGNAM
                       MOVE BKPSGAGE       TO BP-PSGAGE
                       MOVE BKPSGSEX       TO BP-PSGSEX
                       MOVE BKQUOTA        TO BP-QUOTA
                       MOVE BKBRTHTY       TO BP-BERTHTY
                       MOVE WS-BOOKED-COUNT TO BP-BERTHNO
                       MOVE WS-COMPUTED-FARE TO BP-FARAMT
                       MOVE 'A'            TO BP-BOOKSTS
                       MOVE 0 TO BP-CANDT BP-CANTM BP-REFAMT
                       MOVE WS-NEW-TXN     TO BP-TXNNO
                       WRITE BOOKPF-REC
                       IF WS-BOK-STATUS NOT = '00'
                           MOVE 'ERROR SAVING BOOKING - SEAT RELEASED'
                               TO CNMSG1
                           ADD 1 TO TP-AVLSEAT
                           EVALUATE BKQUOTA
                               WHEN 'GN' ADD 1 TO TP-GENSEAT
                               WHEN 'LD' ADD 1 TO TP-LADSEAT
                               WHEN 'SC' ADD 1 TO TP-SCTSEAT
                               WHEN OTHER CONTINUE
                           END-EVALUATE
                           REWRITE TRAINPF-REC
                       ELSE
                           MOVE WS-NEW-TXN      TO PP-TXNNO
                           MOVE WS-NEW-PNR      TO PP-PNRNO
                           MOVE WS-LOGIN-USRID  TO PP-USRID
                           MOVE WS-COMPUTED-FARE TO PP-AMOUNT
                           MOVE CNPAYMOD        TO PP-PAYMOD
                           MOVE 'S'             TO PP-PAYSTS
                           MOVE WS-CURR-DATE    TO PP-PAYDT
                           MOVE WS-CURR-TIME    TO PP-PAYTM
                           MOVE CNPAYREF        TO PP-REFNO
                           WRITE PAYPF-REC
                           IF WS-PAY-STATUS NOT = '00'
                               MOVE 'PAYMENT ERROR - BOOKING REVERSED'
                                   TO CNMSG1
                               MOVE 'C' TO BP-BOOKSTS
                               REWRITE BOOKPF-REC
                               ADD 1 TO TP-AVLSEAT
                               EVALUATE BKQUOTA
                                   WHEN 'GN' ADD 1 TO TP-GENSEAT
                                   WHEN 'LD' ADD 1 TO TP-LADSEAT
                                   WHEN 'SC' ADD 1 TO TP-SCTSEAT
                                   WHEN OTHER CONTINUE
                               END-EVALUATE
                               REWRITE TRAINPF-REC
                           ELSE
                               MOVE WS-NEW-PNR TO CNPNRNO
                               MOVE 'Y' TO WS-BOOKING-OK
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

       5400-GENERATE-PNR.
           MOVE 9999999999 TO BP-PNRNO.
           START BOOKPF KEY IS NOT GREATER THAN BP-PNRNO
               INVALID KEY MOVE 'N' TO WS-FOUND
               NOT INVALID KEY MOVE 'Y' TO WS-FOUND
           END-START.
           IF WS-FOUND = 'Y'
               READ BOOKPF NEXT RECORD
                   AT END MOVE 'N' TO WS-FOUND
               END-READ
           END-IF.
           IF WS-FOUND = 'Y'
               COMPUTE WS-NEW-PNR = BP-PNRNO + 1
           ELSE
               MOVE 1000000001 TO WS-NEW-PNR
           END-IF.

       5500-GENERATE-TXN.
           MOVE WS-NEW-PNR TO WS-NEW-PNR-X.
           STRING 'TX' WS-CURR-DATE WS-NEW-PNR-X(9:2)
               DELIMITED BY SIZE INTO WS-NEW-TXN.

      ******************************************************************
      * 4. PNR GENERATION & TRACKING - PNR STATUS ENQUIRY
      ******************************************************************
       7000-PNR-STATUS.
           MOVE 'N' TO WS-BACK-TO-MENU.
           MOVE 0 TO PNPNRNO.
           MOVE SPACES TO PNMSG1.
           MOVE 'N' TO WS-DONE-PNR.
           PERFORM UNTIL WS-DONE-PNR = 'Y' OR WS-EXIT-PGM = 'Y'
                   OR WS-BACK-TO-MENU = 'Y'
               MOVE ZEROS TO PNRFMT-IND-AREA
               WRITE PNRFMT
               READ RAILDSPF RECORD
               EVALUATE TRUE
                   WHEN PNRFMT-IND(3) = '1'
                       MOVE 'Y' TO WS-DONE-PNR
                       MOVE 'Y' TO WS-EXIT-PGM
                   WHEN PNRFMT-IND(12) = '1'
                       MOVE 'Y' TO WS-DONE-PNR
                   WHEN OTHER
                       MOVE PNPNRNO TO BP-PNRNO
                       READ BOOKPF
                           INVALID KEY CONTINUE
                       END-READ
                       IF WS-BOK-STATUS NOT = '00'
                           MOVE 'PNR NOT FOUND - PLEASE RE-CHECK'
                               TO PNMSG1
                           MOVE SPACES TO PNTRNAM PNSRCST PNDSTST
                           MOVE SPACES TO PNPSGNAM PNPSGSEX PNQUOTA
                           MOVE SPACES TO PNBRTHTY PNBOKSTS
                           MOVE 0 TO PNJRNDAT PNPSGAGE PNBRTHNO
                           MOVE 0 TO PNFARAMT PNREFAMT
                       ELSE
                           MOVE BP-TRNNO TO TP-TRNNO
                           READ TRAINPF
                               INVALID KEY CONTINUE
                           END-READ
                           MOVE TP-TRNNAM  TO PNTRNAM
                           MOVE TP-SRCSTN  TO PNSRCST
                           MOVE TP-DSTSTN  TO PNDSTST
                           MOVE BP-JRNDAT  TO PNJRNDAT
                           MOVE BP-PSGNAM  TO PNPSGNAM
                           MOVE BP-PSGAGE  TO PNPSGAGE
                           MOVE BP-PSGSEX  TO PNPSGSEX
                           MOVE BP-QUOTA   TO PNQUOTA
                           MOVE BP-BERTHTY TO PNBRTHTY
                           MOVE BP-BERTHNO TO PNBRTHNO
                           MOVE BP-FARAMT  TO PNFARAMT
                           MOVE BP-BOOKSTS TO PNBOKSTS
                           MOVE BP-REFAMT  TO PNREFAMT
                           MOVE SPACES     TO PNMSG1
                       END-IF
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 6. CANCELLATION & REFUND
      ******************************************************************
       8000-CANCEL-TICKET.
           MOVE 'N' TO WS-BACK-TO-MENU.
           MOVE 0 TO CXPNRNO.
           MOVE SPACES TO CXMSG1.
           MOVE 'N' TO WS-DONE-CAN.
           PERFORM UNTIL WS-DONE-CAN = 'Y' OR WS-EXIT-PGM = 'Y'
                   OR WS-BACK-TO-MENU = 'Y'
               MOVE ZEROS TO CANFMT-IND-AREA
               WRITE CANFMT
               READ RAILDSPF RECORD
               EVALUATE TRUE
                   WHEN CANFMT-IND(3) = '1'
                       MOVE 'Y' TO WS-DONE-CAN
                       MOVE 'Y' TO WS-EXIT-PGM
                   WHEN CANFMT-IND(12) = '1'
                       MOVE 'Y' TO WS-DONE-CAN
                   WHEN OTHER
                       MOVE CXPNRNO TO BP-PNRNO
                       READ BOOKPF
                           INVALID KEY CONTINUE
                       END-READ
                       EVALUATE TRUE
                           WHEN WS-BOK-STATUS NOT = '00'
                               MOVE 'PNR NOT FOUND' TO CXMSG1
                           WHEN BP-BOOKSTS = 'C'
                               PERFORM 8050-SHOW-CANCEL-DETAIL
                               MOVE 'THIS TICKET IS ALREADY CANCELLED'
                                   TO CXMSG1
                           WHEN BP-USRID NOT = WS-LOGIN-USRID
                                   AND WS-LOGIN-USRTYP NOT = 'A'
                               MOVE 'PNR DOES NOT BELONG TO YOU'
                                   TO CXMSG1
                           WHEN CANFMT-IND(10) = '1'
                               PERFORM 8050-SHOW-CANCEL-DETAIL
                               PERFORM 8100-CALCULATE-REFUND
                               MOVE WS-REFUND-AMT TO CXREFAMT
                               PERFORM 8200-PROCESS-CANCELLATION
                               MOVE 'Y' TO WS-DONE-CAN
                               MOVE 'Y' TO WS-BACK-TO-MENU
                           WHEN OTHER
                               PERFORM 8050-SHOW-CANCEL-DETAIL
                               PERFORM 8100-CALCULATE-REFUND
                               MOVE WS-REFUND-AMT TO CXREFAMT
                               MOVE
                       'REFUND SHOWN ABOVE - PRESS F10 TO CONFIRM'
                                   TO CXMSG1
                       END-EVALUATE
               END-EVALUATE
           END-PERFORM.

       8050-SHOW-CANCEL-DETAIL.
           MOVE BP-TRNNO TO TP-TRNNO.
           READ TRAINPF
               INVALID KEY CONTINUE
           END-READ.
           MOVE TP-TRNNAM  TO CXTRNAM.
           MOVE BP-PSGNAM  TO CXPSGNAM.
           MOVE BP-JRNDAT  TO CXJRNDAT.
           MOVE BP-QUOTA   TO CXQUOTA.
           MOVE BP-FARAMT  TO CXFARAMT.
           MOVE BP-BOOKSTS TO CXBOKSTS.
           MOVE BP-REFAMT  TO CXREFAMT.

       8100-CALCULATE-REFUND.
           COMPUTE WS-JRN-INT = FUNCTION INTEGER-OF-DATE(BP-JRNDAT).
           COMPUTE WS-CUR-INT = FUNCTION INTEGER-OF-DATE(WS-CURR-DATE).
           COMPUTE WS-DAYS-DIFF = WS-JRN-INT - WS-CUR-INT.
           EVALUATE TRUE
               WHEN BP-QUOTA = 'TQ'
                   MOVE 0 TO WS-REFUND-AMT
               WHEN WS-DAYS-DIFF < 0
                   MOVE 0 TO WS-REFUND-AMT
               WHEN WS-DAYS-DIFF >= 2
                   COMPUTE WS-REFUND-AMT ROUNDED = BP-FARAMT * 0.90
               WHEN WS-DAYS-DIFF = 1
                   COMPUTE WS-REFUND-AMT ROUNDED = BP-FARAMT * 0.50
               WHEN OTHER
                   COMPUTE WS-REFUND-AMT ROUNDED = BP-FARAMT * 0.25
           END-EVALUATE.

       8200-PROCESS-CANCELLATION.
           MOVE 'C' TO BP-BOOKSTS.
           MOVE WS-CURR-DATE TO BP-CANDT.
           MOVE WS-CURR-TIME TO BP-CANTM.
           MOVE WS-REFUND-AMT TO BP-REFAMT.
           REWRITE BOOKPF-REC.
           IF WS-BOK-STATUS = '00'
               MOVE BP-TRNNO TO TP-TRNNO
               READ TRAINPF
                   INVALID KEY CONTINUE
               END-READ
               ADD 1 TO TP-AVLSEAT
               EVALUATE BP-QUOTA
                   WHEN 'GN' ADD 1 TO TP-GENSEAT
                   WHEN 'LD' ADD 1 TO TP-LADSEAT
                   WHEN 'SC' ADD 1 TO TP-SCTSEAT
                   WHEN OTHER CONTINUE
               END-EVALUATE
               REWRITE TRAINPF-REC
               MOVE BP-TXNNO TO PP-TXNNO
               READ PAYPF
                   INVALID KEY CONTINUE
               END-READ
               IF WS-PAY-STATUS = '00'
                   MOVE 'R' TO PP-PAYSTS
                   REWRITE PAYPF-REC
               END-IF
               MOVE 'CANCELLATION CONFIRMED - REFUND AMOUNT SHOWN ABOVE'
                   TO CXMSG1
               MOVE ZEROS TO CANFMT-IND-AREA
               WRITE CANFMT
               READ RAILDSPF RECORD
           ELSE
               MOVE 'ERROR PROCESSING CANCELLATION - TRY AGAIN'
                   TO CXMSG1
           END-IF.

      ******************************************************************
      * 7. ADMIN FUNCTIONALITY
      ******************************************************************
       9000-ADMIN-MENU.
           MOVE 'N' TO WS-BACK-TO-MENU.
           MOVE SPACES TO AMMSG1.
           MOVE 'N' TO WS-DONE-ADM.
           PERFORM UNTIL WS-DONE-ADM = 'Y' OR WS-EXIT-PGM = 'Y'
                   OR WS-BACK-TO-MENU = 'Y'
               MOVE ZEROS TO ADMMNU-IND-AREA
               WRITE ADMMNU
               READ RAILDSPF RECORD
               EVALUATE TRUE
                   WHEN ADMMNU-IND(3) = '1'
                       MOVE 'Y' TO WS-DONE-ADM
                       MOVE 'Y' TO WS-EXIT-PGM
                   WHEN ADMMNU-IND(12) = '1'
                       MOVE 'Y' TO WS-DONE-ADM
                   WHEN AMOPTION = 1
                       PERFORM 9100-ADMIN-TRAIN-MAINT
                   WHEN AMOPTION = 2
                       PERFORM 9200-ADMIN-REPORTS
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM.

       9100-ADMIN-TRAIN-MAINT.
           MOVE 0 TO ATTRNNO.
           MOVE SPACES TO ATTRNAM ATSRCST ATSRCCOD ATDSTST ATDSTCOD.
           MOVE SPACES TO ATRUNDAY ATMSG1.
           MOVE 0 TO ATDEPTIM ATARRTIM ATTOTSET ATGENSET ATLADSET.
           MOVE 0 TO ATSCTSET ATFARE.
           MOVE 'A' TO ATTRNSTS.
           MOVE 'N' TO WS-DONE-ATM.
           PERFORM UNTIL WS-DONE-ATM = 'Y' OR WS-EXIT-PGM = 'Y'
                   OR WS-BACK-TO-MENU = 'Y'
               MOVE ZEROS TO ADMTRN-IND-AREA
               WRITE ADMTRN
               READ RAILDSPF RECORD
               EVALUATE TRUE
                   WHEN ADMTRN-IND(3) = '1'
                       MOVE 'Y' TO WS-DONE-ATM
                       MOVE 'Y' TO WS-EXIT-PGM
                   WHEN ADMTRN-IND(12) = '1'
                       MOVE 'Y' TO WS-DONE-ATM
                   WHEN OTHER
                       PERFORM 9150-SAVE-TRAIN
               END-EVALUATE
           END-PERFORM.

       9150-SAVE-TRAIN.
           IF (ATGENSET + ATLADSET + ATSCTSET) NOT = ATTOTSET
               MOVE 'GEN + LADIES + SR.CITIZEN SEATS MUST EQUAL TOTAL'
                   TO ATMSG1
           ELSE
               MOVE 'N' TO WS-FOUND
               IF ATTRNNO NOT = 0
                   MOVE ATTRNNO TO TP-TRNNO
                   READ TRAINPF
                       INVALID KEY MOVE 'N' TO WS-FOUND
                       NOT INVALID KEY MOVE 'Y' TO WS-FOUND
                   END-READ
               END-IF
               IF WS-FOUND = 'Y'
                   COMPUTE WS-BOOKED-COUNT = TP-TOTSEAT - TP-AVLSEAT
                   IF ATTOTSET < WS-BOOKED-COUNT
                       MOVE
                       'CANNOT REDUCE SEATS BELOW CURRENT BOOKINGS'
                           TO ATMSG1
                   ELSE
                       MOVE ATTRNAM   TO TP-TRNNAM
                       MOVE ATSRCST   TO TP-SRCSTN
                       MOVE ATSRCCOD  TO TP-SRCCOD
                       MOVE ATDSTST   TO TP-DSTSTN
                       MOVE ATDSTCOD  TO TP-DSTCOD
                       MOVE ATDEPTIM  TO TP-DEPTIM
                       MOVE ATARRTIM  TO TP-ARRTIM
                       MOVE ATRUNDAY  TO TP-RUNDAY
                       MOVE ATTOTSET  TO TP-TOTSEAT
                       COMPUTE TP-AVLSEAT = ATTOTSET - WS-BOOKED-COUNT
                       MOVE ATGENSET  TO TP-GENSEAT
                       MOVE ATLADSET  TO TP-LADSEAT
                       MOVE ATSCTSET  TO TP-SCTSEAT
                       MOVE ATFARE    TO TP-FARE
                       MOVE ATTRNSTS  TO TP-TRNSTS
                       REWRITE TRAINPF-REC
                       IF WS-TRN-STATUS = '00'
                           MOVE 'TRAIN UPDATED SUCCESSFULLY' TO ATMSG1
                       ELSE
                           MOVE 'ERROR UPDATING TRAIN - TRY AGAIN'
                               TO ATMSG1
                       END-IF
                   END-IF
               ELSE
                   IF ATTRNNO = 0
                       PERFORM 9160-GENERATE-NEW-TRNNO
                   END-IF
                   MOVE ATTRNNO   TO TP-TRNNO
                   MOVE ATTRNAM   TO TP-TRNNAM
                   MOVE ATSRCST   TO TP-SRCSTN
                   MOVE ATSRCCOD  TO TP-SRCCOD
                   MOVE ATDSTST   TO TP-DSTSTN
                   MOVE ATDSTCOD  TO TP-DSTCOD
                   MOVE ATDEPTIM  TO TP-DEPTIM
                   MOVE ATARRTIM  TO TP-ARRTIM
                   MOVE ATRUNDAY  TO TP-RUNDAY
                   MOVE ATTOTSET  TO TP-TOTSEAT
                   MOVE ATTOTSET  TO TP-AVLSEAT
                   MOVE ATGENSET  TO TP-GENSEAT
                   MOVE ATLADSET  TO TP-LADSEAT
                   MOVE ATSCTSET  TO TP-SCTSEAT
                   MOVE ATFARE    TO TP-FARE
                   MOVE ATTRNSTS  TO TP-TRNSTS
                   WRITE TRAINPF-REC
                   IF WS-TRN-STATUS = '00'
                       MOVE TP-TRNNO TO ATTRNNO
                       MOVE
                       'NEW TRAIN ADDED SUCCESSFULLY'
                           TO ATMSG1
                   ELSE
                       MOVE 'ERROR ADDING NEW TRAIN - TRY AGAIN'
                           TO ATMSG1
                   END-IF
               END-IF
           END-IF.

       9160-GENERATE-NEW-TRNNO.
           MOVE 99999 TO TP-TRNNO.
           START TRAINPF KEY IS NOT GREATER THAN TP-TRNNO
               INVALID KEY MOVE 'N' TO WS-FOUND
               NOT INVALID KEY MOVE 'Y' TO WS-FOUND
           END-START.
           IF WS-FOUND = 'Y'
               READ TRAINPF NEXT RECORD
                   AT END MOVE 'N' TO WS-FOUND
               END-READ
           END-IF.
           IF WS-FOUND = 'Y'
               COMPUTE ATTRNNO = TP-TRNNO + 1
           ELSE
               MOVE 10001 TO ATTRNNO
           END-IF.

       9200-ADMIN-REPORTS.
           MOVE 0 TO RPTRNNO.
           MOVE SPACES TO RPMSG1.
           MOVE 'N' TO WS-DONE-RPT.
           PERFORM UNTIL WS-DONE-RPT = 'Y' OR WS-EXIT-PGM = 'Y'
                   OR WS-BACK-TO-MENU = 'Y'
               MOVE ZEROS TO RPTFMT-IND-AREA
               WRITE RPTFMT
               READ RAILDSPF RECORD
               EVALUATE TRUE
                   WHEN RPTFMT-IND(3) = '1'
                       MOVE 'Y' TO WS-DONE-RPT
                       MOVE 'Y' TO WS-EXIT-PGM
                   WHEN RPTFMT-IND(12) = '1'
                       MOVE 'Y' TO WS-DONE-RPT
                   WHEN OTHER
                       PERFORM 9250-RUN-REPORT
               END-EVALUATE
           END-PERFORM.

       9250-RUN-REPORT.
           MOVE 0 TO WS-RPT-TOTBK WS-RPT-ACTBK WS-RPT-CANBK.
           MOVE 0 TO WS-RPT-REV WS-RPT-REF WS-RPT-USRCNT.
           MOVE 0 TO BP-PNRNO.
           START BOOKPF KEY IS NOT LESS THAN BP-PNRNO
               INVALID KEY MOVE 'N' TO WS-FOUND
               NOT INVALID KEY MOVE 'Y' TO WS-FOUND
           END-START.
           IF WS-FOUND = 'Y'
               MOVE 'N' TO WS-EOF
               PERFORM UNTIL WS-EOF = 'Y'
                   READ BOOKPF NEXT RECORD
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           IF RPTRNNO = 0 OR BP-TRNNO = RPTRNNO
                               ADD 1 TO WS-RPT-TOTBK
                               ADD BP-FARAMT TO WS-RPT-REV
                               IF BP-BOOKSTS = 'A'
                                   ADD 1 TO WS-RPT-ACTBK
                               ELSE
                                   ADD 1 TO WS-RPT-CANBK
                                   ADD BP-REFAMT TO WS-RPT-REF
                               END-IF
                           END-IF
                   END-READ
               END-PERFORM
           END-IF.
           MOVE SPACES TO UP-USRID.
           START USERPF KEY IS NOT LESS THAN UP-USRID
               INVALID KEY MOVE 'N' TO WS-FOUND
               NOT INVALID KEY MOVE 'Y' TO WS-FOUND
           END-START.
           IF WS-FOUND = 'Y'
               MOVE 'N' TO WS-EOF
               PERFORM UNTIL WS-EOF = 'Y'
                   READ USERPF NEXT RECORD
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           ADD 1 TO WS-RPT-USRCNT
                   END-READ
               END-PERFORM
           END-IF.
           MOVE WS-RPT-TOTBK  TO RPTOTBK.
           MOVE WS-RPT-ACTBK  TO RPACTBK.
           MOVE WS-RPT-CANBK  TO RPCANBK.
           MOVE WS-RPT-REV    TO RPTOTREV.
           MOVE WS-RPT-REF    TO RPTOTREF.
           MOVE WS-RPT-USRCNT TO RPUSRCNT.
           MOVE 'REPORT GENERATED SUCCESSFULLY' TO RPMSG1.

      ******************************************************************
      * TERMINATION
      ******************************************************************
       9900-TERMINATE.
           CLOSE TRAINPF.
           CLOSE BOOKPF.
           CLOSE USERPF.
           CLOSE PAYPF.
           CLOSE RAILDSPF.
