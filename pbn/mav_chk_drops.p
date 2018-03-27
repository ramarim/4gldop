/*                                                                                                    */
/*                                                                                                    */
/*                                      �ய-����஫� ���㬥�⮢                                      */
/*                                                                                                    */
/*                                                                                                    */

PAUSE 0 BEFORE-HIDE NO-MESSAGE.

&GLOB f_IN "/qbis/wrk/imp-exp/0000/monitor/drops.txt"

DEF STREAM s_IN.
DEF VAR i       AS INT                  NO-UNDO.
DEF VAR j1      AS INT                  NO-UNDO.
DEF VAR j2      AS INT                  NO-UNDO.
DEF VAR i_      AS CHAR FORMAT "X(6)"   NO-UNDO.
DEF VAR bic     AS CHAR FORMAT "X(9)"   NO-UNDO.
DEF VAR msg     AS CHAR FORMAT "X(48)"  NO-UNDO.
DEF VAR msg_1   AS CHAR FORMAT "X(5)"   NO-UNDO.
DEF VAR msg_2   AS CHAR FORMAT "X(5)"   NO-UNDO.
DEF VAR msg_3   AS CHAR FORMAT "X(5)"   NO-UNDO.
DEF VAR inn     AS CHAR FORMAT "X(12)"  NO-UNDO.
DEF VAR inn_b   AS CHAR                 NO-UNDO.
DEF VAR acct    AS CHAR FORMAT "X(20)"  NO-UNDO.
DEF VAR corr    AS CHAR FORMAT "X(20)"  NO-UNDO.
DEF VAR str     AS CHAR FORMAT "X(167)" NO-UNDO.
DEF VAR str_lst AS CHAR                 NO-UNDO.
DEF VAR chk     AS LOG                  NO-UNDO.
DEF VAR sum     AS DEC                  NO-UNDO.

DEF TEMP-TABLE ttDrops NO-UNDO
FIELD acct AS CHAR FORMAT "x(20)"
FIELD bic  AS CHAR FORMAT "x(9)"
FIELD inn  AS CHAR FORMAT "x(12)"
.

FUNCTION Put2Log RETURN CHAR (INPUT iStr AS CHAR, iDoc AS INT, iChk AS INT):
  DEF VAR fpath  AS CHAR NO-UNDO.
  DEF VAR fname  AS CHAR NO-UNDO.
  
  ASSIGN
  iStr  = TRIM(iStr, CHR(10))
  fpath = "/qbis/wrk/imp-exp/0000/monitor/"
  fname = fpath + 
          "log_" +
          STRING(YEAR(TODAY), "9999") +
          STRING(MONTH(TODAY), "99") +
          STRING(DAY(TODAY), "99")
  .
  
  DEF VAR dt      AS CHAR FORMAT "x(19)" NO-UNDO.
  DEF VAR doc     AS CHAR FORMAT "x(7)"  NO-UNDO.
  DEF VAR doc_all AS CHAR FORMAT "x(11)" NO-UNDO.
  DEF VAR chk_all AS CHAR FORMAT "x(11)" NO-UNDO.
  
  ASSIGN
  dt      = STRING(TODAY, "99.99.9999") + " " + STRING(TIME, "HH:MM:SS")
  doc     = STRING(NUM-ENTRIES(iStr, CHR(10)))
  doc_all = STRING(iDoc)
  chk_all = STRING(iChk)
  .
  
  IF SEARCH(fname) NE ? THEN DO:
    OUTPUT TO VALUE(fname) APPEND.
  END.
  
  IF SEARCH(fname) EQ ? THEN DO:
    OUTPUT TO VALUE(fname).
  END.
  
  PUT " [" dt              " ������� �������� �����]                                                        " SKIP.
  PUT "  ��� ���. N ���.      �㬬�          ���           �/���               �/���            ���      " SKIP.
  PUT " ---------- ------ ---------------- --------- -------------------- -------------------- ------------ " SKIP.
  REPEAT i = 1 TO NUM-ENTRIES(iStr, CHR(10)):
    PUT ENTRY(i, iStr, CHR(10)) FORMAT "x(103)" SKIP.
  END.
  
  dt = STRING(TODAY, "99.99.9999") + " " + STRING(TIME, "HH:MM:SS").
  
  PUT " --------------------------------------------------------------------------------------------------- " SKIP.
  PUT " ������� ���㬥�⮢  : " doc "                                                                       " SKIP.
  PUT " �஢�७� ���㬥�⮢: " doc_all "                                                                   " SKIP.
  PUT " ��᫮ �஢�ப      : " chk_all "                                                                   " SKIP.
  PUT " --------------------------------------------------------------------------------------------------- " SKIP.
  PUT " [" dt              " ������� �������� ��������]                                                     " SKIP.
  PUT "                                                                                                     " SKIP.
  PUT "                                                                                                     " SKIP.
  OUTPUT CLOSE.
  RETURN "OK!".
END.

/*----------------------------------------------------------------------------------------------------*/

IF SEARCH({&f_IN}) NE ? THEN DO:
  INPUT STREAM s_IN FROM {&f_IN}.
  REPEAT:
    CREATE ttDrops.
    IMPORT STREAM s_IN ttDrops NO-ERROR.
  END.
  DELETE ttDrops.
  INPUT STREAM s_IN CLOSE.
  
  {setdest.i}
  PUT "                                                                                                                                                                        " SKIP.
  PUT "  ����-�������� ���������� (���,�/���,�/���,���)                                                                                                                      " SKIP.
  PUT "                                                                                                                                                                        " SKIP.
  PUT " ��������������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ " SKIP.
  PUT " �            �        �                  �                               ����������                               �                                                  � " SKIP.
  PUT " �  ��� ���. � N ���. �      �㬬�       ������������������������������������������������������������������������Ĵ  ������� �஢�ન                              � " SKIP.
  PUT " �            �        �                  �    ���    �        �/���        �        �/���        �     ���      �                                                  � " SKIP.
  PUT " ��������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ " SKIP.
  
  {tmprecid.def}
  FOR EACH tmprecid NO-LOCK,
                    FIRST op WHERE RECID(op) = tmprecid.id NO-LOCK,
                    FIRST op-entry WHERE op-entry.op = op.op NO-LOCK,
                    FIRST op-bank WHERE op-bank.op = op.op NO-LOCK.
  
    ASSIGN
    chk   = TRUE
    bic   = TRIM(op-bank.bank-code)
    corr  = TRIM(SUBSTR(op-bank.corr-acct, 1, 20))
    acct  = TRIM(SUBSTR(op.ben-acct, 1, 20))
    inn   = TRIM(op.inn)
    inn_b = ""
    sum   = op-entry.amt-rub
    msg   = ""
    msg_1 = ""
    msg_2 = ""
    msg_3 = ""
    j1    = j1 + 1
    .
    
    FIND FIRST cust-ident WHERE cust-ident.cust-cat = "�"
                            AND cust-ident.cust-code-type = "���"
                            AND cust-ident.cust-code <> ""
                            AND cust-ident.cust-code = inn NO-LOCK NO-ERROR.
    IF AVAIL cust-ident THEN inn_b = cust-ident.cust-code.
    
    FOR EACH ttDrops WHERE (ttDrops.inn  = inn  AND ttDrops.inn <> inn_b) 
                        OR (ttDrops.acct = acct AND ttDrops.bic  = bic) NO-LOCK.
      IF ttDrops.acct = acct AND ttDrops.bic  = bic THEN DO:
        ASSIGN
        msg_1 = "[���]"
        msg_2 = "[�/�]"
        .
      END.
      IF ttDrops.inn  = inn AND ttDrops.inn <> inn_b THEN msg_3 = "[���]".
      IF msg_1 = "[���]" AND 
         msg_2 = "[�/�]" AND 
         msg_3 = "[���]" THEN LEAVE.
    END.
    
    msg = TRIM(msg_1 + " " + msg_2 + " " + msg_3).
    
    IF msg <> "" THEN DO:
      msg = "������⥫�� ���㬥��!" + " " + msg.
      RUN chst-op.p (RECID(op), "�") NO-ERROR.
      IF ERROR-STATUS:ERROR OR RETURN-VALUE <> "" THEN DO:
        MESSAGE " �� 㤠���� �������� ����� ���㬥��-���筨��." SKIP
        RETURN-VALUE 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN ERROR.
      END.
      
      IF bic  = ? THEN bic  = "".
      IF corr = ? THEN corr = "".
      IF acct = ? THEN acct = "".
      IF inn  = ? THEN inn  = "".
        
      ASSIGN
      i = i + 1
      str = " � " + STRING(op.op-date, "99.99.9999") + " � " +
                    STRING(op.doc-num, "x(6)")       + " � " +
                    STRING(sum, "z,zzz,zzz,zz9.99")  + " � " +
                    STRING(bic, "x(9)")              + " � " +
                    STRING(corr, "x(20)")            + " � " +
                    STRING(acct, "x(20)")            + " � " +
                    STRING(inn, "x(12)")             + " � " +
                    STRING(msg, "x(48)")             + " � "
      .
      PUT str SKIP.
      str = " " + STRING(op.op-date, "99.99.9999") + " " +
                  STRING(op.doc-num, "x(6)")       + " " +
                  STRING(sum, "z,zzz,zzz,zz9.99")  + " " +
                  STRING(bic, "x(9)")              + " " +
                  STRING(corr, "x(20)")            + " " +
                  STRING(acct, "x(20)")            + " " +
                  STRING(inn, "x(12)")             + " "
                  .
      str_lst = str_lst + str + CHR(10).
    END.
  END.
  
  i_ = STRING(i,"zzzzz9").
  
  PUT " ��������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ " SKIP.
  PUT " � �⮣� ���㬥�⮢ :" i_ "                                                                                        � ���㬥��� ��ॢ����� � ����� '�'                � " SKIP.
  PUT " ���������������������������������������������������������������������������������������������������������������������������������������������������������������������� " SKIP.
  PUT "                                                                                                                                                                        " SKIP.
  {preview.i}

  FOR EACH ttDrops NO-LOCK.
    j2 = j2 + 1.
  END.

  Put2Log(str_lst, j1, j2).
END.
ELSE MESSAGE "���� " + {&f_IN} + " �� ������!" VIEW-AS ALERT-BOX.


/*
�����������������Ŀ
�����������������Ĵ
�        �        �
�������������������
*/