/*                                                                                                    */
/*                                                                                                    */
/*                               ���� �� ������樨 �����⮢ �� ��ਮ�                               */
/*                                                                                                    */
/*                                                                                                    */

PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF VAR dateBeg AS DATE            NO-UNDO.
DEF VAR dateEnd AS DATE INIT TODAY NO-UNDO.

dateBeg = DATE(MONTH(TODAY),01,YEAR(TODAY)).

FORM
SKIP(1)
dateBeg
FORMAT "99.99.9999"
LABEL  ' ��ਮ� � '
HELP   '������ ���� ��砫� ��ਮ��.'
dateEnd
FORMAT "99.99.9999"
LABEL  '��'
HELP   '������ ���� ����砭�� ��ਮ��.'
SKIP
SKIP(1)
WITH FRAME fr_main WIDTH 40 CENTERED ROW 10 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[�������� �����⮢]".
UPDATE dateBeg dateEnd WITH FRAME fr_main.

/*----------------------------------------------------------------------------------------------------*/

DEF VAR str     AS CHAR FORMAT "x(109)"               NO-UNDO.
DEF VAR date_b  AS CHAR FORMAT "x(10)"                NO-UNDO.
DEF VAR date_e  AS CHAR FORMAT "x(10)"                NO-UNDO.
DEF VAR summ    AS DEC                                NO-UNDO.
DEF VAR ink_tl  AS DEC  FORMAT "z,zzz,zzz,zz9.99"     NO-UNDO.
DEF VAR kom_tl  AS DEC  FORMAT "z,zzz,zzz,zz9.99"     NO-UNDO.
DEF VAR acct_tl AS INT  FORMAT "zzzzzzzzzzzzzzzzzzz9" NO-UNDO.


DEF TEMP-TABLE ttOp NO-UNDO
FIELD name      AS CHAR FORMAT "x(40)"
FIELD acct      LIKE op-entry.acct-db
FIELD summ_ink  LIKE op-entry.amt-rub
FIELD summ_kom  LIKE op-entry.amt-rub
.

ASSIGN
date_b = STRING(dateBeg,"99.99.9999")
date_e = STRING(dateEnd,"99.99.9999")
.

FOR EACH op WHERE op.op-date >= dateBeg
              AND op.op-date <= dateEnd
              AND CAN-DO("�50415",op.op-kind) NO-LOCK,
              FIRST op-entry WHERE op-entry.op = op.op NO-LOCK BREAK BY op-entry.acct-db.

  IF NOT LAST-OF(op-entry.acct-db) THEN DO:
    summ = summ + op-entry.amt-rub.
  END.

  IF LAST-OF(op-entry.acct-db) THEN DO:
    CREATE ttOp.
    ASSIGN
    summ = summ + op-entry.amt-rub.
    ttOp.acct = op-entry.acct-db.
    ttOp.summ_kom = summ.
    summ = 0.
    .
  END.
END.


FOR EACH ttOp NO-LOCK.
  FOR EACH op WHERE op.op-date >= dateBeg
                AND op.op-date <= dateEnd
                AND CAN-DO("030100021,030100022,030310a,030100023,0303101a",op.op-kind) NO-LOCK,
                FIRST op-entry WHERE op-entry.op = op.op
                                 AND CAN-DO("20202*,40906..........." + SUBSTR(ttOp.acct,17,4),SUBSTR(op-entry.acct-db,1,20))
                                 AND op-entry.acct-cr BEGINS ttOp.acct NO-LOCK BREAK BY op-entry.acct-cr.

    IF NOT LAST-OF(op-entry.acct-cr) THEN DO:
      summ = summ + op-entry.amt-rub.
    END.

    IF LAST-OF(op-entry.acct-cr) THEN DO:
      ASSIGN
      summ = summ + op-entry.amt-rub.
      ttOp.summ_ink = summ.
      summ = 0.
      .
    END.
  END.
END.


FOR EACH ttOp NO-LOCK, FIRST acct WHERE acct.acct BEGINS ttOp.acct NO-LOCK.
  IF ttOp.acct BEGINS "407" THEN DO:
    FIND FIRST cust-corp WHERE cust-corp.cust-id = acct.cust-id NO-LOCK NO-ERROR.
    IF AVAIL cust-corp THEN DO:
      ttOp.name = cust-stat + " " + cust-corp.name-short.
    END.
  END.
  IF ttOp.acct BEGINS "408" THEN DO:
    FIND FIRST person WHERE person.person-id = acct.cust-id NO-LOCK NO-ERROR.
    IF AVAIL person THEN DO:
      ttOp.name = name-last + " " + first-names.
    END.
  END.
END.


{setdest.i}
PUT "                                                                                                           " SKIP.
PUT "                                           ���������� ��������                                             " SKIP.
PUT "                                                                                                           " SKIP.
PUT "  ��ਮ� c " date_b " �� " date_e "                                                                        " SKIP.
PUT "                                                                                                           " SKIP.
PUT " �������������������������������������������������������������������������������������������������������Ŀ " SKIP.
PUT " �           ������������ ������           �     ����� ���      � �㬬� ������樨 �  �㬬� �����ᨨ  � " SKIP.
PUT " �������������������������������������������������������������������������������������������������������Ĵ " SKIP.

FOR EACH ttOp NO-LOCK.
  ASSIGN 
  acct_tl = acct_tl + 1
  ink_tl = ink_tl + ttOp.summ_ink
  kom_tl = kom_tl + ttOp.summ_kom
  .
  str = " � " + STRING(ttOp.name,"x(40)") +
        " � " + STRING(ttOp.acct,"x(20)") +
        " � " + STRING(ttOp.summ_ink,"z,zzz,zzz,zz9.99") +
        " � " + STRING(ttOp.summ_kom,"z,zzz,zzz,zz9.99") +
        " � "
        .
  PUT str SKIP.
END.

PUT " �������������������������������������������������������������������������������������������������������Ĵ " SKIP.
PUT " � ����� :                                  � "          acct_tl " � "       ink_tl " � "       kom_tl " � " SKIP.
PUT " ��������������������������������������������������������������������������������������������������������� " SKIP.
PUT "                                                                                                           " SKIP.
PUT "                                                                                                           " SKIP.
{preview.i}



/*
""
�����������������Ŀ
�����������������Ĵ
�        �        �
�������������������
*/