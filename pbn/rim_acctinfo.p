{globals.i}
{sh-defs.i}
PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF VAR i         AS INT INIT 0 NO-UNDO.
DEF VAR date1     AS DATE INIT TODAY NO-UNDO.
DEF VAR date2     AS DATE INIT TODAY NO-UNDO.
DEF VAR sFilter   AS CHAR NO-UNDO.


DEF VAR str       AS CHAR NO-UNDO.
DEF VAR fname     AS CHAR NO-UNDO.
DEF VAR user_ip   AS CHAR FORMAT "X(15)" NO-UNDO.
DEF VAR fpath     AS CHAR INIT "/qbis/wrk/imp-exp/0000/preview/" NO-UNDO.
user_ip=TRIM(SUBSTRING(OS-GETENV("SSH_CLIENT"),1,INDEX(OS-GETENV("SSH_CLIENT")," "))).
fname = fpath + "_tmp@" + user_ip.


DEF TEMP-TABLE ttRec NO-UNDO
    FIELD fAcct     AS CHAR
    FIELD fAcctName AS CHAR
    FIELD fClName   AS CHAR
    FIELD fClUNKG   AS CHAR
    FIELD fClKod    AS CHAR
    FIELD OBDT      AS DEC
    FIELD OBKT      AS DEC
    FIELD is_dep    AS CHAR
.


/* Имя клиента + номер */
FUNCTION GetNameID RETURN CHAR (INPUT iID AS INT):
  DEF VAR result AS CHAR NO-UNDO.
  FIND FIRST person WHERE person.person-id EQ iID NO-LOCK NO-ERROR.
  IF AVAIL person THEN result = STRING(person.person-id, ">>>>>>9") + "|" + person.name-last + " " + person.first-names.
  FIND FIRST cust-corp WHERE INT64(cust-corp.cust-id) EQ iID NO-LOCK NO-ERROR.
  IF AVAIL cust-corp THEN 
  DO:
      IF cust-corp.name-short EQ "" THEN
      result = STRING(cust-corp.cust-id, ">>>>>>9") + "|" + cust-corp.name-corp.
      ELSE
      result = STRING(cust-corp.cust-id, ">>>>>>9") + "|" + cust-corp.cust-stat + " " + cust-corp.name-short.
  END.
  RETURN TRIM(result).
END.


/* Получаем УНКг клиента */
FUNCTION Get_UNKg RETURN CHAR (INPUT iID AS INT):
  DEF VAR result AS CHAR NO-UNDO.
  FIND FIRST person WHERE person.person-id EQ iID NO-LOCK NO-ERROR.
  IF AVAIL person THEN result = STRING(GetXAttrValueEx("person", String(iID), "УНКг", "")).
  FIND FIRST cust-corp WHERE INT64(cust-corp.cust-id) EQ iID NO-LOCK NO-ERROR.
  IF AVAIL cust-corp THEN result = STRING(GetXAttrValueEx("cust-corp", String(iID), "УНКг", "")).
  IF result EQ ? THEN result = "".
  RETURN TRIM(result).
END.


/* форма */
FORM
SKIP(1)
date1
FORMAT "99.99.9999"
LABEL  '    Дата начальная'
HELP   'Введите дату начала'
SKIP
date2
FORMAT "99.99.9999"
LABEL  '    Дата  конечная'
HELP   'Введите дату конца'
SKIP
WITH FRAME fr_main WIDTH 35 CENTERED ROW 10 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[Отчет счетам]".
UPDATE date1 date2 WITH FRAME fr_main.


{tmprecid.def}
FIND FIRST tmprecid NO-LOCK NO-ERROR.
IF NOT AVAIL tmprecid THEN
DO:
      MESSAGE "Не выбраны счета!" VIEW-AS ALERT-BOX.
      RETURN.
END.


FOR EACH tmprecid NO-LOCK,
    FIRST acct WHERE RECID(acct) EQ tmprecid.id NO-LOCK:
    CREATE ttRec.
    ASSIGN
        ttRec.fAcct     = SUBSTRING(acct.number,1,20)
        ttRec.fAcctName = IF acct.details EQ ? THEN "?" ELSE TRIM(REPLACE(acct.details,"|",""))
        ttRec.fClName   = GetNameID(acct.cust-id)
        ttRec.fClUNKG   = Get_UNKg(acct.cust-id)
    .
    /* Обороты за период */
    RUN acct-pos IN h_base ( acct.acct,
                             acct.currency,
                             date1,
                             date2,
                             CHR(251)).
    ttRec.OBDT = sh-db.
    ttRec.OBKT = sh-cr.
    FOR EACH loan WHERE CAN-DO("l_att_deal,l_att_d_max,l_att_line,l_att_overnight,l_att_qline,l_att_veks,loan-repo-bm,l_att_d_acct,l_att_d_depo,l_att_d_kred,l_att_d_mbd,l_att_d_mbk,l_att_prs,l_att_l_bill,l_att_l_sec,l_att_q_curr,l_att_q_pape,l_att_d_mbdov,l_att_d_mbkov,loan_attract", loan.class-code)
       AND loan.contract EQ 'Депоз'
       AND loan.cust-id EQ acct.cust-id
       NO-LOCK.
        ttRec.is_dep = ttRec.is_dep + ";" + "Есть депозит".
    END.
END.


OS-DELETE VALUE(fname).
OUTPUT TO VALUE(fname) CONVERT TARGET "1251".
PUT UNFORMATTED "kdz_acctinfo" SKIP.
PUT UNFORMATTED "Счет|Наименование счета|Код клиента|Наименование клиента|УНКг клиента|Обороты по ДТ|Обороты по КТ|Наличие депозита" SKIP.
FOR EACH ttRec NO-LOCK BY ttRec.fClUNKG.
    str = ttRec.fAcct     + "|" +
          ttRec.fAcctName + "|" +
          ttRec.fClName   + "|" +
          ttRec.fClUNKG   + "|" +
          REPLACE(TRIM(String(ABSOLUTE(ttRec.OBDT) , "zzzzzzzzzzzz9.99")), "." , "," ) + "|" +
          REPLACE(TRIM(String(ABSOLUTE(ttRec.OBKT) , "zzzzzzzzzzzz9.99")), "." , "," ) + "|" +
          ttRec.is_dep
    .
    ASSIGN
        i = i + 1
        sFilter = sFilter + ttRec.fClUNKG + ","
    .
/*
    IF i EQ 500 THEN DO:
      sFilter = TRIM( sFilter, "," ) + CHR(10).
      i = 0.
    END.
*/
    PUT UNFORMATTED str SKIP.
END. 
PUT "Фильтр:|||||||" SKIP.
sFilter = TRIM( sFilter, "," ) + "|||||||".
PUT UNFORMATTED sFilter SKIP.
OUTPUT CLOSE.

MESSAGE "Файл " + fname + " сформирован !!!" VIEW-AS ALERT-BOX.