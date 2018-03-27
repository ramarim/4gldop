/*                                                                                                    */
/*                                                                                                    */
/*                                  Выписка для ООО "Дес"                                             */
/*                                                                                                    */
/*                                                                                                    */

PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF VAR date1 AS DATE INIT TODAY NO-UNDO.
DEF VAR date2 AS DATE INIT TODAY NO-UNDO.

FORM
SPACE(2)
date1
FORMAT "99.99.9999"
LABEL  'Дата начала   '
HELP   'Введите дату начала периода.'
SKIP
SPACE(2)
date2
FORMAT "99.99.9999"
LABEL  'Дата окончания'
HELP   'Введите дату окончания периода.'
SPACE(2)
SKIP

WITH FRAME fr_main CENTERED ROW 10 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[ ВЫПИСКА ПО СЧЕТУ ]".
UPDATE date1 date2 WITH FRAME fr_main.

DEF VAR i                 AS INT                  NO-UNDO.
DEF VAR sum               AS DEC                  NO-UNDO.
DEF VAR date1_            AS CHAR FORMAT "x(10)"  NO-UNDO.
DEF VAR date2_            AS CHAR FORMAT "x(10)"  NO-UNDO.
DEF VAR acct_cust         AS CHAR FORMAT "x(20)"  NO-UNDO.
DEF VAR acct_cust_details AS CHAR FORMAT "x(109)" NO-UNDO.
DEF VAR acct_corr         AS CHAR FORMAT "x(20)"  NO-UNDO.
DEF VAR ost_in            AS CHAR FORMAT "x(17)"  NO-UNDO.
DEF VAR ost_out           AS CHAR FORMAT "x(17)"  NO-UNDO.
DEF VAR ob_dt             AS CHAR FORMAT "x(16)"  NO-UNDO.
DEF VAR ob_kt             AS CHAR FORMAT "x(16)"  NO-UNDO.
DEF VAR details_          AS CHAR FORMAT "x(25)"  NO-UNDO.
DEF VAR details_lst       AS CHAR                 NO-UNDO.
DEF VAR row_data          AS CHAR                 NO-UNDO.
DEF VAR sum_dt            AS DEC                  NO-UNDO.
DEF VAR sum_kt            AS DEC                  NO-UNDO.
DEF VAR count_dt          AS INT                  NO-UNDO.
DEF VAR count_kt          AS INT                  NO-UNDO.
DEF VAR sum_dt_           AS CHAR FORMAT "x(16)"  NO-UNDO.
DEF VAR sum_kt_           AS CHAR FORMAT "x(16)"  NO-UNDO.
DEF VAR count_dt_         AS CHAR FORMAT "x(13)"  NO-UNDO.
DEF VAR count_kt_         AS CHAR FORMAT "x(13)"  NO-UNDO.

DEF TEMP-TABLE ttDoc NO-UNDO
FIELD op-date      AS DATE FORMAT "99.99.9999"
FIELD doc-num      LIKE op.doc-num
FIELD doc-type     LIKE op.doc-type
FIELD bank-code    AS CHAR FORMAT "x(9)"
FIELD ben-acct     LIKE op.ben-acct
FIELD acct-db      AS CHAR FORMAT "x(20)"
FIELD acct-cr      AS CHAR FORMAT "x(20)"
FIELD amt-rub      LIKE op-entry.amt-rub
FIELD details      LIKE op.details
FIELD num-dev      AS CHAR FORMAT "x(7)"
.

{globals.i}
{sh-defs.i}
{tmprecid.def}

FUNCTION StrWrap RETURN CHAR (INPUT iStr AS CHAR, INPUT iLen AS INT, INPUT iChr AS CHAR):
  DEF VAR i      AS INT  NO-UNDO.
  DEF VAR j      AS INT  NO-UNDO.
  DEF VAR n      AS INT  NO-UNDO. 
  DEF VAR result AS CHAR NO-UNDO.
  ASSIGN
  j = 1
  iStr = REPLACE(iStr,iChr,"")
  iStr = REPLACE(iStr,CHR(10),"") 
  n = LENGTH(iStr) / iLen
  .
  IF n * iLen < LENGTH(iStr) THEN n = n + 1. 
  DO i = 1 TO n:
    ASSIGN
    result = result + SUBSTRING(iStr,j,iLen) + iChr
    j = j + iLen
    .
  END.
  RETURN TRIM(result,iChr).
END.

FUNCTION GetItemDetails RETURN CHAR (INPUT iDetails AS CHAR, 
                                     INPUT iFind    AS CHAR, 
                                     INPUT iLen     AS INT):
  DEF VAR result AS CHAR NO-UNDO.
  DEF VAR pos    AS INT  NO-UNDO.
  
  ASSIGN
  pos = INDEX(iDetails, iFind)
  result = SUBSTR(iDetails, pos + LENGTH(iFind), iLen)
  .
  
  RETURN result.
END.

FUNCTION GetDevAdres RETURN CHAR (INPUT iNumDev AS CHAR):
  DEF VAR result AS CHAR NO-UNDO.
  
  FOR EACH loan WHERE loan.contract = "card-equip"
                  AND CAN-DO("card-equip",loan.class-code)
                  AND loan.doc-num = iNumDev
                  NO-LOCK.
    result = TRIM(loan.user-o[1]).
  END.
  
  RETURN result.
END.

/*----------------------------------------------------------------------------------------------------*/

ASSIGN
date1_ = STRING(date1, "99.99.9999")
date2_ = STRING(date2, "99.99.9999")
.

FOR EACH tmprecid NO-LOCK, FIRST acct WHERE RECID(acct) = tmprecid.id NO-LOCK.
  ASSIGN
  acct_cust         = acct.acct
  acct_cust_details = acct.details
  .
  
  RUN acct-pos IN h_base (acct.acct, acct.currency, date1 - 1, date1 - 1, ?).
  IF sh-bal  < 0 THEN ost_in = STRING(ABS(sh-bal), "-z,zzz,zzz,zz9.99").
  IF sh-bal >= 0 THEN ost_in = STRING(sh-bal, "-z,zzz,zzz,zz9.99").
  
  RUN acct-pos IN h_base (acct.acct, acct.currency, date2, date2, ?).
  IF sh-bal  < 0 THEN ost_out = STRING(ABS(sh-bal), "-z,zzz,zzz,zz9.99").
  IF sh-bal >= 0 THEN ost_out = STRING(sh-bal, "-z,zzz,zzz,zz9.99").
END.

FOR EACH op-entry WHERE op-entry.op-date >= date1 
                    AND op-entry.op-date <= date2 
                    AND op-entry.op-status BEGINS "√"
                    AND (op-entry.acct-db = acct_cust OR op-entry.acct-cr = acct_cust) NO-LOCK,
                    FIRST op WHERE op.op = op-entry.op NO-LOCK.
  CREATE ttDoc.
  ASSIGN
  ttDoc.op-date   = op.op-date
  ttDoc.doc-num   = op.doc-num
  ttDoc.doc-type  = op.doc-type
  ttDoc.bank-code = ""
  ttDoc.ben-acct  = op.ben-acct
  ttDoc.acct-db   = op-entry.acct-db
  ttDoc.acct-cr   = op-entry.acct-cr
  ttDoc.amt-rub   = op-entry.amt-rub
  ttDoc.details   = op.details
  ttDoc.num-dev   = GetItemDetails(op.details, "Номер POS-терминала ", 7)
  .
  
  FOR FIRST op-bank WHERE op-bank.op = op.op NO-LOCK.
    ttDoc.bank-code = op-bank.bank-code.
  END.
END.

FOR EACH ttDoc WHERE CAN-DO("30232810800000000231*,30232810000000000031*", ttDoc.acct-db) NO-LOCK 
                     BREAK BY ttDoc.acct-db 
                           BY ttDoc.num-dev
                           BY ttDoc.op-date.
  
  IF NOT LAST-OF(ttDoc.num-dev) THEN DO:
    sum = sum + ttDoc.amt-rub.
    DELETE ttDoc.
  END.
  
  IF LAST-OF(ttDoc.num-dev) THEN DO:
    ASSIGN
    ttDoc.doc-num = ""
    ttDoc.amt-rub = sum + ttDoc.amt-rub
    ttDoc.details = "Перевод средств по операциям с использованием карт согласно договору об эквайринговом обслуживании. " + 
                    "Номер POS-терминала " + ttDoc.num-dev + " (магазин, " + GetDevAdres(ttDoc.num-dev) + ")"
    sum           = 0
    .
  END.
END.

{setdest.i}

PUT " АО КБ 'ОБРАЗЕЦ'                                                                                                                      " SKIP.
PUT "                                                                                                                                      " SKIP.
PUT "      ЛИЦЕВОЙ СЧЕТ                                       " acct_cust        "                                                         " SKIP.
PUT "                                                                                                                                      " SKIP.
PUT "   Наименование счета : " acct_cust_details                                                                                         " " SKIP.
PUT "                                                                                                                                      " SKIP.
PUT "   Период : с " date1_ " по " date2_ "                                                                                                " SKIP.
PUT "                                                                                                                                      " SKIP.
PUT "   Сальдо на начало дня : " ost_in        "                                                                                           " SKIP.
PUT " ┌────────────┬────────┬───────┬───────────┬──────────────────────┬─────────────────────────────────────┬───────────────────────────┐ " SKIP.
PUT " │    Дата    │ Номер  │  Код  │    Код    │   Корреспондирующий  │        Обороты в нац. валюте        │    Содержание операции    │ " SKIP.
PUT " │  опер.дня  │ докум. │  док. │   банка   │         счет         │      Дебет              Кредит      │                           │ " SKIP.
PUT " ├────────────┼────────┼───────┼───────────┼──────────────────────┼──────────────────┬──────────────────┼───────────────────────────┤ " SKIP.

FOR EACH ttDoc NO-LOCK BY ttDoc.op-date.
  ASSIGN
  acct_corr    = ""
  ob_dt        = ""
  ob_kt        = ""
  details_lst  = StrWrap(ttDoc.details, 25, "|")
  .
  
  IF ttDoc.acct-db = acct_cust THEN DO:
    ASSIGN
    acct_corr = ttDoc.acct-cr
    ob_dt     = STRING(ttDoc.amt-rub, "z,zzz,zzz,zz9.99")
    count_dt  = count_dt + 1
    sum_dt    = sum_dt + ttDoc.amt-rub
    .
  END.
  
  IF ttDoc.acct-cr = acct_cust THEN DO:
    ASSIGN
    acct_corr = ttDoc.acct-db
    ob_kt     = STRING(ttDoc.amt-rub, "z,zzz,zzz,zz9.99")
    count_kt  = count_kt + 1
    sum_kt    = sum_kt + ttDoc.amt-rub
    .
  END.
  
  IF ttDoc.ben-acct <> "" THEN acct_corr = ttDoc.ben-acct.
  
  row_data = " │ " + STRING(ttDoc.op-date, "99.99.9999") + 
             " │ " + STRING(ttDoc.doc-num,       "x(6)") + 
             " │ " + STRING(ttDoc.doc-type,      "x(5)") + 
             " │ " + STRING(ttDoc.bank-code,     "x(9)") + 
             " │ " + STRING(acct_corr,          "x(20)") + 
             " │ " + STRING(ob_dt,              "x(16)") + 
             " │ " + STRING(ob_kt,              "x(16)") + 
             " │ " + STRING(ttDoc.details,      "x(25)") +
             " │ "
             .
  
  PUT UNFORMATTED row_data SKIP.
  
  DO i = 2 TO NUM-ENTRIES(details_lst, "|"):
    details_ = ENTRY(i, details_lst, "|").
    PUT " │            │        │       │           │                      │                  │                  │ " details_              " │ " SKIP.
  END.
END.

ASSIGN
count_dt_ = STRING(count_dt,  "zzzzzzzzzzzz9")
count_kt_ = STRING(count_kt,  "zzzzzzzzzzzz9")
sum_dt_   = STRING(sum_dt, "z,zzz,zzz,zz9.99")
sum_kt_   = STRING(sum_kt, "z,zzz,zzz,zz9.99")
.

PUT " ├────────────┼────────┴───────┴───────────┴──────────────────────┼──────────────────┼──────────────────┼───────────────────────────┤ " SKIP.
PUT " │ И Т О Г О: │            документов по ДТ - " count_dt_ "       │ " sum_dt_      " │                  │                           │ " SKIP.
PUT " │            │            документов по КТ - " count_kt_ "       │                  │ " sum_kt_      " │                           │ " SKIP.
PUT " └────────────┴───────────────────────────────────────────────────┴──────────────────┴──────────────────┴───────────────────────────┘ " SKIP.
PUT "   Сальдо на конец дня : " ost_out       "                                                                                            " SKIP.
PUT "                                                                                                                                      " SKIP.

{preview.i}

/*
┌────────┬────────┐
├────────┼────────┤
│        │        │
└────────┴────────┘
*/
