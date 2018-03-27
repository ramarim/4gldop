/*                                                                                                    */
/*                                                                                                    */
/*                                   Сводный кассовый ордер (ОВО)                                     */
/*                                                                                                    */
/*                                                                                                    */

/* kdz modif*/
PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF INPUT PARAM iParam AS CHARACTER NO-UNDO.
/* end modif*/

DEF VAR i             AS INT                  NO-UNDO.
DEF VAR j             AS INT                  NO-UNDO.
DEF VAR i_            AS CHAR FORMAT "x(6)"   NO-UNDO.
DEF VAR n_doc         AS CHAR FORMAT "x(9)"   NO-UNDO.
DEF VAR n_kss         AS CHAR FORMAT "x(9)"   NO-UNDO.
DEF VAR name_kss      AS CHAR FORMAT "x(35)"  NO-UNDO.
DEF VAR date_         AS CHAR FORMAT "X(20)"  NO-UNDO.
DEF VAR acct_Dt       AS CHAR FORMAT "x(20)"  NO-UNDO.
DEF VAR acct_Ct       AS CHAR FORMAT "x(20)"  NO-UNDO.
DEF VAR tSumm         AS DEC                  NO-UNDO.
DEF VAR tSumm_        AS CHAR FORMAT "x(12)"  NO-UNDO.
DEF VAR tSummPr       AS CHAR                 NO-UNDO.
DEF VAR tSummPrInt    AS CHAR                 NO-UNDO.
DEF VAR tSummPrDec    AS CHAR                 NO-UNDO.
DEF VAR tSummPr1      AS CHAR FORMAT "x(55)"  NO-UNDO.
DEF VAR tSummPr2      AS CHAR FORMAT "x(55)"  NO-UNDO.
DEF VAR tSummPr3      AS CHAR FORMAT "x(55)"  NO-UNDO.
DEF VAR tSummPr4      AS CHAR FORMAT "x(55)"  NO-UNDO.
DEF VAR tSummPr5      AS CHAR FORMAT "x(55)"  NO-UNDO.
DEF VAR ks            AS CHAR FORMAT "x(6)"   NO-UNDO.
DEF VAR ks1           AS CHAR FORMAT "x(7)"   NO-UNDO.
DEF VAR ks2           AS CHAR FORMAT "x(7)"   NO-UNDO.
DEF VAR ks3           AS CHAR FORMAT "x(7)"   NO-UNDO.
DEF VAR doc_n         AS CHAR FORMAT "x(9)"   NO-UNDO.
DEF VAR summ          AS DEC                  NO-UNDO.
DEF VAR summ_         AS CHAR FORMAT "x(12)"  NO-UNDO.
DEF VAR summ1_        AS CHAR FORMAT "x(12)"  NO-UNDO.
DEF VAR summ2_        AS CHAR FORMAT "x(12)"  NO-UNDO.
DEF VAR summ3_        AS CHAR FORMAT "x(12)"  NO-UNDO.
DEF VAR ist           AS CHAR FORMAT "x(89)"  NO-UNDO.
DEF VAR nazn          AS CHAR FORMAT "x(103)" NO-UNDO.
DEF VAR details_      AS CHAR FORMAT "x(40)"  NO-UNDO.
DEF VAR details       AS CHAR                 NO-UNDO.
DEF VAR vsp           AS CHAR FORMAT "x(42)"  NO-UNDO.
DEF VAR fiop          AS CHAR                 NO-UNDO.
DEF VAR user_name1    AS CHAR FORMAT "x(35)"  NO-UNDO.
DEF VAR user_name2    AS CHAR FORMAT "x(35)"  NO-UNDO.
DEF VAR bik           AS CHAR FORMAT "x(9)"   NO-UNDO.
DEF VAR inn           AS CHAR FORMAT "x(10)"  NO-UNDO.

/* kdvav modif */
DEF VAR op_username AS CHAR NO-UNDO.
DEF VAR op_doljnost AS CHAR NO-UNDO.
DEF VAR s_sign      AS CHAR NO-UNDO.
DEF VAR s_DolKont   AS CHAR NO-UNDO.
DEF VAR s_FIOKont   AS CHAR NO-UNDO.

FUNCTION GetParam RETURN CHAR (INPUT iName AS CHAR,INPUT iData AS CHAR):
  DEF VAR i AS INT NO-UNDO.
  DEF VAR result AS CHAR NO-UNDO.
  REPEAT i = 1 TO NUM-ENTRIES(iData,";"):
    IF ENTRY(1,ENTRY(i,iData,";"),"=") = iName THEN DO:
      result =  ENTRY(2,ENTRY(i,iData,";"),"=").
      LEAVE.
    END.
  END.
  RETURN result.
END.

ASSIGN
s_DolKont = GetParam("Doljn" ,iParam)
s_FIOKont = GetParam("FIO"   ,iParam)
.
/* end modif */


DEF BUFFER bOp FOR op.

DEF TEMP-TABLE ttDoc NO-UNDO
FIELD acct-db LIKE op-entry.acct-db
FIELD acct-cr LIKE op-entry.acct-cr
FIELD amt-rub LIKE op-entry.amt-rub
FIELD op-date LIKE op-entry.op-date
FIELD user-id LIKE op-entry.user-id
FIELD symbol  LIKE op-entry.symbol
FIELD details LIKE op.details
.

DEF TEMP-TABLE ttReestrPN NO-UNDO
FIELD due-date LIKE op.due-date
FIELD op       LIKE op.op
.

{globals.i}
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

FUNCTION DateExt RETURN CHAR (INPUT iDate AS DATE):
  DEF VAR result AS CHAR NO-UNDO.
  DEF VAR month_ AS CHAR No-UNDO.
  IF MONTH(iDate) = 1  THEN month_ = "января".
  IF MONTH(iDate) = 2  THEN month_ = "февраля".
  IF MONTH(iDate) = 3  THEN month_ = "марта".
  IF MONTH(iDate) = 4  THEN month_ = "апреля".
  IF MONTH(iDate) = 5  THEN month_ = "мая".
  IF MONTH(iDate) = 6  THEN month_ = "июня".
  IF MONTH(iDate) = 7  THEN month_ = "июля".
  IF MONTH(iDate) = 8  THEN month_ = "августа".
  IF MONTH(iDate) = 9  THEN month_ = "сентября".
  IF MONTH(iDate) = 10 THEN month_ = "октября".
  IF MONTH(iDate) = 11 THEN month_ = "ноября".
  IF MONTH(iDate) = 12 THEN month_ = "декабря".
  result = STRING(DAY(iDate),"99")    + " " +
           month_                     + " " +
           STRING(YEAR(iDate),"9999") + " г.".
  RETURN result.
END.

FUNCTION GetPayer RETURN CHAR (INPUT iOp AS INT):
  DEF VAR result AS CHAR NO-UNDO.
  FOR FIRST op WHERE op.op = iOp NO-LOCK, FIRST op-entry OF op NO-LOCK.
    IF op-entry.acct-cr BEGINS "70601" THEN result = TRIM(op.name-ben). 
    ELSE DO:
      result = TRIM(GetXattrValueEx("op", STRING(op.op), "ФИО", "")).
      IF result = "" THEN result = TRIM(GetXattrValueEx("op", STRING(op.op), "ПНФИО", "")).
    END.
  END.
  RETURN result.
END.

/*----------------------------------------------------------------------------------------------------*/

FOR FIRST tmprecid NO-LOCK, FIRST op WHERE RECID(op) = tmprecid.id NO-LOCK.
  n_doc = op.doc-num.
END.

FOR EACH tmprecid NO-LOCK, FIRST op WHERE RECID(op) = tmprecid.id NO-LOCK,
                           EACH links WHERE links.source-id  = STRING(op.op)
                                        AND links.beg-date  <= op.op-date
                                        AND (links.end-date >= op.op-date
                                         OR links.end-date = ?) NO-LOCK,
                                         EACH bOp WHERE bOp.op = INT64(links.target-id)
                                                    AND bOp.due-date = links.beg-date NO-LOCK.
  CREATE ttReestrPN.
  ASSIGN
  ttReestrPN.op       = bOp.op
  ttReestrPN.due-date = bOp.due-date
  .
  
END.

FOR EACH ttReestrPN NO-LOCK, FIRST op WHERE op.op = ttReestrPN.op 
                                        AND op.due-date = ttReestrPN.due-date NO-LOCK, 
                                        FIRST op-entry OF op WHERE op-entry.op-status = 'АС' NO-LOCK.
  
  CREATE ttDoc.
  ASSIGN
  ttDoc.acct-db = op-entry.acct-db
  ttDoc.acct-cr = op-entry.acct-cr
  ttDoc.amt-rub = op-entry.amt-rub  
  ttDoc.user-id = op-entry.user-id
  ttDoc.symbol  = op-entry.symbol
  ttDoc.op-date = op.due-date
  ttDoc.details = op.details
  .
END.

FOR EACH ttDoc NO-LOCK BREAK BY ttDoc.symbol.
  tSumm = tSumm + ttDoc.amt-rub.
  summ = summ + ttDoc.amt-rub.
  IF NOT LAST-OF(ttDoc.symbol) THEN DELETE ttDoc.
  IF LAST-OF(ttDoc.symbol) THEN DO:
    ttDoc.amt-rub = summ.
    summ = 0.
  END.
END.

FOR FIRST ttDoc NO-LOCK.
  ASSIGN
  date_   = DateExt(ttDoc.op-date)
  acct_Dt = ttDoc.acct-db
  acct_Ct = ttDoc.acct-cr
  tSumm_  = REPLACE(STRING(tSumm,"zzzzzzzz9.99"),".","-")
  ist     = "От совершения операций перевода денежных средст по распоряжению физических лиц"
  nazn    = "Перевод денежных средств без открытия счета согласно реестру платежей за " + date_
  .
  
  FOR FIRST _user WHERE _user._userid = ttDoc.user-id NO-LOCK.
    fiop = TRIM(GetXattrValueEx("_user",_user._userid,"ФИОП","")).
    IF fiop = "" THEN fiop = _user._user-name.
    ASSIGN
    user_name1 = SUBSTRING(fiop,1,35)
    user_name2 = SUBSTRING(fiop,36)
    /* kdvav modif */
    op_doljnost = TRIM(GetXattrValueEx("_user",_user._userid,"Должность",""))
    op_username = _user._user-name
    /* end modif */
    .
  END.
  
  FOR FIRST acct WHERE acct.acct = ttDoc.acct-db NO-LOCK.
    ASSIGN
    n_kss    = IF INDEX(acct.details,"№") > 0 THEN TRIM(SUBSTRING(acct.details,INDEX(acct.details,"№"),4)) ELSE ""
    name_kss = IF CAN-DO("20202810.0072*", ttDoc.acct-db) THEN "Касса ДО 'Тюменский'" ELSE "Касса отдела вкладных операций"
    vsp      = IF CAN-DO("20202810000100000010*,20202810800720000001*", ttDoc.acct-db) THEN "АО КБ 'Приобье'" ELSE "Операционная касса " + n_kss + " АО КБ 'Приобье'"
    .
  END.
END.

RUN x-amtstr.p (tSumm, "643", YES, YES, OUTPUT tSummPrInt, OUTPUT tSummPrDec).
ASSIGN
tSummPr = tSummPrInt + " " + tSummPrDec + " " + "(РОССИЙСКИЙ РУБЛЬ (810))"
tSummPr1 = SUBSTRING(tSummPr,1,55)
tSummPr2 = SUBSTRING(tSummPr,56,55)
tSummPr3 = SUBSTRING(tSummPr,101,55)
tSummPr4 = SUBSTRING(tSummPr,156,55)
tSummPr5 = SUBSTRING(tSummPr,201)
.

ASSIGN
bik = "047169777"
inn = "8603010518"
.


IF acct_Ct BEGINS "7" THEN DO:
  ASSIGN
  bik  = ""
  inn  = ""
  ist  = "Прочие поступления"
  nazn = "Комиссия за перевод денежных средств физических лиц согласно реестру платежей за " + date_
  vsp  = ""
  .
END.

{setdest.i}
PUT "                                                        ┌─────────────────────┐   ┌ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─  ┐" SKIP.
PUT "                                                        │ Код формы документа │   │     Отрывной талон к приходному    │" SKIP.
PUT "                                                        │   по ОКУД 0402008   │   │      кассовому ордеру № " n_doc "  │" SKIP.
PUT "                                                        └─────────────────────┘   └ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─  ┘" SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                  ╔════════════════════════════════════╗" SKIP.
PUT "                                                                                  ║         Место для наклейки         ║" SKIP.
PUT "                                        ╔═════════╗       " date_            "    ║          отрывного талона          ║" SKIP.
PUT "           Приходный кассовый ордер  N  ║" n_doc "║     ───────────────────────   ╠════════════════════════════════════╣" SKIP.
PUT "                                        ╚═════════╝               Дата            ║                                    ║" SKIP.
PUT "                                                                                  ║           Сумма цифрами            ║" SKIP.
PUT "                                                                  ДЕБЕТ           ║                                    ║" SKIP.
PUT "                                                       ╔══════════════════════════╬══════════════════╤═════════════════╣" SKIP.
PUT "          От кого: " user_name1                      " ║счет N                    ║                  │                 ║" SKIP.
PUT "                   " user_name2                      " ║" acct_Dt          "      ║ " tSumm_   "     │                 ║" SKIP.
PUT "                                                       ║                          ║                  │                 ║" SKIP.
PUT "          ─────────────────────────────────────────────╚══════════════════════════╣                  │                 ║" SKIP.
PUT "                                                                  КРЕДИТ          ║                  │                 ║" SKIP.
PUT "          ─────────────────────────────────────────────╔══════════════════════════╣                  │                 ║" SKIP.
PUT "          Получатель: АО КБ 'Приобье'                  ║счет N                    ║                  │                 ║" SKIP.
PUT "                                                       ║" acct_Ct          "      ║                  │                 ║" SKIP.
PUT "          ─────────────────────────────────────────────║──────────────────────────║                  │                 ║" SKIP.
PUT "                                                       ║счет N                    ║                  │                 ║" SKIP.
PUT "                                                       ║                          ║                  │                 ║" SKIP.
PUT "          ─────────────────────────────────────────────╚══════════════════════════╬══════════════════╧═════════════════╣" SKIP.
PUT "          ИНН " inn    "                 Счет №         ────────────────────      ║       в том числе по символам:     ║" SKIP.
PUT "              ─────────────────────────         ──────────────────────────────────║────────────┬───────────────────────║" SKIP.
PUT "          Наименование банка─вносителя " vsp                                    " ║   символ   │         сумма         ║" SKIP.
PUT "          ────────────────────────────────────────────────────────────────────────║────────────┼───────────────────────║" SKIP.

i = 0.
FOR EACH ttDoc NO-LOCK.
  i = i + 1.
  IF i = 1 THEN DO:
    ASSIGN
    ks1 = ttDoc.symbol
    summ1_ = REPLACE(STRING(ttDoc.amt-rub,"zzzzzzzz9.99"),".","-")
    .
  END.

  IF i = 2 THEN DO:
    ASSIGN
    ks2 = ttDoc.symbol
    summ2_ = REPLACE(STRING(ttDoc.amt-rub,"zzzzzzzz9.99"),".","-")
    .
  END.

  IF i = 3 THEN DO:
    ASSIGN
    ks3 = ttDoc.symbol
    summ3_ = REPLACE(STRING(ttDoc.amt-rub,"zzzzzzzz9.99"),".","-")
    .
  END.
END.

PUT "                                                            БИК " bik   "         ║     " ks1 "│          "   summ1_ " ║" SKIP.
PUT "          ─────────────────────────────────────────────────     ──────────────────║────────────┼───────────────────────║" SKIP.
PUT "          Наименование банка─получателя " vsp                                    "║     " ks2 "│          "   summ2_ " ║" SKIP.
PUT "          ────────────────────────────────────────────────────────────────────────║────────────┼───────────────────────║" SKIP.
PUT "                                                            БИК " bik   "         ║     " ks3 "│          "   summ3_ " ║" SKIP.
PUT "          ─────────────────────────────────────────────────     ──────────────────║────────────┼───────────────────────║" SKIP.

i = 0.
FOR EACH ttDoc NO-LOCK.
  i = i + 1.
  IF i > 3 THEN DO:
    ASSIGN
    ks = ttDoc.symbol
    summ_ = REPLACE(STRING(ttDoc.amt-rub,"zzzzzzzz9.99"),".","-")
    .
    PUT "                                                                                  ║     " ks " │          "    summ_ " ║" SKIP.
    PUT "                                                                                  ║────────────┼───────────────────────║" SKIP.
  END.
END.

PUT "          Сумма прописью: " tSummPr1                                            " ║            │                       ║" SKIP.
PUT "                          " tSummPr2                                            " ║────────────┼───────────────────────║" SKIP.
PUT "                          " tSummPr3                                            " ║            │                       ║" SKIP.
PUT "                          " tSummPr4                                            " ║────────────────┬───────────────────║" SKIP.
PUT "                          " tSummPr5                                            " ║ Шифр документа │ 04                ║" SKIP.
PUT "          ────────────────────────────────────────────────────────────────────────╚════════════════════════════════════╝" SKIP.
PUT "                                                                                                                        " SKIP.
PUT "          ──────────────────────────────────────────────────────────────────────────────────────────────────────────────" SKIP.
PUT "          Источник поступления: "ist                                                                                      SKIP.
PUT "          " nazn                                                                                                          SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.

/* kdvav modif */
IF op_doljnost EQ "Ведущий бухгалтер-кассир" THEN
DO:
PUT UNFORMATTED "          Вноситель                                                         Ведущий                                     " SKIP.
/*PUT UNFORMATTED "                                                                  бухгалтер-кассир          " op_username       SKIP.*/
s_FIOKont = IF LENGTH(s_FIOKont)   < 19 THEN s_FIOKont + FILL(" ",20 - LENGTH(s_FIOKont))   ELSE s_FIOKont.
PUT UNFORMATTED "                             " s_DolKont "         " s_FIOKont  "бухгалтер-кассир          " op_username              SKIP.

END.
ELSE
DO:
   s_sign    = IF LENGTH(op_doljnost) < 25 THEN FILL(" ",25 - LENGTH(op_doljnost)) ELSE "".
   s_FIOKont = IF LENGTH(s_FIOKont)   < 19 THEN s_FIOKont + FILL(" ",20 - LENGTH(s_FIOKont))   ELSE s_FIOKont.
PUT UNFORMATTED "          Вноситель          " s_DolKont "         " s_FIOKont op_doljnost s_sign op_username              SKIP.
END.
/* end modif */

/*PUT "Вноситель                                                                                                     " SKIP.*/
PUT "                    ──────── ───────────────── ──────── ─────────────────── ──────────────── ──────── ──────────────────" SKIP.
PUT "                    (личная    (наименование   (личная       (фамилия,       (наименование   (личная      (фамилия,     " SKIP.
PUT "                    подпись)     должности)    подпись)      инициалы)         должности)    подпись)     инициалы)     " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.
PUT "                                                                                                                        " SKIP.

PUT "                                        Реестр платежей за " date_            "                                                    " SKIP.
PUT "    АО КБ 'Приобье'                                                                                   операционная касса " n_kss " " SKIP.
PUT "    " name_kss                                                                                                                       SKIP.
PUT "    Вид операции: " nazn                                                                                                "          " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT "    ┌────────┬───────────┬──────────────────────┬──────────────────────┬──────────────┬──────────────────────────────────────────┐ " SKIP.
PUT "    │   КС   │   № док.  │        ДЕБЕТ         │        КРЕДИТ        │    Сумма     │            Назначение платежа            │ " SKIP.
PUT "    ├────────┼───────────┼──────────────────────┼──────────────────────┼──────────────┼──────────────────────────────────────────┤ " SKIP.

i = 0.
FOR EACH ttReestrPN NO-LOCK, FIRST op WHERE op.op = ttReestrPN.op
                                        AND op.due-date = ttReestrPN.due-date NO-LOCK,
                                        FIRST op-entry OF op WHERE op-entry.op-status = 'АС' NO-LOCK.
  ASSIGN
  i = i + 1
  ks = op-entry.symbol
  doc_n = op.doc-num
  acct_Dt = op-entry.acct-db
  acct_Ct = op-entry.acct-cr
  summ_ = STRING(op-entry.amt-rub,"zzzzzzzz9.99")
  details = StrWrap(op.details + "; Плательщик: " + GetPayer(op.op) ,40,"|")
  details_ = ENTRY(1,details,"|")
  .
  PUT "    │ " ks " │ " doc_n " │ " acct_Dt          " │ " acct_Ct          " │ "    summ_ " │ " details_                             " │ " SKIP.
  DO j = 2 TO NUM-ENTRIES(details,"|"):
    details_ = ENTRY(j,details,"|").
    PUT "    │        │           │                      │                      │              │ " details_                             " │ " SKIP.
  END.
END.

i_ = STRING(i).

PUT "    └────────┴───────────┴──────────────────────┴──────────────────────┴──────────────┴──────────────────────────────────────────┘ " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT "    Итого документов: " i_ " на сумму " tSumm_   "                                                                                 " SKIP.
PUT "    Сумма прописью: " tSummPr1                                            "" tSummPr2                                            " " SKIP.
PUT "                    " tSummPr3                                            "" tSummPr4                                            " " SKIP.
PUT "                    " tSummPr5                                            "                                                        " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT "                                                                                                                                   " SKIP.

FOR EACH ttDoc NO-LOCK.
  ASSIGN
  ks = ttDoc.symbol
  summ_ = STRING(ttDoc.amt-rub,"zzzzzzzz9.99")
  .
  PUT "    Итого по КС " ks "    "    summ_ "                                                                                           " SKIP.
END.

PUT "                                                                                                                                   " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT "    Бухгалтер - кассир ________________________________________                                                                    " SKIP.
{preview.i}


/*
┌────────┬────────┐
├────────┼────────┤
│        │        │
└────────┴────────┘
*/
