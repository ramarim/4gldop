/*                                                                                                    */
/*                                                                                                    */
/*                    Акт инвентаризации по дебиторской/кредиторской задолженности                    */
/*                                                                                                    */
/*                                                                                                    */

PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF VAR date     AS DATE INIT TODAY NO-UNDO.
DEF VAR date_cl  AS DATE INIT TODAY NO-UNDO.
DEF VAR groupLst AS INT VIEW-AS RADIO-SET
RADIO-BUTTONS "Задолженность дебиторская", 1,
              "Задолженность кредиторская", 2 NO-UNDO.

FORM
SKIP(1)
date
FORMAT "99.99.9999"
LABEL  '                  Дата отчета'
HELP   'Введите отчетную дату'
SKIP(1)
date_cl
FORMAT "99.99.9999"
LABEL  '  Исключить счета закрытые до'
HELP   'Введите дату закрытия счета'
SPACE(2)
SKIP(1)
groupLst
LABEL  '  Тип'
HELP   'Выберите тип инвентаризации (Пробел - выбор,TAB - переход)'
SKIP(1)

WITH FRAME fr_main CENTERED ROW 8 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[ ИНВЕНТАРИЗАЦИЯ ]".
UPDATE date date_cl groupLst WITH FRAME fr_main.

DEF INPUT PARAM iParam   AS CHAR NO-UNDO.
DEF VAR fName            AS CHAR NO-UNDO.
DEF VAR className        AS CHAR NO-UNDO.
DEF VAR str              AS CHAR NO-UNDO.
DEF VAR itogo            AS DEC  NO-UNDO.
DEF VAR subName          AS CHAR NO-UNDO.
DEF VAR acctMsk          AS CHAR NO-UNDO.
DEF VAR itogo_all        AS DEC  NO-UNDO.
DEF VAR acctMsk_db       AS CHAR NO-UNDO.
DEF VAR acctMsk_cr       AS CHAR NO-UNDO.

FUNCTION GetParam RETURN CHAR (INPUT iName AS CHAR, INPUT iData AS CHAR):
  DEF VAR i AS INT NO-UNDO.
  DEF VAR result AS CHAR NO-UNDO.
  REPEAT i = 1 TO NUM-ENTRIES(iData, ";"):
    IF ENTRY(1, ENTRY(i, iData, ";"), "=") = iName THEN DO:
      result =  ENTRY(2, ENTRY(i, iData, ";"), "=").
      LEAVE.
    END.
  END.
  RETURN result.
END.

/*----------------------------------------------------------------------------------------------------*/

ASSIGN
fName      = "_spool.tmp"
className  = GetParam("class", iParam)
acctMsk_cr = "45115*," +
             "45215*," +
             "45415*," +
             "45515*," +
             "45818*," +
             "45918*"
acctMsk_db = "45107*," +
             "45108*," +
             "45109*," +
             "45201*," +
             "45203*," +
             "45204*," +
             "45205*," +
             "45206*," +
             "45207*," +
             "45208*," +
             "45306*," +
             "45307*," +
             "45401*," +
             "45405*," +
             "45406*," +
             "45407*," +
             "45408*," +
             "45502*," +
             "45503*," +
             "45505*," +
             "45506*," +
             "45507*," +
             "45509*," +
             "45812*," +
             "45814*," +
             "45815*," +
             "45912*," +
             "45914*," +
             "45915*," +
             "47427*"
.

CASE INT64(groupLst:SCREEN-VALUE):
  WHEN 1 THEN DO:
    ASSIGN
    acctMsk = acctMsk_db
    subName = "mav_inv_opis_db"
    .
  END.
  WHEN 2 THEN DO:
    ASSIGN
    acctMsk = acctMsk_cr
    subName = "mav_inv_opis_cr"
    .
  END.
END CASE.

{globals.i}
{sh-defs.i}
{sh-temp.i new}

OUTPUT TO VALUE (fName) CONVERT TARGET "1251".

PUT UNFORMATTED subName SKIP.
FOR EACH acct WHERE CAN-DO(acctMsk,acct.acct) 
                AND acct.close-date > date_cl NO-LOCK BREAK BY acct.bal-acct.
  RUN acct-pos IN h_base (acct.acct,acct.currency,date - 1,date - 1,?).
  str =       TRIM(STRING(acct.details)) + 
        "|" + TRIM(STRING(acct.number)) + 
        "|" + TRIM(REPLACE(STRING(ABS(sh-bal),"zzzzzzzzz9.99"),".",",")) + 
        "|" + "0,00" +
        "|" + TRIM(REPLACE(STRING(ABS(sh-bal),"zzzzzzzzz9.99"),".",",")) + 
        "|" + "0,00"
        .
  PUT UNFORMATTED str SKIP.
  IF NOT LAST-OF(acct.bal-acct) THEN itogo = itogo + ABS(sh-bal).
  IF LAST-OF(acct.bal-acct) THEN DO:
    ASSIGN
    itogo = itogo + ABS(sh-bal)
    str = "|ИТОГО по " + TRIM(STRING(acct.bal-acct,"99999")) + ":|" + TRIM(REPLACE(STRING(itogo,"zzzzzzzzz9.99"),".",",")) + "||" + TRIM(REPLACE(STRING(itogo,"zzzzzzzzz9.99"),".",",")) + "|"
    .
    PUT UNFORMATTED str SKIP.
    ASSIGN
    itogo_all = itogo_all + itogo
    itogo = 0
    .
  END.
END.
str = "|ИТОГО:|" + TRIM(REPLACE(STRING(itogo_all,"zzzzzzzzz9.99"),".",",")) + "||" + TRIM(REPLACE(STRING(itogo_all,"zzzzzzzzz9.99"),".",",")) + "|" .
PUT UNFORMATTED str SKIP.
OUTPUT CLOSE.

RUN sndbispc.p("file=" + fName + ";class=" + className).
