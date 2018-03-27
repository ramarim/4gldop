/*                                                                                                    */
/*                                                                                                    */
/*                            Задолженность за услугу "СМС-информирование"                            */
/*                                                                                                    */
/*                                                                                                    */

PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF VAR dateBeg AS DATE NO-UNDO.
DEF VAR dateEnd AS DATE NO-UNDO.

ASSIGN
dateEnd = TODAY
dateBeg = ADD-INTERVAL(DATE(MONTH(dateEnd), DAY(dateEnd), YEAR(dateEnd)), -3, "MONTH")
.

FORM
SPACE(2)
dateBeg
FORMAT "99.99.9999"
LABEL  'Дата начала   '
HELP   'Введите дату начала периода задолженности.'
SKIP
SPACE(2)
dateEnd
FORMAT "99.99.9999"
LABEL  'Дата окончания'
HELP   'Введите дату окончания периода задолженности.'
SPACE(2)
SKIP

WITH FRAME frMain CENTERED ROW 10 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[ ЗАДОЛЖЕННОСТЬ ЗА СМС ]".
UPDATE dateBeg dateEnd WITH FRAME frMain.

DEF VAR sOut       AS CHAR                NO-UNDO.
DEF VAR debtCount  AS INT                 NO-UNDO.
DEF VAR komAcctLst AS CHAR                NO-UNDO.
DEF VAR ttlAcct    AS INT                 NO-UNDO.
DEF VAR ttlDebt    AS INT                 NO-UNDO.
DEF VAR ttlOstIn   AS DEC                 NO-UNDO.
DEF VAR ttl1       AS CHAR FORMAT "x(8)"  NO-UNDO.
DEF VAR ttl2       AS CHAR FORMAT "x(8)"  NO-UNDO.
DEF VAR ttl3       AS CHAR FORMAT "x(12)" NO-UNDO.

DEF TEMP-TABLE ttOpEntry NO-UNDO
FIELD acct    LIKE acct.acct
FIELD op-date LIKE op-entry.op-date
FIELD acct-db LIKE op-entry.acct-db
FIELD acct-cr LIKE op-entry.acct-cr
FIELD amt-rub LIKE op-entry.amt-rub
.

DEF TEMP-TABLE ttDebtSMS NO-UNDO
FIELD acct      LIKE acct.acct
FIELD acctDebt  LIKE acct.acct
FIELD fio       AS CHAR
FIELD loan      AS CHAR
FIELD amount    AS INT
FIELD ostIn     AS DEC
.

{globals.i}
{sh-defs.i}

FUNCTION GetPersonFIO RETURN CHAR (INPUT iAcct AS CHAR):
  DEF VAR result AS CHAR NO-UNDO.
  
  FOR FIRST acct WHERE acct.acct = iAcct NO-LOCK,
                 FIRST person WHERE person.person-id = acct.cust-id NO-LOCK.
    result = name-last + " " + first-names.
  END.
  
  RETURN result.
END.

FUNCTION GetPersonInfo RETURN CHAR (INPUT iAcct AS CHAR, INPUT iType AS CHAR):
  DEF BUFFER bLoanAcct FOR loan-acct.
  DEF VAR personRS   AS CHAR NO-UNDO.
  DEF VAR personLoan AS CHAR NO-UNDO.
  DEF VAR result     AS CHAR NO-UNDO.
  
  FOR EACH loan-acct WHERE loan-acct.acct = iAcct
                       AND loan-acct.currency = "" NO-LOCK,
                       EACH bLoanAcct WHERE bLoanAcct.contract = loan-acct.contract 
                                        AND bLoanAcct.cont-code = loan-acct.cont-code 
                                        AND CAN-DO("40817*,40820*", bLoanAcct.acct) NO-LOCK.
    ASSIGN
    personRS = bLoanAcct.acct
    personLoan = ENTRY(1, loan-acct.cont-code, "@")
    .
  END.

  IF iType = "счет"    THEN result = personRS.
  IF iType = "договор" THEN result = personLoan.
  
  RETURN result.
END.

FUNCTION GetAcctOstIn RETURN DEC (INPUT iAcct     AS CHAR, 
                                  INPUT iCurrency AS CHAR, 
                                  INPUT iDate     AS DATE):
  DEF VAR result AS DEC NO-UNDO.
  
  RUN acct-pos IN h_base (iAcct, iCurrency, iDate - 1, iDate - 1, ?).
  IF sh-bal  < 0 THEN result = ABS(sh-bal).
  IF sh-bal >= 0 THEN result = sh-bal.
  
  RETURN result.
END.

/*----------------------------------------------------------------------------------------------------*/

komAcctLst = "70601810900007777777*,70601810400111111111*,70601810100055555555*".

FOR EACH acct WHERE CAN-DO("47423*", acct.acct) 
                AND CAN-DO("OP*", acct.user-id) 
                AND acct.close-date = ? NO-LOCK.
  
  FOR EACH op-entry WHERE op-entry.op-date >= dateBeg
                      AND op-entry.op-date <= dateEnd
                      AND op-entry.acct-db = acct.acct 
                      AND op-entry.op-status BEGINS "√" NO-LOCK.
    CREATE ttOpEntry.
    ASSIGN
    ttOpEntry.acct    = acct.acct
    ttOpEntry.op-date = op-entry.op-date
    ttOpEntry.acct-db = op-entry.acct-db
    ttOpEntry.acct-cr = op-entry.acct-cr
    ttOpEntry.amt-rub = op-entry.amt-rub
    .
  END.
  
  FOR EACH op-entry WHERE op-entry.op-date >= dateBeg
                      AND op-entry.op-date <= dateEnd
                      AND op-entry.acct-cr = acct.acct
                      AND op-entry.op-status BEGINS "√" NO-LOCK.
    CREATE ttOpEntry.
    ASSIGN
    ttOpEntry.acct    = acct.acct
    ttOpEntry.op-date = op-entry.op-date
    ttOpEntry.acct-db = op-entry.acct-db
    ttOpEntry.acct-cr = op-entry.acct-cr
    ttOpEntry.amt-rub = op-entry.amt-rub
    .
  END.
END.

FOR EACH ttOpEntry NO-LOCK BREAK BY ttOpEntry.acct
                                 BY ttOpEntry.op-date DESC.
  
  IF FIRST-OF(ttOpEntry.acct) THEN debtCount = 0. 
  IF NOT LAST-OF(ttOpEntry.acct) OR LAST-OF(ttOpEntry.acct) THEN DO:
    IF debtCount <> -1 THEN DO:
      IF ttOpEntry.acct-db = ttOpEntry.acct AND 
      CAN-DO(komAcctLst, ttOpEntry.acct-cr) THEN DO:
        debtCount = debtCount + 1.
      END.
      ELSE debtCount = -1.
    END.
  END.
  
  IF LAST-OF(ttOpEntry.acct) AND debtCount >= 3 THEN DO:
    CREATE ttDebtSMS.
    ASSIGN
    ttDebtSMS.fio      = GetPersonFIO(ttOpEntry.acct)
    ttDebtSMS.loan     = GetPersonInfo(ttOpEntry.acct, "договор")
    ttDebtSMS.acct     = GetPersonInfo(ttOpEntry.acct, "счет")
    ttDebtSMS.acctDebt = ttOpEntry.acct
    ttDebtSMS.amount   = debtCount
    ttDebtSMS.ostIn    = GetAcctOstIn(ttOpEntry.acct, "", dateEnd)
    .
  END.
  
  /*
  DISPLAY
  ttOpEntry.acct 
  ttOpEntry.op-date
  ttOpEntry.acct-db FORMAT "x(5)"
  ttOpEntry.acct-cr FORMAT "x(5)"
  ttOpEntry.amt-rub
  .
  */
END.

{setdest.i}

PUT "                                                                                                                                        " SKIP.
PUT "                                  СЧЕТА ТРЕБОВАНИЙ С ЗАДОЛЖЕННОСТЬЮ ЗА УСЛУГУ 'СМС-информирование'                                      " SKIP.
PUT "                                                                                                                                        " SKIP.
PUT " ┌─────────────────────────────────────┬────────────────────────┬──────────────────────┬──────────────────────┬────────┬──────────────┐ " SKIP.
PUT " │                                     │                        │                      │                      │        │              │ " SKIP.
PUT " │        Наименование клиента         │     Номер договора     │    Расчетный счет    │   Счет требований    │ Кол-во │   Входящий   │ " SKIP.
PUT " │                                     │                        │                      │                      │ задолж.│   остаток    │ " SKIP.
PUT " ├─────────────────────────────────────┼────────────────────────┼──────────────────────┼──────────────────────┼────────┼──────────────┤ " SKIP.

FOR EACH ttDebtSMS NO-LOCK.
  ASSIGN
  ttlAcct  = ttlAcct  + 1
  ttlDebt  = ttlDebt  + ttDebtSMS.amount
  ttlOstIn = ttlOstIn + ttDebtSMS.ostIn
  sOut     =   " │ " + STRING(ttDebtSMS.fio,            "x(35)")
             + " │ " + STRING(ttDebtSMS.loan,           "x(22)")
             + " │ " + STRING(ttDebtSMS.acct,           "x(20)")
             + " │ " + STRING(ttDebtSMS.acctDebt,       "x(20)")
             + " │ " + STRING(ttDebtSMS.amount,        "zzzzz9")
             + " │ " + STRING(ttDebtSMS.ostIn,   "z,zzz,zz9.99")
             + " │ "
  .
  PUT UNFORMATTED sOut SKIP.
  .
END.

ASSIGN
ttl1 = STRING(ttlAcct,      "zzzzzzz9")
ttl2 = STRING(ttlDebt,      "zzzzzzz9")
ttl3 = STRING(ttlOstIn, "z,zzz,zz9.99")
.

PUT " ├─────────────────────────────────────┼────────────────────────┼──────────────────────┼──────────────────────┼────────┼──────────────┤ " SKIP.
PUT " │ ИТОГО:                     " ttl1 " │                        │                      │                      │" ttl2 "│ "     ttl3 " │ " SKIP.
PUT " └─────────────────────────────────────┴────────────────────────┴──────────────────────┴──────────────────────┴────────┴──────────────┘ " SKIP.
PUT "                                                                                                                                        " SKIP.

{preview.i}


/*
""
┌────────┬────────┐
├────────┼────────┤
│        │        │
└────────┴────────┘
*/
