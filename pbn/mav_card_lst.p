/*                                                                                                    */
/*                                                                                                    */
/*                       Реестр карт. Для отправки поручений КартЗакр и СМСВыкл                       */
/*                                                                                                    */
/*                                                                                                    */

PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF VAR dateOpen AS DATE NO-UNDO.

dateOpen = DATE(01, 01, 2014).

FORM
SPACE(1)
dateOpen
FORMAT "99.99.9999"
LABEL  "Карты открыты с "
HELP   "Введите дату открытия карт"
SPACE(1)
SKIP

WITH FRAME frMain CENTERED ROW 10 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[ РЕЕСТР КАРТ ]".
UPDATE dateOpen WITH FRAME frMain.

DEF INPUT PARAM iParam AS CHAR            NO-UNDO.
DEF VAR fName          AS CHAR            NO-UNDO.
DEF VAR className      AS CHAR            NO-UNDO.
DEF VAR sOut           AS CHAR            NO-UNDO.
DEF VAR tDay           AS DATE INIT TODAY NO-UNDO.
DEF VAR ostIn          AS DEC             NO-UNDO.
DEF VAR lastOper       AS CHAR            NO-UNDO.
DEF VAR pCardClose     AS CHAR            NO-UNDO.
DEF VAR pSMSOn         AS CHAR            NO-UNDO.
DEF VAR pSMSOff        AS CHAR            NO-UNDO.
DEF VAR i              AS INT             NO-UNDO.

DEF BUFFER bLoan FOR loan.

DEF TEMP-TABLE ttCard NO-UNDO
FIELD card          LIKE loan.doc-num
FIELD card-status   LIKE loan.loan-status
FIELD card-end-date LIKE loan.end-date
FIELD loan          LIKE loan.doc-ref
FIELD loan-status   LIKE loan.loan-status
FIELD acct          LIKE acct.acct
FIELD acct-status   AS CHAR FORMAT "x(6)"
FIELD person-fio    AS CHAR
FIELD p-card-close  AS CHAR
FIELD p-sms-on      AS CHAR
FIELD p-sms-off     AS CHAR
.

{globals.i}
{sh-defs.i}

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

FUNCTION GetLastOper RETURN DATE (INPUT acct AS CHAR):
  DEF VAR result AS DATE NO-UNDO.
  DEF VAR date1  AS DATE NO-UNDO.
  DEF VAR date2  AS DATE NO-UNDO.
  
  FOR LAST op-entry WHERE op-entry.acct-db = acct
                      AND op-entry.op-status BEGINS "√" NO-LOCK.
    date1 = op-entry.op-date.
  END.
  
  FOR LAST op-entry WHERE op-entry.acct-cr = acct
                      AND op-entry.op-status BEGINS "√" NO-LOCK.
    date2 = op-entry.op-date.
  END.
  
  IF date1 <> ? AND date2 <> ? THEN result = MAX(date1, date2).
  ELSE DO:
    IF date1 = ? THEN result = date2.
    IF date2 = ? THEN result = date1.
  END.
  
  RETURN result.
END.

/*----------------------------------------------------------------------------------------------------*/

ASSIGN
fName     = "_spool.tmp"
className = GetParam("class", iParam) /*class=xlsvbs-work class=xlsvbs-test*/
.

FOR EACH loan WHERE loan.contract = "card" 
                AND loan.open-date >= dateOpen NO-LOCK,
    FIRST bLoan WHERE bLoan.contract = loan.parent-contract
                  AND bLoan.cont-code = loan.parent-cont-code NO-LOCK,
    FIRST person WHERE person.person-id = loan.cust-id NO-LOCK,
    EACH loan-acct WHERE loan-acct.cont-code = bLoan.cont-code
                      AND loan-acct.contract = bLoan.contract
                      AND loan-acct.acct-type = "SCS@" NO-LOCK,
    FIRST acct WHERE acct.acct = loan-acct.acct NO-LOCK.
  
  ASSIGN
  pCardClose = ""
  pSMSOn     = ""
  pSMSOff    = ""
  .
  
  FOR LAST op-int WHERE CAN-DO("КартЗакр", op-int.class-code)
                    AND op-int.file-name = "loan"
                    AND op-int.destination = "КС"
                    AND op-int.surrogate = "card," + loan.cont-code NO-LOCK.
    
    pCardClose = STRING(op-int.op-int-status, "x(3)") + " " +
                 STRING(op-int.create-date)           + " " +
                 STRING(op-int.create-time, "HH:MM")
    .
  END.

  FOR LAST op-int WHERE CAN-DO("СМСВкл", op-int.class-code)
                    AND op-int.file-name = "loan"
                    AND op-int.destination = "КС"
                    AND op-int.surrogate = "card," + loan.cont-code NO-LOCK.
    
    pSMSOn = STRING(op-int.op-int-status, "x(3)") + " " +
             STRING(op-int.create-date)           + " " +
             STRING(op-int.create-time, "HH:MM")
    .
  END.

  FOR LAST op-int WHERE CAN-DO("СМСВыкл", op-int.class-code)
                    AND op-int.file-name = "loan"
                    AND op-int.destination = "КС"
                    AND op-int.surrogate = "card," + loan.cont-code NO-LOCK.
    
    pSMSOff = STRING(op-int.op-int-status, "x(3)") + " " +
              STRING(op-int.create-date)           + " " +
              STRING(op-int.create-time, "HH:MM")
    .
  END.
  
  CREATE ttCard.
  ASSIGN
  ttCard.card          = loan.doc-num
  ttCard.card-status   = loan.loan-status
  ttCard.card-end-date = loan.end-date
  ttCard.loan          = bLoan.doc-ref
  ttCard.loan-status   = bLoan.loan-status
  ttCard.acct          = loan-acct.acct
  ttCard.acct-status   = IF acct.close-date = ? THEN "открыт" ELSE "закрыт"
  ttCard.person-fio    = person.name-last + " " + person.first-name
  ttCard.p-card-close  = pCardClose
  ttCard.p-sms-on      = pSMSOn
  ttCard.p-sms-off     = pSMSOff
  .
END.

OUTPUT TO VALUE (fname) CONVERT TARGET "1251".

PUT UNFORMATTED "mav_card_lst"             SKIP.
PUT UNFORMATTED STRING(NOW)                SKIP.
PUT UNFORMATTED STRING(tDay, "99.99.9999") SKIP.

FOR EACH ttCard NO-LOCK,
    FIRST acct WHERE acct.acct = ttCard.acct NO-LOCK.
  
  RUN acct-pos IN h_base (acct.acct, acct.currency, tDay - 1, tDay - 1, ?).
  IF CAN-DO("А,П", acct.side) AND sh-bal  < 0 THEN ostIn = ABS(sh-bal).
  IF CAN-DO("А,П", acct.side) AND sh-bal >= 0 THEN ostIn = sh-bal.
  
  ASSIGN
  i = i + 1
  lastOper = IF GetLastOper(ttCard.acct) = ? THEN "" ELSE STRING(GetLastOper(ttCard.acct), "99.99.9999")
  sOut = TRIM(STRING(i                                 )) + "|" + 
         TRIM(STRING(ttCard.person-fio                 )) + "|" + 
         TRIM(STRING(ttCard.acct, "x(20)"              )) + "|" + 
         TRIM(STRING(ttCard.acct-status                )) + "|" + 
         TRIM(STRING(ostIn,           "zzzzzzzzzzz9.99")) + "|" + 
         TRIM(STRING(lastOper                          )) + "|" + 
         TRIM(STRING(ttCard.loan                       )) + "|" + 
         TRIM(STRING(ttCard.loan-status                )) + "|" + 
         TRIM(STRING(ttCard.card                       )) + "|" + 
         TRIM(STRING(ttCard.card-status                )) + "|" + 
         TRIM(STRING(ttCard.card-end-date, "99.99.9999")) + "|" + 
         TRIM(STRING(ttCard.p-card-close               )) + "|" + 
         TRIM(STRING(ttCard.p-sms-on                   )) + "|" + 
         TRIM(STRING(ttCard.p-sms-off                  ))
  sOut = REPLACE(sOut, "√", "V")
  .
  
  PUT UNFORMATTED sOut SKIP.
END.

OUTPUT CLOSE.

RUN sndbispc.p("file=" + fName + ";class=" + className).
