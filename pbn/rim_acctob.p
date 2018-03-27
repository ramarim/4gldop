/*
      Comment: Справка об оборотах по счету
   Parameters:
         Uses:
      Used by:
     Modified: 
*/
{globals.i}
{sh-defs.i}
{sh-temp.i new}
{tmprecid.def}

PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF VAR str       AS CHAR NO-UNDO.
DEF VAR fname     AS CHAR NO-UNDO.
DEF VAR user_ip   AS CHAR FORMAT "X(15)" NO-UNDO.
DEF VAR fpath     AS CHAR INIT "/qbis/wrk/imp-exp/0000/preview/" NO-UNDO.
user_ip=TRIM(SUBSTRING(OS-GETENV("SSH_CLIENT"),1,INDEX(OS-GETENV("SSH_CLIENT")," "))).
fname = fpath + "_tmp@" + user_ip.

DEF VAR date1 AS DATE INIT TODAY NO-UNDO.
DEF VAR date2 AS DATE INIT TODAY NO-UNDO.
DEF VAR mDate AS DATE INIT TODAY NO-UNDO.
DEF VAR s_date1  AS CHAR NO-UNDO.
DEF VAR s_date2  AS CHAR NO-UNDO.
DEF VAR dObDT AS DEC NO-UNDO.
DEF VAR dObKT AS DEC NO-UNDO.
DEF VAR dItogoDT AS DEC NO-UNDO.
DEF VAR dItogoKT AS DEC NO-UNDO.
DEF VAR groupLst1   AS INT VIEW-AS RADIO-SET RADIO-BUTTONS "Ежедневные", 1,"Ежемесячные",2,"Квартальные",3,"Годовые",4 NO-UNDO.
DEF VAR groupLst2   AS INT VIEW-AS RADIO-SET RADIO-BUTTONS "Все", 1,"Исключить операции с судным счетом",2 NO-UNDO.


FORM
SPACE(2)
date1
FORMAT "99.99.9999"
LABEL  ' Дата c '
HELP   'Введите дату начала периода.'
SPACE(2)
date2
FORMAT "99.99.9999"
LABEL  'Дата по'
HELP   'Введите дату окончания периода.'
SPACE(2)
groupLst1
LABEL  '   Периодичность'
HELP   'Выберите периодичность (Пробел - выбрать, TAB - завершить выбор).'
SKIP(1)
groupLst2
LABEL  '   Операции'
HELP   '(Пробел - выбрать, TAB - завершить выбор).'
SKIP
WITH FRAME fr_main WIDTH 53 CENTERED ROW 5 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[ СПРАВКА ОБ ОБОРОТАХ ПО СЧЕТУ ]".
UPDATE date1 date2 groupLst1 groupLst2 WITH FRAME fr_main.

ASSIGN
/* s_acct     = GetParam("Acct" ,iParam) */
s_date1    = STRING(DAY(date1),"99") + "." + STRING(MONTH(date1),"99") + "." + STRING(YEAR(date1),"9999")
s_date2    = STRING(DAY(date2),"99") + "." + STRING(MONTH(date2),"99") + "." + STRING(YEAR(date2),"9999")
.


DEF BUFFER bAcctPos FOR acct-pos.
DEF BUFFER bOpEntry FOR op-entry.
DEF BUFFER bAcct    FOR acct.

DEF VAR ostIn      AS DEC                  NO-UNDO.
DEF VAR ostOut     AS DEC                  NO-UNDO.
DEF VAR skp        AS CHAR FORMAT "X(2)"   NO-UNDO.
DEF VAR acct1      LIKE acct.acct          NO-UNDO.
DEF VAR acct2      LIKE acct.number        NO-UNDO.
DEF VAR acct1_side LIKE acct.side          NO-UNDO.
DEF VAR acct1_nz   LIKE acct.Details       NO-UNDO.
DEF VAR acct2_nz   LIKE acct.Details       NO-UNDO.
DEF VAR currency1  LIKE acct.currency      NO-UNDO.
DEF VAR details    LIKE op.details         NO-UNDO.
DEF VAR summDt     LIKE op-entry.amt-rub   NO-UNDO.
DEF VAR summKt     LIKE op-entry.amt-rub   NO-UNDO.
DEF VAR op_date    LIKE op.op-date         NO-UNDO.
DEF VAR doc_num    LIKE op.doc-num         NO-UNDO.
DEF VAR doc_type   LIKE op.doc-type        NO-UNDO.
DEF VAR contr-date AS CHAR                 NO-UNDO.
DEF VAR kau-db     AS CHAR                 NO-UNDO.
DEF VAR kau-cr     AS CHAR                 NO-UNDO.
DEF VAR trns       AS CHAR                 NO-UNDO.

/* таблица пров */
DEF TEMP-TABLE ttRec NO-UNDO
FIELD sOp_date   AS CHAR
FIELD dOp_date   AS DATE
FIELD sSumDT     AS CHAR
FIELD fSumDT     AS DEC
FIELD sSumKT     AS CHAR
FIELD fSumKT     AS DEC
FIELD sAcct2     AS CHAR
FIELD sAcct2_nz  AS CHAR
FIELD sIn        AS CHAR
FIELD fIn        AS DEC
FIELD sOut       AS CHAR
FIELD fOut       AS DEC
FIELD sDetails   AS CHAR
.

DEF TEMP-TABLE ttOB NO-UNDO
FIELD dOp_date AS DATE
FIELD sOp_date AS CHAR
FIELD sObDT    AS CHAR
FIELD sObKT    AS CHAR
FIELD dObDT    AS DEC
FIELD dObKT    AS DEC
FIELD sPer     AS CHAR
.


DEF TEMP-TABLE ttOBGr NO-UNDO
FIELD dOp_date AS DATE
FIELD sObDT    AS CHAR
FIELD sObKT    AS CHAR
FIELD dObDT    AS DEC
FIELD dObKT    AS DEC
FIELD sPer     AS CHAR
.


/* Функция исключения */
FUNCTION isNotExcAcct RETURN LOGICAL (INPUT iAcct AS CHAR):
  DEF VAR result AS LOGICAL NO-UNDO.
  IF iAcct BEGINS "30233810"
  OR iAcct BEGINS "45106810"
  OR iAcct BEGINS "45107810"
  OR iAcct BEGINS "45108810"
  OR iAcct BEGINS "45201810"
  OR iAcct BEGINS "45204810"
  OR iAcct BEGINS "45205810"
  OR iAcct BEGINS "45206810"
  OR iAcct BEGINS "45207810"
  OR iAcct BEGINS "45208810"
  OR iAcct BEGINS "45305810"
  OR iAcct BEGINS "45306810"
  OR iAcct BEGINS "45307810"
  OR iAcct BEGINS "45406810"
  OR iAcct BEGINS "45407810"
  OR iAcct BEGINS "45408810"
  OR iAcct BEGINS "45504810"
  OR iAcct BEGINS "45505810"
  OR iAcct BEGINS "45506810"
  OR iAcct BEGINS "45507810"
  OR iAcct BEGINS "45509810"
  THEN result = FALSE.
  ELSE result = TRUE.
  RETURN result.
END.


FUNCTION MonthName RETURN CHAR (INPUT iNum AS CHAR):
  DEF VAR result AS CHAR NO-UNDO.
  IF iNum EQ "01" THEN result = "Январь".
  IF iNum EQ "02" THEN result = "Февраль".
  IF iNum EQ "03" THEN result = "Март".
  IF iNum EQ "04" THEN result = "Апрель".
  IF iNum EQ "05" THEN result = "Май".
  IF iNum EQ "06" THEN result = "Июнь".
  IF iNum EQ "07" THEN result = "Июль".
  IF iNum EQ "08" THEN result = "Август".
  IF iNum EQ "09" THEN result = "Сентябрь".
  IF iNum EQ "10" THEN result = "Октябрь".
  IF iNum EQ "11" THEN result = "Ноябрь".
  IF iNum EQ "12" THEN result = "Декабрь".
  RETURN result.
END.


FUNCTION QuartName RETURN CHAR (INPUT iMonth AS CHAR):
  DEF VAR result AS CHAR NO-UNDO.
  IF CAN-DO("01,02,03",iMonth) THEN result = "1 квартал".
  IF CAN-DO("04,05,06",iMonth) THEN result = "2 квартал".
  IF CAN-DO("07,08,09",iMonth) THEN result = "3 квартал".
  IF CAN-DO("10,11,12",iMonth) THEN result = "4 квартал".
  RETURN result.
END.



FOR EACH tmprecid NO-LOCK, FIRST acct WHERE RECID(acct) EQ tmprecid.id NO-LOCK.
    ASSIGN
    acct1      = acct.acct
    acct1_side = acct.side
    acct1_nz   = acct.Details
    currency1  = acct.currency
    .
    DO mDate = date1 TO date2:
        FOR EACH op-entry WHERE op-entry.op-date EQ mDate
                            AND (op-entry.op-status BEGINS "√")
                            AND (op-entry.acct-db EQ acct1 OR op-entry.acct-cr EQ acct1)
                            AND (op-entry.acct-db EQ acct1 OR op-entry.acct-cr EQ acct1)
                            NO-LOCK,
                            FIRST op WHERE op.op = op-entry.op NO-LOCK BREAK BY op-entry.op-date BY op.op.

            IF op-entry.acct-db <> acct1 THEN
            DO:
              ASSIGN
              acct2 = SUBSTRING(op-entry.acct-db,1,20)
              summDt = 0.00
              summKt = op-entry.amt-rub
              .
            END.
            IF op-entry.acct-cr <> acct1 THEN 
            DO:
              ASSIGN
              acct2 = SUBSTRING(op-entry.acct-cr,1,20)
              summDt = op-entry.amt-rub
              summKt = 0.00
              .
            END.
            IF acct2 = ? THEN
            DO:
              FIND FIRST bOpEntry WHERE bOpEntry.op-date = op-entry.op-date 
                                    AND bOpEntry.op      = op-entry.op 
                                    AND bOpEntry.acct-db <> op-entry.acct-db NO-LOCK NO-ERROR.
              IF AVAIL bOpEntry THEN
              DO:
                IF bOpEntry.acct-db <> ? THEN acct2 = SUBSTRING(bOpEntry.acct-db, 1, 20).
                IF bOpEntry.acct-cr <> ? THEN acct2 = SUBSTRING(bOpEntry.acct-cr, 1, 20).
              END.
            END.
            IF op.ben-acct <> "" AND op.ben-acct <> ? THEN
            DO:
              ASSIGN
              acct2    = op.ben-acct
              acct2_nz = op.name-ben
              .
            END.

            FIND FIRST op-bank WHERE op-bank.op = op-entry.op NO-LOCK NO-ERROR.
            IF NOT AVAIL op-bank THEN DO:
              FIND FIRST bAcct WHERE bAcct.number BEGINS acct2 NO-LOCK NO-ERROR.
              IF AVAIL bAcct THEN acct2_nz = TRIM(bAcct.Details).
            END.

            RUN acct-pos IN h_base (acct1, currency1, op.op-date - 1, op.op-date - 1, ?).
            IF CAN-DO("А,П", acct1_side) AND sh-bal  < 0 THEN ostIn = ABS(sh-bal).
            IF CAN-DO("А,П", acct1_side) AND sh-bal >= 0 THEN ostIn = sh-bal.

            RUN acct-pos IN h_base (acct1, currency1, op.op-date, op.op-date, ?).
            IF CAN-DO("А,П", acct1_side) AND sh-bal  < 0 THEN ostOut = ABS(sh-bal).
            IF CAN-DO("А,П", acct1_side) AND sh-bal >= 0 THEN ostOut = sh-bal.
            
            IF op.contract-date NE ? THEN contr-date = String(DAY(op.contract-date),"99") + "." + String(MONTH(op.contract-date),"99") + "." + String(YEAR(op.contract-date),"9999"). ELSE contr-date = "?".
            IF op-entry.kau-db  NE ? THEN kau-db     = op-entry.kau-db.
            IF op-entry.kau-cr  NE ? THEN kau-cr     = op-entry.kau-cr.
            trns = op.op-kind.

            ASSIGN
            op_date   = IF op.op-date      <> ? THEN op.op-date      ELSE ?
            doc_num   = IF op.doc-num      <> ? THEN op.doc-num      ELSE ""
            doc_type  = IF op.doc-type     <> ? THEN op.doc-type     ELSE ""
            details   = IF op.details      <> ? THEN op.details      ELSE ""
            skp       = IF op-entry.symbol <> ? THEN op-entry.symbol ELSE ""
            acct1_nz  = IF acct1_nz         EQ ? THEN ""              ELSE acct1_nz
            acct2_nz  = IF acct2_nz         EQ ? THEN ""              ELSE acct2_nz
            details = REPLACE(details,CHR(10),"")
            details = REPLACE(details,"|"," ")
            details = "[" + contr-date + "][" + kau-db + "][" + kau-cr + "][" + trns + "] " + details.
            .

            /* Все или исключать */
            CASE INT64(groupLst2:SCREEN-VALUE):
            WHEN 1 THEN DO: 
                CREATE ttRec.
                ASSIGN
                ttRec.sOp_date  = TRIM(STRING(op_date, "99.99.9999"))
                ttRec.dOp_date  = op_date
                ttRec.sSumDT    = TRIM(STRING(summDt, "-zzzzzzzzzzzzzzz9.99"))
                ttRec.fSumDT    = summDt
                ttRec.sSumKT    = TRIM(STRING(summKt, "-zzzzzzzzzzzzzzz9.99"))
                ttRec.fSumKT    = summKt
                ttRec.sAcct2    = TRIM(STRING(acct2, "x(20)"))
                ttRec.sAcct2_nz = TRIM(STRING(TRIM(REPLACE(acct2_nz, CHR(10), ""))))
                ttRec.sIn       = TRIM(STRING(ostIn,  "-zzzzzzzzzzzzzzz9.99"))
                ttRec.fIn       = ostIn
                ttRec.sOut      = TRIM(STRING(ostOut, "-zzzzzzzzzzzzzzz9.99"))
                ttRec.fOut      = ostOut
                ttRec.sDetails  = details
                .
            END.
            WHEN 2 THEN DO:
              IF isNotExcAcct(acct2) THEN
              DO:
                CREATE ttRec.
                ASSIGN
                ttRec.sOp_date  = TRIM(STRING(op_date, "99.99.9999"))
                ttRec.dOp_date  = op_date
                ttRec.sSumDT    = TRIM(STRING(summDt, "-zzzzzzzzzzzzzzz9.99"))
                ttRec.fSumDT    = summDt
                ttRec.sSumKT    = TRIM(STRING(summKt, "-zzzzzzzzzzzzzzz9.99"))
                ttRec.fSumKT    = summKt
                ttRec.sAcct2    = TRIM(STRING(acct2, "x(20)"))
                ttRec.sAcct2_nz = TRIM(STRING(TRIM(REPLACE(acct2_nz, CHR(10), ""))))
                ttRec.sIn       = TRIM(STRING(ostIn,  "-zzzzzzzzzzzzzzz9.99"))
                ttRec.fIn       = ostIn
                ttRec.sOut      = TRIM(STRING(ostOut, "-zzzzzzzzzzzzzzz9.99"))
                ttRec.fOut      = ostOut
                ttRec.sDetails  = details
                .
              END.
            END.
            END CASE.
        END.
    END.
END.


/* Обсчет оборотов фильтрованных */
DO mDate = date1 TO date2:
    ASSIGN
    dObDT = 0.0000
    dObKT = 0.0000
    .
    FOR EACH ttRec WHERE ttRec.dOp_date EQ mDate NO-LOCK BY ttRec.dOp_date.
        ASSIGN
        dObDT = dObDT + ttRec.fSumDT
        dObKT = dObKT + ttRec.fSumKT
        .
    END.
    IF dObDT > 0.0000 OR dObKT > 0.0000 THEN
    DO:
        CREATE ttOB.
        ASSIGN
            ttOB.dOp_date = mDate
            ttOB.sOp_date = String(DAY(mDate),"99") + "." + String(MONTH(mDate),"99") + "." + String(YEAR(mDate),"9999")
            ttOB.sObDT = TRIM( STRING(dObDT, "-zzzzzzzzzzzzzzz9.99") )
            ttOB.sObKT = TRIM( STRING(dObKT, "-zzzzzzzzzzzzzzz9.99") )
            ttOB.dObDT = dObDT
            ttOB.dObKT = dObKT
        .
        CASE INT64(groupLst1:SCREEN-VALUE):
            WHEN 1 THEN DO:
                ttOB.sPer = String(DAY(mDate),"99") + "." + String(MONTH(mDate),"99") + "." + String(YEAR(mDate),"9999").
            END.
            WHEN 2 THEN DO:
                ttOB.sPer = MonthName(String(MONTH(mDate),"99")) + " " + String(YEAR(mDate),"9999").
            END.
            WHEN 3 THEN DO:
                ttOB.sPer = QuartName(String(MONTH(mDate),"99")) + " " + String(YEAR(mDate),"9999").
            END.
            WHEN 4 THEN DO:
                ttOB.sPer = String(YEAR(mDate),"9999").
            END.
        END CASE.
    END.
END.


/* Обсчет сгруппированных */
ASSIGN
    dObDT = 0.0000
    dObKT = 0.0000
.
FOR EACH ttOB NO-LOCK BREAK BY ttOB.sPer.
    IF NOT LAST-OF(ttOB.sPer) THEN
    DO:
        ASSIGN
        dObDT = dObDT + ttOB.dObDT
        dObKT = dObKT + ttOB.dObKT
        .
    END.
    ELSE
    IF LAST-OF(ttOB.sPer) THEN
    DO:
      ASSIGN
      dObDT = dObDT + ttOB.dObDT
      dObKT = dObKT + ttOB.dObKT
      .
      IF dObDT > 0.0000 OR dObKT > 0.0000 THEN
      DO:
        CREATE ttOBGr.
        ASSIGN
        ttOBGr.dOp_date = ttOB.dOp_date
        ttOBGr.sPer  = ttOB.sPer
        ttOBGr.dObDT = dObDT
        ttOBGr.dObKT = dObKT
        ttOBGr.sObDT = TRIM(STRING(dObDT,"-zzzzzzzzzzzzzzz9.99"))
        ttOBGr.sObKT = TRIM(STRING(dObKT,"-zzzzzzzzzzzzzzz9.99"))
        dObDT        = 0.0000
        dObKT        = 0.0000
        .
      END.
    END.
END.


/* Вывод на экран */
DEF VAR s1 AS CHAR NO-UNDO.
DEF VAR s2 AS CHAR NO-UNDO.
DEF VAR s3 AS CHAR NO-UNDO.
DEF VAR s4 AS CHAR NO-UNDO.
{setdest.i}
PUT UNFORMATTED "АО 'Банк'                                                   " SKIP.
PUT UNFORMATTED "                                                                  " SKIP.
PUT UNFORMATTED "        Справка об оборотах по счету " STRING(acct1,"X(20)")        SKIP.
PUT UNFORMATTED "               за период с " s_date1 " по " s_date2                 SKIP.
PUT UNFORMATTED "                                                                  " SKIP.
PUT UNFORMATTED "                                                                  " SKIP.
CASE INT64(groupLst2:SCREEN-VALUE):
  WHEN 1 THEN DO:
    PUT UNFORMATTED "Режим: Все операции                                           " SKIP.
  END.
  WHEN 2 THEN DO:
    PUT UNFORMATTED "Режим: Исключить операции с судным счетом                     " SKIP.
  END.
END CASE.
PUT UNFORMATTED "┌────────────────────┬─────────────────────┬─────────────────────┐" SKIP.
PUT UNFORMATTED "│       Период       │  Обороты по Дебету  │  Обороты по Кредиту │" SKIP.
PUT UNFORMATTED "├────────────────────┼─────────────────────┼─────────────────────┤" SKIP.
ASSIGN
dItogoDT = 0.0000
dItogoKT = 0.0000
.
FOR EACH ttOBGr NO-LOCK BY ttOBGr.dOp_date.
    ASSIGN
    dItogoDT = dItogoDT + ttOBGr.dObDT
    dItogoKT = dItogoKT + ttOBGr.dObKT
    .
    IF LENGTH(ttOBGr.sPer) < 20    THEN s1 = FILL(" ",20 - LENGTH(ttOBGr.sPer)) + TRIM(ttOBGr.sPer).
    ELSE s1 = ttOBGr.sPer.

    IF LENGTH(ttOBGr.sObDT)   < 21 THEN s2 = FILL(" ",21 - LENGTH(ttOBGr.sObDT)) + TRIM(ttOBGr.sObDT).
    ELSE s2 = ttOBGr.sObDT.

    IF LENGTH(ttOBGr.sObKT)   < 21 THEN s3 = FILL(" ",21 - LENGTH(ttOBGr.sObKT)) + TRIM(ttOBGr.sObKT).
    ELSE s3 = ttOBGr.sObKT.

    PUT UNFORMATTED "│" s1 "│" s2 "│" s3 "│" SKIP.
    ASSIGN
    s1 = ""
    s2 = ""
    s3 = ""
    .
END.
PUT UNFORMATTED "├────────────────────┼─────────────────────┼─────────────────────┤" SKIP.

ASSIGN
s2 = TRIM(STRING(dItogoDT,"-zzzzzzzzzzzzzzz9.99"))
s3 = TRIM(STRING(dItogoKT,"-zzzzzzzzzzzzzzz9.99"))
.

IF LENGTH(s2)   < 21 THEN s2 = FILL(" ",21 - LENGTH(s2)) + TRIM(s2).
IF LENGTH(s3)   < 21 THEN s3 = FILL(" ",21 - LENGTH(s3)) + TRIM(s3).
PUT UNFORMATTED "│       ИТОГО:       │" s2 "│" s3 "│" SKIP.
ASSIGN
s1 = ""
s2 = ""
s3 = ""
.
FIND FIRST _user WHERE _user._userid EQ userid('bisquit') NO-LOCK NO-ERROR.
IF AVAIL _user THEN s4 = TRIM(_user._user-name).
PUT UNFORMATTED "└────────────────────┴─────────────────────┴─────────────────────┘" SKIP.
PUT UNFORMATTED "                                                                  " SKIP.
PUT UNFORMATTED "Исполнитель: " s4                                                   SKIP.
{preview.i}