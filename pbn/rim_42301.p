/*
      Comment: График оплаты основного долга
   Parameters:
         Uses:
      Used by:
     Modified: 
*/
{globals.i}
{tmprecid.def}
{intrface.get loan}
{intrface.get xclass}
{intrface.get i254}
/*
{intrface.get rsrv}
*/
{sh-defs.i}

PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF INPUT PARAM iParam AS CHARACTER NO-UNDO.

DEF VAR str     AS CHAR NO-UNDO.
DEF VAR fname   AS CHAR NO-UNDO.
DEF VAR user_ip AS CHAR FORMAT "X(15)" NO-UNDO.
DEF VAR fpath   AS CHAR INIT "/qbis/wrk/imp-exp/0000/preview/" NO-UNDO.
user_ip=TRIM(SUBSTRING(OS-GETENV("SSH_CLIENT"),1,INDEX(OS-GETENV("SSH_CLIENT")," "))).
/*fname = fpath + "_tmp@" + user_ip.*/
fname = "_spool1.tmp".

DEF VAR mDate   AS DATE INIT TODAY NO-UNDO.
DEF VAR vOBKT1  AS DEC NO-UNDO.
DEF VAR vOBKT2  AS DEC NO-UNDO.
DEF VAR vOUTKT1 AS DEC NO-UNDO.
DEF VAR vOUTKT2 AS DEC NO-UNDO.
DEF BUFFER bloan-acct FOR loan-acct.

/* k beg obesp */
DEF VAR sObVidDog   AS CHAR NO-UNDO.
DEF VAR sObDate-beg AS CHAR NO-UNDO.
DEF VAR sObDate-end AS CHAR NO-UNDO.
DEF VAR sObName     AS CHAR NO-UNDO.
/* k end obesp */


DEF TEMP-TABLE ttRec NO-UNDO
  FIELD date         AS DATE
  FIELD doc-ref      AS CHAR
  FIELD dog_name     AS CHAR
  FIELD phone_dr     AS CHAR
  FIELD acct         AS CHAR
  FIELD acctdet      AS CHAR
  FIELD accttype     AS CHAR
  FIELD ob_not       AS DEC
  FIELD ob_yes       AS DEC
  FIELD ost_not      AS DEC
  FIELD ost_yes      AS DEC
.


/* форма */
FORM
SKIP(1)
mDate
FORMAT "99.99.9999"
LABEL  '           Дата'
HELP   'Дата остатоков и оборотов'
SKIP(1)
WITH FRAME fr_main WIDTH 40 CENTERED ROW 10 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[Остатки и пополнения 42301 на дату]".
UPDATE mDate WITH FRAME fr_main.




/* Телефон клиента */
FUNCTION GetPhone RETURN CHAR (INPUT iID AS INT):
  DEF VAR result AS CHAR NO-UNDO.
  FIND FIRST person WHERE person.person-id EQ iID NO-LOCK NO-ERROR.
  IF AVAIL person THEN 
  DO:
     result = "Тел.:" + STRING(TRIM(GetXAttrValueEx("person", String(iID), "Телефон3",""))) + " " +
              "Тел.дом.:" + STRING(TRIM(GetXAttrValueEx("person", String(iID), "phone-home","")))
     .
  END.
  IF result EQ ? THEN result = "".
  RETURN result.
END.


/* Имя клиента */
FUNCTION GetNameID RETURN CHAR (INPUT iID AS INT):
  DEF VAR result AS CHAR NO-UNDO.
  FIND FIRST person WHERE person.person-id EQ iID NO-LOCK NO-ERROR.
  IF AVAIL person THEN result = person.name-last + " " + person.first-names.
  FIND FIRST cust-corp WHERE INT64(cust-corp.cust-id) EQ iID NO-LOCK NO-ERROR.
  IF AVAIL cust-corp THEN result = cust-corp.cust-stat + " " + cust-corp.name-short.
  RETURN TRIM(result).
END.


FOR EACH loan WHERE CAN-DO ('!l_all_pk,!l_all_pkn,*', TRIM (loan.class-code))
                AND loan.cust-cat EQ 'Ч' AND loan.Filial-id EQ '0000'
                AND loan.branch-id EQ '0000' NO-LOCK,
               LAST loan-acct OF loan WHERE loan-acct.since LE loan.since
                AND CAN-DO('!4540*,!45509*,!30233*,45505*,45506*,45507*,45502*,45503*,45504*', TRIM(loan-acct.acct))
                AND loan-acct.acct-type EQ "Кредит" 
                AND loan.close-date EQ ? 
                NO-LOCK.

    FIND LAST bloan-acct WHERE bloan-acct.cont-code EQ     loan.cont-code
                           AND bloan-acct.acct      BEGINS "42301810"
                           AND bloan-acct.acct-type EQ     "КредРасч"
                           NO-LOCK NO-ERROR.
    IF AVAIL bloan-acct THEN
    DO:
        ASSIGN
        vOBKT1  = 0.00
        vOUTKT1 = 0.00
        .
        RUN acct-pos IN h_base (bloan-acct.acct,"",mDate,mDate,"√").
        ASSIGN
            vOBKT1  = sh-cr
            vOUTKT1 = IF sh-bal LE 0.00 THEN ABS(sh-bal) ELSE 0.00
        .
        ASSIGN
        vOBKT2  = 0.00
        vOUTKT2 = 0.00
        .

        RUN acct-pos IN h_base (bloan-acct.acct,"",mDate,mDate,"П").
        ASSIGN
            vOBKT2  = sh-cr
            vOUTKT2 = IF sh-bal LE 0.00 THEN ABS(sh-bal) ELSE 0.00
        .

        IF ABS(vOBKT1) > 0.00 OR ABS(vOBKT2) > 0.00 THEN
        DO:
            CREATE ttRec.
            ASSIGN
            ttRec.date     = mDate
            ttRec.dog_name = GetNameID(loan.cust-id)
            ttRec.doc-ref  = loan.doc-ref
            ttRec.phone_dr = GetPhone(loan.cust-id)
            ttRec.acct     = TRIM(bloan-acct.acct)
            ttRec.accttype = "Расчетный"
            ttRec.ob_yes   = vOBKT1
            ttRec.ob_not   = vOBKT2
            ttRec.ost_yes  = vOUTKT1
            ttRec.ost_not  = vOUTKT2
            .
        END.
    END.

    FOR EACH term-obl WHERE term-obl.cont-code BEGINS loan.doc-ref AND
                        term-obl.contract EQ 'Кредит' AND term-obl.idnt EQ 5 NO-LOCK
                        BY term-obl.end-date BY term-obl.cont-code.

        ASSIGN
        sObVidDog   = ""
        sObDate-beg = ""
        sObDate-end = ""
        sObName     = ""
        .

        /* вид договора обеспечения */
        sObVidDog = GetCodeName("ВидОб", GetXAttrValueEx ("term-obl",term-obl.contract + "," + term-obl.cont-code + ",5," + STRING(term-obl.end-date) + "," + STRING(term-obl.nn),"ВидОб","")).
        sObName = GetNameID(term-obl.fop).

        /* Срок обязательства начало */
        IF term-obl.fop-date <> ? THEN
        sObDate-beg = String(DAY(term-obl.fop-date),"99") + "." + String(MONTH(term-obl.fop-date),"99") + "." + String(YEAR(term-obl.fop-date),"9999").

        /* Срок обязательства окончение */
        IF term-obl.end-date <> ? THEN 
        sObDate-end = String(DAY(term-obl.end-date),"99") + "." + String(MONTH(term-obl.end-date),"99") + "." + String(YEAR(term-obl.end-date),"9999").

        IF sObVidDog BEGINS "Поручительство" THEN
        DO:
            FOR EACH acct WHERE acct.cust-id    EQ term-obl.fop
                            AND acct.close-date EQ ?
                            AND CAN-DO("40802810*,40702810*",acct.acct)
                            NO-LOCK.

                ASSIGN
                vOBKT1  = 0.00
                vOUTKT1 = 0.00
                .
                RUN acct-pos IN h_base (acct.acct,"",mDate,mDate,"√").
                ASSIGN
                    vOBKT1  = sh-cr
                    vOUTKT1 = IF sh-bal LE 0.00 THEN ABS(sh-bal) ELSE 0.00
                .

                ASSIGN
                vOBKT2  = 0.00
                vOUTKT2 = 0.00
                .
                RUN acct-pos IN h_base (acct.acct,"",mDate,mDate,"П").
                ASSIGN
                vOBKT2  = sh-cr
                vOUTKT2 = IF sh-bal LE 0.00 THEN ABS(sh-bal) ELSE 0.00
                .
/*
                IF   ABS(vOBKT1) > 0.00
                  OR ABS(vOBKT2) > 0.00
                  OR ABS(vOUTKT1)> 0.00
                  OR ABS(vOUTKT2)> 0.00
                THEN
                DO:
*/
                FIND FIRST ttRec WHERE ttRec.acct EQ TRIM(STRING(acct.acct,"x(20)")) NO-LOCK NO-ERROR.
                IF NOT AVAIL ttRec THEN
                DO:
                    CREATE ttRec.
                    ASSIGN
                    ttRec.date     = mDate
                    ttRec.dog_name = GetNameID(loan.cust-id)
                    ttRec.doc-ref  = loan.doc-ref


                    ttRec.doc-ref     = IF CAN-DO("2010-0159*,2011-0085*,2011-0113*,2011-0129*,2012-0023*,2012-0090*,2012-0100*,2012-0122*,2012-0128*,2012-0149*,2013-0002*,2013-0063*,2013-0085*,2013-0102*,2013-0108*,2013-0152*,2013-0153*,2013-0165*,2014-0066*,2015-0118*,5000-0579*,5000-0588*,2010-0132*,2011-0050*,2011-0158*,2011-0174*,2011-0194*,2011-0244*,2011-0263*,2012-0032*,2012-0079*,2012-0096*,2013-0025*,2013-0028*,2013-0049*,2013-0073*,2013-0105*,2013-0109*,2013-0151*,2013-0210*,2014-0030*,2014-0076*", loan.doc-ref) THEN loan.doc-ref + "  Конвертер!" ELSE loan.doc-ref
                    ttRec.doc-ref     = IF CAN-DO("2012-0023*,2012-0090*,2012-0100*,2012-0122*,2013-0002*,2013-0063*,2013-0085*,2013-0102*,5000-0588*", loan.doc-ref) THEN loan.doc-ref + "  6пар!!!" ELSE loan.doc-ref
                    ttRec.doc-ref     = IF CAN-DO("2013-0108*", loan.doc-ref) THEN  loan.doc-ref + "  32пар!!!"  ELSE loan.doc-ref
                    ttRec.doc-ref     = IF CAN-DO("2013-0153*", loan.doc-ref) THEN  loan.doc-ref + "  16пар!!!"  ELSE loan.doc-ref
                    ttRec.doc-ref     = IF CAN-DO("2013-0165*,5000-0579*", loan.doc-ref) THEN loan.doc-ref + "  31пар!!!" ELSE loan.doc-ref
                    ttRec.doc-ref     = IF CAN-DO("1601-0249*,2013-0099*", loan.doc-ref) THEN loan.doc-ref + "  Проверить!" ELSE loan.doc-ref


                    ttRec.phone_dr = GetPhone(loan.cust-id)
                    ttRec.acct     = TRIM(STRING(acct.acct,"x(20)"))
                    ttRec.accttype = "Поручитель"
                    ttRec.acctdet  = REPLACE(REPLACE(TRIM(acct.details),CHR(10),""),"|","")
                    ttRec.ob_yes   = vOBKT1
                    ttRec.ob_not   = vOBKT2
                    ttRec.ost_yes  = vOUTKT1
                    ttRec.ost_not  = vOUTKT2
                    .
                END.
/*
                END.
*/
            END.
        END.
    END.
END.


/* Выгрузка */
OS-DELETE VALUE(fname).
OUTPUT TO VALUE(fname) CONVERT TARGET "1251".
PUT UNFORMATTED "k_42301" SKIP.
/*PUT UNFORMATTED "Дата|Номер договора|Заемщик|Телефон|Счет 42301|Пополнения акцептованные|Остаток акцептованный|Пополнения неакцептованные|Остаток неакцептованный" SKIP.*/
FOR EACH ttRec NO-LOCK BY ttRec.doc-ref BY ttRec.accttype BY ttRec.acct.
    str = String(DAY(ttRec.date),"99") + "." + String(MONTH(ttRec.date),"99") + "." + String(YEAR(ttRec.date),"9999") + "|" +
          ttRec.doc-ref                                                                                               + "|" +
          ttRec.dog_name                                                                                              + "|" +
          ttRec.phone_dr                                                                                              + "|" +
          STRING(ttRec.acct,"X(20)")                                                                                  + "|" +
          ttRec.acctdet                                                                                               + "|" +
          ttRec.accttype                                                                                              + "|" +
          REPLACE(TRIM(STRING(ttRec.ob_yes , "-zzzzzzzzzzzzz9.99")),".",",")                                          + "|" +
          REPLACE(TRIM(STRING(ttRec.ost_yes, "-zzzzzzzzzzzzz9.99")),".",",")                                          + "|" +
          REPLACE(TRIM(STRING(ttRec.ob_not , "-zzzzzzzzzzzzz9.99")),".",",")                                          + "|" +
          REPLACE(TRIM(STRING(ttRec.ost_not, "-zzzzzzzzzzzzz9.99")),".",",")
    .
    PUT UNFORMATTED str SKIP.
END.
OUTPUT CLOSE.
MESSAGE "Файл " + fname + " сформирован !!!" VIEW-AS ALERT-BOX.


/* Передать на вывод */
RUN sndbispc.p("file=" + fname + ";class=" + iParam).