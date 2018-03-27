/*                                                                                                    */
/*                                                                                                    */
/*                           âç¥â ¯® ®âªàëâë¬/§ ªàëâë¬ áç¥â ¬ §  ¯¥à¨®¤                              */
/*                                                                                                    */
/*                                                                                                    */

PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF VAR dateBeg AS DATE INIT TODAY NO-UNDO.
DEF VAR dateEnd AS DATE INIT TODAY NO-UNDO.
DEF VAR acctMsk AS CHAR INIT "*"   NO-UNDO.
DEF VAR userLst AS CHAR INIT "*"   NO-UNDO.

FORM
SKIP(1)
dateBeg
FORMAT "99.99.9999"
LABEL  '      „ â  ­ ç « '
HELP   '‚¢¥¤¨â¥ ¤ âã ­ ç «  ¯¥à¨®¤ .'
SKIP
dateEnd
FORMAT "99.99.9999"
LABEL  '   „ â  ®ª®­ç ­¨ï'
HELP   '‚¢¥¤¨â¥ ¤ âã ®ª®­ç ­¨ï ¯¥à¨®¤ .'
SKIP
acctMsk
FORMAT 'x(45)'
LABEL  '     Œ áª  áç¥â®¢'
HELP   '‚¢¥¤¨â¥ ¬ áªã áç¥â®¢.'
SPACE(2)
SKIP(1)
WITH FRAME fr_main WIDTH 70 CENTERED ROW 10 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[ ’Š›’›…/‡€Š›’›… ‘—…’€ ]".

UPDATE dateBeg dateEnd acctMsk WITH FRAME fr_main.

/*-------------------------------------------------------------------------------------------------------------------------*/

DEF INPUT PARAM iParam AS CHAR                 NO-UNDO.
DEF VAR str            AS CHAR FORMAT "x(115)" NO-UNDO.
DEF VAR dateBeg_       AS CHAR FORMAT "x(10)"  NO-UNDO.
DEF VAR dateEnd_       AS CHAR FORMAT "x(10)"  NO-UNDO.
DEF VAR details        AS CHAR FORMAT "x(68)"  NO-UNDO.
DEF VAR dogRKO         AS CHAR FORMAT "x(17)"  NO-UNDO.
DEF VAR user_id        AS CHAR                 NO-UNDO.

ASSIGN
user_id  = USERID('bisquit')
acctMsk  = REPLACE(acctMsk, " ", "")
dateBeg_ = STRING(dateBeg, "99.99.9999")
dateEnd_ = STRING(dateEnd, "99.99.9999")
.

{setdest.i}
PUT "                                            ’—…’  ’Š›’›Œ ‘—…’€Œ                                             " SKIP.
PUT "                                                                                                                 " SKIP.
PUT "  ç «®    ¯¥à¨®¤ : " dateBeg_ "                                                                                 " SKIP.
PUT " ª®­ç ­¨¥ ¯¥à¨®¤ : " dateEnd_ "                                                                                 " SKIP.
PUT "                                                                                                                 " SKIP.
PUT "                                                 ’Š›’›… ‘—…’€                                                  " SKIP.
PUT "                                                                                                                 " SKIP.
PUT " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ " SKIP.
PUT " ³ „ â  ®âªàëâ¨ï ³     ®¬¥à áç¥â       ³                         ¨¬¥­®¢ ­¨¥ áç¥â                             ³ " SKIP.
PUT " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ " SKIP.
FOR EACH acct WHERE CAN-DO(acctMsk, acct.number)
                AND acct.open-date >= dateBeg 
                AND acct.open-date <= dateEnd 
                AND acct.open-date <> ? NO-LOCK,
                FIRST loan-acct WHERE loan-acct.acct BEGINS acct.number NO-LOCK.
  details = "".
  IF acct.details <> ? THEN details = acct.details.
  str = " ³   " + STRING(acct.open-date, "99.99.9999") + 
        "  ³ "  + STRING(acct.number, "x(20)")         + 
        " ³ "   + STRING(details, "x(68)")             + 
        " ³".
  dogRKO = "".
  IF CAN-DO(iParam, user_id) THEN dogRKO = REPLACE(loan-acct.cont-code, "@0000", "").
  PUT str dogRKO SKIP.
END.
PUT " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ " SKIP.
{preview.i}

{setdest.i}
PUT "                                            ’—…’  ‡€Š›’›Œ ‘—…’€Œ                                            " SKIP.
PUT "                                                                                                                 " SKIP.
PUT "  ç «®    ¯¥à¨®¤ : " dateBeg_ "                                                                                 " SKIP.
PUT " ª®­ç ­¨¥ ¯¥à¨®¤ : " dateEnd_ "                                                                                 " SKIP.
PUT "                                                                                                                 " SKIP.
PUT "                                                 ‡€Š›’›… ‘—…’€                                                  " SKIP.
PUT "                                                                                                                 " SKIP.
PUT " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ " SKIP.
PUT " ³ „ â  § ªàëâ¨ï ³     ®¬¥à áç¥â       ³                         ¨¬¥­®¢ ­¨¥ áç¥â                             ³ " SKIP.
PUT " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ " SKIP.
FOR EACH acct WHERE CAN-DO(acctMsk, acct.number)
                AND acct.close-date >= dateBeg 
                AND acct.close-date <= dateEnd 
                AND acct.close-date <> ? NO-LOCK,
                FIRST loan-acct WHERE loan-acct.acct BEGINS acct.number NO-LOCK.
  details = "".
  IF acct.details <> ? THEN details = acct.details.
  str = " ³   " + STRING(acct.close-date, "99.99.9999") + 
        "  ³ "  + STRING(acct.number, "x(20)")          + 
        " ³ "   + STRING(details, "x(68)")              + 
        " ³".
  dogRKO = "".
  IF CAN-DO(iParam, user_id) THEN dogRKO = REPLACE(loan-acct.cont-code, "@0000", "").
  PUT str dogRKO SKIP.
END.
PUT " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ " SKIP.
{preview.i}



/*
""
ÚÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ¿
ÃÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄ´
³        ³        ³
ÀÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ
*/
