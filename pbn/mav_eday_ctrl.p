/*                                                                                                    */
/*                                                                                                    */
/*                         Žâç¥â ¯® íª®­®¬¨áâ ¬ ŠŽ ¤«ï ¥¦¥¤­¥¢­®£® ª®­âà®«ï                          */
/*                                                                                                    */
/*                                                                                                    */

PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF VAR dateBeg AS DATE INIT TODAY NO-UNDO.
DEF VAR dateEnd AS DATE INIT TODAY NO-UNDO.

FORM
SPACE(2)
dateBeg
FORMAT "99.99.9999"
LABEL  '¥à¨®¤ á'
HELP   '‚¢¥¤¨â¥ ¤ âã ­ ç «  ¯¥à¨®¤ .'
SPACE(1)
dateEnd
FORMAT "99.99.9999"
LABEL  '¯®'
HELP   '‚¢¥¤¨â¥ ¤ âã ®ª®­ç ­¨ï ¯¥à¨®¤ .'
SPACE(2)
SKIP

WITH FRAME frMain CENTERED ROW 10 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[ Ž’—…’ Ž ŠŽŽŒˆ‘’€Œ ]".
UPDATE dateBeg dateEnd WITH FRAME frMain.

/*----------------------------------------------------------------------------------------------------*/

DEF VAR dateBeg_  AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR dateEnd_  AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR countKLB  AS INT                 NO-UNDO.
DEF VAR countPPB  AS INT                 NO-UNDO.
DEF VAR countSCH  AS INT                 NO-UNDO.
DEF VAR countOVZ  AS INT                 NO-UNDO.
DEF VAR totalKLB  AS INT                 NO-UNDO.
DEF VAR totalPPB  AS INT                 NO-UNDO.
DEF VAR totalSCH  AS INT                 NO-UNDO.
DEF VAR totalOVZ  AS INT                 NO-UNDO.
DEF VAR userName  AS CHAR                NO-UNDO.
DEF VAR strPut    AS CHAR                NO-UNDO.

ASSIGN
dateBeg_  = STRING(dateBeg, "99.99.9999")
dateEnd_  = STRING(dateEnd, "99.99.9999")
.

/*
klb*                      //ª«¨¥­â-¡ ­ª
scan*,0101-01r,0101-03r   //¯« â¥¦­ë¥ ¯®àãç¥­¨ï ­  ¡ã¬ £¥
ª50004b                   //á­ïâ¨¥ ¯® ç¥ªã
ª50001                    //®¡êï¢«¥­¨¥ ­  ¢§­®á (¢ ª ááã)
*/

{setdest.i}

PUT "                                                                                               " SKIP.
PUT "  ¥à¨®¤ á " dateBeg_ " ¯® " dateEnd_ "                                                        " SKIP.
PUT "                                                                                               " SKIP.
PUT "                                                                                               " SKIP.
PUT "                                                                                               " SKIP.
PUT "                         Ž’—…’ Ž ŠŽŽŒˆ‘’€Œ „‹Ÿ …†…„…‚ŽƒŽ ŠŽ’Ž‹Ÿ                         " SKIP.
PUT "                                                                                               " SKIP.
PUT " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ " SKIP.
PUT " ³                                          ³              Š®«¨ç¥áâ¢® ®¯¥à æ¨©               ³ " SKIP.
PUT " ³                                          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄ´ " SKIP.
PUT " ³         ”ˆŽ íª®­®¬¨áâ  ¯® ãç¥âã          ³             ³ « â¥¦­ë¥ ³ ‘­ïâ¨¥  ³ Ž¡êï¢«¥­¨¥ ³ " SKIP.
PUT " ³                                          ³ Š«¨¥­â-¡ ­ª ³ ¯®àãç¥­¨ï ³   ¯®    ³ ­  ¢§­®á   ³ " SKIP.
PUT " ³                                          ³             ³ ­  ¡ã¬ £¥ ³  ç¥ªã   ³ (¢ ª ááã)  ³ " SKIP.
PUT " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´ " SKIP.

FOR EACH op-entry WHERE op-entry.op-date >= dateBeg
                    AND op-entry.op-date <= dateEnd
                    AND op-entry.op-status BEGINS "û" NO-LOCK,
                    FIRST op WHERE op.op = op-entry.op 
                               AND CAN-DO("*", op.op-kind) 
                               AND CAN-DO("OR*",     op.user-id) NO-LOCK
                               BREAK BY op.user-id.
  
  IF NOT LAST-OF(op.user-id) THEN DO:
    IF CAN-DO("klb*",                    op.op-kind) THEN countKLB = countKLB + 1.
    IF CAN-DO("scan*,0101-01r,0101-03r", op.op-kind) THEN countPPB = countPPB + 1.
    IF CAN-DO("ª50004b",                 op.op-kind) THEN countSCH = countSCH + 1.
    IF CAN-DO("ª50001",                  op.op-kind) THEN countOVZ = countOVZ + 1.
  END.
  
  IF LAST-OF(op.user-id) THEN DO:
    IF CAN-DO("klb*",                    op.op-kind) THEN countKLB = countKLB + 1.
    IF CAN-DO("scan*,0101-01r,0101-03r", op.op-kind) THEN countPPB = countPPB + 1.
    IF CAN-DO("ª50004b",                 op.op-kind) THEN countSCH = countSCH + 1.
    IF CAN-DO("ª50001",                  op.op-kind) THEN countOVZ = countOVZ + 1.
    
    FIND FIRST _user WHERE _user._userid = op.user-id NO-LOCK NO-ERROR.
    userName = _user._user-name.
    
    strPut  = " ³ " + STRING(userName,       "x(40)") +
              " ³ " + STRING(countKLB, "zzzzzzzzzz9") +
              " ³ " + STRING(countPPB,   "zzzzzzzz9") +
              " ³ " + STRING(countSCH,     "zzzzzz9") +
              " ³ " + STRING(countOVZ,  "zzzzzzzzz9") +
              " ³ "
    .
    PUT UNFORMATTED strPut SKIP.
    
    ASSIGN
    userName = ""
    totalKLB = totalKLB + countKLB
    totalPPB = totalPPB + countPPB
    totalSCH = totalSCH + countSCH
    totalOVZ = totalOVZ + countOVZ
    countKLB = 0
    countPPB = 0
    countSCH = 0
    countOVZ = 0
    .
  END.
END.

PUT " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´ " SKIP.

strPut  = " ³ " + STRING("ˆ’ŽƒŽ:",       "x(40)") +
          " ³ " + STRING(totalKLB, "zzzzzzzzzz9") +
          " ³ " + STRING(totalPPB,   "zzzzzzzz9") +
          " ³ " + STRING(totalSCH,     "zzzzzz9") +
          " ³ " + STRING(totalOVZ,  "zzzzzzzzz9") +
          " ³ "
.
PUT UNFORMATTED strPut SKIP.

PUT " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙ " SKIP.
PUT "                                                                                               " SKIP.

{preview.i}


/*
ÚÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ¿
ÃÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄ´
³        ³        ³
ÀÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ
*/
