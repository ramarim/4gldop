/*                                                                                                    */
/*                                                                                                    */
/*                                ░╔╔АБЮ ╙═АА╝╒КЕ ╞Ю╝╒╝╓╝╙ (▌┌▌)                                      */
/*                                                                                                    */
/*                                                                                                    */

PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF VAR i         AS INT  NO-UNDO.
DEF VAR dateRpt   AS DATE INIT TODAY NO-UNDO.
DEF VAR user_id   AS CHAR FORMAT "X(8)"   NO-UNDO.
DEF VAR user_name AS CHAR FORMAT "X(40)"  NO-UNDO.
DEF VAR str       AS CHAR FORMAT "x(155)" NO-UNDO.
DEF VAR s_dateRpt AS CHAR FORMAT "x(10)"  NO-UNDO.
DEF VAR SumDt     AS CHAR FORMAT "x(12)"  NO-UNDO.
DEF VAR SumCt     AS CHAR FORMAT "x(12)"  NO-UNDO.
DEF VAR sTSumDt   AS CHAR FORMAT "x(12)"  NO-UNDO.
DEF VAR sTSumCt   AS CHAR FORMAT "x(12)"  NO-UNDO.
DEF VAR TSumDt    AS DEC  NO-UNDO.
DEF VAR TSumCt    AS DEC  NO-UNDO.
DEF NEW SHARED VARIABLE list-id AS CHARACTER NO-UNDO.

FORM
dateRpt
FORMAT "99.99.9999"
LABEL  '     └═Б═'
HELP   '┌╒╔╓╗Б╔ ╓═БЦ ╝╞╔Ю═Ф╗╝╜╜╝ё╝ ╓╜О.'
SKIP
user_id
FORMAT 'x(10)'
LABEL  '┼╝╜БЮ╝╚╔Ю'
HELP   '┌╒╔╓╗Б╔ А╝БЮЦ╓╜╗╙═ ═╙Ф╔╞Б╝╒═╒Х╔ё╝ ╓╝╙Ц╛╔╜Б(К).'
WITH FRAME fr_main CENTERED ROW 10 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[ ┼─▒▒▌┌⌡┘ └▌┼⌠▄┘█▓⌡ ]".

ON F1 OF user_id IN FRAME fr_main DO:
IF user_id:SCREEN-VALUE NE "*" THEN
list-id = user_id:SCREEN-VALUE.
ELSE list-id = "".
DO TRANSACTION:
RUN op-user1.p(4).
END.
IF LASTKEY EQ 10 THEN
user_id:SCREEN-VALUE = list-id.
END.

UPDATE dateRpt user_id WITH FRAME fr_main.
s_dateRpt = STRING(dateRpt,"99.99.9999").

/*----------------------------------------------------------------*/

{setdest.i}
PUT "                                                                                                                                                    " SKIP.
PUT "                                                                                                                                                    " SKIP.
PUT "                                                     ░┘┘▒▓░ ┼─▒▒▌┌⌡∙ ▐░▌┌▌└▌┼ ┤─ " s_dateRpt "                                                      " SKIP.
PUT "                                                                                                                                                    " SKIP.
PUT "  зддддддддбддддддддддддддддддддддбддддддддддддддддддддддбддддддддддддддбддддддддддддддбддддбдддддддддддддддддддддддддддддддддддддддддддддддддддд©  " SKIP.
PUT "  Ё        Ё                      Ё                      Ё              Ё              Ё    Ё                                                    Ё  " SKIP.
PUT "  Ё N ╓╝╙. Ё       ▒Г╔Б └Б        Ё       ▒Г╔Б ┼Б        Ё   ▒Ц╛╛═ └Б   Ё   ▒Ц╛╛═ ┼Б   Ё ┼▒ Ё                ▒╝╓╔Ю╕═╜╗╔ ╝╞Ю═Ф╗╗                  Ё  " SKIP.
PUT "  Ё        Ё                      Ё                      Ё              Ё              Ё    Ё                                                    Ё  " SKIP.
PUT "  цддддддддеддддддддддддддддддддддеддддддддддддддддддддддеддддддддддддддеддддддддддддддеддддедддддддддддддддддддддддддддддддддддддддддддддддддддд╢  " SKIP.

FIND FIRST _user WHERE _user._userid = user_id NO-LOCK NO-ERROR.
user_name = _user._user-name.

FOR EACH op-entry WHERE op-entry.op-date = dateRpt 
                    AND (op-entry.acct-db BEGINS '20202810'
                     OR  op-entry.acct-cr BEGINS '20202810') 
                    AND  op-entry.op-status = 'Ш' NO-LOCK,
FIRST op WHERE op-entry.op = op.op 
           AND (op.user-inspector = user_id 
            OR (op.user-inspector = "" 
           AND  op.user-id = user_id)) NO-LOCK BY op-entry.amt-rub.
  
  SumDt = "".
  SumCt = "".
  
  IF op-entry.acct-db BEGINS '20202810' THEN DO:
    SumDt = STRING(op-entry.amt-rub,"zzzzzzzz9.99").
    TSumDt = TSumDt + op-entry.amt-rub.
  END.

  IF op-entry.acct-cr BEGINS '20202810' THEN DO:
    SumCt = STRING(op-entry.amt-rub,"zzzzzzzz9.99").
    TSumCt = TSumCt + op-entry.amt-rub.
  END.

  str = ' Ё ' + STRING(op.doc-num,"999999")                      +
        ' Ё ' + STRING(SUBSTRING(op-entry.acct-db,1,20),"x(20)") +
        ' Ё ' + STRING(SUBSTRING(op-entry.acct-cr,1,20),"x(20)") +
        ' Ё ' + STRING(SumDt,"x(12)")                            +
        ' Ё ' + STRING(SumCt,"x(12)")                            +
        ' Ё ' + STRING(op-entry.symbol,"x(2)")                   +
        ' Ё ' + STRING(REPLACE(op.details,chr(10),""),"x(50)")   + 
        ' Ё '
        .
        
  PUT " " str " " SKIP. 
END.

ASSIGN
sTSumDt = STRING(TSumDt,"zzzzzzzz9.99")
sTSumCt = STRING(TSumCt,"zzzzzzzz9.99")
.

PUT "  цддддддддаддддддддддддддддддддддаддддддддддддддддддддддеддддддддддддддеддддддддддддддеддддадддддддддддддддддддддддддддддддддддддддддддддддддддд╢  " SKIP.
PUT "  Ё ┬▓▌┐▌:                                               Ё " sTSumDt  " Ё " sTSumCt  " Ё                                                         Ё  " SKIP.
PUT "  юддддддддддддддддддддддддддддддддддддддддддддддддддддддаддддддддддддддаддддддддддддддаддддддддддддддддддддддддддддддддддддддддддддддддддддддддды  " SKIP.
PUT "                                                                                                                                                    " SKIP.
PUT "    │ЦЕё═╚Б╔Ю-╙═АА╗Ю                                                 " user_name                            "                                       " SKIP.
PUT "   ддддддддддддддддддддддддддд        дддддддддддддддддддддд        дддддддддддддддддддддддддддддддддддддддддд                                      " SKIP.
PUT "    (╜═╗╛╔╜╝╒═╜╗╔ ╓╝╚╕╜╝АБ╗)             (╚╗Г╜═О ╞╝╓╞╗АЛ)            (Д═╛╗╚╗О ╗ ╗╜╗Ф╗═╚К)                                                           " SKIP.
PUT "                                                                                                                                                    " SKIP.
{preview.i}


/*
зддддддддбдддддддд©
цддддддддедддддддд╢
Ё        Ё        Ё
юддддддддадддддддды
*/