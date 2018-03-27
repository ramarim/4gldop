{ globals.i }        
{intrface.get xclass}
PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF VAR str       AS CHAR FORMAT "x(105)" NO-UNDO.
DEF VAR iCount    AS INT INITIAL 0 NO-UNDO.
DEF VAR date1     AS DATE NO-UNDO.
DEF VAR date2     AS DATE NO-UNDO.
DEF VAR dateBeg   AS CHAR FORMAT "x(10)"  NO-UNDO.
DEF VAR dateEnd   AS CHAR FORMAT "x(10)"  NO-UNDO.

DEF VAR fstr      AS CHAR NO-UNDO.
DEF VAR fname     AS CHAR NO-UNDO.
DEF VAR user_ip   AS CHAR FORMAT "X(15)" NO-UNDO.
DEF VAR fpath     AS CHAR INIT "/qbis/wrk/imp-exp/0000/preview/" NO-UNDO.
user_ip=TRIM(SUBSTRING(OS-GETENV("SSH_CLIENT"),1,INDEX(OS-GETENV("SSH_CLIENT")," "))).
fname = fpath + "_tmp@" + user_ip.

/* форма ввода дат */
FORM
SKIP(1)
date1
FORMAT "99.99.9999"
LABEL  '            Дата'
HELP   'Введите дату начала'  
SKIP
date2
FORMAT "99.99.9999"
LABEL  '            Дата'
HELP   'Введите дату конца'  
SKIP
WITH FRAME fr_main CENTERED ROW 10 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[ СООБЩЕНИЯ С ОШИБКАМИ  ]".
UPDATE date1 date2 WITH FRAME fr_main.


ASSIGN
dateBeg = STRING(date1,"99.99.9999")
dateEnd = STRING(date2,"99.99.9999")
.


DEF TEMP-TABLE ttErrorMsg NO-UNDO
FIELD ftype  AS CHAR
FIELD facct  AS CHAR
FIELD ferr   AS CHAR
.


DEF TEMP-TABLE ttErrorTaxX NO-UNDO
FIELD err-code  AS CHAR
FIELD err-name  AS CHAR
.

FOR EACH xattr WHERE xattr.class-code EQ 'ErrorTaxX' NO-LOCK.
    CREATE ttErrorTaxX.
    ASSIGN
    err-code = xattr.xattr-code
    err-name = xattr.name 
    .
END.


FOR EACH Seance WHERE Seance.op-kind EQ 'e-clx502' AND Seance.filial-id EQ
"0000" AND (Seance.SeanceDate >= date1 AND Seance.SeanceDate <= date2) NO-LOCK.

FOR EACH Packet WHERE Packet.ParentID = 0 AND Packet.SeanceID EQ Seance.SeanceID AND
Packet.State EQ 'ОШБК' AND Packet.filial-id EQ "0000" NO-LOCK.
    FIND FIRST PackObject WHERE PackObject.PacketID EQ Packet.PacketID NO-LOCK.
    IF AVAIL(PackObject) THEN DO:
        CREATE ttErrorMsg.
        ASSIGN
        ftype = TRIM(PackObject.file-name)
        facct = SUBSTRING(PackObject.Surrogate,1,20)
        .
        FIND FIRST ttErrorTaxX WHERE Packet.PackError EQ ttErrorTaxX.err-code NO-LOCK.
        IF AVAIL(ttErrorTaxX) THEN 
             ttErrorMsg.ferr =TRIM("(" + ttErrorTaxX.err-code + ") " + ttErrorTaxX.err-name).
        ELSE ttErrorMsg.ferr = "Unknown Error!".
    END.
END.
END.


{setdest.i}
PUT "             Сообщения с ошибками (статус ОШБК) с " dateBeg " по " dateEnd                                  SKIP.
PUT "                                                                                                          " SKIP.
PUT "                                                                                                          " SKIP.
PUT " ┌────────┬──────────────────────┬──────────────────────────────────────────────────────────────────────┐ " SKIP.
PUT " │ № П/П  │ Номер счета          │ Ошибка                                                               │ " SKIP.
PUT " ├────────┼──────────────────────┼──────────────────────────────────────────────────────────────────────┤ " SKIP.
FOR EACH ttErrorMsg NO-LOCK.
    iCount =  iCount + 1.
    str = " │" + STRING(iCount,">>>>>>>9") + "│" + STRING(ttErrorMsg.facct,"x(22)") + "│" + STRING(ttErrorMsg.ferr,"x(70)") + "│".
    PUT str SKIP.
END.
PUT " └────────┴──────────────────────┴──────────────────────────────────────────────────────────────────────┘ " SKIP.
{preview.i}



OUTPUT TO VALUE (fname) CONVERT TARGET "1251".
PUT UNFORMATTED "TYPE|ACCT|ERR" SKIP.
FOR EACH ttErrorMsg NO-LOCK.
    fstr = ttErrorMsg.ftype + "|" +
          ttErrorMsg.facct + "|" +
          ttErrorMsg.ferr
    .
    PUT UNFORMATTED fstr SKIP.
END.
OUTPUT CLOSE.
MESSAGE "Файл " + fname + " сформирован !!!" VIEW-AS ALERT-BOX.

