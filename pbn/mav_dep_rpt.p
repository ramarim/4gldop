/*                                                                                                    */
/*                                                                                                    */
/*                  └╔╞╝╖╗БК. ▌БГ╔Б ╝ ╞Ю╗╒╚╔Г╔╜╗╗/А╜ОБ╗╗ ╓╔╜╔╕╜КЕ АЮ╔╓АБ╒ ╖═ ╞╔Ю╗╝╓                   */
/*                                                                                                    */
/*                                                                                                    */

PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF NEW SHARED VARIABLE list-id AS CHAR            NO-UNDO.
DEF VAR i                       AS INT  INIT 1     NO-UNDO.
DEF VAR dateRpt1                AS DATE INIT TODAY NO-UNDO.
DEF VAR dateRpt2                AS DATE INIT TODAY NO-UNDO.
DEF VAR dpsType                 AS CHAR INIT "*"   NO-UNDO.
DEF VAR dpsVal                  AS CHAR VIEW-AS COMBO-BOX LIST-ITEMS "*", "810", "840", "978" INIT "*" NO-UNDO.

{globals.i}
{pick-val.i}

FORM
SKIP(1)
dateRpt1
FORMAT "99.99.9999"
LABEL  " ▐╔Ю╗╝╓ А"
HELP   "┌╒╔╓╗Б╔ ╓═БЦ ╜═Г═╚═ ╞╔Ю╗╝╓═."
dateRpt2
FORMAT "99.99.9999"
LABEL  "╞╝"
HELP   "┌╒╔╓╗Б╔ ╓═БЦ ╝╙╝╜Г═╜╗О ╞╔Ю╗╝╓═."
dpsVal
FORMAT "x(3)"
LABEL  " ┌═╚НБ═ ╒╙╚═╓═"
HELP   "┌К║╔Ю╗Б╔ ╒═╚НБЦ ╒╙╚═╓═ (╙╚═╒╗Х═ F1 ╗╚╗ ┌█┬┤)."
SKIP(1)
dpsType
FORMAT "x(47)"
LABEL  " ▓╗╞ ╒╙╚═╓═"
HELP   "┌К║╔Ю╗Б╔ Б╗╞ ╒╙╚═╓═"
SKIP
SPACE(61)

WITH FRAME fr_main CENTERED ROW 8 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[┌┼▀─└⌡. ▐░┬┌▀┘≈┘█┬┘/▒█÷▓┬┘]".

ON F1 OF dpsType IN FRAME fr_main DO:
  DO TRANSACTION:
    RUN browseld.p ("code", "class" + CHR(1) + "parent", "cont-type" + CHR(1) + "cont-type", "", 1).
  END.
  list-id = TRIM(list-id + "," + pick-value, ",").
  dpsType:SCREEN-VALUE = list-id.
  IF LASTKEY = 27 THEN DO:
    ASSIGN
    list-id = ""
    dpsType:SCREEN-VALUE = "*"
    .
  END.
  RETURN NO-APPLY.
END.

ON F1 OF dpsVal IN FRAME fr_main DO:
  ASSIGN
  i = IF i = 4 THEN 1 ELSE i + 1
  dpsVal:SCREEN-VALUE = ENTRY(i, "*,810,840,978")
  .
END.

UPDATE dateRpt1 dateRpt2 dpsVal dpsType WITH FRAME fr_main.

/*----------------------------------------------------------------------------------------------------*/

DEF INPUT PARAM iParam AS CHAR                NO-UNDO.
DEF VAR loan_osn_i     AS INT                 NO-UNDO.
DEF VAR loan_prc_i     AS INT                 NO-UNDO.
DEF VAR dep_i          AS INT                 NO-UNDO.
DEF VAR date1_         AS CHAR FORMAT "X(10)" NO-UNDO.
DEF VAR date2_         AS CHAR FORMAT "X(10)" NO-UNDO.
DEF VAR dpsName        AS CHAR FORMAT "X(40)" NO-UNDO.
DEF VAR dc             AS CHAR FORMAT "X(6)"  NO-UNDO.
DEF VAR acct_osn_lst   AS CHAR                NO-UNDO.
DEF VAR acct_prc_lst   AS CHAR                NO-UNDO.
DEF VAR ost_in_osn     AS DEC                 NO-UNDO.
DEF VAR ost_out_osn    AS DEC                 NO-UNDO.
DEF VAR oborot_dt_osn  AS DEC                 NO-UNDO.
DEF VAR oborot_kt_osn  AS DEC                 NO-UNDO.
DEF VAR ost_in_prc     AS DEC                 NO-UNDO.
DEF VAR ost_out_prc    AS DEC                 NO-UNDO.
DEF VAR oborot_dt_prc  AS DEC                 NO-UNDO.
DEF VAR oborot_kt_prc  AS DEC                 NO-UNDO.
DEF VAR dt             AS DEC                 NO-UNDO.
DEF VAR t_ost_in       AS DEC                 NO-UNDO.
DEF VAR t_oborot_dt    AS DEC                 NO-UNDO.
DEF VAR t_oborot_kt    AS DEC                 NO-UNDO.
DEF VAR t_ost_out      AS DEC                 NO-UNDO.
DEF VAR ost_in_o_      AS CHAR FORMAT "X(16)" NO-UNDO.
DEF VAR ost_out_o_     AS CHAR FORMAT "X(16)" NO-UNDO.
DEF VAR oborot_dt_o_   AS CHAR FORMAT "X(16)" NO-UNDO.
DEF VAR oborot_kt_o_   AS CHAR FORMAT "X(16)" NO-UNDO.
DEF VAR ost_in_p_      AS CHAR FORMAT "X(16)" NO-UNDO.
DEF VAR ost_out_p_     AS CHAR FORMAT "X(16)" NO-UNDO.
DEF VAR oborot_dt_p_   AS CHAR FORMAT "X(16)" NO-UNDO.
DEF VAR oborot_kt_p_   AS CHAR FORMAT "X(16)" NO-UNDO.
DEF VAR t_             AS CHAR FORMAT "X(6)"  NO-UNDO.
DEF VAR t_ost_in_      AS CHAR FORMAT "X(16)" NO-UNDO.
DEF VAR t_oborot_dt_   AS CHAR FORMAT "X(16)" NO-UNDO.
DEF VAR t_oborot_kt_   AS CHAR FORMAT "X(16)" NO-UNDO.
DEF VAR t_ost_out_     AS CHAR FORMAT "X(16)" NO-UNDO.
DEF VAR str            AS CHAR                NO-UNDO.

DEF TEMP-TABLE ttOsnFull NO-UNDO
FIELD acct       AS CHAR
FIELD class-code AS CHAR
FIELD cont-type  AS CHAR
FIELD cont-code  AS CHAR
FIELD currency   AS CHAR
.

DEF TEMP-TABLE ttPrcFull NO-UNDO
FIELD acct       AS CHAR
FIELD class-code AS CHAR
FIELD cont-type  AS CHAR
FIELD cont-code  AS CHAR
FIELD currency   AS CHAR
.

DEF TEMP-TABLE ttOsn NO-UNDO
FIELD count      AS INT
FIELD currency   AS CHAR
FIELD dpsName    AS CHAR
FIELD ost_in     AS DEC
FIELD ost_out    AS DEC
FIELD oborot_dt  AS DEC
FIELD oborot_kt  AS DEC
.

DEF TEMP-TABLE ttPrc NO-UNDO
FIELD count      AS INT
FIELD currency   AS CHAR
FIELD dpsName    AS CHAR
FIELD ost_in     AS DEC
FIELD ost_out    AS DEC
FIELD oborot_dt  AS DEC
FIELD oborot_kt  AS DEC
.

ASSIGN
acct_osn_lst = "loan-dps-t,loan-dps-ts,loan-dps-p"
acct_prc_lst = "loan-dps-int"
date1_  = STRING(dateRpt1, "99.99.9999")
date2_  = STRING(dateRpt2, "99.99.9999")
dpsType = TRIM(dpsType)
dpsVal  = REPLACE(dpsVal, "810", "")
i       = 0
.

{sh-defs.i}

FUNCTION GetAcctSum RETURN CHAR (INPUT iAcct     AS CHAR,
                                 INPUT iCurrency AS CHAR,
                                 INPUT iDate1 AS DATE,
                                 INPUT iDate2 AS DATE):
  
  DEF VAR result    AS CHAR NO-UNDO.
  DEF VAR ostIn     AS DEC  NO-UNDO.
  DEF VAR ostOut    AS DEC  NO-UNDO.
  DEF VAR oborot_Dt AS DEC  NO-UNDO.
  DEF VAR oborot_Kt AS DEC  NO-UNDO.
  
  RUN acct-pos IN h_base (iAcct, iCurrency, iDate1 - 1, iDate1 - 1, ?).
  IF sh-bal < 0 THEN ostIn = ABS(sh-bal).
  IF sh-bal > 0 THEN ostIn = sh-bal.
  
  RUN acct-pos IN h_base (iAcct, iCurrency, iDate2, iDate2, ?).
  IF sh-bal < 0 THEN ostOut = ABS(sh-bal).
  IF sh-bal > 0 THEN ostOut = sh-bal.
  
  RUN acct-pos IN h_base (iAcct, iCurrency, iDate1, iDate2, ?).
  ASSIGN
  oborot_Dt = sh-db
  oborot_Kt = sh-cr
  result = STRING(ostIn)     + ";" +
           STRING(ostOut)    + ";" +
           STRING(oborot_Dt) + ";" +
           STRING(oborot_Kt)
  .
  
  RETURN result.
END.

FOR EACH loan WHERE loan.close-date >= dateRpt1
                AND loan.open-date  <= dateRpt2
                AND loan.contract = 'dps'
                AND CAN-DO(dpsVal, loan.currency)
                AND CAN-DO(dpsType, loan.cont-type) NO-LOCK,
                EACH loan-acct OF loan WHERE CAN-DO(acct_osn_lst + "," + acct_prc_lst, loan-acct.acct-type)
                NO-LOCK BREAK BY loan.class-code.
  
  IF CAN-DO(acct_osn_lst, loan-acct.acct-type) THEN DO:
    CREATE ttOsnFull.
    ASSIGN
    ttOsnFull.acct       = loan-acct.acct
    ttOsnFull.class-code = loan.class-code
    ttOsnFull.cont-type  = loan.cont-type
    ttOsnFull.cont-code  = loan.cont-code
    ttOsnFull.currency   = loan-acct.currency
    .
  END.
  
  IF CAN-DO(acct_prc_lst, loan-acct.acct-type) THEN DO:
    CREATE ttPrcFull.
    ASSIGN
    ttPrcFull.acct       = loan-acct.acct
    ttPrcFull.class-code = loan.class-code
    ttPrcFull.cont-type  = loan.cont-type
    ttPrcFull.cont-code  = loan.cont-code
    ttPrcFull.currency   = loan-acct.currency
    .
  END.
  
END.

FOR EACH ttOsnFull NO-LOCK BREAK BY ttOsnFull.class-code.
  IF NOT LAST-OF(ttOsnFull.class-code) THEN DO:
    ASSIGN
    dep_i         = dep_i + 1
    ost_in_osn    = ost_in_osn    + DEC(ENTRY(1, GetAcctSum(ttOsnFull.acct, ttOsnFull.currency, dateRpt1, dateRpt2), ";"))
    ost_out_osn   = ost_out_osn   + DEC(ENTRY(2, GetAcctSum(ttOsnFull.acct, ttOsnFull.currency, dateRpt1, dateRpt2), ";"))
    oborot_dt_osn = oborot_dt_osn + DEC(ENTRY(3, GetAcctSum(ttOsnFull.acct, ttOsnFull.currency, dateRpt1, dateRpt2), ";"))
    oborot_kt_osn = oborot_kt_osn + DEC(ENTRY(4, GetAcctSum(ttOsnFull.acct, ttOsnFull.currency, dateRpt1, dateRpt2), ";"))
    .
  END.
  
  IF LAST-OF(ttOsnFull.class-code) THEN DO:
    FOR FIRST code WHERE code.class = 'cont-type'
                     AND code.parent = 'cont-type'
                     AND code.code = ttOsnFull.cont-type NO-LOCK.
                     
      dpsName = code.name.
    END.
    
    CREATE ttOsn.
    ASSIGN
    dep_i           = dep_i  + 1
    loan_osn_i      = loan_osn_i + 1
    ttOsn.count     = dep_i
    ttOsn.currency  = ttOsnFull.currency
    ttOsn.dpsName   = dpsName + "(" + ttOsnFull.class-code + ")"
    ttOsn.ost_in    = ost_in_osn    + DEC(ENTRY(1, GetAcctSum(ttOsnFull.acct, ttOsnFull.currency, dateRpt1, dateRpt2), ";"))
    ttOsn.ost_out   = ost_out_osn   + DEC(ENTRY(2, GetAcctSum(ttOsnFull.acct, ttOsnFull.currency, dateRpt1, dateRpt2), ";"))
    ttOsn.oborot_dt = oborot_dt_osn + DEC(ENTRY(3, GetAcctSum(ttOsnFull.acct, ttOsnFull.currency, dateRpt1, dateRpt2), ";"))
    ttOsn.oborot_kt = oborot_kt_osn + DEC(ENTRY(4, GetAcctSum(ttOsnFull.acct, ttOsnFull.currency, dateRpt1, dateRpt2), ";"))
    ost_in_osn      = 0
    ost_out_osn     = 0
    oborot_dt_osn   = 0
    oborot_kt_osn   = 0
    dep_i           = 0
    .
  END.
END.

FOR EACH ttPrcFull NO-LOCK BREAK BY ttPrcFull.acct.
  IF NOT FIRST-OF(ttPrcFull.acct) THEN DO:
    DELETE ttPrcFull.
  END.
END.

FOR EACH ttPrcFull NO-LOCK BREAK BY ttPrcFull.class-code.
  IF NOT LAST-OF(ttPrcFull.class-code) THEN DO:
    ASSIGN
    dep_i         = dep_i + 1
    ost_in_prc    = ost_in_prc    + DEC(ENTRY(1, GetAcctSum(ttPrcFull.acct, ttPrcFull.currency, dateRpt1, dateRpt2), ";"))
    ost_out_prc   = ost_out_prc   + DEC(ENTRY(2, GetAcctSum(ttPrcFull.acct, ttPrcFull.currency, dateRpt1, dateRpt2), ";"))
    oborot_dt_prc = oborot_dt_prc + DEC(ENTRY(3, GetAcctSum(ttPrcFull.acct, ttPrcFull.currency, dateRpt1, dateRpt2), ";"))
    oborot_kt_prc = oborot_kt_prc + DEC(ENTRY(4, GetAcctSum(ttPrcFull.acct, ttPrcFull.currency, dateRpt1, dateRpt2), ";"))
    .
  END.
  
  IF LAST-OF(ttPrcFull.class-code) THEN DO:
    FOR FIRST code WHERE code.class = 'cont-type'
                     AND code.parent = 'cont-type'
                     AND code.code = ttPrcFull.cont-type NO-LOCK.
                     
      dpsName = code.name.
    END.
    
    CREATE ttPrc.
    ASSIGN
    dep_i           = dep_i  + 1
    loan_prc_i      = loan_prc_i + 1
    ttPrc.count     = dep_i
    ttPrc.currency  = ttPrcFull.currency
    ttPrc.dpsName   = dpsName + "(" + ttPrcFull.class-code + ")"
    ttPrc.ost_in    = ost_in_prc    + DEC(ENTRY(1, GetAcctSum(ttPrcFull.acct, ttPrcFull.currency, dateRpt1, dateRpt2), ";"))
    ttPrc.ost_out   = ost_out_prc   + DEC(ENTRY(2, GetAcctSum(ttPrcFull.acct, ttPrcFull.currency, dateRpt1, dateRpt2), ";"))
    ttPrc.oborot_dt = oborot_dt_prc + DEC(ENTRY(3, GetAcctSum(ttPrcFull.acct, ttPrcFull.currency, dateRpt1, dateRpt2), ";"))
    ttPrc.oborot_kt = oborot_kt_prc + DEC(ENTRY(4, GetAcctSum(ttPrcFull.acct, ttPrcFull.currency, dateRpt1, dateRpt2), ";"))
    ost_in_prc      = 0
    ost_out_prc     = 0
    oborot_dt_prc   = 0
    oborot_kt_prc   = 0
    dep_i           = 0
    .
  END.
END.

/*----------------------------------------------------------------------------------------------------*/

{setdest.i}

IF iParam = "debug" THEN DO:
  FOR EACH ttOsnFull NO-LOCK.
    str = ttOsnFull.acct + " ; " + ttOsnFull.class-code + " ; " + ttOsnFull.cont-type + " ; " + ttOsnFull.cont-code.
    PUT UNFORMATTED str SKIP.
  END.
  
  PUT "" SKIP.
  PUT "----------------------------------------------------------------------------------------------------" SKIP.
  PUT "" SKIP.
  
  FOR EACH ttPrcFull NO-LOCK.
    str = ttPrcFull.acct + " ; " + ttPrcFull.class-code + " ; " + ttPrcFull.cont-type + " ; " + ttPrcFull.cont-code.
    PUT UNFORMATTED str SKIP.
  END.
END.

PUT "                                                                                                                                   " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT "                               ▐░┬┌▀┘≈┘█┬┘/▒█÷▓┬┘ └┘█┘├█⌡∙ ▒░┘└▒▓┌ ▒ " date1_ " ▐▌ " date2_ "                                      " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT " зддддддддддддддддддддддддддддддддддддддддддбддддддддбддддддддддддддддддбддддддддддддддддддбддддддддддддддддддбдддддддддддддддддд© " SKIP.
PUT " Ё ▓╗╞ ╒╙╚═╓═                               Ё ┼╝╚-╒╝ Ё ┌Е╝╓ОИ╗╘ ╝АБ═Б╝╙ Ё ▌║╝Ю╝Б ╞╝ ╓╔║╔БЦ Ё ▌║╝Ю╝Б ╞╝ ╙Ю╔╓╗БЦЁ ┬АЕ. ╝АБ═Б╝╙     Ё " SKIP.
PUT " цддддддддддддддддддддддддддддддддддддддддддеддддддддеддддддддддддддддддеддддддддддддддддддеддддддддддддддддддедддддддддддддддддд╢ " SKIP.

i = 0.

FOR EACH ttOsn NO-LOCK BREAK BY ttOsn.currency.
  ASSIGN
  i            = i + 1
  dpsName      = ttOsn.dpsName
  dc           = STRING(ttOsn.count,               "zzzzz9")
  ost_in_o_    = STRING(ttOsn.ost_in,    "z,zzz,zzz,zz9.99")
  ost_out_o_   = STRING(ttOsn.ost_out,   "z,zzz,zzz,zz9.99")
  oborot_dt_o_ = STRING(ttOsn.oborot_dt, "z,zzz,zzz,zz9.99")
  oborot_kt_o_ = STRING(ttOsn.oborot_kt, "z,zzz,zzz,zz9.99")
  .
  IF NOT LAST-OF(ttOsn.currency) THEN DO:
    ASSIGN
    dt          = dt          + ttOsn.count
    t_ost_in    = t_ost_in    + ttOsn.ost_in
    t_oborot_dt = t_oborot_dt + ttOsn.oborot_dt
    t_oborot_kt = t_oborot_kt + ttOsn.oborot_kt
    t_ost_out   = t_ost_out   + ttOsn.ost_out 
    .
    PUT " Ё " dpsName                              " Ё " dc " Ё " ost_in_o_    " Ё " oborot_dt_o_ " Ё " oborot_kt_o_ " Ё " ost_out_o_   " Ё " SKIP.
  END.
  IF LAST-OF(ttOsn.currency) THEN DO:
    ASSIGN
    dt           = dt          + ttOsn.count
    t_ost_in     = t_ost_in    + ttOsn.ost_in
    t_oborot_dt  = t_oborot_dt + ttOsn.oborot_dt
    t_oborot_kt  = t_oborot_kt + ttOsn.oborot_kt
    t_ost_out    = t_ost_out   + ttOsn.ost_out 
    t_           =  STRING(dt,                    "zzzzz9")
    t_ost_in_    =  STRING(t_ost_in,    "z,zzz,zzz,zz9.99")
    t_oborot_dt_ =  STRING(t_oborot_dt, "z,zzz,zzz,zz9.99")
    t_oborot_kt_ =  STRING(t_oborot_kt, "z,zzz,zzz,zz9.99")
    t_ost_out_   =  STRING(t_ost_out,   "z,zzz,zzz,zz9.99")
    .
    PUT " Ё " dpsName                              " Ё " dc " Ё " ost_in_o_    " Ё " oborot_dt_o_ " Ё " oborot_kt_o_ " Ё " ost_out_o_   " Ё " SKIP.
    PUT " цддддддддддддддддддддддддддддддддддддддддддеддддддддеддддддддддддддддддеддддддддддддддддддеддддддддддддддддддедддддддддддддддддд╢ " SKIP.
    PUT " Ё ┬▓▌┐▌ :                                  Ё " t_ " Ё " t_ost_in_    " Ё " t_oborot_dt_ " Ё " t_oborot_kt_ " Ё " t_ost_out_   " Ё " SKIP.

    IF i <> loan_osn_i THEN DO:
      PUT " цддддддддддддддддддддддддддддддддддддддддддеддддддддеддддддддддддддддддеддддддддддддддддддеддддддддддддддддддедддддддддддддддддд╢ " SKIP.
    END.
    
    ASSIGN
    dt          = 0
    t_ost_in    = 0
    t_oborot_dt = 0
    t_oborot_kt = 0
    t_ost_out   = 0
    .
  END.
END.

PUT " юддддддддддддддддддддддддддддддддддддддддддаддддддддаддддддддддддддддддаддддддддддддддддддаддддддддддддддддддадддддддддддддддддды " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT "                                      █─≈┬▒▀┘██⌡┘ ▐░▌√┘█▓⌡ ▒ " date1_ " ▐▌ " date2_ "                                              " SKIP.
PUT "                                                                                                                                   " SKIP.
PUT " зддддддддддддддддддддддддддддддддддддддддддбддддддддбддддддддддддддддддбддддддддддддддддддбддддддддддддддддддбдддддддддддддддддд© " SKIP.
PUT " Ё ▓╗╞ ╒╙╚═╓═                               Ё ┼╝╚-╒╝ Ё ┌Е╝╓ОИ╗╘ ╝АБ═Б╝╙ Ё ▌║╝Ю╝Б ╞╝ ╓╔║╔БЦ Ё ▌║╝Ю╝Б ╞╝ ╙Ю╔╓╗БЦЁ ┬АЕ. ╝АБ═Б╝╙     Ё " SKIP.
PUT " цддддддддддддддддддддддддддддддддддддддддддеддддддддеддддддддддддддддддеддддддддддддддддддеддддддддддддддддддедддддддддддддддддд╢ " SKIP.

i = 0.

FOR EACH ttPrc NO-LOCK BREAK BY ttPrc.currency.
  ASSIGN
  i            = i + 1
  dpsName      = ttPrc.dpsName
  dc           = STRING(ttPrc.count,               "zzzzz9")
  ost_in_p_    = STRING(ttPrc.ost_in,    "z,zzz,zzz,zz9.99")
  ost_out_p_   = STRING(ttPrc.ost_out,   "z,zzz,zzz,zz9.99")
  oborot_dt_p_ = STRING(ttPrc.oborot_dt, "z,zzz,zzz,zz9.99")
  oborot_kt_p_ = STRING(ttPrc.oborot_kt, "z,zzz,zzz,zz9.99")
  .
  IF NOT LAST-OF(ttPrc.currency) THEN DO:
    ASSIGN
    dt          = dt          + ttPrc.count
    t_ost_in    = t_ost_in    + ttPrc.ost_in
    t_oborot_dt = t_oborot_dt + ttPrc.oborot_dt
    t_oborot_kt = t_oborot_kt + ttPrc.oborot_kt
    t_ost_out   = t_ost_out   + ttPrc.ost_out
    .
    PUT " Ё " dpsName                              " Ё " dc " Ё " ost_in_p_    " Ё " oborot_dt_p_ " Ё " oborot_kt_p_ " Ё " ost_out_p_   " Ё " SKIP.
  END.
  IF LAST-OF(ttPrc.currency) THEN DO:
    ASSIGN
    dt           = dt          + ttPrc.count
    t_ost_in     = t_ost_in    + ttPrc.ost_in
    t_oborot_dt  = t_oborot_dt + ttPrc.oborot_dt
    t_oborot_kt  = t_oborot_kt + ttPrc.oborot_kt
    t_ost_out    = t_ost_out   + ttPrc.ost_out
    t_           =  STRING(dt,                    "zzzzz9")
    t_ost_in_    =  STRING(t_ost_in,    "z,zzz,zzz,zz9.99")
    t_oborot_dt_ =  STRING(t_oborot_dt, "z,zzz,zzz,zz9.99")
    t_oborot_kt_ =  STRING(t_oborot_kt, "z,zzz,zzz,zz9.99")
    t_ost_out_   =  STRING(t_ost_out,   "z,zzz,zzz,zz9.99")
    .
    PUT " Ё " dpsName                              " Ё " dc " Ё " ost_in_p_    " Ё " oborot_dt_p_ " Ё " oborot_kt_p_ " Ё " ost_out_p_   " Ё " SKIP.
    PUT " цддддддддддддддддддддддддддддддддддддддддддеддддддддеддддддддддддддддддеддддддддддддддддддеддддддддддддддддддедддддддддддддддддд╢ " SKIP.
    PUT " Ё ┬▓▌┐▌ :                                  Ё " t_ " Ё " t_ost_in_    " Ё " t_oborot_dt_ " Ё " t_oborot_kt_ " Ё " t_ost_out_   " Ё " SKIP.
    
    IF i <> loan_prc_i THEN DO:
      PUT " цддддддддддддддддддддддддддддддддддддддддддеддддддддеддддддддддддддддддеддддддддддддддддддеддддддддддддддддддедддддддддддддддддд╢ " SKIP.
    END.
    
    ASSIGN
    dt          = 0
    t_ost_in    = 0
    t_oborot_dt = 0
    t_oborot_kt = 0
    t_ost_out   = 0
    .
  END.
END.

PUT " юддддддддддддддддддддддддддддддддддддддддддаддддддддаддддддддддддддддддаддддддддддддддддддаддддддддддддддддддадддддддддддддддддды " SKIP.
PUT "                                                                                                                                   " SKIP.
{preview.i}


/*
зддддддддбдддддддд©
цддддддддедддддддд╢
Ё        Ё        Ё
юддддддддадддддддды
*/
