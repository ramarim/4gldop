/*                                                                                                    */
/*                                                                                                    */
/*                               ê••·‚‡ ·Á•‚Æ¢ ÑÇ ß†™‡Î‚ÎÂ ¢ ™Æ≠Ê• £Æ§†                               */
/*                                                                                                    */
/*                                                                                                    */

DEF INPUT PARAM iParam  AS CHAR                NO-UNDO. /*™1208,™1209,™1210,™1211,™1212,™1213*/
DEF VAR i               AS INT                 NO-UNDO.
DEF VAR dog_i           AS INT                 NO-UNDO.
DEF VAR n               AS CHAR FORMAT "x(5)"  NO-UNDO.
DEF VAR ost_t           AS DEC                 NO-UNDO.
DEF VAR ost_t_          AS CHAR FORMAT "x(12)" NO-UNDO.
DEF VAR date_cl_        AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR date_cl         AS DATE                NO-UNDO.
DEF VAR pers_adres_full AS CHAR                NO-UNDO.
DEF VAR pers_rekv_full  AS CHAR                NO-UNDO.
DEF VAR pers_adres      AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR pers_rekv       AS CHAR FORMAT "x(30)" NO-UNDO.


DEF TEMP-TABLE ttDV NO-UNDO
FIELD dog_n      AS CHAR FORMAT "x(15)"
FIELD acct       AS CHAR FORMAT "x(20)"
FIELD ost        AS CHAR FORMAT "x(12)"
FIELD pers_fio   AS CHAR FORMAT "x(40)"
FIELD pers_adres AS CHAR FORMAT "x(30)"
FIELD pers_rekv  AS CHAR FORMAT "x(30)"
.


FUNCTION GetKom RETURN DEC (INPUT iAcct AS CHAR, INPUT iTranz AS CHAR, INPUT iDate AS DATE):
  DEF VAR result AS DEC NO-UNDO.
  FOR EACH op-entry WHERE op-entry.op-date = iDate
                      AND op-entry.acct-db = iAcct NO-LOCK,
                      FIRST op OF op-entry WHERE CAN-DO(iTranz, op.op-kind) NO-LOCK.
    result = op-entry.amt-rub.
  END.
  RETURN result.
END.

FUNCTION StrWrap RETURN CHAR (INPUT iStr AS CHAR, INPUT iLen AS INT, INPUT iChr AS CHAR):
  DEF VAR i      AS INT  NO-UNDO.
  DEF VAR j      AS INT  NO-UNDO.
  DEF VAR n      AS INT  NO-UNDO. 
  DEF VAR result AS CHAR NO-UNDO.
  ASSIGN
  j = 1
  iStr = REPLACE(iStr,iChr,"")
  iStr = REPLACE(iStr,CHR(10),"") 
  n = LENGTH(iStr) / iLen
  .
  IF n * iLen < LENGTH(iStr) THEN n = n + 1. 
  DO i = 1 TO n:
    ASSIGN
    result = result + SUBSTRING(iStr,j,iLen) + iChr
    j = j + iLen
    .
  END.
  RETURN TRIM(result,iChr).
END.

/*----------------------------------------------------------------------------------------------------*/

{globals.i}
{tmprecid.def}
FOR EACH tmprecid NO-LOCK, FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK, 
                           FIRST person WHERE person.person-id = loan.cust-id NO-LOCK,
                           FIRST loan-acct WHERE loan-acct.cont-code = loan.cont-code
                                             AND loan-acct.contract = "dps"
                                             AND CAN-DO("loan-dps-p", loan-acct.acct-type) NO-LOCK.
  IF i = 0 THEN DO:
    ASSIGN
    i        = i + 1
    date_cl  = loan.close-date
    date_cl_ = STRING(date_cl, "99.99.9999")
    .
  END.
  
  CREATE ttDV.
  ASSIGN
  ttDV.dog_n      = SUBSTR(loan.doc-ref, 1, 15)
  ttDV.acct       = SUBSTR(loan-acct.acct, 1, 20)
  ttDV.ost        = STRING(GetKom(loan-acct.acct, iParam, date_cl), "z,zzz,zz9.99")
  ost_t           = ost_t + GetKom(loan-acct.acct, iParam, date_cl)
  ttDV.pers_fio   = person.name-last + " " + person.first-names
  ttDV.pers_adres = person.address[1]
  ttDV.pers_rekv  = person.document + " ; " + 
                    person.issue    + " ; " + 
                    GetXAttrValueEx("person", STRING(loan.cust-id), "Document4Date_vid", "")
  .
END.

{setdest.i}
PUT "                                                                                                                                                                                " SKIP.
PUT "      êÖÖëíê ëóÖíéÇ 'ÑÆ ¢Æ·‚‡•°Æ¢†≠®Ô' áÄäêõíõï " date_cl_ "£.                                                                                                                  " SKIP.
PUT "                                                                                                                                                                                " SKIP.
PUT " ⁄ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø " SKIP.
PUT " ≥   ¸   ≥ çÆ¨•‡ §Æ£Æ¢Æ‡†  ≥     ã®Ê•¢Æ© ·Á•‚     ≥   é·‚†‚Æ™    ≥              î.à.é. ™´®•≠‚†              ≥         Ä§‡•· ™´®•≠‚†          ≥  ê•™¢®ß®‚Î §Æ™.„§.´. ™´®•≠‚†   ≥ " SKIP.
PUT " √ƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥ " SKIP.

FOR EACH ttDV NO-LOCK.
  ASSIGN
  dog_i           = dog_i + 1
  n               = STRING(dog_i, "zzzz9")
  pers_adres_full = StrWrap(ttDV.pers_adres, 30, "|")
  pers_rekv_full  = StrWrap(ttDV.pers_rekv, 30, "|")
  pers_adres      = ENTRY(1, pers_adres_full, "|")
  pers_rekv       = ENTRY(1, pers_rekv_full, "|")
  .
  
  PUT " ≥ " n " ≥ " ttDV.dog_n  " ≥ " ttDV.acct        " ≥ " ttDV.ost " ≥ " ttDV.pers_fio                        " ≥ " pers_adres                 " ≥ " pers_rekv                  " ≥ " SKIP.
  DO i = 2 TO NUM-ENTRIES(pers_rekv_full, "|"):
    ASSIGN
    pers_adres = IF i <= NUM-ENTRIES(pers_adres_full, "|") THEN  ENTRY(i, pers_adres_full, "|") ELSE ""
    pers_rekv = ENTRY(i, pers_rekv_full, "|")
    .
    PUT " ≥       ≥                 ≥                      ≥              ≥                                          ≥ " pers_adres                 " ≥ " pers_rekv                  " ≥ " SKIP.
  END.
END.

ost_t_ = STRING(ost_t, "z,zzz,zz9.99").

PUT " √ƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥ " SKIP.
PUT " ≥ Ç·•£Æ §Æ£Æ¢Æ‡Æ¢: " n "                         ≥ " ost_t_   " ≥                                                                                                            ≥ " SKIP.
PUT " ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ " SKIP.
PUT "                                                                                                                                                                                " SKIP.
{preview.i}


/*
⁄ƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒø
√ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒ¥
≥        ≥        ≥
¿ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒŸ
*/
