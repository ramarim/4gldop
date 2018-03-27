/*                                                                                                    */
/*                                                                                                    */
/*                                 фонд капитального ремонта (выгрузка в файл)                        */
/*                                                                                                    */
/*                                                                                                    */

DEFINE INPUT PARAMETER iString  AS CHAR NO-UNDO. /*параметры*/
DEFINE INPUT PARAMETER iDateDoc AS DATE NO-UNDO. /*дата документов*/
DEFINE INPUT PARAMETER iDatePl  AS DATE NO-UNDO. /*дата платежей*/
DEF VAR iTranz                  AS CHAR NO-UNDO. /*код транзакции*/
DEF VAR iINN                    AS CHAR NO-UNDO. /*ИНН клиента*/
DEF VAR iNazn                   AS CHAR NO-UNDO. /*назначение*/
DEF VAR str                     AS CHAR NO-UNDO.
DEF VAR date_pl                 AS CHAR NO-UNDO.
DEF VAR acct_pl                 AS CHAR NO-UNDO.
DEF VAR city                    AS CHAR NO-UNDO.
DEF VAR street                  AS CHAR NO-UNDO.
DEF VAR domkorp                 AS CHAR NO-UNDO.
DEF VAR kvart                   AS CHAR NO-UNDO.
DEF VAR sum_pl                  AS CHAR NO-UNDO.
DEF VAR sum_pn                  AS CHAR NO-UNDO.
DEF VAR num_tr                  AS CHAR NO-UNDO.
DEF VAR type_pl                 AS CHAR NO-UNDO.
DEF VAR csdk                    AS CHAR NO-UNDO.
DEF VAR pnAdres                 AS CHAR NO-UNDO.
DEF VAR count                   AS INT  NO-UNDO.
DEF VAR total_pl                AS DEC  NO-UNDO.
DEF VAR total_pn                AS DEC  NO-UNDO.
DEF VAR num_reestr              AS CHAR NO-UNDO.
DEF VAR fname                   AS CHAR NO-UNDO.
DEF VAR year                    AS CHAR NO-UNDO.
DEF VAR month                   AS CHAR NO-UNDO.
DEF VAR day                     AS CHAR NO-UNDO.

DEF BUFFER bOp FOR op.
DEF VAR fpath AS CHAR INIT "/qbis/wrk/imp-exp/0000/kompl/kaprem/" NO-UNDO.

/*
FUNCTION ReestrNum RETURN CHAR (INPUT iStr AS CHAR,
                                INPUT iBef AS CHAR,
                                INPUT iAft AS CHAR):
  
  DEF VAR idx_1  AS INT  NO-UNDO.
  DEF VAR idx_2  AS INT  NO-UNDO.
  DEF VAR result AS CHAR NO-UNDO.

  ASSIGN
  idx_1 = INDEX(iStr,iBef) + LENGTH(iBef)
  idx_2 = INDEX(iStr,iAft,idx_1)
  result = SUBSTRING(iStr,idx_1,idx_2 - idx_1)
  .
  RETURN TRIM(result).
END.
*/

FUNCTION ReestrNum RETURN CHAR (INPUT iOp   AS INT,
                                INPUT iDate AS DATE):
  
  DEF VAR result AS CHAR NO-UNDO.

  FOR FIRST links WHERE links.link-id = 22
                    AND links.source-id = STRING(iOp)
                    AND links.beg-date <= iDate
                    AND (links.end-date >= iDate OR links.end-date = ?) NO-LOCK,
                    FIRST loan WHERE loan.class-code = ENTRY(1, links.target-id)
                                 AND loan.cont-code  = ENTRY(2, links.target-id) NO-LOCK.
    result = TRIM(loan.doc-num).
  END.
  
  RETURN result.
END.

/*----------------------------------------------------------------------------------------------------*/

ASSIGN
iTranz = ENTRY(1,iString,"|")
iINN   = ENTRY(2,iString,"|")
iNazn  = ENTRY(3,iString,"|")
.

{globals.i}
FOR EACH op WHERE op.op-date = iDateDoc
              AND op.op-kind = iTranz
              AND op.inn = iINN 
              AND op.details MATCHES(iNazn) NO-LOCK.
  ASSIGN
  count      = 0
  total_pl   = 0
  total_pn   = 0
  year       = SUBSTRING(STRING(YEAR(iDateDoc),"9999"),3,2)
  month      = STRING(MONTH(iDateDoc),"99")
  day        = STRING(DAY(iDateDoc),"99")
  /*num_reestr = ReestrNum(op.details,"Реестр платежей","от")*/
  num_reestr = ReestrNum(op.op, iDateDoc)
  fname      = fpath + "BP" + year + month + day + "_" + num_reestr + ".txt"
  .
  OUTPUT TO VALUE (fname) CONVERT TARGET "1251".
  FOR EACH links WHERE links.target-id = STRING(op.op)
                   AND links.beg-date = iDatePl NO-LOCK,
                   FIRST bOp WHERE bOp.op = INT64(links.source-id) NO-LOCK,
                   FIRST op-entry WHERE op-entry.op = bOp.op NO-LOCK.
    
    ASSIGN
    date_pl = STRING(bOp.doc-date,"99.99.9999")
    acct_pl = GetXattrValueEx("op",STRING(bOp.op),"ЛицевойСч","")
    city    = GetXattrValueEx("op",STRING(bOp.op),"Город","")
    street  = GetXattrValueEx("op",STRING(bOp.op),"Улица","")
    domkorp = GetXattrValueEx("op",STRING(bOp.op),"ДомКорпус","")
    kvart   = GetXattrValueEx("op",STRING(bOp.op),"Квартира","")
    /*sum_pl  = TRIM(STRING(op-entry.amt-rub,"zzzzzzzz9.99"))*/
    sum_pn  = IF GetXattrValueEx("op",STRING(bOp.op),"ПНСуммаПени","") <> "" THEN
                 REPLACE(GetXattrValueEx("op",STRING(bOp.op),"ПНСуммаПени",""), ",", "") ELSE "0"
    sum_pl  = TRIM(STRING(op-entry.amt-rub - DEC(sum_pn), "zzzzzzzz9.99"))
    num_tr  = STRING(bOp.op)
    /*type_pl = GetXattrValueEx("op",STRING(bOp.op),"ТипПлКапРем","")*/
    pnAdres = GetXattrValueEx("op",STRING(bOp.op),"ПНАдрес","")
    csdk    = city + ";" + street + ";" + domkorp + ";" + kvart
    csdk    = IF csdk = ";;;" THEN "" ELSE csdk
    pnAdres = pnAdres + csdk
    str = date_pl + ";" +
          acct_pl + ";" +
          pnAdres + ";" +
          sum_pl  + ";" +
          num_tr  + ";" +
          /*type_pl + ";"*/
          sum_pn + ";"
    count = count + 1
    total_pl = total_pl + DEC(sum_pl)
    total_pn = total_pn + DEC(sum_pn)
    .
    PUT UNFORMATTED str SKIP.
  END.

  str = "#;" + STRING(count)                         + ";" + 
               TRIM(STRING(total_pl,"zzzzzzzz9.99")) + ";" + 
               TRIM(STRING(total_pn,"zzzzzzzz9.99")) + ";" + 
               num_reestr                            + ";"
               .
  
  PUT UNFORMATTED str SKIP.
  OUTPUT CLOSE.
  MESSAGE "Файл " + fname + " сформирован!!!" VIEW-AS ALERT-BOX.
END.
