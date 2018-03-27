/*                                                                                                    */
/*                                                                                                    */
/*                               Эквайринг. Список контактных телефонов                               */
/*                                                                                                    */
/*                                                                                                    */

DEF VAR sOut AS CHAR NO-UNDO.
DEF BUFFER bLoan FOR loan.

{globals.i}

FUNCTION GetCustPhone RETURN CHAR(INPUT iCustID AS INT):
  DEF VAR result AS CHAR NO-UNDO.
  
  FOR FIRST cust-corp WHERE cust-corp.cust-id = iCustID NO-LOCK.
    result = GetXAttrValueEx("cust-corp", 
                             STRING(iCustID),
                             "tel",
                             "").
  END.
          
  IF result = "" THEN DO:
    FOR FIRST person WHERE person.person-id = iCustID NO-LOCK.
      result = TRIM(person.phone[1],",").
      IF result = "" THEN DO:
         result = TRIM(person.phone[2] + "," + GetXattrValue("person", STRING(person.person-id),"cell-phone"),",").
         IF result = "" THEN result = GetXAttrValue("person", STRING(person.person-id), "Телефон3").
      END.
    END.
  END.
  
  RETURN result.
END.

FUNCTION GetCustName RETURN CHAR(INPUT iCustID AS INT):
  DEF VAR result AS CHAR NO-UNDO.

  FOR FIRST cust-corp WHERE cust-corp.cust-id = iCustID NO-LOCK.
    result = cust-corp.cust-stat + " " + cust-corp.name-short.
  END.
  
  IF result = "" THEN DO:
    FOR FIRST person WHERE person.person-id = iCustID NO-LOCK.
      result = person.name-last + " " + person.first-name.
    END.
  END.
        
  RETURN result.
END.

FUNCTION GetPOSList RETURN CHAR(INPUT iLoan AS CHAR):
  DEF VAR result AS CHAR NO-UNDO.
  
  FOR EACH loan WHERE loan.contract = "card-equip"
                  AND loan.class-code = "card-equip"
                  AND loan.parent-cont-code = iLoan NO-LOCK.
    result = result + "," + loan.doc-num.
  END.

  RETURN TRIM(result, ",").
END.

/*----------------------------------------------------------------------------------------------------*/

{setdest.i}

ASSIGN
sOut =          "Номер договора"       +
       CHR(9) + "Наименование клиента" +
       CHR(9) + "Список терминалов"    +
       CHR(9) + "Контактные телефоны"
sOut = CODEPAGE-CONVERT(sOut, "1251", "IBM866")
.
PUT UNFORMATTED sOut SKIP.

FOR EACH bLoan WHERE bLoan.contract = "card-acq"
                 AND bLoan.class-code = "card-loan-acqcust"
                 AND bLoan.loan-status = "ОТКР" NO-LOCK.
  
  ASSIGN
  sOut =          STRING(bLoan.doc-ref)               +
         CHR(9) + STRING(GetCustName(bLoan.cust-id))  +
         CHR(9) + STRING(GetPOSList(bLoan.doc-ref))   +
         CHR(9) + STRING("'" + GetCustPhone(bLoan.cust-id))
  sOut = CODEPAGE-CONVERT(sOut, "1251", "IBM866")
  .
  PUT UNFORMATTED sOut SKIP.
END.

{preview.i}
