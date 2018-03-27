/*                                                                                                    */
/*                                                                                                    */
/*                           Заявление на открытие пластиковой карты (ОВО)                            */
/*                                                                                                    */
/*                                                                                                    */

DEF VAR fName           AS CHAR NO-UNDO.
DEF VAR className       AS CHAR NO-UNDO.
DEF VAR sOut            AS CHAR NO-UNDO.
DEF VAR clientName      AS CHAR NO-UNDO.
DEF VAR clientShname    AS CHAR NO-UNDO.
DEF VAR sotrdol         AS CHAR NO-UNDO.
DEF VAR sotrFIO         AS CHAR NO-UNDO.
DEF VAR personID        AS CHAR NO-UNDO.
DEF VAR clientDocser    AS CHAR NO-UNDO.
DEF VAR clientDocnum    AS CHAR NO-UNDO.
DEF VAR clientDocwho1   AS CHAR NO-UNDO.
DEF VAR clientDocwho2   AS CHAR NO-UNDO.
DEF VAR clientDoctype   AS CHAR NO-UNDO.
DEF VAR clientDocdate   AS CHAR NO-UNDO.
DEF VAR clientAddrcntr  AS CHAR NO-UNDO.
DEF VAR clientPhone     AS CHAR NO-UNDO.
DEF VAR clientMob       AS CHAR NO-UNDO.
DEF VAR clientPhonewrk  AS CHAR NO-UNDO.
DEF VAR clientMail      AS CHAR NO-UNDO.
DEF VAR clientCitizen   AS CHAR NO-UNDO.
DEF VAR clientBirthday  AS CHAR NO-UNDO.
DEF VAR clientBirthsite AS CHAR NO-UNDO.
DEF VAR clientINN       AS CHAR NO-UNDO.
DEF VAR derjpol         AS CHAR NO-UNDO.

{globals.i}
{tmprecid.def}

FUNCTION GetSNDoc RETURN CHAR (INPUT iType  AS CHAR,
                               INPUT iSNDoc AS CHAR):
  DEF VAR nDocLst AS INT  NO-UNDO.
  DEF VAR docSer  AS CHAR NO-UNDO.
  DEF VAR docNum  AS CHAR NO-UNDO.
  DEF VAR result  AS CHAR NO-UNDO.
  
  ASSIGN
  nDocLst = NUM-ENTRIES(iSNDoc, " ")
  docNum  = TRIM(ENTRY(nDocLst, iSNDoc, " "))
  docSer  = TRIM(RIGHT-TRIM(iSNDoc, docNum))
  .
  
  IF iType = "серия" THEN result = docSer.
  IF iType = "номер" THEN result = docNum.
  IF iType = ""      THEN result = iSNDoc.
  
  RETURN result.
END.

FUNCTION GetDateIssueDoc RETURN DATE (INPUT iCustID  AS INT,
                                      INPUT iTypeDoc AS CHAR,
                                      INPUT iSNDoc   AS CHAR):
  DEF VAR result AS DATE NO-UNDO.
  
  FOR FIRST cust-ident WHERE cust-ident.class-code = "p-cust-ident"
                         AND cust-ident.cust-cat = "Ч"
                         AND cust-ident.cust-id = iCustID
                         AND cust-ident.cust-code-type = iTypeDoc
                         AND cust-ident.cust-code = iSNDoc NO-LOCK.
    
    result = cust-ident.open-date.
  END.
  
  RETURN result.
END.

FUNCTION GetUserFIO RETURN CHAR (INPUT iUserID  AS CHAR):
  DEF VAR result AS CHAR NO-UNDO.
  
  FOR FIRST _user WHERE _user._userid = iUserID NO-LOCK.
    result = _user._user-name.
    
    IF NUM-ENTRIES(_user._user-name, " ") = 3 THEN DO:
      result = ENTRY(1, _user._user-name, " ") + " " + 
               SUBSTR(ENTRY(2, _user._user-name, " "), 1, 1) + "." + " " +
               SUBSTR(ENTRY(3, _user._user-name, " "), 1, 1) + "."
               .
    END.
  END.
  
  RETURN result.
END.

FUNCTION GetCountryName RETURN CHAR (INPUT iCountryID  AS CHAR):
  DEF VAR result AS CHAR NO-UNDO.
  
  FOR FIRST country WHERE country.country-id = iCountryID NO-LOCK.
    result = country.country-name.
  END.
  
  RETURN result.
END.

/*----------------------------------------------------------------------------------------------------*/

ASSIGN
fName     = "mav-ankfl-w.bvn"
className = "docvar"
.

FOR EACH tmprecid NO-LOCK,
    FIRST person WHERE RECID(person) = tmprecid.id NO-LOCK.
  
  ASSIGN
  clientName      = person.name-last + " " + person.first-names
  clientShname    = person.name-last + " " +
                    SUBSTR(ENTRY(1, person.first-names, " "), 1, 1) + "." +
                    SUBSTR(ENTRY(2, person.first-names, " "), 1, 1) + "."
  sotrdol         = GetXAttrValue("_user", STRING(USERID("bisquit")), "Должность")
  sotrFIO         = GetUserFIO(USERID("bisquit"))
  personID        = STRING(person.person-id)
  clientDocnum    = GetSNDoc("номер", person.document)
  clientDocser    = GetSNDoc("серия", person.document)
  clientDocwho1   = ENTRY(1, person.issue)
  clientDocwho2   = ENTRY(2, person.issue)
  clientDoctype   = person.document-id
  clientDocdate   = STRING(GetDateIssueDoc(person.person-id, clientDoctype, person.document), "99/99/9999")
  clientAddrcntr  = person.address[1]
  clientPhone     = IF TRIM(person.phone[1], ",") <> "" THEN TRIM(person.phone[1], ",") ELSE TRIM(person.phone[2], ",")
  clientMob       = GetXAttrValueEx("person", STRING(person.person-id), "cell-phone", "")
  clientPhonewrk  = GetXAttrValue("person", STRING(person.person-id), "Телефон3")
  clientMail      = GetXAttrValue("person", STRING(person.person-id), "e-mail")
  clientCitizen   = GetCountryName(GetXAttrValue("person", STRING(person.person-id), "country-id2"))
  clientBirthday  = STRING(person.birthday, "99/99/9999")
  clientBirthsite = GetXAttrValue("person", STRING(person.person-id), "BirthPlace")
  clientINN       = person.inn
  derjpol         = STRING(person.gender, "муж/жен")
  .
END.

OUTPUT TO VALUE (fName) CONVERT TARGET "1251".

sOut = "client-name:"      + clientName      + CHR(10) +
       "client-shname:"    + clientShname    + CHR(10) +
       "sotrdol:"          + sotrdol         + CHR(10) +
       "sotrFIO:"          + sotrFIO         + CHR(10) +
       "person-id:"        + personID        + CHR(10) +
       "client-docser:"    + clientDocser    + CHR(10) +
       "client-docnum:"    + clientDocnum    + CHR(10) +
       "client-docwho1:"   + clientDocwho1   + CHR(10) +
       "client-docwho2:"   + clientDocwho2   + CHR(10) +
       "client-doctype:"   + clientDoctype   + CHR(10) +
       "client-docdate:"   + clientDocdate   + CHR(10) +
       "client-addrcntr:"  + clientAddrcntr  + CHR(10) +
       "client-phone:"     + clientPhone     + CHR(10) +
       "client-mob:"       + clientMob       + CHR(10) +
       "client-phonewrk:"  + clientPhonewrk  + CHR(10) +
       "client-mail:"      + clientMail      + CHR(10) +
       "client-citizen:"   + clientCitizen   + CHR(10) +
       "client-birthday:"  + clientBirthday  + CHR(10) +
       "client-birthsite:" + clientBirthsite + CHR(10) +
       "client-inn:"       + clientINN       + CHR(10) +
       "derjpol:"          + derjpol
       .

PUT UNFORMATTED sOut SKIP.

OUTPUT CLOSE.

RUN sndbispc.p("file=" + fName + ";class=" + className).
