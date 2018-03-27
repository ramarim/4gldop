/*
     Filename: cons-bo.p
      Comment: Сводный банковский ордер
   Parameters:
         Uses:
      Used by:
      Created: never nobody
     Modified: 
*/

{globals.i}
{tmprecid.def}
{intrface.get tmess}
{intrface.get strng}
{parsin.def}
{bankord.def}

FUNCTION GetLastSN RETURN CHAR (INPUT iDate AS DATE):
  DEF VAR result AS CHAR NO-UNDO.
  
  FOR EACH Seance WHERE Seance.op-kind = "i-salamt"
                    AND Seance.SeanceDate = iDate
                    NO-LOCK BY Seance.SeanceDate
                            BY Seance.SeanceTime.
                            
    result = STRING(INT64(Seance.Number), "9999").
  END.
  
  RETURN result.
END.

/* переменная из cons-bo.p */
DEF VAR curr       AS LOG  INIT YES   NO-UNDO.
DEF VAR seanceDate AS DATE INIT TODAY NO-UNDO.

DEFINE INPUT PARAMETER iParams AS CHARACTER.

&GLOBAL-DEFINE FILE_sword_p YES
&GLOBAL-DEFINE CONSOLIDATED yes

DEFINE VAR rid AS RECID NO-UNDO.

DEFINE TEMP-TABLE tt-op NO-UNDO
   FIELD op  LIKE op.op
   FIELD rcd AS RECID
INDEX ByOp op.

DEFINE VAR mAcctDbCheck AS CHARACTER NO-UNDO.
DEFINE VAR mAcctDbBad   AS LOGICAL   NO-UNDO.
DEFINE VAR mAcctCrCheck AS CHARACTER NO-UNDO.
DEFINE VAR mAcctCrBad   AS LOGICAL   NO-UNDO.
DEFINE VAR mDocNumType  AS CHARACTER INIT "1" NO-UNDO.
DEFINE VAR mDocNum      AS CHARACTER NO-UNDO.
DEFINE VAR mDocSort     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDocDetails    AS CHARACTER              NO-UNDO.
DEFINE VARIABLE mDocDetailsLen AS INT64     INITIAL 1000 NO-UNDO.
DEFINE VARIABLE vStmp   AS CHARACTER NO-UNDO.

FOR EACH tmprecid NO-LOCK,
FIRST op WHERE RECID(op) = tmprecid.id NO-LOCK:
   CREATE tt-op.
   ASSIGN
      seanceDate = op.op-date
      tt-op.op   = op.op
      tt-op.rcd  = RECID(op)
   .
   RELEASE tt-op.
END.
RELEASE op.
RELEASE op-entry.

FORM
   mDocNumType VIEW-AS RADIO-SET VERTICAL
               RADIO-BUTTONS "Из первого документа","1","Другой:","2"
               LABEL "Реквизиты документа"
   mDocNum     NO-LABEL
               AT ROW 2 COL 33
   SKIP
   mDocSort    VIEW-AS RADIO-SET VERTICAL
               RADIO-BUTTONS "В порядке ввода","1","По сумме","2","По счету и сумме","3","По счету (с учетом ключа)","4"
               LABEL "Порядок сортировки"
               AT ROW 3 COL 2
               SKIP
               &IF DEFINED(MEMORN) = 0 &THEN
               "Назначение платежа:"
               &ELSE
               "Содержание операции:"
               &ENDIF
               SKIP
   mDocDetails VIEW-AS EDITOR SIZE 50 BY 8 SCROLLBAR-VERTICAL
               NO-LABEL
WITH FRAME ParamsFrm CENTERED OVERLAY ROW 6 SIDE-LABELS TITLE "[ ПАРАМЕТРЫ ПЕЧАТИ ]".

ON ENTRY OF FRAME ParamsFrm DO:
    APPLY "VALUE-CHANGED" TO mDocNumType.
    IF mDocNumType:SENSITIVE
    THEN APPLY "ENTRY" TO mDocNumType.
    ELSE IF mDocSort:SENSITIVE
    THEN APPLY "ENTRY" TO mDocSort.
    ELSE IF mDocDetails:SENSITIVE
    THEN APPLY "ENTRY" TO mDocDetails.
    RUN CheckDocDetailsLen NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        RUN ReportDocDetailsError.
        RETURN NO-APPLY.
    END.
END.

ON VALUE-CHANGED OF mDocNumType IN FRAME ParamsFrm DO:
    ASSIGN mDocNumType.
    mDocNum:VISIBLE = (mDocNumType = "2").
    IF mDocNumType = "1" THEN DO:
       FIND FIRST tmprecid NO-LOCK NO-ERROR.
       FIND FIRST op WHERE RECID(op)  = tmprecid.id NO-LOCK NO-ERROR.
       IF AVAIL(op) THEN DO:
          mDocNum = op.doc-num.
          IF GetParamByNameAsChar(iParams, "НазначениеПлатежа", "") EQ "" OR
             GetParamByNameAsChar(iParams, "НазначениеПлатежа", "") EQ ?
          THEN
             mDocDetails:SCREEN-VALUE = GetXAttrValueEx("op",
                                                        STRING(op.op),
                                                        "alt-details",
                                                        op.details). 
       END.
       ASSIGN mDocDetails.
    END.
END.

ON VALUE-CHANGED OF mDocNum IN FRAME ParamsFrm DO:
    ASSIGN mDocNum.
END.

ON LEAVE OF mDocNum IN FRAME ParamsFrm DO:
    FOR EACH tmprecid  NO-LOCK:
       FIND FIRST op WHERE RECID(op)  = tmprecid.id
                       AND op.doc-num = mDocNum:SCREEN-VALUE 
       NO-LOCK NO-ERROR.
       IF AVAIL(op) THEN LEAVE.
    END. 
    IF AVAIL(op) AND
       (GetParamByNameAsChar(iParams, "НазначениеПлатежа", "") EQ "" OR
       GetParamByNameAsChar(iParams, "НазначениеПлатежа", "") EQ ?)
    THEN
       mDocDetails:SCREEN-VALUE = GetXAttrValueEx("op",
                                                  STRING(op.op),
                                                  "alt-details",
                                                  op.details).       
END.

ON GO OF FRAME ParamsFrm ANYWHERE DO:
   ASSIGN mDocNumType.
   RUN CheckDocDetailsLen NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
       RUN ReportDocDetailsError.
       RETURN NO-APPLY.
   END.
   IF mDocNumType = "2" THEN DO:
      ASSIGN mDocNum.
      IF NOT CAN-FIND(FIRST tmprecid WHERE CAN-FIND(FIRST op WHERE RECID(op)  = tmprecid.id
                                                               AND op.doc-num = mDocNum NO-LOCK) NO-LOCK)
      THEN DO:
         RUN Fill-SysMes IN h_tmess ("","","-1","Среди выбранных документов нет документа с номером " + GetNullStr(mDocNum)).
         RETURN NO-APPLY.
      END.
   END.
END.

ON ANY-PRINTABLE OF mDocDetails IN FRAME ParamsFrm DO:
    RUN CheckDocDetailsLen NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN NO-APPLY.
END.

PROCEDURE CheckDocDetailsLen.
    DO WITH FRAME ParamsFrm:
        IF mDocDetailsLen > 0 AND
           LENGTH(mDocDetails:SCREEN-VALUE) >= mDocDetailsLen
        THEN
            RETURN ERROR.
    END.
END PROCEDURE.

PROCEDURE ReportDocDetailsError.
    DO WITH FRAME ParamsFrm:
       RUN Fill-SysMes IN h_tmess ("",
                                   "",
                                   "-1",
                                   "Длина поля ~"" +
                                   &IF DEFINED(MEMORN) = 0 &THEN
                                   "Назначение платежа"
                                   &ELSE
                                   "Содержание операции"
                                   &ENDIF
                                   + "~" (" +
                                   STRING(LENGTH(mDocDetails:SCREEN-VALUE)) +
                                   ") превышает допустимую для данного " +
                                   "класса документа (" +
                                   STRING(mDocDetailsLen) + ")").
    END.
END PROCEDURE.

PROCEDURE GetBaseOp.
    DEFINE PARAMETER BUFFER b-op FOR op.

    IF mDocNumType = "1" THEN DO:
        FOR FIRST tt-op
        BY tt-op.op:
            FIND FIRST b-op
            WHERE
                b-op.op = tt-op.op
            NO-LOCK NO-ERROR.
        END.
    END.
    ELSE DO:
        FIND FIRST tt-op
        WHERE
            CAN-FIND(FIRST op
                     WHERE
                         op.op      = tt-op.op AND
                         op.doc-num = mDocNum)
        NO-LOCK NO-ERROR.
        IF AVAILABLE tt-op THEN
            FIND FIRST b-op
            WHERE
                b-op.op = tt-op.op
            NO-LOCK NO-ERROR.
    END.
END PROCEDURE.

PAUSE 0.

RUN GetBaseOp(BUFFER op).
FIND FIRST xattr WHERE
    xattr.class-code = op.class-code AND
    xattr.xattr-code = "details"
NO-LOCK NO-ERROR.
mDocDetailsLen = IF AVAILABLE xattr THEN INT64(TRIM(xattr.data-format, "x()"))
                                    ELSE 0
                 NO-ERROR.
mDocDetailsLen = MINIMUM(mDocDetailsLen, {&max-bankord-details-length}).

mDocDetails = GetParamByNameAsChar(iParams, "НазначениеПлатежа", mDocDetails).

DO WITH FRAME ParamsFrm
ON ERROR UNDO, LEAVE
ON ENDKEY UNDO, LEAVE:
    DISPLAY
        mDocNumType
        mDocNum
        mDocSort
        mDocDetails
    .
    PAUSE 0.
    UPDATE
        mDocNumType
        mDocNum
        mDocSort
        mDocDetails
    .
END.
HIDE FRAME ParamsFrm.

IF LASTKEY = KEYCODE("ESC") THEN DO:
   {intrface.del}
   RETURN.
END.

RUN SetSysConf IN h_base ("cons-ord-override-details", mDocDetails).
IF GetParamByNameAsChar(iParams, "Штамп", "") EQ "КЛБ" THEN
   vStmp = "MBStamp".
ELSE IF GetParamByNameAsChar(iParams, "Штамп", "") EQ "КБ" THEN
   vStmp = "MBIStamp".
ELSE
   vStmp = "PPStamp".
RUN SetSysConf IN h_base ("StampTmpl", vStmp).

/* переопределяем номер банковского ордера */
mDocNum = GetLastSN(seanceDate).

&IF DEFINED(MEMORN) &THEN
   &IF DEFINED(curr) NE 0 &THEN
      {memornv.p}
   &ELSE
      &IF DEFINED(ops) NE 0 &THEN        
         {memorn_ops.p}
      &ELSE
         {memorn.p}
      &ENDIF
   &ENDIF   
&ELSE

   &IF DEFINED(curr) NE 0 &THEN
   &IF DEFINED(variant) NE 0 &THEN
      {bankordw.p}
   &ELSE
      {bankordv.p}
   &ENDIF
   &ELSE
      &IF DEFINED(ops) NE 0 &THEN        
         {mav_bankord_ops.p}
      &ELSE
         /*{bankord.p}*/
         {mav_bankord_ops.p}
      &ENDIF
   &ENDIF
&ENDIF   
RUN DeleteOldDataProtocol IN h_base ("cons-ord-override-details").
