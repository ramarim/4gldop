/*                                                                                                    */
/*                                                                                                    */
/*                                ������ ���ᮢ�� �஢���� (���)                                      */
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
LABEL  '     ���'
HELP   '������ ���� ����樮����� ���.'
SKIP
user_id
FORMAT 'x(10)'
LABEL  '����஫��'
HELP   '������ ���㤭��� ��楯⮢��襣� ���㬥��(�).'
WITH FRAME fr_main CENTERED ROW 10 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[ �������� ��������� ]".

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
PUT "                                                     ������ �������� �������� �� " s_dateRpt "                                                      " SKIP.
PUT "                                                                                                                                                    " SKIP.
PUT "  ����������������������������������������������������������������������������������������������������������������������������������������������Ŀ  " SKIP.
PUT "  �        �                      �                      �              �              �    �                                                    �  " SKIP.
PUT "  � N ���. �       ��� ��        �       ��� ��        �   �㬬� ��   �   �㬬� ��   � �� �                ����ঠ��� ���樨                  �  " SKIP.
PUT "  �        �                      �                      �              �              �    �                                                    �  " SKIP.
PUT "  ����������������������������������������������������������������������������������������������������������������������������������������������Ĵ  " SKIP.

FIND FIRST _user WHERE _user._userid = user_id NO-LOCK NO-ERROR.
user_name = _user._user-name.

FOR EACH op-entry WHERE op-entry.op-date = dateRpt 
                    AND (op-entry.acct-db BEGINS '20202810'
                     OR  op-entry.acct-cr BEGINS '20202810') 
                    AND  op-entry.op-status = '�' NO-LOCK,
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

  str = ' � ' + STRING(op.doc-num,"999999")                      +
        ' � ' + STRING(SUBSTRING(op-entry.acct-db,1,20),"x(20)") +
        ' � ' + STRING(SUBSTRING(op-entry.acct-cr,1,20),"x(20)") +
        ' � ' + STRING(SumDt,"x(12)")                            +
        ' � ' + STRING(SumCt,"x(12)")                            +
        ' � ' + STRING(op-entry.symbol,"x(2)")                   +
        ' � ' + STRING(REPLACE(op.details,chr(10),""),"x(50)")   + 
        ' � '
        .
        
  PUT " " str " " SKIP. 
END.

ASSIGN
sTSumDt = STRING(TSumDt,"zzzzzzzz9.99")
sTSumCt = STRING(TSumCt,"zzzzzzzz9.99")
.

PUT "  ����������������������������������������������������������������������������������������������������������������������������������������������Ĵ  " SKIP.
PUT "  � �����:                                               � " sTSumDt  " � " sTSumCt  " �                                                         �  " SKIP.
PUT "  ������������������������������������������������������������������������������������������������������������������������������������������������  " SKIP.
PUT "                                                                                                                                                    " SKIP.
PUT "    ��壠���-�����                                                 " user_name                            "                                       " SKIP.
PUT "   ���������������������������        ����������������������        ������������������������������������������                                      " SKIP.
PUT "    (������������ ��������)             (��筠� �������)            (䠬���� � ���樠��)                                                           " SKIP.
PUT "                                                                                                                                                    " SKIP.
{preview.i}


/*
�����������������Ŀ
�����������������Ĵ
�        �        �
�������������������
*/