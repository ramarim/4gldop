/*                                                                                                    */
/*                                                                                                    */
/*                         ���� �� ������⠬ ��� ��� ����������� ����஫�                          */
/*                                                                                                    */
/*                                                                                                    */

PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF VAR dateBeg AS DATE INIT TODAY NO-UNDO.
DEF VAR dateEnd AS DATE INIT TODAY NO-UNDO.

FORM
SPACE(2)
dateBeg
FORMAT "99.99.9999"
LABEL  '��ਮ� �'
HELP   '������ ���� ��砫� ��ਮ��.'
SPACE(1)
dateEnd
FORMAT "99.99.9999"
LABEL  '��'
HELP   '������ ���� ����砭�� ��ਮ��.'
SPACE(2)
SKIP

WITH FRAME frMain CENTERED ROW 10 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[ ����� �� ����������� ]".
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
klb*                      //������-����
scan*,0101-01r,0101-03r   //���⥦�� ����祭�� �� �㬠��
�50004b                   //��⨥ �� 祪�
�50001                    //������� �� ����� (� �����)
*/

{setdest.i}

PUT "                                                                                               " SKIP.
PUT "  ��ਮ� � " dateBeg_ " �� " dateEnd_ "                                                        " SKIP.
PUT "                                                                                               " SKIP.
PUT "                                                                                               " SKIP.
PUT "                                                                                               " SKIP.
PUT "                         ����� �� ����������� ��� ����������� ��������                         " SKIP.
PUT "                                                                                               " SKIP.
PUT " �������������������������������������������������������������������������������������������Ŀ " SKIP.
PUT " �                                          �              ������⢮ ����権               � " SKIP.
PUT " �                                          ������������������������������������������������Ĵ " SKIP.
PUT " �         ��� ������� �� ����          �             � ���⥦�� � ���⨥  � ������� � " SKIP.
PUT " �                                          � ������-���� � ����祭�� �   ��    � �� �����   � " SKIP.
PUT " �                                          �             � �� �㬠�� �  祪�   � (� �����)  � " SKIP.
PUT " �������������������������������������������������������������������������������������������Ĵ " SKIP.

FOR EACH op-entry WHERE op-entry.op-date >= dateBeg
                    AND op-entry.op-date <= dateEnd
                    AND op-entry.op-status BEGINS "�" NO-LOCK,
                    FIRST op WHERE op.op = op-entry.op 
                               AND CAN-DO("*", op.op-kind) 
                               AND CAN-DO("OR*",     op.user-id) NO-LOCK
                               BREAK BY op.user-id.
  
  IF NOT LAST-OF(op.user-id) THEN DO:
    IF CAN-DO("klb*",                    op.op-kind) THEN countKLB = countKLB + 1.
    IF CAN-DO("scan*,0101-01r,0101-03r", op.op-kind) THEN countPPB = countPPB + 1.
    IF CAN-DO("�50004b",                 op.op-kind) THEN countSCH = countSCH + 1.
    IF CAN-DO("�50001",                  op.op-kind) THEN countOVZ = countOVZ + 1.
  END.
  
  IF LAST-OF(op.user-id) THEN DO:
    IF CAN-DO("klb*",                    op.op-kind) THEN countKLB = countKLB + 1.
    IF CAN-DO("scan*,0101-01r,0101-03r", op.op-kind) THEN countPPB = countPPB + 1.
    IF CAN-DO("�50004b",                 op.op-kind) THEN countSCH = countSCH + 1.
    IF CAN-DO("�50001",                  op.op-kind) THEN countOVZ = countOVZ + 1.
    
    FIND FIRST _user WHERE _user._userid = op.user-id NO-LOCK NO-ERROR.
    userName = _user._user-name.
    
    strPut  = " � " + STRING(userName,       "x(40)") +
              " � " + STRING(countKLB, "zzzzzzzzzz9") +
              " � " + STRING(countPPB,   "zzzzzzzz9") +
              " � " + STRING(countSCH,     "zzzzzz9") +
              " � " + STRING(countOVZ,  "zzzzzzzzz9") +
              " � "
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

PUT " �������������������������������������������������������������������������������������������Ĵ " SKIP.

strPut  = " � " + STRING("�����:",       "x(40)") +
          " � " + STRING(totalKLB, "zzzzzzzzzz9") +
          " � " + STRING(totalPPB,   "zzzzzzzz9") +
          " � " + STRING(totalSCH,     "zzzzzz9") +
          " � " + STRING(totalOVZ,  "zzzzzzzzz9") +
          " � "
.
PUT UNFORMATTED strPut SKIP.

PUT " ��������������������������������������������������������������������������������������������� " SKIP.
PUT "                                                                                               " SKIP.

{preview.i}


/*
�����������������Ŀ
�����������������Ĵ
�        �        �
�������������������
*/
