&IF DEFINED (PARSER-PARSSEN-P) &THEN
/*
  Что делает: Рассчитывает комиссию РКО (обналичка)
  Синтаксис : КомОбналРКО(счет_дт, 
                          дата_документа , 
                          сумма_документа, 
                          код_транзакции, 
                          id_клиента_счета_дт, 
                          номер_операции).
*/

DEF VAR exclude_lst AS CHAR NO-UNDO.
exclude_lst =
"*з/пл*" + "," +
"*зар*плат*" + "," +
"*числ*аванс" + "," +
"*алимент*" + "," +
"*преми*" + "," +
"*единовр*пособ*" + "," +
"*пособ*реб*" + "," +
"*з/п*" + "," +
"*зар.пл*"
.

FUNCTION CheckNazn RETURN LOGICAL (INPUT nazn AS CHAR):
  DEF VAR i AS INT.
  DEF VAR chk AS LOGICAL INIT YES.    
  REPEAT i = 1 TO NUM-ENTRIES(exclude_lst):
    IF (nazn MATCHES ENTRY(i, exclude_lst)) = TRUE THEN DO:
      chk = NO.
      LEAVE.
    END.
  END.
  RETURN chk.
END.

FUNCTION CheckTarif RETURN LOGICAL (INPUT iAcct AS CHAR):
  DEF VAR chk AS LOGICAL INIT YES.
  DEF VAR surrogate AS CHAR FORMAT "x(30)" NO-UNDO.
  FIND FIRST loan-acct WHERE loan-acct.acct BEGINS iAcct NO-LOCK NO-ERROR.
  IF AVAIL loan-acct THEN DO:
    surrogate = TRIM(contract) + ',' + TRIM(cont-code) + ',' + STRING(since, "99/99/99").
    FIND FIRST signs WHERE signs.file-name BEGINS 'loan-cond'
                       AND signs.code BEGINS 'ТарифПлан'
                       AND signs.surrogate BEGINS surrogate NO-LOCK NO-ERROR.
    IF AVAIL signs THEN DO:
      IF signs.xattr-value <> '63' THEN chk = NO.
    END.
  END.
  RETURN chk.
END.

PROCEDURE КомОбналРКО:
  DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO.
  IF NOT Pars-ValidParam(6) THEN RETURN.
  DEF VAR acct_cust   AS CHAR          NO-UNDO.
  DEF VAR op_date     AS DATE          NO-UNDO.
  DEF VAR c_tranz     AS CHAR          NO-UNDO.
  DEF VAR cust_id     AS INT           NO-UNDO.
  DEF VAR cust_lst    AS CHAR          NO-UNDO.
  DEF VAR cust_id_s   AS CHAR          NO-UNDO.
  DEF VAR kom_chk     AS INT           NO-UNDO.
  DEF VAR s_doc       AS DEC           NO-UNDO.
  DEF VAR s_accum     AS DEC           NO-UNDO.
  DEF VAR s_all       AS DEC           NO-UNDO.
  DEF VAR s_kom6      AS DEC           NO-UNDO.
  DEF VAR s_kom6_3    AS DEC           NO-UNDO.
  DEF VAR s_kom3_5    AS DEC           NO-UNDO.
  DEF VAR s_kom5      AS DEC           NO-UNDO.
  DEF VAR s_kom       AS DEC           NO-UNDO.
  DEF VAR p_kom6      AS DEC           NO-UNDO.
  DEF VAR p_kom6_3    AS DEC           NO-UNDO.
  DEF VAR p_kom3_5    AS DEC           NO-UNDO.
  DEF VAR p_kom5      AS DEC           NO-UNDO.
  DEF VAR n_oper      AS INT           NO-UNDO.
  DEF VAR bal_lst     AS CHAR          NO-UNDO.
  DEF VAR doc_nazn    AS CHAR          NO-UNDO.
  DEF VAR i           AS INT           NO-UNDO.
  DEF VAR chk_cust    AS INT           NO-UNDO.
  DEF VAR chk_nazn    AS LOG  INIT YES NO-UNDO.
  DEF VAR chk_tarif   AS LOG  INIT YES NO-UNDO.
  DEF VAR ind_tarif   AS LOG  INIT NO  NO-UNDO.
  
  ASSIGN
  acct_cust = Pars-GetString(0)
  op_date   = DATE(Pars-GetString(1))
  s_doc     = Pars-GetDec(2)
  c_tranz   = Pars-GetString(3)
  cust_id   = Pars-GetInt(4)
  n_oper    = Pars-GetInt(5)
  .
  
  IF c_tranz = "к50001" THEN DO:
    ASSIGN
    cust_lst = ":98442:4911:5225:2771:"
    p_kom6   = 1.00
    p_kom6_3 = 1.50
    p_kom3_5 = 3.00
    p_kom5   = 8.00
    bal_lst  = "20202*"
    .
  END.
  
  IF c_tranz = "к50003" THEN DO:
    ASSIGN
    cust_lst = ":98442:4911:5225:2771:"
    p_kom6   = 1.50
    p_kom6_3 = 2.00
    p_kom3_5 = 4.00
    p_kom5   = 8.00
    bal_lst  = "20202*"
    .
  END.
  
  IF c_tranz = "к200021" THEN DO:
    ASSIGN
    cust_lst = ":98337:2771:"
    p_kom6   = 1.00 /*1.50*/
    p_kom6_3 = 2.00
    p_kom3_5 = 4.00
    p_kom5   = 8.00
    bal_lst  = "40817*,40820*,42301*,42601*"
    .
    FOR EACH op WHERE op.op-date=op_date AND op.op=n_oper.
    doc_nazn = op.details.
    END.
  END.

  ASSIGN
  cust_id_s = ":" + STRING(cust_id) + ":"
  chk_cust  = INDEX(cust_lst, cust_id_s)
  /*chk_tarif = CheckTarif(acct_cust)*/
  .

  IF c_tranz = "к200021" THEN chk_nazn = CheckNazn(doc_nazn).
  
  IF chk_nazn  = YES AND 
     /*chk_tarif = YES AND */
     chk_cust  = 0 THEN kom_chk = 0. ELSE kom_chk = 1.
  
  IF kom_chk = 1 THEN DO:
    s_kom = 0.00.
    IF chk_cust > 0 THEN MESSAGE "Клиент имеет индивидуальный тариф !!!" VIEW-AS ALERT-BOX.
    /*IF chk_tarif = NO OR chk_cust > 0 THEN MESSAGE "Клиент имеет индивидуальный тариф !!!" VIEW-AS ALERT-BOX.*/
  END.
  
  IF kom_chk = 0 THEN DO: 
    FOR EACH op-entry WHERE op-entry.op-date >= DATE(MONTH(op_date), 01, YEAR(op_date)) 
                        AND op-entry.op-date <= op_date 
                        AND op-entry.acct-db BEGINS acct_cust 
                        AND CAN-DO(bal_lst, SUBSTRING(op-entry.acct-cr, 1, 20)) 
                        AND op-entry.op-status BEGINS '√' NO-LOCK,
                        FIRST op WHERE op-entry.op = op.op 
                                   AND op-entry.op-date = op.op-date 
                                   AND op.op-kind = c_tranz NO-LOCK.
      IF c_tranz <> "к200021" THEN s_accum = s_accum + op-entry.amt-rub.
      IF c_tranz = "к200021" AND CheckNazn(op.detail) = YES THEN s_accum = s_accum + op-entry.amt-rub.
    END.
    
    s_all = s_accum + s_doc.

    /* ООО "ОБРАЗЕЦ" (индивидуальный тариф) от 19.10.2015г. */
    IF acct_cust = "40702810000000005555" THEN DO:
      ASSIGN
      ind_tarif = YES
      p_kom6    = 1
      p_kom6_3  = 2
      .
      IF s_all <= 600000 THEN s_kom6 = s_doc / 100 * p_kom6.
      IF s_all >  600000 THEN DO:
        IF s_accum <= 600000 THEN DO:
          ASSIGN
          s_kom6   = (600000 - s_accum) / 100 * p_kom6.
          s_kom6_3 = (s_all  - 600000)  / 100 * p_kom6_3.
          .
        END. 
        IF s_accum > 600000 THEN DO:
          s_kom6_3 = s_doc / 100 * p_kom6_3.
        END.
      END.
    END.
    
    IF ind_tarif = NO THEN DO:
      IF CAN-DO("к50003", c_tranz) THEN DO:
        ASSIGN
        p_kom6   = 1.50
        p_kom6_3 = 2.50
        .
        
        IF s_all <= 600000 THEN s_kom6 = s_doc / 100 * p_kom6.
        IF s_all >  600000 THEN DO:
          IF s_accum <= 600000 THEN DO:
            ASSIGN
            s_kom6   = (600000 - s_accum) / 100 * p_kom6.
            s_kom6_3 = (s_all  - 600000)  / 100 * p_kom6_3.
            .
          END. 
          IF s_accum > 600000 THEN DO:
            s_kom6_3 = s_doc / 100 * p_kom6_3.
          END.
        END.
      END.
      
      IF NOT CAN-DO("к50003", c_tranz) THEN DO:
        IF s_all <= 600000 THEN s_kom6 = s_doc / 100 * p_kom6.
        IF s_all >  600000 AND s_all <= 3000000 THEN DO:
          IF s_accum <= 600000 THEN DO:
            ASSIGN
            s_kom6   = (600000 - s_accum) / 100 * p_kom6.
            s_kom6_3 = (s_all  - 600000)  / 100 * p_kom6_3.
            .
          END. 
          IF s_accum > 600000 THEN DO:
            s_kom6_3 = s_doc / 100 * p_kom6_3.
          END.
        END.
        IF s_all > 3000000 AND s_all <= 5000000 THEN DO:
          IF s_accum <= 600000 THEN DO:
            ASSIGN
            s_kom6   = (600000  - s_accum) / 100 * p_kom6.
            s_kom6_3 = (3000000 - 600000)  / 100 * p_kom6_3.
            s_kom3_5 = (s_all   - 3000000) / 100 * p_kom3_5.
            .
          END.
          IF s_accum > 600000 AND s_accum <= 3000000 THEN DO:
            ASSIGN
            s_kom6_3 = (3000000 - s_accum) / 100 * p_kom6_3.
            s_kom3_5 = (s_all   - 3000000) / 100 * p_kom3_5.
            .
          END.
          IF s_accum > 3000000 THEN DO:
            s_kom3_5 = s_doc / 100 * p_kom3_5.
          END.
        END.
        IF s_all > 5000000 THEN DO:
          IF s_accum <= 600000 THEN DO:
            ASSIGN
            s_kom6   = (600000  - s_accum) / 100 * p_kom6.
            s_kom6_3 = (3000000 - 600000)  / 100 * p_kom6_3.
            s_kom3_5 = (5000000 - 3000000) / 100 * p_kom3_5.
            s_kom5   = (s_all   - 5000000) / 100 * p_kom5.
            .
          END.
          IF s_accum > 600000 and s_accum <=3000000 THEN DO:
            ASSIGN
            s_kom6_3 = (3000000 - s_accum) / 100 * p_kom6_3.
            s_kom3_5 = (5000000 - 3000000) / 100 * p_kom3_5.
            s_kom5   = (s_all   - 5000000) / 100 * p_kom5.
            .
          END.
          IF s_accum > 3000000 and s_accum <= 5000000 THEN DO:
            ASSIGN
            s_kom3_5 = (5000000 - s_accum) / 100 * p_kom3_5.
            s_kom5   = (s_all   - 5000000) / 100 * p_kom5.
            .
          END.
          IF s_accum > 5000000 THEN DO:
            s_kom5 = s_doc / 100 * p_kom5.
          END.
        END.
      END.
    END.
    
    s_kom = s_kom6 + s_kom6_3 + s_kom3_5 + s_kom5.
  END.
  
  RUN Pars-SetResult (s_kom).
  is-ok = TRUE.
END PROCEDURE.

&ENDIF



&IF DEFINED (PARSER-PARSSEN-P) &THEN
/*
  Что делает: Рассчитывает комиссию РКО (инкассация)
  Синтаксис : КомИнкассРКО(счет_клиента, операционная_дата).
*/

FUNCTION GetNumValue RETURN DEC (INPUT s AS CHAR, INPUT d AS CHAR):
  DEF VAR i AS INT NO-UNDO.
  DEF VAR v AS DEC NO-UNDO.
  DEF VAR r AS DEC NO-UNDO.
  
  REPEAT i = 1 TO NUM-ENTRIES(s, d):
    ASSIGN v = DEC(TRIM(ENTRY(i, s, d))) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR AND v <> 0.00 THEN r = v.
  END.
  
  RETURN r.
END.

PROCEDURE КомИнкассРКО:
  DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO.
  IF NOT Pars-ValidParam(2) THEN RETURN.
  DEF VAR acct_cust   AS CHAR NO-UNDO.
  DEF VAR op_date     AS DATE NO-UNDO.
  DEF VAR tranz_b_lst AS CHAR NO-UNDO.
  DEF VAR tranz_m_lst AS CHAR NO-UNDO.
  DEF VAR summ        AS DEC  NO-UNDO.
  DEF VAR summ_b      AS DEC  NO-UNDO.
  DEF VAR summ_m      AS DEC  NO-UNDO.
  DEF VAR summ_dop    AS DEC  NO-UNDO.
  DEF VAR pr_b        AS DEC  NO-UNDO.
  DEF VAR pr_m        AS DEC  NO-UNDO.
  DEF VAR tarif       AS CHAR NO-UNDO.
  DEF VAR s           AS CHAR NO-UNDO.
  DEF VAR s1          AS CHAR NO-UNDO.
  
  ASSIGN
  acct_cust   = Pars-GetString(0)
  op_date     = DATE(Pars-GetString(1))
  tranz_b_lst = "030100021,030100022,030310a"
  tranz_m_lst = "030100023,0303101a"
  tarif       = "Базовый"
  .
  
  /* ООО "Газмяс" */
  IF acct_cust BEGINS "40702810800000007777" THEN DO:
    ASSIGN
    pr_b  = 0.20
    pr_m  = 3.00              /*изменено с 1.40 на 3.00 (23.10.2015)*/
    tarif = "Индивидуальный"
    .
  END.
  
  /* ЗАО "Сла" */
  IF acct_cust BEGINS "40702810900000008888" THEN DO:
    ASSIGN
    pr_b  = 0.30
    pr_m  = 1.20
    tarif = "Индивидуальный"
    .
  END.
  
  /* ООО "СОВА" */
  IF acct_cust BEGINS "40702810600000009999" THEN DO:
    ASSIGN
    pr_b  = 0.40 
    pr_m  = 1.20
    tarif = "Индивидуальный"
    .
  END.

  /* ИП Наталья Васильевна */
  IF acct_cust BEGINS "40802810600000000311" THEN DO:
    ASSIGN
    pr_b  = 0.20
    pr_m  = 1.20
    tarif = "Индивидуальный"
    .
  END.
  
  /* ООО "ВАО" */
  IF acct_cust BEGINS "40702810400000002333" THEN DO:
    ASSIGN
    pr_b  = 0.20
    pr_m  = 1.20
    tarif = "Индивидуальный"
    .
  END.

  IF tarif = "Индивидуальный" THEN DO:
    /*банкноты и монеты*/
    FOR EACH op-entry WHERE op-entry.op-date = op_date 
                        AND op-entry.acct-cr BEGINS acct_cust 
                        AND op-entry.op-status BEGINS '√' NO-LOCK, 
                        FIRST op WHERE op.op = op-entry.op 
                                   AND (CAN-DO(tranz_b_lst, op.op-kind) 
                                    OR  CAN-DO(tranz_m_lst, op.op-kind)) NO-LOCK.
      IF CAN-DO(tranz_b_lst, op.op-kind) THEN summ_b = summ_b + op-entry.amt-rub.
      IF CAN-DO(tranz_m_lst, op.op-kind) THEN summ_m = summ_m + op-entry.amt-rub.
    END.
    summ = (summ_b / 100 * pr_b) + (summ_m / 100 * pr_m).
  END.
  
  IF tarif = "Базовый" THEN DO:
    /*банкноты*/
    FOR EACH op-entry WHERE op-entry.op-date = op_date 
                        AND op-entry.acct-cr BEGINS acct_cust 
                        AND op-entry.op-status BEGINS '√' NO-LOCK, 
                        FIRST op WHERE op.op = op-entry.op 
                                   AND CAN-DO(tranz_b_lst, op.op-kind) 
                                   AND op.details MATCHES ("*Базовый тариф*") NO-LOCK.
      ASSIGN
      s    = SUBSTRING(op.details, INDEX(op.details, "Базовый тариф"))
      s1   = ENTRY(2, s, ";")
      summ = summ + GetNumValue(s1, " ")
      .
    END.
    /*монеты*/
    FOR EACH op-entry WHERE op-entry.op-date = op_date 
                        AND op-entry.acct-cr BEGINS acct_cust 
                        AND op-entry.op-status BEGINS '√' NO-LOCK, 
                        FIRST op WHERE op.op = op-entry.op 
                                   AND CAN-DO(tranz_m_lst, op.op-kind) NO-LOCK.
      summ_m = summ_m + op-entry.amt-rub.
    END.
    summ = summ + summ_m / 100 * 1.20.
  END.

  RUN Pars-SetResult (summ).
  is-ok = TRUE.
END PROCEDURE.

&ENDIF



&IF DEFINED (PARSER-DETAILS-P) &THEN
/*
  Что делает: Рассчитывает комиссию за инкассацию по документу.
              Результат записывается в поле "Содержание".
  Синтаксис : КомИнкассДок(сумма_документа)
*/

PROCEDURE КомИнкассДок:
  DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO.
  IF NOT Pars-ValidParam(1) THEN RETURN.
  DEF VAR sum AS DEC  NO-UNDO.
  DEF VAR kom AS DEC  NO-UNDO.
  DEF VAR str AS CHAR NO-UNDO.
  
  sum = Pars-GetDec(0).
  
  /*--------------------------------------------------------------------------------------*/
  
  PAUSE 0 BEFORE-HIDE NO-MESSAGE.
  DEF VAR kolSum AS INT NO-UNDO.
  DEF VAR tarifLst AS INT VIEW-AS RADIO-SET
                   RADIO-BUTTONS 
                   "Базовый", 1,
                   "Индивидуальный", 2 
                   NO-UNDO.
  
  DEF VAR prKom AS DEC VIEW-AS COMBO-BOX 
                INNER-LINES 3 LIST-ITEMS 
                "0.10",
                "0.20",
                "0.17" 
                DROP-DOWN-LIST 
                INIT "0.10" 
                NO-UNDO.
  
  FORM
  SKIP(1)
  SKIP(1)
  tarifLst
  LABEL  "     Тариф"
  HELP   "Выберите тарифный план (Пробел - выбрать, TAB - завершить выбор)."
  SKIP(1)
  prKom
  FORMAT "9.99"
  LABEL  "     Процент комиссии"
  HELP   "Выберите процент комиссии"
  SKIP
  SKIP(1)
  kolSum
  FORMAT "99"
  LABEL  "     Количество сумок"
  HELP   "Введите количество сумок."
  SKIP
  SKIP(2)
  WITH FRAME fr_main WIDTH 38 CENTERED ROW 7 OVERLAY SIDE-LABELS TITLE "[РАСЧЕТ КОМИССИИ]".
  
  tarifLst:SCREEN-VALUE = tarifLst:ENTRY(1) NO-ERROR.
  
  DO WHILE kolSum = 0 AND INT64(tarifLst:SCREEN-VALUE) = 1:
    UPDATE tarifLst prKom kolSum WITH FRAME fr_main.
  END.

  IF INT64(tarifLst:SCREEN-VALUE) = 2 THEN str = "(Индивидуальный тариф)".
  IF INT64(tarifLst:SCREEN-VALUE) = 1 THEN DO:
    IF prKom = 0.10 THEN kom = (sum / 100 * prKom ) + ( kolSum * 300.00 ).
    ELSE kom = (sum / 100 * prKom ).
    str = "(Базовый тариф:кол-во сумок = " + STRING(kolSum) + ";" + 
          "cумма комиссии = " + TRIM(STRING(kom, "zzzzzzzz9.99")) + " )".
  END.
  
  RUN Pars-SetCHARResult(str).
  is-ok = TRUE.
END PROCEDURE.

&ENDIF



&IF DEFINED (PARSER-PARSACCT-P) &THEN
/*
  Что делает: Возвращает счет 70606*,70601* для указанного счета риска (60312*,60323*,61011*,60701*).
              !!!Только для ОБУиО!!!
  Синтаксис : СчетДохРасх283П(1, счет_риска)  -  счет 70606*
              СчетДохРасх283П(2, счет_риска)  -  счет 70601*
*/

PROCEDURE СчетДохРасх283П:
  DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO.
  IF NOT Pars-ValidParam(2) THEN RETURN.
  DEF VAR i      AS INT  NO-UNDO.
  DEF VAR acct_r AS CHAR NO-UNDO.
  DEF VAR rs     AS CHAR NO-UNDO.
  
  ASSIGN
  i      = Pars-GetInt(0)
  acct_r = Pars-GetString(1)
  .
  
  IF acct_r BEGINS "60312" THEN rs = "70606810000002530221,70601810500001630546".
  IF acct_r BEGINS "61011" THEN rs = "70606810600002530294,70601810500001630546".
  IF acct_r BEGINS "60701" THEN rs = "70606810000002530205,70601810500001630546".
  IF acct_r BEGINS "60323" THEN DO:
    FOR FIRST acct WHERE acct.acct BEGINS acct_r NO-LOCK.
      IF CAN-DO("*гос*пошлина*", acct.details)   THEN rs = "70606810700002530288,70601810300001630513".
      IF CAN-DO("*недостача*АТМ*", acct.details) THEN rs = "70606810600002530294,70601810500001630546".
    END.
  END.
  
  RUN Pars-SetCHARResult(ENTRY(i, rs)).
  is-ok = TRUE.
END PROCEDURE.

&ENDIF



/*
  Что делает: Определяет является ли платеж бюджетным.
  Синтаксис : БюджетП(номер_проводки)
*/

PROCEDURE БюджетП:
  DEF OUTPUT PARAM is-ok AS LOG NO-UNDO.
  IF NOT Pars-ValidParam(1) THEN RETURN.
  DEF VAR vOpEntryNum AS INT64 NO-UNDO.
  DEF VAR vOpRecId    AS INT64 NO-UNDO.
  DEF VAR vBenAcct    AS CHAR  NO-UNDO.
  DEF VAR vNameBen    AS CHAR  NO-UNDO.
  DEF VAR vImpStr     AS CHAR  NO-UNDO.
  DEF VAR val_1       AS CHAR  NO-UNDO.
  DEF VAR val_2       AS CHAR  NO-UNDO.
  DEF VAR vResult     AS INT   NO-UNDO.
  
  ASSIGN
  vOpEntryNum = Pars-GetInt(0)
  vResult     = 0
  .
  
  FIND FIRST xwop WHERE xwop.op-templ = vOpEntryNum NO-LOCK NO-ERROR.
  IF AVAIL xwop THEN vOpRecId = xwop.Op-Recid.
  ELSE RETURN.
  
  FOR FIRST op WHERE RECID(op) = vOpRecId NO-LOCK.
    ASSIGN
    vBenAcct = op.ben-acct
    vNameBen = op.name-ben
    .
  END.
  
  IF vBenAcct BEGINS "40101" THEN vResult = 1.
  
  INPUT FROM VALUE("/qbis/wrk/imp-exp/0000/logs/keyval.txt").
  REPEAT:
    IMPORT UNFORMATTED vImpStr.
    ASSIGN
    vImpStr = CODEPAGE-CONVERT(TRIM(vImpStr), "IBM866", "1251")
    val_1   = ENTRY(1, vImpStr, ";")
    val_2   = ENTRY(2, vImpStr, ";")
    .
    IF vBenAcct BEGINS val_1 AND CAN-DO(val_2, vNameBen) THEN DO:
      vResult = 0.
    END.
  END.
  INPUT CLOSE.
  
  RUN Pars-SetResult (vResult).
  is-ok = TRUE.
END PROCEDURE.



/*
  Что делает: Возвращает номер операции исходного документа для комиссии
  Синтаксис : ИсхДокКомПП(номер_проводки)
  Автор     : RUSH
*/

PROCEDURE ИсхДокКомПП:
  DEF OUTPUT PARAM is-ok AS LOG NO-UNDO.
  IF NOT Pars-ValidParam(1) THEN RETURN.
  DEF VAR vOpEntryNum AS INT64 NO-UNDO.
  DEF VAR vOpRecId    AS INT64 NO-UNDO.
  DEF VAR vResult     AS INT   NO-UNDO.
  
  ASSIGN
  vOpEntryNum = Pars-GetInt(0)
  vResult     = 0
  .
  
  FIND FIRST xwop WHERE xwop.op-templ = vOpEntryNum NO-LOCK NO-ERROR.
  IF AVAIL xwop THEN vOpRecId = xwop.Op-Recid.
  ELSE RETURN.
  
  FOR FIRST op WHERE RECID(op) = vOpRecId NO-LOCK.
    ASSIGN
    vResult = op.op
    .
  END.
  
  RUN Pars-SetResult (vResult).
  is-ok = TRUE.
END PROCEDURE.



/*
  Что делает: Возвращает технологию платежа
  Синтаксис : ТехПлат(номер_проводки)
  Автор     : RUSH
*/
PROCEDURE ТехПлат:
  DEF OUTPUT PARAM is-ok AS LOG NO-UNDO.
  IF NOT Pars-ValidParam(1) THEN RETURN.
  DEF VAR vOpEntryNum AS INT64 NO-UNDO.
  DEF VAR vOpRecId    AS INT64 NO-UNDO.
  DEF VAR vResult     AS CHAR  NO-UNDO.
  
  ASSIGN
  vOpEntryNum = Pars-GetInt(0)
  vResult     = ""
  .
  
  FIND FIRST xwop WHERE xwop.op-templ = vOpEntryNum NO-LOCK NO-ERROR.
  IF AVAIL xwop THEN vOpRecId = xwop.Op-Recid.
  ELSE RETURN.
  
  FOR FIRST op WHERE RECID(op) = vOpRecId, FIRST op-entry WHERE op-entry.op = op.op NO-LOCK.
    ASSIGN
    vResult = op-entry.type
    .
  END.
  
  RUN Pars-SetCHARResult(vResult).
  is-ok = TRUE.
END PROCEDURE.
