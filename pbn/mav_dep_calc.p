/*                                                                                                    */
/*                                                                                                    */
/*                                       Депозитный калькулятор                                       */
/*                                                                                                    */
/*                                                                                                    */

PAUSE 0 BEFORE-HIDE NO-MESSAGE.
DEF VAR dateBeg AS DATE NO-UNDO.
DEF VAR dateEnd AS DATE NO-UNDO.
DEF VAR dateRst AS DATE NO-UNDO.
DEF VAR psLst   AS INT VIEW-AS RADIO-SET
RADIO-BUTTONS "основная",1,
              "штрафная",2 NO-UNDO.

FORM
SKIP(1)
psLst
LABEL  ' Ставка'
HELP   'Выберите ставку для расчета (Пробел - выбрать, TAB - завершить выбор).'
SKIP(1)
dateBeg
FORMAT "99.99.9999"
LABEL  ' Период с '
HELP   'Введите дату начала периода.'
dateEnd
FORMAT "99.99.9999"
LABEL  'по'
HELP   'Введите дату окончания периода.'
SKIP
SKIP(1)
dateRst
FORMAT "99.99.9999"
LABEL  '         Дата расторжения'
HELP   'Введите дату расторжения договора.'
SKIP
SKIP(1)
WITH FRAME fr_main WIDTH 40 CENTERED ROW 10 OVERLAY SIDE-LABELS
COLOR MESSAGES TITLE "[Депозитный калькулятор]".

{tmprecid.def}
FOR EACH tmprecid NO-LOCK, FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK.
  ASSIGN
  dateBeg = loan.open-date
  dateEnd = loan.close-date
  dateRst = loan.close-date
  .
  LEAVE.
END.

IF dateEnd = ? THEN dateEnd = TODAY.
IF dateRst = ? THEN dateRst = TODAY.

UPDATE psLst dateBeg dateEnd dateRst WITH FRAME fr_main.

/*----------------------------------------------------------------------------------------------------*/

DEF VAR date_b    AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR date_e    AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR n_dog     AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cust_fio  AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR ostatok   AS DEC                 NO-UNDO.
DEF VAR rasch     AS DEC                 NO-UNDO.
DEF VAR rasch_tl  AS DEC                 NO-UNDO.
DEF VAR days      AS INT                 NO-UNDO.
DEF VAR days_tl   AS INT                 NO-UNDO.
DEF VAR prc_st    AS DEC                 NO-UNDO.
DEF VAR ps_norm   AS DEC                 NO-UNDO.
DEF VAR ps_down   AS DEC                 NO-UNDO.
DEF VAR ps_norm_  AS CHAR FORMAT "x(12)" NO-UNDO.
DEF VAR ps_down_  AS CHAR FORMAT "x(12)" NO-UNDO.
DEF VAR str       AS CHAR FORMAT "x(90)" NO-UNDO.
DEF VAR date1     AS DATE                NO-UNDO.
DEF VAR date2     AS DATE                NO-UNDO.
DEF VAR m-comm    AS CHAR                NO-UNDO.
DEF VAR s-comm    AS CHAR                NO-UNDO.

DEF TEMP-TABLE ttDateLst NO-UNDO
FIELD op-date LIKE op-entry.op-date
.

{globals.i}
{sh-defs.i}
{dpsproc.def}

FUNCTION DateDiff RETURN INT (INPUT iDateBeg AS DATE,INPUT iDateEnd AS DATE):
  DEF VAR days AS INT NO-UNDO.
  DO WHILE iDateBeg < iDateEnd:
  days = days + 1.
  iDateBeg = ADD-INTERVAL(iDateBeg, 1, "day" ).
  END.
  RETURN days.
END.

FOR EACH tmprecid NO-LOCK, FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK,
                           FIRST person WHERE person.person-id = loan.cust-id NO-LOCK,
                           FIRST loan-cond WHERE loan-cond.cont-code = loan.cont-code NO-LOCK,
                           FIRST loan-acct WHERE loan-acct.cont-code = loan.cont-code
                                             AND loan-acct.contract = "dps"
                                             AND CAN-DO("loan-dps-t,loan-dps-p",loan-acct.acct-type) NO-LOCK.
  
  RUN Get_Last_Commi     in h_dpspc(RECID(loan),loan-cond.since + 1,loan-cond.since + 1,OUTPUT m-comm).
  RUN Get_Last_Pen-Commi in h_dpspc(RECID(loan),loan-cond.since + 1,loan-cond.since + 1,OUTPUT s-comm).
  
  FOR EACH comm-rate WHERE comm-rate.commission = s-comm NO-LOCK BY comm-rate.period DESC BY comm-rate.since DESC.
    IF DateDiff(dateBeg,dateRst) >= comm-rate.period AND loan-cond.since >= comm-rate.since THEN DO:
       ps_down = comm-rate.rate-comm.
       LEAVE.
    END.
  END.
  
  RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, loan-cond.since, loan-cond.since, ?).
  
  FOR EACH comm-rate WHERE comm-rate.commission = m-comm 
                       AND ABS(sh-bal) >= comm-rate.min-value NO-LOCK BY comm-rate.since DESC 
                                                                      BY comm-rate.min-value DESC.
    IF loan-cond.since >= comm-rate.since THEN DO:
       ps_norm = comm-rate.rate-comm.
       LEAVE.
    END.
  END.
  
  CASE INT64(psLst:SCREEN-VALUE):
    WHEN 1 THEN prc_st = ps_norm.
    WHEN 2 THEN prc_st = ps_down.
  END CASE.
  
  ASSIGN
  ps_norm_ = STRING(ps_norm,"z9.99")
  ps_down_ = STRING(ps_down,"z9.99")
  date_b = STRING(dateBeg,"99.99.9999")
  date_e = STRING(dateEnd,"99.99.9999")
  n_dog = REPLACE(loan.cont-code,"@0000","")
  cust_fio = name-last + " " + first-names
  .
  
  {setdest.i}
  PUT "                                                                " SKIP.
  PUT "                     ДЕПОЗИТНЫЙ КАЛЬКУЛЯТОР                     " SKIP.
  PUT "                                                                " SKIP.
  PUT "  Номер договора: " n_dog            "                          " SKIP.
  PUT "  Основная % ставка: " ps_norm_ "                               " SKIP.
  PUT "  Штрафная % ставка: " ps_down_ "                               " SKIP.
  PUT "  ФИО клиента: " cust_fio                                         SKIP.
  PUT "                                                                " SKIP.
  PUT "  За период c " date_b " по " date_e "                          " SKIP.
  PUT " ┌────────────┬───────────────┬───────┬───────────────┬───────┐ " SKIP.
  PUT " │    Дата    │    Остаток    │ Дней  │  Рассчитано   │   %   │ " SKIP.
  PUT " ├────────────┼───────────────┼───────┼───────────────┼───────┤ " SKIP.
  
  FOR EACH op-entry WHERE op-entry.op-date >= dateBeg 
                      AND op-entry.op-date <= dateEnd
                      AND op-entry.acct-cr = loan-acct.acct 
                      AND op-entry.op-status BEGINS "√" NO-LOCK.
  
    IF NOT CAN-DO("70603*,70608*",op-entry.acct-db) AND 
       NOT CAN-DO("70603*,70608*",op-entry.acct-cr) THEN DO:
       CREATE ttDateLst.
       ttDateLst.op-date = op-entry.op-date.
    END.
  END.
  
  CREATE ttDateLst.
  ttDateLst.op-date = dateEnd.
  
  FIND FIRST ttDateLst NO-LOCK NO-ERROR.
  IF AVAIL ttDateLst THEN DELETE ttDateLst.
  
  FOR EACH op-entry WHERE op-entry.op-date >= dateBeg 
                      AND op-entry.op-date <= dateEnd
                      AND op-entry.acct-cr = loan-acct.acct 
                      AND op-entry.op-status BEGINS "√" NO-LOCK.
  
    IF NOT CAN-DO("70603*,70608*",op-entry.acct-db) 
       AND NOT CAN-DO("70603*,70608*",op-entry.acct-cr) 
       AND op-entry.op-date <> loan.close-date THEN DO:
       FIND FIRST ttDateLst NO-LOCK NO-ERROR.
       IF AVAIL ttDateLst THEN DO:
         date2 = ttDateLst.op-date. 
         DELETE ttDateLst.
       END.
  
       ASSIGN
       ostatok = ostatok + op-entry.amt-rub
       date1 = op-entry.op-date
       days = DateDiff(date1,date2)
       days_tl = days_tl + days
       rasch = ostatok * days * 0.01 * prc_st / DateDiff(DATE(01,01,YEAR(op-entry.op-date)),DATE(01,01,YEAR(op-entry.op-date) + 1)) 
       rasch_tl = rasch_tl + rasch
       .
  
       str = " │ " + STRING(op-entry.op-date,"99.99.9999") +
             " │ " + STRING(ostatok,"zzzzzzzzz9.99")       +
             " │ " + STRING(days,"zzzz9")                  +
             " │ " + STRING(rasch,"zzzzzzzzz9.99")         +
             " │ " + STRING(prc_st,"z9.99")                +
             " │ "
             .
       PUT str SKIP.
    END.
  END.
  
  PUT " ├────────────┴───────────────┼───────┼───────────────┼───────┤ " SKIP.
  str = " │ ИТОГО:" + STRING("","x(20)")         +
        " │ " + STRING(days_tl,"zzzz9")          +
        " │ " + STRING(rasch_tl,"zzzzzzzzz9.99") +
        " │ " + STRING("","x(5)")                +
        " │ "
        .
  PUT str SKIP.
  PUT " └────────────────────────────┴───────┴───────────────┴───────┘ " SKIP.
  PUT "                                                                " SKIP.
  
  {preview.i}
  LEAVE.
END.



/*
""
┌────────┬────────┐
├────────┼────────┤
│        │        │
└────────┴────────┘
*/