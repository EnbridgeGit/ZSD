REPORT zlsdr003 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 132.

************************************************************************
* Program Description
* This abap reads a Banner GL input file, map each record to a condition
* type record using mapping table ZGLCOND and splits them into component
* records. Component record data will be read from condition tables A032
* & KONP.  The new component records will be reported and the variances
* between the GL amount and the sum of the calculated component amounts
* will be reported.
*----------------------------------------------------------------------
* Changes:
* 2013/02/25 gymana SDP41704 - replace all references to KONH with A032
*
************************************************************************

************************************************************************
*    TABLES                                                            *
************************************************************************
TABLES: zglcond,          "GL / Condition Type Mapping Table
*        konh,             "Conditions Header Table           "SDP41704
        A032,             "Price Group/Material Table         "SDP41704
        konp.             "conditions Item Table

************************************************************************
*    SELECT OPTIONS                                                    *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
PARAMETER: infile LIKE filenameci-fileextern DEFAULT
                '/usr/sap/interfaces/P01/BANNER/zbis100rrorder.dat'.
SELECTION-SCREEN END OF BLOCK box.

************************************************************************
*    VARIABLES                                                         *
************************************************************************

* Used to parse file into various fields

DATA:  BEGIN OF bann_file OCCURS 0,
       process_date       TYPE d,
       serv_type(4)       TYPE c,
       serv_cat(4)        TYPE c,
       gl_classcd(4)      TYPE c,
       serv_class(2)      TYPE c,
       town_cd(2)         TYPE c,
       munic_cd(4)        TYPE c,
       budget_ind(1)      TYPE c,
       cust_num(16)       TYPE n,
       trans_amt_sign(1)  TYPE c,
       trans_amt(13)      TYPE c,
       cust_chrg_sign(1)  TYPE c,
       cust_chrg(11)      TYPE c,
       cons_sign(1)       TYPE c,
       cons(13)           TYPE c,
       no_of_custs(6)     TYPE n,
       eff_date           TYPE d,
       rate_class(4)      TYPE c,
       END OF bann_file.

DATA:  BEGIN OF intab OCCURS 0,
       gl_classcd(4)      TYPE c,
       rate_class(2)      TYPE c,
       amount             TYPE p DECIMALS 2,
       cons               TYPE p DECIMALS 3,
       eff_date           TYPE d,
       END OF intab.

DATA: itab_glcond LIKE zglcond OCCURS 0 WITH HEADER LINE.

DATA:  BEGIN OF itab_condtype OCCURS 0,
         knumh    LIKE A032-knumh,
         kschl    LIKE A032-kschl,
         VTWEG    LIKE a032-VTWEG,
         kbetr    LIKE konp-kbetr,
         kpein    LIKE konp-kpein,
         kmein    LIKE konp-kmein,
         datab    LIKE A032-datab,
         datbi    LIKE A032-datbi,
         vtext    LIKE zglcond-vtext,
         cons(13) TYPE p DECIMALS 3,
         amt(13)  TYPE p DECIMALS 2,
       END OF itab_condtype.

DATA:  BEGIN OF wa_A032 OCCURS 0,                             "SDP41704
         knumh    LIKE A032-knumh,                            "SDP41704
         kschl    LIKE A032-kschl,                            "SDP41704
         VTWEG    like a032-VTWEG,
         datab    LIKE A032-datab,                            "SDP41704
         datbi    LIKE A032-datbi,                            "SDP41704
       END OF wa_A032.                                        "SDP41704

DATA:  BEGIN OF err_file OCCURS 0,
       gl_classcd(4)      TYPE c,
       rate_class(2)      TYPE c,
       amount             TYPE p DECIMALS 2,
       eff_date           TYPE d,
       err_msg(80)        TYPE c,
       END OF err_file.

DATA:  BEGIN OF split_table OCCURS 0,
         kschl        LIKE A032-kschl,
         VTWEG        like a032-VTWEG,
         vtext        LIKE zglcond-vtext,
         kbetr        LIKE konp-kbetr,
         datab        LIKE A032-datab,
         cons         TYPE p DECIMALS 3,
         amt          TYPE p DECIMALS 2,
         tot_type_amt TYPE p DECIMALS 2,
       END OF split_table.

DATA:  splttab LIKE split_table OCCURS 0 WITH HEADER LINE.

DATA:  inrec(251),
       ln_cntr                    TYPE i VALUE 0,
       w_rate                     LIKE zrrate-rate,
       w_effdate                  LIKE bann_file-eff_date,
       w_cons(13)                 TYPE p DECIMALS 3,
       w_trans_amt(13)            TYPE p DECIMALS 2,
       sum_trans_amt(13)          TYPE p DECIMALS 2,
       w_grand_tot_trans_amt(15)  TYPE p DECIMALS 2,
       w_file_tot_trans_amt(15)   TYPE p DECIMALS 2,
       w_variance(13)             TYPE p DECIMALS 2,
       va_amt                     TYPE p DECIMALS 2,
       split_sum_amt(13)          TYPE p DECIMALS 2,
       sum_amt_by_split_cd(13)    TYPE p DECIMALS 2,
       prev_kschl                 LIKE splttab-kschl,
       w_ctype_not_found_flag(1)  TYPE c,
       w_errfile_created_flag(1)  TYPE c,
       msg_text(50).

************************************************************************
*    START OF SELECTION (MAINLINE)                                     *
************************************************************************
START-OF-SELECTION.

  PERFORM open_files.                      "Open files
  PERFORM split_file.
  PERFORM generate_split_sum_rpt.
  PERFORM generate_err_rpt.

END-OF-SELECTION.
************************************************************************
* Routine to open the physical files for input & output in text mode.
*************************  OPEN_FILES **********************************
FORM open_files.
  OPEN DATASET infile FOR INPUT IN TEXT MODE
               MESSAGE msg_text.

  IF sy-subrc NE 0.
    WRITE: 'File cannot be opened. Reason: ', msg_text.
  ENDIF.
* Read Payfile and perform payroll check.
  DO.
    READ DATASET infile INTO bann_file.
    IF sy-subrc <> '0'.
      EXIT.
    ENDIF.
    APPEND bann_file.
  ENDDO.

ENDFORM.                    "OPEN_FILES


************************************************************************
*  This routine reads the Banner input file one record at a time and
*  maps the GL class & service class to the correct RR component codes.
************************************************************************
FORM split_file.
  ln_cntr = '0'.
  LOOP AT bann_file.
    MOVE bann_file-trans_amt TO w_trans_amt.
    IF w_trans_amt = 0.

      MOVE-CORRESPONDING bann_file TO intab.
      MOVE bann_file-cons TO w_cons.
      IF bann_file-cons_sign = '-'.
        w_cons = w_cons * -1.
      ENDIF.
      MOVE bann_file-trans_amt TO w_trans_amt.
      IF bann_file-trans_amt_sign = '-'.
        w_trans_amt = w_trans_amt * -1.
      ENDIF.
      MOVE w_cons TO intab-cons.
      MOVE w_trans_amt TO intab-amount.
      APPEND intab.

    ELSE.
      IF ln_cntr = 0.
        PERFORM write_reconc_hdg.
      ENDIF.

      PERFORM map_cond_type.

      IF ln_cntr >= 55.
        NEW-PAGE.
        PERFORM write_reconc_hdg.
      ENDIF.
    ENDIF.
  ENDLOOP.

  FORMAT INTENSIFIED ON.
  SKIP 2.
  WRITE /46 'Input File Total:'.
  WRITE (14) w_file_tot_trans_amt UNDER text-011.
  WRITE /46 'Component Grand Total:'.
  WRITE (14) w_grand_tot_trans_amt UNDER text-011.
  ln_cntr = ln_cntr + 4.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "SPLIT_FILE

************************************************************************
* Using the GL Class code, find the corresponding condition types
************************************************************************
FORM  map_cond_type.

data: ls_ZLSDBN002 type ZLSDBN002.

  MOVE 'N' TO w_ctype_not_found_flag.
  MOVE bann_file-cons TO w_cons.
  IF bann_file-cons_sign = '-'.
    w_cons = w_cons * -1.
  ENDIF.
  MOVE bann_file-trans_amt TO w_trans_amt.
  IF bann_file-trans_amt_sign = '-'.
    w_trans_amt = w_trans_amt * -1.
  ENDIF.

* Select all Condition Type component records that match the Banner GL
* class code and the rate class code.
  REFRESH itab_glcond.
  CLEAR itab_glcond.

  SELECT * INTO TABLE itab_glcond
    FROM zglcond
   WHERE glcode = bann_file-gl_classcd AND
         ratecl = bann_file-rate_class.

  IF sy-subrc <> 0.
    MOVE-CORRESPONDING bann_file TO err_file.
    MOVE w_trans_amt TO err_file-amount.
    MOVE 'No matching condition type codes found in table ZGLCOND.'
         TO err_file-err_msg.
    APPEND err_file.
    MOVE 'Y' TO w_ctype_not_found_flag.
    MOVE 'Y' TO w_errfile_created_flag.
    "------------
    LOOP AT itab_glcond.
         itab_glcond-VTWEG = 'Z0'.
         modify itab_glcond.
    ENDLOOP.
  else.
    "record found
    CLEAR ls_ZLSDBN002.
    SELECT SINGLE * from ZLSDBN002 INTO ls_ZLSDBN002
      WHERE TOWNCODE = bann_file-town_cd
        AND MUNICODE = bann_file-munic_cd.
    LOOP AT itab_glcond.
         itab_glcond-VTWEG = ls_zlsdbn002-VTWEG.
         modify itab_glcond.
    ENDLOOP.
   "----------------
  ENDIF.

  IF w_ctype_not_found_flag <> 'Y'.
    PERFORM process-split-records.
    IF w_ctype_not_found_flag <> 'Y'.
      PERFORM print-split-detail.
    ENDIF.
  ENDIF.

ENDFORM.                    "MAP_RATERIDER

************************************************************************
* Determine the transaction amt for each split.
************************************************************************
FORM process-split-records.

  REFRESH itab_condtype.
  CLEAR   itab_condtype.

  sum_trans_amt = 0.

  LOOP AT itab_glcond.
*    SELECT SINGLE konh~knumh konh~kschl konp~kbetr           "SDP41704
*           konp~kpein konp~kmein konh~datab konh~datbi       "SDP41704
*      INTO itab_condtype                                     "SDP41704
*      FROM ( konh INNER JOIN konp                            "SDP41704
*             ON konh~knumh = konp~knumh )                    "SDP41704
*     WHERE konh~kschl = itab_glcond-kschl                    "SDP41704
*       AND konh~datab <= bann_file-eff_date                  "SDP41704
*       AND konh~datbi >= bann_file-eff_date                  "SDP41704
*       AND loevm_ko <> 'X'.                                  "SDP41704
                                                              "SDP41704
    CLEAR wa_A032.                                            "SDP41704
                                                              "SDP41704
    SELECT SINGLE knumh kschl VTWEG datab datbi               "SDP41704
      INTO wa_A032                                            "SDP41704
      FROM A032                                               "SDP41704
     WHERE kschl = itab_glcond-kschl                          "SDP41704
       AND VTWEG = itab_glcond-VTWEG
       AND datab <= bann_file-eff_date                        "SDP41704
       AND datbi >= bann_file-eff_date.                       "SDP41704

    IF sy-subrc = 0.                                          "SDP41704
       SELECT * FROM konp                                     "SDP41704
        WHERE knumh = wa_A032-knumh                           "SDP41704
          AND kschl = itab_glcond-kschl                       "SDP41704
          AND loevm_ko <> 'X'.                                "SDP41704

        IF sy-subrc = 0.                                      "SDP41704
           MOVE wa_A032-knumh to itab_condtype-knumh.         "SDP41704
           MOVE wa_A032-kschl to itab_condtype-kschl.         "SDP41704
           itab_condtype-VTWEG = wa_a032-VTWEG.
           MOVE wa_A032-datab to itab_condtype-datab.         "SDP41704
           MOVE wa_A032-datbi to itab_condtype-datbi.         "SDP41704
           MOVE konp-kbetr to itab_condtype-kbetr.            "SDP41704
           MOVE konp-kpein to itab_condtype-kpein.            "SDP41704
           MOVE konp-kmein to itab_condtype-kmein.            "SDP41704
           itab_condtype-vtext = itab_glcond-vtext.
           itab_condtype-cons = w_cons.
           itab_condtype-amt = ( w_cons / itab_condtype-kpein ) *
                               itab_condtype-kbetr.
           APPEND itab_condtype.
           PERFORM load_condtypes.
           CLEAR itab_condtype.
        ELSE.
           MOVE-CORRESPONDING bann_file TO err_file.
           MOVE w_trans_amt TO err_file-amount.
           MOVE 'No cond. type detail records found in table A032/KONP.'
            TO err_file-err_msg.
           APPEND err_file.
           MOVE 'Y' TO w_errfile_created_flag.
           MOVE 'Y' TO w_ctype_not_found_flag.
        ENDIF.                                                "SDP41704
       ENDSELECT.                                             "SDP41704
    ELSE.                                                     "SDP41704
       MOVE-CORRESPONDING bann_file TO err_file.
       MOVE w_trans_amt TO err_file-amount.
       MOVE 'No cond. type detail records found in table A032/KONP.'
            TO err_file-err_msg.
       APPEND err_file.
       MOVE 'Y' TO w_errfile_created_flag.
       MOVE 'Y' TO w_ctype_not_found_flag.
    ENDIF.                                                    "SDP41704
  ENDLOOP.

ENDFORM.                    "PROCESS-SPLIT-RECORDS

************************************************************************
*  This routine creates a summary table by Condition Type code and
*  effective date.  Amounts are totaled for each cond. type / effective
*  date and those amount totals are summed up by condition type code.
************************************************************************
FORM load_condtypes.

  READ TABLE splttab WITH KEY kschl = itab_condtype-kschl
                              VTWEG = itab_condtype-VTWEG
                              datab = itab_condtype-datab.

* If there is a match in table splttab, then add itab_condtype
* amounts to the splttab record (sum up)
* If no match, then a new record is added to splttab.

  IF sy-subrc = 0.
* sum up dollar amounts.
    MOVE splttab-amt TO va_amt.
    ADD itab_condtype-amt TO va_amt.
    MOVE va_amt TO splttab-amt.
*      shift splttab-wrbtr right deleting trailing space.
* sum up non-zero consumption.
*      if isort-MENGE <> '/'.
*        if splttab-menge <> '/'.
*           move splttab-menge to va_menge.
*        else.
*           move 0 to va_menge.
*        endif.
*        add isort-menge to va_menge.
*        move va_menge to splttab-menge.
*        shift splttab-menge right deleting trailing space.
*        move isort-meins to splttab-meins.
*      endif.
    MODIFY splttab INDEX sy-tabix TRANSPORTING amt.
    CLEAR splttab.
  ELSE.
*  no match in splttab. Add itab_condtype to splttab.
    IF itab_condtype-amt = '0'.
    ELSE.
      MOVE-CORRESPONDING itab_condtype TO splttab.
      APPEND splttab. CLEAR splttab.
    ENDIF.
  ENDIF.
ENDFORM.                    "load_condtypes

************************************************************************
* Print each input record and it's corresponding condition splits on
* the detail report.
************************************************************************
FORM print-split-detail.

* Print original Banner record.

  FORMAT INTENSIFIED ON.
  WRITE: /     bann_file-gl_classcd UNDER text-004,
               bann_file-rate_class UNDER text-005,
          (13) w_cons UNDER text-006 DECIMALS 3,
          (10) bann_file-eff_date UNDER text-010 USING
                                 EDIT MASK '____/__/__',
          (13) w_trans_amt UNDER text-011 DECIMALS 2.
  SKIP.
  ln_cntr = ln_cntr + 2.
  ADD w_trans_amt TO w_file_tot_trans_amt.
  FORMAT INTENSIFIED OFF.

* Print component splits

  LOOP AT itab_condtype.
    WRITE: /     itab_condtype-kschl  UNDER text-004,
                 itab_condtype-VTWEG  UNDER text-024,
                 itab_condtype-vtext  UNDER text-005,
            (13) itab_condtype-cons   UNDER text-006,
                 itab_condtype-kbetr  UNDER text-007,
            (10) itab_condtype-datab  UNDER text-010 USING
                                      EDIT MASK '____/__/__',
            (13) itab_condtype-amt    UNDER text-011.
    sum_trans_amt = sum_trans_amt + itab_condtype-amt.
    ln_cntr = ln_cntr + 1.
  ENDLOOP.

  SKIP.
  w_variance = w_trans_amt - sum_trans_amt.
  WRITE: /46   'Component Total Amt',
          (13) sum_trans_amt UNDER text-011,
          111  'Variance: ',
          (10)  w_variance.

  ADD sum_trans_amt TO w_grand_tot_trans_amt.
  SKIP.
  ln_cntr = ln_cntr + 3.

ENDFORM.                    "PRINT-AND-WRITE-SPLIT-DETAIL

************************************************************************
* Generate Split Report header
************************************************************************
FORM write_reconc_hdg.

  CLEAR ln_cntr.
  FORMAT INTENSIFIED ON.
  WRITE: /1 text-001, 36 text-002.
  WRITE: 106 text-dte, sy-datum, text-amp, sy-uzeit.

  WRITE: /44 'File Process Date:'.
  WRITE (10) bann_file-process_date USING EDIT MASK '____/__/__'.
  WRITE: 121 text-pge, sy-pagno.
  SKIP.
  WRITE: /3 text-003, 79 text-009.
*  WRITE: /3 text-004, 13 text-024, 13 text-005, 35 text-006, 53 text-007,
*  79 text-010, 95 text-011.
  WRITE: /3 text-004, 13 text-024, 18 text-005, 39 text-006, 57 text-007,
            83 text-010, 99 text-011.
  SKIP.
  MOVE '6' TO ln_cntr.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "WRITE_RECONC_HDG

************************************************************************
* Generate Summary by Split Code Report
************************************************************************
FORM generate_split_sum_rpt.

  PERFORM write_sum_hdg.
* Loop through SPLTTAB and generate a summary line for each split code &
* rate with non-zero amounts.

  SORT splttab.
  LOOP AT splttab.
    IF ln_cntr >= 59.
      PERFORM write_sum_hdg.
    ENDIF.
* Print summed amount by split code for the previous code.
    IF NOT splttab-kschl = prev_kschl AND
       NOT prev_kschl IS INITIAL AND
       NOT sum_amt_by_split_cd IS INITIAL.
      WRITE (15) sum_amt_by_split_cd UNDER text-021.
      SKIP.
      MOVE 0 TO sum_amt_by_split_cd.
      MOVE splttab-kschl TO prev_kschl.
    ENDIF.

    IF splttab-amt <> 0.
      WRITE /(4)  splttab-kschl UNDER text-018.
      WRITE  (20) splttab-vtext UNDER text-005.
      WRITE  (15) splttab-kbetr UNDER text-007.
      WRITE  (10) splttab-datab UNDER text-009.
      WRITE  (15)  splttab-amt  UNDER text-019.
      ln_cntr = ln_cntr + 1.
      ADD splttab-amt TO split_sum_amt.
      ADD splttab-amt TO sum_amt_by_split_cd.
      MOVE splttab-kschl TO prev_kschl.
    ENDIF.
  ENDLOOP.

  IF NOT sum_amt_by_split_cd IS INITIAL.
    WRITE (15) sum_amt_by_split_cd UNDER text-021.
  ENDIF.
  SKIP 2.

  WRITE /60 'SPLIT SUMMARY GRAND TOTAL'.
  WRITE (15) split_sum_amt UNDER text-021.
  ln_cntr = ln_cntr + 2.

  PERFORM generate_zeroamt_report.

ENDFORM.                    "GENERATE_SPLIT_SUM_RPT

************************************************************************
* Generate Summary by Split Code Report Header
************************************************************************

FORM write_sum_hdg.
  NEW-PAGE.
  CLEAR ln_cntr.
  FORMAT INTENSIFIED ON.
  WRITE: /1 text-001, 41 text-016.
  WRITE: 106 text-dte, sy-datum, text-amp, sy-uzeit.
  WRITE: /50 sy-datum.
  WRITE: 121 text-pge, sy-pagno.
  SKIP.
  WRITE: /3 text-017, 57 text-009, 100 text-020.
  WRITE: /3 text-018, 15 text-005, 30 text-007, 57 text-010,
         77 text-019, 100 text-021 .
  SKIP.
  MOVE '6' TO ln_cntr.
  FORMAT INTENSIFIED OFF.
ENDFORM.                    "WRITE_SUM_HDG

************************************************************************
* Generate Banner Zero Amt Record Listing
************************************************************************
FORM generate_zeroamt_report.

  PERFORM write_zeroamt_hdg.

  LOOP AT intab.
    IF ln_cntr >= 59.
      PERFORM write_zeroamt_hdg.
    ENDIF.
    WRITE: /    intab-gl_classcd  UNDER text-004,
                intab-rate_class  UNDER text-014,
           (13) intab-amount      UNDER text-011,
           (14) intab-cons        UNDER text-023,
           (10) intab-eff_date    UNDER text-010 USING
                                     EDIT MASK '____/__/__'.
    ln_cntr = ln_cntr + 1.
  ENDLOOP.

ENDFORM.                    "generate_zeroamt_report

************************************************************************
* Generate Banner Zero Amt Record Listing Header
************************************************************************

FORM write_zeroamt_hdg.
  NEW-PAGE.
  CLEAR ln_cntr.
  FORMAT INTENSIFIED ON.
  WRITE: /1 text-001, 41 text-022.
  WRITE: 106 text-dte, sy-datum, text-amp, sy-uzeit.
  WRITE: /50 sy-datum.
  WRITE: 121 text-pge, sy-pagno.
  SKIP.
  WRITE: /3 text-003, 11 text-013, 58 text-009.
  WRITE: /3 text-004, 11 text-014, 20 text-011, 37 text-023,
          58 text-010.
  SKIP.
  MOVE '6' TO ln_cntr.
  FORMAT INTENSIFIED OFF.
ENDFORM.                    "write_zeroamt_hdg

************************************************************************
* Generate Error Report
************************************************************************
FORM generate_err_rpt.

  PERFORM write_err_hdg.
  IF w_errfile_created_flag = 'Y'.
    LOOP AT err_file.
      IF ln_cntr >= 59.
        PERFORM write_err_hdg.
      ENDIF.
      WRITE: /    err_file-gl_classcd  UNDER text-004,
                  err_file-rate_class  UNDER text-014,
             (13) err_file-amount      UNDER text-011,
             (10) err_file-eff_date    UNDER text-010 USING
                                       EDIT MASK '____/__/__',
                   err_file-err_msg    UNDER text-015.
      ln_cntr = ln_cntr + 1.
    ENDLOOP.
  ELSE.
    WRITE 43 '*** No errors to report ***'.
  ENDIF.
ENDFORM.                    "GENERATE_ERR_RPT

************************************************************************
* Generate Error Report Header
************************************************************************

FORM write_err_hdg.
  NEW-PAGE.
  CLEAR ln_cntr.
  FORMAT INTENSIFIED ON.
  WRITE: /1 text-001, 41 text-012.
  WRITE: 106 text-dte, sy-datum, text-amp, sy-uzeit.
  WRITE: /50 sy-datum.
  WRITE: 121 text-pge, sy-pagno.
  SKIP.
  WRITE: /3 text-003, 11 text-013, 37 text-009.
  WRITE: /3 text-004, 11 text-014, 20 text-011, 37 text-010,
         52 text-015.
  SKIP.
  MOVE '6' TO ln_cntr.
  FORMAT INTENSIFIED OFF.
ENDFORM.                    "WRITE_ERR_HDG
