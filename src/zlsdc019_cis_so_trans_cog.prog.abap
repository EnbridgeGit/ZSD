*&--------------------------------------------------------------------*
*& Report  ZLSDC019_CIS_SO_TRANS_COG
*&
*&--------------------------------------------------------------------*
*&*********************************************************************
*  Author:      Durgaprakash                                          *
*  Date:        May 2021.                                             *
*  Project:     Cost Of Gas Project                                   *
*  Issue Log:   CHG0216959                                            *
*  Description:                                                       *
*       Copy of ZLSDC017_CIS_SO_TRANSLATE with EGD changes            *
*     - The purpose of this program is to transforms the CTDS file    *
*       and create output files (SS, RR). ZLSDI001 uses these output  *
*       to generate sales order IDocs                                 *
*                                                                     *
*&--------------------------------------------------------------------*
*CHANGES****                                                          *
* CHG0240636
* 7th Feb 2022  birudurd To skip zero value records after aggregation *
*&--------------------------------------------------------------------*
REPORT  zlsdc019_cis_so_trans_cog.
TABLES: vbak,
        vbap,
        filenameci,
        t188.

TYPES: BEGIN OF ty_indata,
       post_date(20),   "Posting Date - yyyymmdd
       bill_date(20),   "Billed Date - yyyymmdd
       srate_type(20),  "Statistical Rate Type
       prctr(20),      "Profit Center
       division(20),    "Division
       acct_class(20),   "Account Class - Data element KT0KL_077T
       rate_cate(20),   "Rate Category
       rev_class(20),    "Revenue Class
       line_type(20),    "Line item type
       doc_type(20),     "SO order type
       gl_acct(20),     "GL Account number
       posting_key(20),  "posting key
       recon_key(20),   "Reconciliation key  - DFOKKOPK-FIKEY
       quant(20),      "Quantity
       amount(20),      "Amount
      END OF ty_indata.

TYPES: BEGIN OF ty_outdata,
        audat(8), "Document Date
        fkdat(8), "Bill Date
        curcy(3), "currency
        bsart(4), "Doc type
        autlf(1), "Complete delivery
        vkorg(4), "Sales org
        vtweg(2), "Distribution channel
        spart(2), "Division
        parvw(3), "Partner function
        partn(17), "Partner
        bstkd(35), "PO number
        dwerk(4),  "Plant
        vkbur(4), "Sales office
        augru(3),  "Order Reason
        konda(2),  "Price Group
        kdgrp(2),  "Customer Group
        prsdt(8),  "Pricing date
        kvgr1(3),  "Customer Group1
        bzirk(4),  "Sales District
        matnr(18),  "Material
        kwmeng(15), "Qty
*        kwmeng LIKE BSEG-MENGE,
        vrkme(3),   "Sales unit
        kschl(4),   "condition type
        kbetr(11),  "Amount
*        kbetr LIKE BSEG-WRBTR,
        kdkg1(2),   "condition group1
        kvgr2(3),   "Customer Group 2
        zuonr(18),  "Assignment (Reconkey)
       END OF   ty_outdata.
TYPES: BEGIN OF ty_outdata1,
        audat(8), "Document Date
        fkdat(8), "Bill Date
        curcy(3), "currency
        bsart(4), "Doc type
        autlf(1), "Complete delivery
        vkorg(4), "Sales org
        vtweg(2), "Distribution channel
        spart(2), "Division
        parvw(3), "Partner function
        partn(17), "Partner
        bstkd(35), "PO number
        dwerk(4),  "Plant
        vkbur(4), "Sales office
        augru(3),  "Order Reason
        konda(2),  "Price Group
        kdgrp(2),  "Customer Group
        prsdt(8),  "Pricing date
        kvgr1(3),  "Customer Group1
        bzirk(4),  "Sales District
        matnr(18),  "Material
        vrkme(3),   "Sales unit
        kschl(4),   "condition type
        kdkg1(2),   "condition group1
        kvgr2(3),   "Customer Group 2
        zuonr(18),  "Assignment (Reconkey)
        kwmeng LIKE bseg-menge, "Qty
        kbetr LIKE bseg-wrbtr,  "Amount
       END OF   ty_outdata1.
DATA: gv_error(1),
      gv_date TYPE datum,
      gv_stime TYPE sy-uzeit,
      gv_etime TYPE sy-uzeit,
      gv_trecords TYPE i,
      gv_osrecords TYPE i,
      gv_rrrecords TYPE i,
      gv_zeroamt_rec TYPE i,
      gv_aggzeroamt_rec TYPE i, "CHG0240636
      gv_exrecords TYPE i,
      gs_indata TYPE ty_indata,
      gs_outdata TYPE ty_outdata,
      gt_indata TYPE TABLE OF ty_indata,
      gt_so_outdata TYPE TABLE OF ty_outdata,
      gt_so_outdata1 TYPE TABLE OF ty_outdata1,
      gt_so_outdata2 TYPE TABLE OF ty_outdata1,
      gs_outdata1 TYPE ty_outdata1,
      gt_rr_outdata TYPE TABLE OF ty_outdata.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETER: p_infile TYPE filenameci-fileextern OBLIGATORY.             "Input File
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
PARAMETER: p_osfile TYPE filenameci-fileextern OBLIGATORY,             "output File
           p_orfile TYPE filenameci-fileextern OBLIGATORY.             "output File
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-001.
PARAMETER: p_vkorg TYPE vbak-vkorg DEFAULT 'Z001' OBLIGATORY,        "Sales Org
            "p_vtweg TYPE vbak-vtweg DEFAULT 'Z0'   OBLIGATORY,        "Dist Chanel
            p_spart TYPE vbak-spart DEFAULT 'Z0'   OBLIGATORY,        "Division
            p_kunnr TYPE vbak-kunnr DEFAULT 'BANNER' OBLIGATORY,     "Customer #
            p_waerk TYPE vbak-waerk  DEFAULT 'CAD' OBLIGATORY,        "SD Doc. Curr
            p_parvw(2) TYPE c       DEFAULT 'AG'  OBLIGATORY,        "SoldTo Party
            p_augru TYPE vbak-augru DEFAULT 'ZAC' OBLIGATORY,         "Order Reason
            p_vrkme TYPE vbap-vrkme DEFAULT 'M3' OBLIGATORY,          "UoM
            p_autlf TYPE vbak-autlf DEFAULT 'X'.                     "Complete Delivery

SELECT-OPTIONS : s_konda FOR t188-konda NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid(3)
              '/COG/zcisbilling100.dat' INTO p_infile.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid(3)
              '/COG/zcisbilling100-so<sorg>.dat' INTO p_osfile.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid(3)
              '/COG/zcisbilling100-rr<sorg>.dat' INTO p_orfile.
  CONDENSE p_infile NO-GAPS.
  CONDENSE p_osfile NO-GAPS.
  CONDENSE p_orfile NO-GAPS.

START-OF-SELECTION.
  gv_date = sy-datum.
  gv_stime = sy-uzeit.

  CLEAR:  gv_etime,
          gv_trecords,
          gv_osrecords,
          gv_rrrecords,
          gv_error.
  REFRESH:gt_indata,
          gt_rr_outdata,
          gt_so_outdata,
          gt_so_outdata1.

  PERFORM validate_parameters.
  PERFORM process_infile.
  CHECK gv_error IS INITIAL.
  PERFORM prepare_data.
  PERFORM output_files.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_INFILE
*&---------------------------------------------------------------------*
*       Process input file and collect data for output files
*----------------------------------------------------------------------*
FORM process_infile .
  DATA: lv_text TYPE char1024.

  OPEN DATASET p_infile FOR INPUT IN TEXT MODE ENCODING DEFAULT WITH SMART LINEFEED.
  IF sy-subrc <> 0.
    gv_error = 'X'.
    MESSAGE e006(zm) WITH p_infile.
  ENDIF.
  DO.
    CLEAR: lv_text,
           gs_indata.
    READ DATASET p_infile INTO lv_text.
    IF sy-subrc <> 0.

      EXIT.
    ENDIF.
    PERFORM split_data USING lv_text
                             gs_indata.
    APPEND gs_indata TO gt_indata.
  ENDDO.
  CLOSE DATASET p_infile.
  IF sy-subrc <> 0.
    gv_error = 'X'.
    MESSAGE e100(zm) WITH 'Unable to close input File data. check security.' p_infile ' ' ' '.
  ENDIF.
ENDFORM.                    " PROCESS_INFILE
*&---------------------------------------------------------------------*
*&      Form  SPLIT_DATA
*&---------------------------------------------------------------------*
*       Column Separator
*----------------------------------------------------------------------*
FORM split_data  USING    p_tab TYPE char1024
                          p_output TYPE ty_indata.

  SPLIT p_tab AT '|' "cl_abap_char_utilities=>horizontal_tab
        INTO   p_output-post_date   "Posting Date - yyyymmdd
               p_output-bill_date   "Billed Date - yyyymmdd
               p_output-srate_type  "Statistical Rate Type
               p_output-prctr      "Profit Center
               p_output-division    "Division
               p_output-acct_class   "Account Class - Data element KT0KL_077T
               p_output-rate_cate   "Rate Category
               p_output-rev_class    "Revenue Class
               p_output-line_type    "Line item type
               p_output-doc_type     "SO order type
               p_output-gl_acct     "GL Account number
               p_output-posting_key  "posting key
               p_output-recon_key   "Reconciliation key  - DFOKKOPK-FIKEY
               p_output-quant      "Quantity
               p_output-amount.      "Amount

  REPLACE ALL OCCURRENCES OF '"' IN p_output-post_date WITH space.
  SHIFT p_output-post_date LEFT DELETING LEADING space.
  CONDENSE p_output-post_date NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-bill_date WITH space.
  SHIFT p_output-bill_date LEFT DELETING LEADING space.
  CONDENSE p_output-bill_date NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-srate_type WITH space.
  SHIFT p_output-srate_type LEFT DELETING LEADING space.
  CONDENSE p_output-srate_type NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-prctr WITH space.
  SHIFT p_output-prctr LEFT DELETING LEADING space.
  CONDENSE p_output-prctr NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-division WITH space.
  SHIFT p_output-division LEFT DELETING LEADING space.
  CONDENSE p_output-division NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-acct_class WITH space.
  SHIFT p_output-acct_class LEFT DELETING LEADING space.
  CONDENSE p_output-acct_class NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-rate_cate WITH space.
  SHIFT p_output-rate_cate LEFT DELETING LEADING space.
  CONDENSE p_output-rate_cate NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-rev_class WITH space.
  SHIFT p_output-rev_class LEFT DELETING LEADING space.
  CONDENSE p_output-rev_class NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-line_type  WITH space.
  SHIFT p_output-line_type LEFT DELETING LEADING space.
  CONDENSE p_output-line_type NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-doc_type WITH space.
  SHIFT p_output-doc_type LEFT DELETING LEADING space.
  CONDENSE p_output-doc_type NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-gl_acct WITH space.
  SHIFT p_output-gl_acct LEFT DELETING LEADING space.
  CONDENSE p_output-gl_acct NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-posting_key WITH space.
  SHIFT p_output-posting_key LEFT DELETING LEADING space.
  CONDENSE p_output-posting_key NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-recon_key WITH space.
  SHIFT p_output-recon_key LEFT DELETING LEADING space.
  "CONDENSE p_output-recon_key NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-quant WITH space.
  REPLACE ALL OCCURRENCES OF ',' IN p_output-quant WITH space.
  SHIFT p_output-quant LEFT DELETING LEADING space.
  CONDENSE p_output-quant NO-GAPS.
  REPLACE ALL OCCURRENCES OF '"' IN p_output-amount WITH space.
  REPLACE ALL OCCURRENCES OF ',' IN p_output-amount WITH space.
  SHIFT p_output-amount LEFT DELETING LEADING space.
  CONDENSE p_output-amount NO-GAPS.

ENDFORM.                    " SPLIT_DATA
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA
*&---------------------------------------------------------------------*
*       Prepare data for output
*----------------------------------------------------------------------*
FORM prepare_data .
  DATA:  lv_rcate(10),
         lv_revclass(4),
         lv_acclass TYPE ktokl_077t,
         lv_reconkey TYPE dfkkopk-fikey,
         lv_prctr TYPE prctr,
        lv_hkont TYPE hkont,
        lv_length TYPE i ,
        lv_ratecls TYPE zlsdbn003-ratecls,
        lv_rc1(3),
        lv_rc2(10),
        lv_qty TYPE kwmeng,
        lv_kbetr TYPE kbetr,
        lv_linetype TYPE z_lineitem,
         lv_sratetype TYPE z_statis_rate,
         lv_uom TYPE t006-msehi,
         lv_uom_iso TYPE t006-isocode,
        ls_zlsdcis01 TYPE zlsdcis04, " COG EGD
        ls_zlsdcis02 TYPE zlsdcis02,
        ls_zlsdbn003 TYPE zlsdbn003.

  lv_uom = p_vrkme.

  CALL FUNCTION 'UNIT_OF_MEASURE_SAP_TO_ISO'
    EXPORTING
      sap_code    = lv_uom
    IMPORTING
      iso_code    = lv_uom_iso
    EXCEPTIONS
      not_found   = 1
      no_iso_code = 2
      OTHERS      = 3.
  IF sy-subrc <> 0.
    WRITE : / 'Sales UoM does not have ISO UoM, can not process ', p_vrkme.
    STOP.
  ENDIF.

  LOOP AT gt_indata INTO gs_indata.
    CLEAR: lv_ratecls,
           lv_length,
           lv_rc1,
           lv_rc2,
           gs_outdata,
           gs_outdata1,
           ls_zlsdcis01,
           ls_zlsdcis02,
           ls_zlsdbn003.
    gv_trecords = gv_trecords + 1.
    "if amount is zero then skip that record.
    lv_kbetr = gs_indata-amount.
    IF lv_kbetr = 0.
      gv_zeroamt_rec = gv_zeroamt_rec + 1.
      CONTINUE.
    ENDIF.
    "expecting date format YYYYMMDD. So comment out below routines
    gs_outdata-audat = gs_indata-post_date.
    gs_outdata-fkdat = gs_indata-bill_date.
    lv_prctr = gs_indata-prctr.
    lv_hkont = gs_indata-gl_acct.
    SPLIT gs_indata-rate_cate AT '_' INTO lv_rc1 lv_rc2.
    CONDENSE lv_rc1 NO-GAPS.
    lv_length = strlen( lv_rc1 ).
    IF lv_length = 1.
      CONCATENATE '0' lv_rc1 INTO lv_ratecls.
    ELSE.
      lv_ratecls = lv_rc1.
    ENDIF.
    PERFORM conversion_alpha CHANGING lv_prctr.
    PERFORM conversion_alpha CHANGING lv_hkont.
    lv_linetype = gs_indata-line_type.
    lv_sratetype = gs_indata-srate_type.
* COG EGD
*    SELECT SINGLE * FROM zlsdcis01 INTO ls_zlsdcis01
*      WHERE line_item = lv_linetype
*        AND statis_rate = lv_sratetype.
* COG EGD
    SELECT SINGLE * FROM zlsdcis02 INTO ls_zlsdcis02
      WHERE prctr = lv_prctr.
* COG EGD
*    SELECT SINGLE * FROM zlsdbn003 INTO ls_zlsdbn003
*    WHERE ratecls = lv_ratecls.
    IF ls_zlsdcis02-werks = 'GEGD'.
      SELECT SINGLE * FROM zlsdcis03
        INTO CORRESPONDING FIELDS OF ls_zlsdbn003
        WHERE ratectg = gs_indata-rate_cate.
*          and ratecls = lv_ratecls.

      SELECT SINGLE * FROM zlsdcis04 INTO ls_zlsdcis01
      WHERE statis_rate = lv_sratetype
        AND line_item = lv_linetype
        AND vkorg = p_vkorg.
    ELSE.
      SELECT SINGLE * FROM zlsdbn003 INTO ls_zlsdbn003
      WHERE ratecls = lv_ratecls.

      SELECT SINGLE * FROM zlsdcis04 INTO ls_zlsdcis01
      WHERE line_item = lv_linetype
        AND statis_rate = lv_sratetype.
    ENDIF.
    IF s_konda IS NOT INITIAL AND ( ls_zlsdbn003-konda IN s_konda ).
      gv_exrecords = gv_exrecords + 1.
      CONTINUE.
    ENDIF.
* COG EGD
    gs_outdata-audat = gs_indata-post_date. "Document Date
    gs_outdata-fkdat = gs_indata-bill_date.  "Bill Date
    gs_outdata-curcy = p_waerk. "currency
    gs_outdata-autlf = p_autlf. "Complete delivery
    gs_outdata-vkorg = p_vkorg. "Sales org
    gs_outdata-vtweg = ls_zlsdcis02-vtweg. "p_vtweg. "Distribution channel
    gs_outdata-spart = p_spart. "Division
    gs_outdata-parvw = p_parvw. "Partner function
    gs_outdata-partn = p_kunnr. "Partner
    "Rate category,#/#, Account Class,#/#, revenue class,#/#, Recon Key
    "instead of Rate cateogry use Rate class.
    lv_rcate = gs_indata-rate_cate.
    lv_revclass = gs_indata-rev_class.
    lv_reconkey = gs_indata-recon_key.
    lv_acclass = gs_indata-acct_class.
    CONCATENATE  lv_ratecls '/' lv_acclass '/' lv_revclass
                             INTO gs_outdata-bstkd. "PO number
    CONDENSE gs_outdata-bstkd NO-GAPS.
    gs_outdata-dwerk = ls_zlsdcis02-werks.  "Plant
    gs_outdata-vkbur = ls_zlsdcis02-vkbur. "Sales office
    gs_outdata-augru = p_augru.  "Order Reason
    gs_outdata-konda = ls_zlsdbn003-konda.   "Price Group
    gs_outdata-kdgrp = ls_zlsdcis01-kdgrp.  "Customer Group
    gs_outdata-prsdt = gs_indata-bill_date.  "Pricing date
    IF gs_indata-acct_class(3) = 'RES'.
      gs_outdata-kvgr1 = 'A1'.
    ELSEIF gs_indata-acct_class(3) = 'COM'.
      gs_outdata-kvgr1 = 'C1'.
    ELSEIF gs_indata-acct_class(3) = 'IND'.
      gs_outdata-kvgr1 = 'I1'.
    ELSEIF gs_indata-acct_class(3) = 'APT'. " COG EGD
      gs_outdata-kvgr1 = 'R1'.
    ELSEIF gs_indata-acct_class(3) = 'UBN'. " COG EGD
      gs_outdata-kvgr1 = 'UB'.
    ELSEIF gs_indata-acct_class(3) = 'TRA'. " COG EGD
      gs_outdata-kvgr1 = 'TR'.
    ELSE.
      gs_outdata-kvgr1 = '???'.
    ENDIF.
    gs_outdata-kvgr2 = ls_zlsdcis01-kvgr2.  "Customer Group2
    gs_outdata-bzirk = ls_zlsdcis02-bzirk.  "Sales District
    gs_outdata-matnr = ls_zlsdcis01-matnr.  "Material
    gs_outdata-vrkme = lv_uom_iso. "p_vrkme.  "Sales unit
    gs_outdata-kschl = ls_zlsdcis01-kscha.   "condition type
    gs_outdata-kdkg1 = ls_zlsdcis01-kdkg1.   "condition group1
    gs_outdata-zuonr = gs_indata-recon_key.  "Assignment (Reconkey)
    "---------------
    lv_qty = gs_indata-quant.
    "---Determine Doc type
    IF lv_qty >= 0.
      gs_outdata-bsart = ls_zlsdcis01-auart_iv.
      gs_outdata-kbetr = lv_kbetr.
    ELSE.
      gs_outdata-bsart = ls_zlsdcis01-auart_rv. "Doc type
      gs_outdata-kbetr = lv_kbetr * -1.
    ENDIF.
    "---Re-determine Doc type if Qty = 0 for SO doc type only, 2021.01.28
    "---Issue defect ID : 732
    IF lv_qty = 0.
      "----------
      IF gs_outdata-bsart <> 'ZRRI' AND gs_outdata-bsart <> 'ZRRR'.
        IF lv_kbetr >= 0.
          gs_outdata-bsart = ls_zlsdcis01-auart_iv.
          gs_outdata-kbetr = lv_kbetr.
        ELSE.
          gs_outdata-bsart = ls_zlsdcis01-auart_rv.
          gs_outdata-kbetr = lv_kbetr * -1.
        ENDIF.
      ENDIF.
      "----------
      lv_qty = '.01'.
    ENDIF.
    "----------
    lv_qty = abs( lv_qty ).
    gs_outdata-kwmeng = lv_qty. "gs_indata-quant. "Qty
    SHIFT gs_outdata-kwmeng RIGHT DELETING TRAILING space.
    "------if ZRRR then flip amount's values
    "-----Date Feb 19/2021 - changes relavant to Amount
*  IF gs_outdata-bsart = 'ZRRR'.
*      gs_outdata-kbetr = lv_kbetr * -1.
*  ELSEIF gs_outdata-bsart = 'ZRRI'.
*      gs_outdata-kbetr = lv_kbetr.
*    ELSE.
*      gs_outdata-kbetr = ABS( lv_kbetr ).
*    ENDIF.
    CONDENSE gs_outdata-kbetr NO-GAPS.
    SHIFT gs_outdata-kbetr RIGHT DELETING TRAILING space.
    "--------------------
    IF gs_outdata-bsart = 'ZRRI' OR
       gs_outdata-bsart = 'ZRRR'.
      gv_rrrecords = gv_rrrecords + 1.
      APPEND gs_outdata TO gt_rr_outdata.
    ELSE.
      gs_outdata-fkdat = gs_indata-post_date.
      gs_outdata-prsdt = gs_indata-post_date.  "Pricing date
*      gv_osrecords = gv_osrecords + 1.
      MOVE-CORRESPONDING gs_outdata TO gs_outdata1.
      COLLECT gs_outdata1 INTO gt_so_outdata1.
    ENDIF.

  ENDLOOP.
  "-------------

  LOOP AT gt_so_outdata1 INTO gs_outdata1.
* Start of change CHG0240636
    IF gs_outdata1-kbetr = 0.
      gv_aggzeroamt_rec = gv_aggzeroamt_rec + 1.
      continue.
    ENDIF.
* End of change CHG0240636
    MOVE-CORRESPONDING gs_outdata1 TO gs_outdata.
    SHIFT gs_outdata-kwmeng RIGHT DELETING TRAILING space.
    CONDENSE gs_outdata-kbetr NO-GAPS.
    SHIFT gs_outdata-kbetr RIGHT DELETING TRAILING space.
    gv_osrecords = gv_osrecords + 1.
    APPEND gs_outdata TO gt_so_outdata.
    CLEAR:gs_outdata,gs_outdata1.
  ENDLOOP.
  SORT gt_rr_outdata BY bsart vkorg vtweg spart dwerk bzirk kdgrp konda matnr kdkg1 kvgr1.
  SORT gt_so_outdata BY bsart vkorg vtweg spart dwerk bzirk kdgrp konda matnr kdkg1 kvgr1.

ENDFORM.                    " PREPARE_DATA
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_FILES
*&---------------------------------------------------------------------*
*       Output Files
*----------------------------------------------------------------------*
FORM output_files .
  DATA: msg(100).
  "----First check there is no issue with output file creation
  IF gt_rr_outdata[] IS NOT INITIAL.
    OPEN DATASET p_orfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      gv_error = 'X'.
      MESSAGE e100(zm) WITH 'Unable to open output File.'  p_orfile ' ' ' '.
    ENDIF.
  ENDIF.
  IF gt_so_outdata IS NOT INITIAL.
    OPEN DATASET p_osfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      gv_error = 'X'.
      MESSAGE e100(zm) WITH 'Unable to open output File.' p_osfile ' ' ' '.
    ENDIF.
  ENDIF.
  WRITE: / '--------------------Log--------------------'.
  SKIP 1.
  WRITE : / 'Date       : ', sy-datum.
  WRITE : / 'Start Time : ', gv_stime.
  SKIP 1.
  "------------------fixed length record
  IF gt_rr_outdata[] IS NOT INITIAL.
    LOOP AT gt_rr_outdata INTO gs_outdata.
      TRANSFER gs_outdata TO p_orfile.
    ENDLOOP.
    CLOSE DATASET p_orfile.
    IF sy-subrc = 0.
      WRITE : / 'File has been created, ' , p_orfile.
    ELSE.
      MESSAGE e100(zm) WITH 'Unable to close output File.' p_orfile ' ' ' '.
    ENDIF.
  ENDIF.
  "-----
  IF gt_so_outdata IS NOT INITIAL.
    LOOP AT gt_so_outdata INTO gs_outdata.
      TRANSFER gs_outdata TO p_osfile.
    ENDLOOP.
    CLOSE DATASET p_osfile.
    IF sy-subrc = 0.
      WRITE: / 'File has been created, ' , p_osfile.
    ELSE.
      MESSAGE e100(zm) WITH 'Unable to close output File.' p_osfile ' ' ' '.
    ENDIF.
  ENDIF.
  "----------
  SKIP 1.
  WRITE: / 'Total number of Records in input file : ', gv_trecords.
  WRITE: / 'Total records skipped with zero Amt   : ', gv_zeroamt_rec.
  WRITE: / 'Total Agg. records skipped with zero Amt   : ', gv_aggzeroamt_rec."CHG0240636
  WRITE: / 'Total number of Records in SO file    : ', gv_osrecords.
  WRITE: / 'Total number of Records in RR file    : ', gv_rrrecords.
  WRITE: / 'Total records skipped on Price Group  : ', gv_exrecords.
  SKIP 1.
  WRITE : / 'End Time   : ', sy-uzeit.
  SKIP 2.
  WRITE: / 'Process has been completed...'.
  WRITE: / '-----------------End of Log-----------------'.

ENDFORM.                    " OUTPUT_FILES
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_ALPHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM conversion_alpha  CHANGING iv_var TYPE clike.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = iv_var
    IMPORTING
      output = iv_var.

ENDFORM.                    " CONVERSION_ALPHA
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_PARAMETERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM validate_parameters .
  DATA: ls_tvko TYPE tvko,
        ls_tvtw TYPE tvtw,
        ls_tspa TYPE tspa,
        ls_kna1 TYPE kna1,
        ls_tcurc TYPE tcurc,
        ls_t006 TYPE t006,
        ls_tvau TYPE tvau,
        ls_tvta TYPE tvta.

  "if output files parameters has <or > or both <> sign then raise error message
  FIND REGEX '[<>]' IN p_osfile.
  IF sy-subrc = 0.
    WRITE : / 'Please check SO Output File parameter, invalid name.'.
    WRITE : / p_osfile.
    STOP.
  ENDIF.
  FIND REGEX '[<>]' IN p_orfile.
  IF sy-subrc = 0.
    WRITE : / 'Please check RR Output File parameter, invalid name.'.
    WRITE : / p_orfile.
    STOP.
  ENDIF.
  SELECT SINGLE * FROM tvko INTO ls_tvko WHERE vkorg = p_vkorg.
  IF sy-subrc <> 0.
    WRITE: / 'Sale Organization does not exist, check SS parameter ', p_vkorg.
    STOP.
  ENDIF.
  SELECT SINGLE * FROM tspa INTO ls_tspa WHERE spart = p_spart.
  IF sy-subrc <> 0.
    WRITE: / 'Division is wrong, check SS parameter ', p_spart.
    STOP.
  ENDIF.
  SELECT SINGLE * FROM kna1 INTO ls_kna1 WHERE kunnr = p_kunnr.
  IF sy-subrc <> 0.
    WRITE: / 'Customer code is wrong, check SS parameter ' , p_kunnr.
    STOP.
  ENDIF.
  SELECT SINGLE * FROM tcurc INTO ls_tcurc WHERE waers = p_waerk.
  IF sy-subrc <> 0.
    WRITE: / 'Currency is wrong. check SS parameter ', p_waerk.
    STOP.
  ENDIF.
  SELECT SINGLE * FROM tvau INTO ls_tvau WHERE augru = p_augru.
  IF sy-subrc <> 0.
    WRITE: / 'Order Reason does not exist, check SS parameter ', p_augru.
    STOP.
  ENDIF.
  SELECT SINGLE * FROM t006 INTO ls_t006 WHERE msehi = p_vrkme.
  IF sy-subrc <> 0.
    WRITE : / 'UoM is wrong, check SS parameter ', p_vrkme.
    STOP.
  ENDIF.

ENDFORM.                    " VALIDATE_PARAMETERS
