*&--------------------------------------------------------------------*
*& Report  ZLSDR004_QRAM_CSF_ADJUSTMENT                               *
*&                                                                    *
*&--------------------------------------------------------------------*
* AUTHOR:      Sajjad Ahmad                                           *
* DATE:        September 2011.                                        *
* PROJECT:     Cost of Gas.                                           *
* ISSUE LOG:   TR804                                                  *
* DESCRIPTION:                                                        *
* This program will adjust the cost of sales posting when price change*
* has occurred and necessary entries to the Cost of Sales need to be  *
* adjusted. The program will find the deliveries posted by the user & *
* retrieve the standard cost from the prior periods. The standard cost*
* will be compared to the current released price on material master   *
* and an FI BDC Session will be created with the adjustment entries.  *
*&--------------------------------------------------------------------*

REPORT  zlsdr004_qram_csf_adjustment.
*TYPE-POOLS: slis.

TABLES:
      bkpf.

DATA: BEGIN OF reptab OCCURS 1,
        mblnr      TYPE mseg-mblnr,
        matnr      TYPE mseg-matnr,
        werks      TYPE mseg-werks,
        lgort      TYPE mseg-lgort,
        dmbtr      TYPE mseg-dmbtr,
        menge      TYPE mseg-menge,
        sgtxt      TYPE mseg-sgtxt,
        sakto      TYPE mseg-sakto,
        stprs      TYPE mbewh-stprs, "Prior Pr. Std. Price (MBEWH-STPRS)
        adj_cost   TYPE mbewh-stprs,
        adj_diff   TYPE mbewh-stprs,
      END OF reptab.
* Batch input data
DATA: BEGIN OF bdcdata OCCURS 1.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

DATA: w_tcode       TYPE tstc-tcode VALUE 'FB01',
      w_amount(13)  TYPE c,
      next_acct     TYPE bseg-hkont,
      next_key      TYPE rf05a-newbs,
      cost_comp_ind TYPE c.

DATA: it_bkpf TYPE TABLE OF bkpf,
      wa_bkpf TYPE bkpf,
      it_mseg TYPE TABLE OF mseg,
      wa_mseg TYPE mseg.


DATA: gr_container TYPE REF TO cl_gui_custom_container,
      gr_alvgrid   TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_layout    TYPE lvc_s_layo.
DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE ok_code.

***********************************************************************
*                      SELECTION SCREEN                               *
***********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-000.
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
    s_belnr  FOR  bkpf-belnr OBLIGATORY.
PARAMETERS:
   p_gjahr TYPE bkpf-gjahr DEFAULT sy-datum+0(4) OBLIGATORY,         "Current Year
*   p_budat type bkpf-budat DEFAULT sy-datum OBLIGATORY.              "Posting Date
   p_matnr TYPE vbap-matnr  DEFAULT 'NATGAS' .  "Material #
SELECTION-SCREEN END OF BLOCK box1.

SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
PARAMETERS:
   p_lfmon  TYPE mbewh-lfmon DEFAULT sy-datum+4(2) OBLIGATORY, "Curr PP
   p_lfgja  TYPE mbewh-lfgja DEFAULT sy-datum(4) OBLIGATORY.   "Curr PY
SELECTION-SCREEN END OF BLOCK box2.
SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-003.
PARAMETERS:
   box_bdc  AS CHECKBOX USER-COMMAND rad,
   p_bukrs TYPE bkpf-bukrs DEFAULT 'UGL',                 "Company Code
   p_bldat TYPE bkpf-bldat DEFAULT sy-datum,             "Document Date
   p_monat TYPE bkpf-monat DEFAULT sy-datum+4(2),       "Current Period
   p_blart TYPE bkpf-blart DEFAULT 'SA',                      "Doc Type
   p_waers TYPE bkpf-waers DEFAULT 'CAD',                     "Currency
   p_xblnr TYPE bkpf-xblnr DEFAULT 'COGS ADJ',          "Reference Doc#
   p_bktxt TYPE bkpf-bktxt DEFAULT 'COGS ADJUSTMENT',  "Doc Header Text
   p_cr_gl TYPE bseg-hkont DEFAULT '0000390000',       "GLAcct-COG ADJ
   p_db_gl TYPE bseg-hkont DEFAULT '0000390000',       "GLAcct-Offset
   p_grupid TYPE apqi-groupid DEFAULT 'ZCOG_CSF_QRA'.    "BDC Session name

SELECTION-SCREEN END OF BLOCK box3.
SELECTION-SCREEN END OF BLOCK box.

***********************************************************************
*                    AT SELECTION-SCREEN OUTPUT                       *
***********************************************************************
AT SELECTION-SCREEN OUTPUT.
  IF box_bdc <> 'X'.
    LOOP AT SCREEN.
      IF screen-name = 'P_BUKRS'  OR screen-name = 'P_BLDAT'  OR
         screen-name = 'P_BUDAT'  OR screen-name = 'P_GJAHR'  OR
         screen-name = 'P_MONAT'  OR screen-name = 'P_BLART'  OR
         screen-name = 'P_WAERS'  OR screen-name = 'P_XBLNR'  OR
         screen-name = 'P_BKTXT'  OR screen-name = 'P_COGADJ' OR
         screen-name = 'P_OFFSET' OR screen-name = 'P_GRUPID'.  "
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

***********************************************************************
*                      START-OF-SELECTION                             *
***********************************************************************
START-OF-SELECTION.

  PERFORM get_data.
  IF box_bdc = 'X' AND cost_comp_ind = 'X'.
    PERFORM create_bdc_session.
  ENDIF.
  PERFORM alv_display.

***********************************************************************
*                    SELECT_ONE_TIME_DATA                             *
***********************************************************************
*Select data from MBEWH table & if no row found, STOP program execution
FORM get_data.

  DATA: fl_awkey TYPE mseg-mblnr.
*  RANGES : fl_docn FOR mseg-mblnr.
  DATA: it_mbewh TYPE TABLE OF mbewh,
        wa_mbewh TYPE mbewh.
  CLEAR: it_bkpf,
         it_mbewh,
         it_mseg.
  SELECT * INTO TABLE it_mbewh FROM mbewh
      WHERE lfgja = p_lfgja
        AND lfmon = p_lfmon.

  SELECT * INTO TABLE it_bkpf FROM bkpf
    WHERE belnr IN s_belnr.
*  clear fl_docn.
*  refresh fl_docn.
  LOOP AT it_bkpf INTO wa_bkpf.
    MOVE wa_bkpf-awkey(10)  TO fl_awkey.
    SELECT * FROM mseg APPENDING TABLE it_mseg
    WHERE mjahr = p_gjahr
      AND matnr = p_matnr
      AND mblnr = fl_awkey.
*      fl_docn-option = 'EQ'.
*      fl_docn-sign   = 'I'.
*      fl_docn-low    = fl_awkey.
*      append fl_docn.
  ENDLOOP.
*  loop at fl_docn.
*    SELECT * FROM mseg APPENDING TABLE it_mseg
*    WHERE mjahr = p_gjahr
*      AND matnr = p_matnr
*      AND mblnr in fl_docn.
*   endloop.
***********************************
  IF it_mseg IS INITIAL.
*    MESSAGE i036(zs) WITH 'Material Doc cannot be found for selection criteria'.
    WRITE: / 'Material Document cannot be found for selection criteria ', p_gjahr, p_matnr.
    STOP.
  ENDIF.
  CLEAR: reptab,
         cost_comp_ind.
  LOOP AT it_mseg INTO wa_mseg.
    CLEAR: wa_mbewh.
    READ TABLE it_mbewh INTO wa_mbewh
      WITH KEY matnr = wa_mseg-matnr
               bwkey = wa_mseg-werks.
    IF sy-subrc <> 0.
*      MESSAGE i036(zs) WITH 'Std Price does not exist for the period selected ' wa_mseg-matnr wa_mseg-werks p_lfmon.
      WRITE: / 'Std Price does not exist for the period selected ', p_lfmon, wa_mseg-matnr, wa_mseg-werks.
      STOP.
    ELSE.
      cost_comp_ind = 'X'.
      reptab-mblnr = wa_mseg-mblnr.
      reptab-matnr = wa_mseg-matnr.
      reptab-werks = wa_mseg-werks.
      reptab-lgort = wa_mseg-lgort.
      reptab-dmbtr = wa_mseg-dmbtr.
      reptab-menge = wa_mseg-menge.
      reptab-sgtxt = wa_mseg-sgtxt.
      reptab-sakto = wa_mseg-sakto.
      reptab-stprs =  wa_mbewh-stprs. "Prior Pr. Std. Price
      reptab-adj_cost = wa_mseg-menge * (  wa_mbewh-stprs / 1000 ).
      reptab-adj_diff = wa_mseg-dmbtr - reptab-adj_cost.
      APPEND reptab.
    ENDIF.
  ENDLOOP.
  SORT reptab BY mblnr werks.
ENDFORM.                    "GET_ONE_TIME_DATA

***********************************************************************
*                        CREATE_BDC_SESSION.                          *
***********************************************************************
FORM create_bdc_session.

  DATA: first_time VALUE 'Y'.

  PERFORM open_batch_session.
  LOOP AT reptab WHERE adj_diff <> 0.
    MOVE reptab-adj_diff TO w_amount.
    IF reptab-adj_diff < 0.
      PERFORM put_sign_in_front CHANGING w_amount.
    ENDIF.
    IF first_time = 'Y'.
      CLEAR first_time.
      PERFORM bdc_screen USING 'SAPMF05A' '100'.
      PERFORM bdc_field  USING 'BKPF-BLDAT' p_bldat.
      PERFORM bdc_field  USING 'BKPF-BUDAT' p_bldat.
      PERFORM bdc_field  USING 'BKPF-BLART' p_blart.
      PERFORM bdc_field  USING 'BKPF-BUKRS' p_bukrs.
      PERFORM bdc_field  USING 'BKPF-MONAT' p_monat.
      PERFORM bdc_field  USING 'BKPF-WAERS' p_waers.
      PERFORM bdc_field  USING 'BKPF-XBLNR' p_xblnr.
      PERFORM bdc_field  USING 'BKPF-BKTXT' p_bktxt.
    ENDIF.
    IF reptab-adj_diff < 0.
      PERFORM bdc_field  USING 'RF05A-NEWBS' '50'.
      PERFORM bdc_field  USING 'RF05A-NEWKO' p_cr_gl.
      MOVE '40' TO next_key.
      MOVE p_db_gl  TO  next_acct.
    ELSE.
      PERFORM bdc_field  USING 'RF05A-NEWBS' '40'.
      PERFORM bdc_field  USING 'RF05A-NEWKO' p_db_gl.
      MOVE '50' TO next_key.
      MOVE p_cr_gl  TO  next_acct.
    ENDIF.
    PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
    PERFORM bdc_screen USING 'SAPMF05A'    '300'.
    PERFORM bdc_field  USING 'BSEG-WRBTR'  w_amount. "REPTAB-ADJ_DIFF.
    PERFORM bdc_field  USING 'BSEG-SGTXT'  reptab-sgtxt.
*  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'  P_MWSKZ.
    PERFORM bdc_field  USING 'DKACB-FMORE'  ' '.

    PERFORM bdc_field  USING 'RF05A-NEWBS' next_key.
    PERFORM bdc_field  USING 'RF05A-NEWKO' next_acct.
    PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
    PERFORM bdc_screen USING 'SAPMF05A'   '300'.
    PERFORM bdc_field  USING 'BSEG-WRBTR'  w_amount. "REPTAB-ADJ_DIFF.
*  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'  P_MWSKZ.
    PERFORM bdc_field  USING 'DKACB-FMORE'  ' '.
    PERFORM bdc_field  USING 'BSEG-SGTXT'  reptab-sgtxt.
  ENDLOOP.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/11'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/03'.
  PERFORM bdc_screen USING 'SAPMF05A' '700'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/11'.

  PERFORM insert_session.
  PERFORM close_session.

ENDFORM.                    "CREATE_BDC_SESSION

*-----------------------------------------------------------------------
*     FORM OPEN_BATCH_SESSION
*-----------------------------------------------------------------------
FORM open_batch_session.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client            = sy-mandt
      group             = p_grupid
      keep              = 'X'
      user              = sy-uname
    EXCEPTIONS
      group_invalid     = 1
      group_is_locked   = 2
      holddate_invalid  = 3
      internal_error    = 4
      queue_error       = 5
      running           = 6
      system_lock_error = 7
      user_invalid      = 8.
  IF sy-subrc <> 0.
    MESSAGE e004(zs) WITH p_grupid.
  ENDIF.
ENDFORM.                    "OPEN_BATCH_SESSION

*-----------------------------------------------------------------------
*     FORM INSERT_SESSION
*-----------------------------------------------------------------------
FORM insert_session.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode          = w_tcode
    TABLES
      dynprotab      = bdcdata
    EXCEPTIONS
      internal_error = 1
      not_open       = 2
      queue_error    = 3
      tcode_invalid  = 4.
  IF sy-subrc <> 0.
    MESSAGE e013(zs) WITH sy-subrc.
  ENDIF.

ENDFORM.                    "INSERT_SESSION
*-----------------------------------------------------------------------
*     FORM CLOSE_SESSION
*-----------------------------------------------------------------------
FORM close_session.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open
      queue_error.
  IF sy-subrc = 0.
    MESSAGE i003(zs) WITH p_grupid.
  ENDIF.
ENDFORM.                    "CLOSE_SESSION

*-----------------------------------------------------------------------
*     FORM BDC_SCREEN
*-----------------------------------------------------------------------
FORM bdc_screen USING program dynpro.

  CLEAR bdcdata.
  bdcdata-program = program.
  bdcdata-dynpro = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.

ENDFORM.                    "BDC_SCREEN

*-----------------------------------------------------------------------
*     FORM BDC_FIELD
*-----------------------------------------------------------------------
FORM bdc_field USING fnam fval.

  DATA: tval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.                    "BDC_FIELD
*----------------------------------------------------------------------*
*       FORM PUT_SIGN_IN_FRONT.                                        *
*----------------------------------------------------------------------*

FORM put_sign_in_front CHANGING value.
  DATA: text1(1) TYPE c.

  SEARCH value FOR '-'.
  IF sy-subrc = 0 AND sy-fdpos <> 0.
    SPLIT value AT '-' INTO value text1.
    CONDENSE value.
*    CONCATENATE '-' VALUE INTO VALUE.
  ELSE.
    CONDENSE value.
  ENDIF.
ENDFORM.                    "PUT_SIGN_IN_FRONT
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG
*&---------------------------------------------------------------------*
*       Build Field Catalog
*----------------------------------------------------------------------*
FORM field_catalog .

  DATA ls_fcat TYPE lvc_s_fcat.

  ls_fcat-fieldname = 'MBLNR'.
  ls_fcat-ref_table = 'MSEG'.
  ls_fcat-ref_field = 'MBLNR'.
  ls_fcat-outputlen = '10'.
  ls_fcat-just      = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'MATNR'.
  ls_fcat-ref_table = 'MSEG'.
  ls_fcat-ref_field = 'MATNR'.
  ls_fcat-outputlen = '18'.
  ls_fcat-just      = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WERKS'.
  ls_fcat-ref_table = 'MSEG'.
  ls_fcat-ref_field = 'WERKS'.
  ls_fcat-outputlen = '4'.
  ls_fcat-just      = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'LGORT'.
  ls_fcat-ref_table = 'MSEG'.
  ls_fcat-ref_field = 'LGORT'.
  ls_fcat-outputlen = '4'.
  ls_fcat-just      = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'DMBTR'.
  ls_fcat-ref_table = 'MSEG'.
  ls_fcat-ref_field = 'DMBTR'.
  ls_fcat-outputlen = '15'.
*  ls_fcat-just      = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'MENGE'.
  ls_fcat-ref_table = 'MSEG'.
  ls_fcat-ref_field = 'MENGE'.
  ls_fcat-outputlen = '15'.
*  ls_fcat-just      = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'SGTXT'.
  ls_fcat-ref_table = 'MSEG'.
  ls_fcat-ref_field = 'SGTXT'.
  ls_fcat-outputlen = '30'.
  ls_fcat-just      = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'SAKTO'.
  ls_fcat-ref_table = 'MSEG'.
  ls_fcat-ref_field = 'SAKTO'.
  ls_fcat-outputlen = '10'.
  ls_fcat-just      = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'STPRS'.
  ls_fcat-ref_table = 'MBEWH'.
  ls_fcat-ref_field = 'STPRS'.
  ls_fcat-outputlen = '12'.
*  ls_fcat-just      = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'ADJ_COST'.
  ls_fcat-ref_table = 'MBEWH'.
  ls_fcat-ref_field = 'STPRS'.
  ls_fcat-outputlen = '12'.
*  ls_fcat-just      = 'X'.
  ls_fcat-coltext = 'Adjusted Cost'.
  ls_fcat-seltext = 'Adjusted Cost'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'ADJ_DIFF'.
  ls_fcat-ref_table = 'MBEWH'.
  ls_fcat-ref_field = 'STPRS'.
  ls_fcat-outputlen = '12'.
*  ls_fcat-just      = 'X'.
  ls_fcat-coltext = 'Cost Difference'.
  ls_fcat-seltext = 'Cost Difference'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.



ENDFORM.                    " FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*      ALV Display
*----------------------------------------------------------------------*
FORM alv_display .

*if gr_container is initial.
  CREATE OBJECT gr_container
    EXPORTING
*     PARENT                      =
      container_name              = 'ALV_CONTAINER1'
*     STYLE                       =
*     LIFETIME                    = lifetime_default
*     REPID                       =
*     DYNNR                       =
*     NO_AUTODEF_PROGID_DYNNR     =
*     EXCEPTIONS
*     CNTL_ERROR                  = 1
*     CNTL_SYSTEM_ERROR           = 2
*     CREATE_ERROR                = 3
*     LIFETIME_ERROR              = 4
*     LIFETIME_DYNPRO_DYNPRO_LINK = 5
*     others                      = 6
      .
  IF sy-subrc <> 0.
*                                MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                                           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  CREATE OBJECT gr_alvgrid
    EXPORTING
*    I_SHELLSTYLE      = 0
*    I_LIFETIME        =
      i_parent          = gr_container
*    I_APPL_EVENTS     = space
*    I_PARENTDBG       =
*    I_APPLOGPARENT    =
*    I_GRAPHICSPARENT  =
*    I_NAME            =
*    I_FCAT_COMPLETE   = SPACE
*  EXCEPTIONS
*    ERROR_CNTL_CREATE = 1
*    ERROR_CNTL_INIT   = 2
*    ERROR_CNTL_LINK   = 3
*    ERROR_DP_CREATE   = 4
*    others            = 5
      .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  PERFORM field_catalog.
  PERFORM prepare_layout.

*endif.
  CALL METHOD gr_alvgrid->set_table_for_first_display
    EXPORTING
*    I_BUFFER_ACTIVE               =
*    I_BYPASSING_BUFFER            =
*    I_CONSISTENCY_CHECK           =
*    I_STRUCTURE_NAME              =
*    IS_VARIANT                    =
*    I_SAVE                        =
*    I_DEFAULT                     = 'X'
      is_layout                     = gs_layout
*    IS_PRINT                      =
*    IT_SPECIAL_GROUPS             =
*    IT_TOOLBAR_EXCLUDING          =
*    IT_HYPERLINK                  =
*    IT_ALV_GRAPHICS               =
*    IT_EXCEPT_QINFO               =
*    IR_SALV_ADAPTER               =
    CHANGING
      it_outtab                     = reptab[]
      it_fieldcatalog               = gt_fieldcat
*    IT_SORT                       =
*    IT_FILTER                     =
*  EXCEPTIONS
*    INVALID_PARAMETER_COMBINATION = 1
*    PROGRAM_ERROR                 = 2
*    TOO_MANY_LINES                = 3
*    others                        = 4
          .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL SCREEN 100.

ENDFORM.                    " ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*       Prepare Layout.
*----------------------------------------------------------------------*
FORM prepare_layout .
  gs_layout-zebra  = 'X'.
  gs_layout-smalltitle = 'X'.
  gs_layout-sel_mode = 'A'.
ENDFORM.                    " PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       Screen PBO module
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STAT_01'.
  SET TITLEBAR 'T01'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       Screen PAI module
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
