REPORT zfglr031_cis_recon.
*&*********************************************************************
*  Author:      Sajjad Ahmad                                          *
*  Date:        December 2020.                                        *
*  Project:     SAP CIS - Finance Sync Project                        *
*  Issue Log:   CHG0202008                                            *
*  Description:                                                       *
*     - CIS - FI Document Summary Report                              *
*       Copy the ZFGLR010 and applied changes Data Extraction logic   *
*       and implemented ALV via SALV
*&--------------------------------------------------------------------*
*CHANGES****                                                          *
*                                                                     *
*&--------------------------------------------------------------------*
*                                                                     *
*&--------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: hotspot_click  FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.
ENDCLASS.               "LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_event_handler
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD hotspot_click.
    PERFORM handle_click USING row column.
  ENDMETHOD.                    "hotspot_click1
ENDCLASS.               "lcl_event_handler
*----------------------------------------------------------------------*
TABLES: bkpf, bseg, tbsl, t001.


TYPES: BEGIN OF ty_data,
     bukrs    LIKE  bseg-bukrs,             "Company Code
     belnr    LIKE  bseg-belnr,             "Doc.#
     gjahr    LIKE  bseg-gjahr,             "year
     blart    LIKE  bkpf-blart,             "document type
     budat    LIKE  bkpf-budat,             "Posting date
     bldat    LIKE  bkpf-bldat,             "document date
     cpudt    LIKE  bkpf-cpudt,             "entry date
     xblnr    LIKE  bkpf-xblnr,             "reference
     bktxt    LIKE  bkpf-bktxt,             "doc header text
     grpid    LIKE  bkpf-grpid,             "Session id
     vbeln_vf TYPE  vbeln_vf,               "Billing document
     hkont    LIKE  bseg-hkont,             "G/L account
     kunnr    LIKE  bseg-kunnr,             "Customer
     kostl    LIKE  bseg-kostl,             "Cost center
     aufnr    LIKE  bseg-aufnr,             "Order number
     anln1    LIKE  bseg-anln1,             "Asset #
     anln2    LIKE  bseg-anln2,             "Sub Asset #
     matnr    LIKE  bseg-matnr,             "Material number
     maktx    LIKE  makt-maktx,             "Material Desc.
     bschl    LIKE  bseg-bschl,             "Posting key
     shkzg    LIKE  bseg-shkzg,             "dr/cr
     wrbtr    LIKE  bseg-wrbtr,             "Amount
     mwskz    LIKE  bseg-mwskz,             "Tax code
     "menge    LIKE  bseg-menge,             "Quantity
     "meins    LIKE  bseg-meins,             "Unit
     sgtxt    LIKE  bseg-sgtxt,             "Item Text
     zuonr    LIKE  bseg-zuonr,             "Assignment
    END OF ty_data.
TYPES: BEGIN OF ty_data_sum,
     bukrs    LIKE  bseg-bukrs,             "Company Code
     hkont    LIKE  bseg-hkont,             "G/L account
     matnr    LIKE  bseg-matnr,             "Material number
     maktx    LIKE  makt-maktx,             "Material Desc.
     budat    LIKE  bkpf-budat,             "Posting date
     wrbtr    LIKE  bseg-wrbtr,             "Amount
    END OF ty_data_sum.
DATA: gt_data TYPE TABLE OF ty_data,
      gt_data_sum TYPE TABLE OF ty_data_sum,
      gt_output   LIKE gt_data WITH HEADER LINE,
      es_variant LIKE disvariant,
      is_variant LIKE disvariant,
      gt_comp_tab TYPE TABLE OF rstrucinfo WITH HEADER LINE.
CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP'.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_bukrs FOR bkpf-bukrs,
                s_kunnr FOR bseg-kunnr OBLIGATORY,
                s_blart FOR bkpf-blart,
                s_budat FOR bkpf-budat,
                s_cpudt FOR bkpf-cpudt.

SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
PARAMETERS: r_alv  RADIOBUTTON GROUP rad1 DEFAULT 'X'
                                      USER-COMMAND cmd,
            r_file   RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK b4.
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-005.
PARAMETERS: r_dtl  RADIOBUTTON GROUP rad2 DEFAULT 'X'
                                      USER-COMMAND cmd,
            r_sum   RADIOBUTTON GROUP rad2.
SELECTION-SCREEN END OF BLOCK b5.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:     p_vari LIKE disvariant-variant  MODIF ID olv.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETER: p_ofile TYPE filenameci-fileextern  MODIF ID ofl.             "output File
SELECTION-SCREEN END OF BLOCK b3.
"---------------

INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid(3)
              '/CIS/zrecon_report.dat' INTO p_ofile.

*-------------------------  AT SELECTION SCREEN-------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  is_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant          = is_variant
*     I_TABNAME_HEADER    =
*     I_TABNAME_ITEM      =
*     IT_DEFAULT_FIELDCAT =
      i_save              = 'A'
    IMPORTING
*     E_EXIT              =
      es_variant          = es_variant
    EXCEPTIONS
      not_found           = 1
      program_error       = 2
      OTHERS              = 3.
  p_vari = es_variant-variant.
  "-----------------

AT SELECTION-SCREEN OUTPUT.
  PERFORM  toggle_functionality.
*-------------------------  START-OF-SELECTION -------------------------
* start of main program
START-OF-SELECTION.

  CLEAR: gt_comp_tab,
         gt_data,
         gt_data_sum.
  IF s_budat[] IS  INITIAL AND
     s_cpudt[] IS INITIAL.
    WRITE : / 'Input at least Posting or Create Date .....'.
    STOP.
  ENDIF.
  IF r_file IS NOT INITIAL AND
     p_ofile IS INITIAL.
    WRITE: / 'Please input output file name....'.
    STOP.
  ENDIF.
  "--------
  PERFORM build_internal_table.
  "--------
  IF r_alv IS NOT INITIAL.
    PERFORM get_components.
    PERFORM display_alv.
  ELSE.
    IF r_dtl IS NOT INITIAL.
      PERFORM output_file.
    ELSE.
      PERFORM output_file_sum.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  BUILD_INTERNAL_TABLE
*&---------------------------------------------------------------------*
*       Get Data for output
*----------------------------------------------------------------------*
FORM build_internal_table.
  DATA: lv_tabix TYPE sy-tabix,
        ls_bsid TYPE bsid,
        ls_bkpf TYPE bkpf,
        ls_bseg TYPE bseg,
        ls_data TYPE ty_data,
        ls_data1 TYPE ty_data,
        lt_data TYPE TABLE OF ty_data,
        ls_data_sum TYPE ty_data_sum,
        ls_makt TYPE makt,
        "ls_faglflexa TYPE faglflexa,
        lt_makt TYPE TABLE OF makt,
        lt_bsid TYPE TABLE OF bsid,
        lt_bkpf TYPE TABLE OF bkpf,
        lt_bseg TYPE TABLE OF bseg.
  "lt_faglflexa TYPE TABLE OF faglflexa.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 10
      text       = 'Data Extraction is in progress'.

  SELECT * FROM bsid INTO TABLE lt_bsid
    WHERE bukrs IN s_bukrs
      AND kunnr IN s_kunnr
      AND budat IN s_budat
      AND blart IN s_blart
      AND cpudt IN s_cpudt.
  IF lt_bsid[] IS INITIAL.
    WRITE: / 'No data for output for selection criteria.'.
    STOP.
  ENDIF.
*DETAIL = 'X'.
  SELECT * FROM bkpf INTO TABLE lt_bkpf
    FOR ALL ENTRIES IN lt_bsid
    WHERE bukrs = lt_bsid-bukrs
      AND belnr = lt_bsid-belnr
      AND gjahr = lt_bsid-gjahr.
  IF lt_bkpf[] IS NOT INITIAL.
    SELECT * FROM bseg INTO TABLE lt_bseg
      FOR ALL ENTRIES IN lt_bkpf
      WHERE bukrs = lt_bkpf-bukrs
        AND belnr = lt_bkpf-belnr
        AND gjahr = lt_bkpf-gjahr.
*    SELECT * FROM faglflexa INTO TABLE lt_faglflexa
*      FOR ALL ENTRIES IN lt_bkpf
*      WHERE gjahr = lt_bkpf-gjahr
*        AND belnr = lt_bkpf-belnr
*        AND rldnr = '0L'
*        AND rbukrs = lt_bkpf-bukrs
*        AND msl <> 0.
    IF lt_bseg[] IS NOT INITIAL.
      SELECT * FROM makt INTO TABLE lt_makt
        FOR ALL ENTRIES IN lt_bseg
        WHERE spras = sy-langu
          AND matnr = lt_bseg-matnr.
    ENDIF.

  ENDIF.
  "----------
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 60
      text       = 'Data Preparation for output in progress'.
  "--------
  SORT lt_bkpf BY bukrs belnr gjahr.
  SORT lt_bseg BY bukrs belnr gjahr buzei.
  LOOP AT lt_bseg INTO ls_bseg.
    CLEAR: ls_data,
           ls_makt,
*           ls_faglflexa,
           ls_bkpf.
    MOVE-CORRESPONDING ls_bseg TO ls_data.
    READ TABLE lt_bkpf INTO ls_bkpf WITH KEY bukrs = ls_bseg-bukrs
                                             belnr = ls_bseg-belnr
                                             gjahr = ls_bseg-gjahr.
    ls_data-blart = ls_bkpf-blart.
    ls_data-budat = ls_bkpf-budat.
    ls_data-bldat = ls_bkpf-bldat.
    ls_data-cpudt = ls_bkpf-cpudt.
    ls_data-xblnr = ls_bkpf-xblnr.
    ls_data-bktxt = ls_bkpf-bktxt.
    ls_data-grpid = ls_bkpf-grpid.
    IF ls_bkpf-awtyp = 'VBRK'.
      ls_data-vbeln_vf = ls_bkpf-awkey.
    ELSE.
      ls_data-vbeln_vf = space.
    ENDIF.
    IF ls_bseg-matnr IS NOT INITIAL.
*      READ TABLE lt_faglflexa INTO ls_faglflexa WITH KEY gjahr = ls_bseg-gjahr
*                                                         belnr = ls_bseg-belnr
*                                                         rbukrs = ls_bseg-bukrs
*                                                         buzei = ls_bseg-buzei.
*      IF sy-subrc = 0.
*        ls_data-menge = ls_faglflexa-msl.
*        ls_data-meins = ls_faglflexa-runit.
*      ELSE.
*        ls_data-menge = space.
*        ls_data-meins = space.
*      ENDIF.
*      "ENDIF.
      READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_bseg-matnr.
      ls_data-maktx = ls_makt-maktx.
    ENDIF.
    IF ls_bseg-shkzg = 'H'.
      ls_data-wrbtr = -1 * ls_data-wrbtr.
      "ls_data-menge = -1 * ls_data-menge. Flex table already keep sign with Qty
    ENDIF.
    "-----------
    APPEND ls_data TO gt_data.
  ENDLOOP.
"----Prepare Summarize data
  IF r_sum IS NOT INITIAL. "Summary
    SORT gt_data BY bukrs hkont matnr budat.
    lt_data[] = gt_data[].
    DELETE ADJACENT DUPLICATES FROM lt_data[] COMPARING bukrs hkont matnr budat.
    LOOP AT lt_data INTO ls_data.
      CLEAR: ls_data_sum,
             lv_tabix.
      MOVE-CORRESPONDING ls_data TO ls_data_sum.
      ls_data_sum-wrbtr = 0.
      READ TABLE gt_data WITH KEY bukrs = ls_data-bukrs
                                  hkont = ls_data-hkont
                                  matnr = ls_data-matnr
                                  budat = ls_data-budat
                                  TRANSPORTING NO FIELDS.
      lv_tabix = sy-tabix.
      LOOP AT gt_data INTO ls_data1 FROM lv_tabix.
        IF ls_data1-bukrs <> ls_data-bukrs OR
           ls_data1-hkont <> ls_data-hkont OR
           ls_data1-matnr <> ls_data-matnr OR
           ls_data1-budat <> ls_data-budat.
          EXIT.
        ENDIF.
        ls_data_sum-wrbtr = ls_data_sum-wrbtr + ls_data1-wrbtr.
      ENDLOOP.
      APPEND ls_data_sum TO gt_data_sum.
    ENDLOOP.
    "--------clear unnecessary internal table
    CLEAR: gt_data, lt_data.
  ENDIF.
ENDFORM.                    "BUILD_INTERNAL_TABLE

*&---------------------------------------------------------------------*
*&      Form  HANDLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_click  USING iv_row TYPE salv_de_row
                         iv_column TYPE salv_de_column.

  DATA: ls_data TYPE ty_data.

  READ TABLE gt_data INTO ls_data INDEX iv_row.
  CHECK sy-subrc = 0.
  CASE iv_column.
    WHEN 'BELNR'.
      IF ls_data-belnr IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD ls_data-belnr.
        SET PARAMETER ID 'BUK' FIELD ls_data-bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_data-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'VBELN_VF'.
      IF ls_data-vbeln_vf IS NOT INITIAL.
        SET PARAMETER ID 'VF' FIELD ls_data-vbeln_vf.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN OTHERS.

  ENDCASE.


ENDFORM.                    " HANDLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_alv .
  DATA: lv_label       TYPE string,
          lv_title       TYPE sytitle,
          lv_msg TYPE lvc_title,
          ls_key         TYPE salv_s_layout_key,
          lo_table       TYPE REF TO cl_salv_table,
          lo_layout      TYPE REF TO cl_salv_layout,
          lo_functions   TYPE REF TO cl_salv_functions,
          lo_display     TYPE REF TO cl_salv_display_settings,
          lo_columns     TYPE REF TO cl_salv_columns_table,
          lo_column      TYPE REF TO cl_salv_column_table,  "#EC NEEDED
          lo_content     TYPE REF TO cl_salv_form_element,
          lo_grid        TYPE REF TO cl_salv_form_layout_grid,
          lo_events_salv TYPE REF TO cl_salv_events_table,
          lo_event       TYPE REF TO lcl_event_handler.

  IF r_dtl IS NOT INITIAL.
    TRY.
        CALL METHOD cl_salv_table=>factory
*        EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*          r_container    = lr_con1
*          container_name = 'ALV_CON1'
          IMPORTING
            r_salv_table = lo_table
          CHANGING
            t_table      = gt_data[].
      CATCH cx_salv_msg .
    ENDTRY.
  ELSE.
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_table
          CHANGING
            t_table      = gt_data_sum[].
      CATCH cx_salv_msg .
    ENDTRY.
  ENDIF.
*Function settings
  lo_functions = lo_table->get_functions( ).
  lo_functions->set_all( abap_true ).
*Display Setting
  lo_display = lo_table->get_display_settings( ).

  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
  "CONCATENATE 'CIS to GL Reconciliation Report' s_budat-low 'To' s_budat-high
  "            'Customer:' p_kunnr INTO lv_msg separated by space.

  "lo_display->set_list_header( lv_msg ).
*Event
  lo_events_salv = lo_table->get_event( ).
  CREATE OBJECT lo_event.
  SET HANDLER: lo_event->hotspot_click
               FOR lo_events_salv.
*Set layout
  lo_layout = lo_table->get_layout( ).
  ls_key-report = sy-repid.
  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  CALL METHOD lo_layout->set_initial_layout
    EXPORTING
      value = p_vari.
*Get columns
  CALL METHOD lo_table->get_columns
    RECEIVING
      value = lo_columns.
  lo_columns->set_optimize( 'X' ).
******Change ALV Fields  - title etc.
  IF r_dtl IS NOT INITIAL.
    PERFORM alv_fields USING lo_columns lo_column.
  ENDIF.
******Display ALV
  CALL METHOD lo_table->display.
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_fields  USING io_columns TYPE REF TO cl_salv_columns_table
                        io_column  TYPE REF TO cl_salv_column_table.


  DATA: lv_column     TYPE lvc_fname,
        lv_long_text  TYPE scrtext_l,
        lv_short_text TYPE scrtext_s,
        lv_med_text   TYPE scrtext_m,
        lv_hotspot    TYPE xfeld,
        lv_length     TYPE lvc_outlen.

  LOOP AT gt_comp_tab.

    CLEAR: lv_long_text,
           lv_short_text,
           lv_med_text,
           lv_hotspot,
           lv_length.
    lv_column = gt_comp_tab-compname.


    CASE lv_column.
      WHEN 'SGTXT'.
        lv_long_text  = 'Item Text'.
        lv_med_text   = 'Item Text'.
        lv_short_text = 'Item Text'.
      WHEN 'BELNR'.
        lv_hotspot = 'X'.
      WHEN 'VBELN_VF'.
        lv_hotspot = 'X'.
    ENDCASE.
    TRY.
        io_column ?= io_columns->get_column( lv_column ).
        IF lv_long_text IS NOT INITIAL.
          CALL METHOD io_column->set_long_text
            EXPORTING
              value = lv_long_text.
          CALL METHOD io_column->set_short_text
            EXPORTING
              value = lv_short_text.
          CALL METHOD io_column->set_medium_text
            EXPORTING
              value = lv_med_text.
        ENDIF.
        IF lv_hotspot IS NOT INITIAL.
          CALL METHOD io_column->set_cell_type
            EXPORTING
              value = if_salv_c_cell_type=>hotspot.
        ENDIF.
*        IF lv_length IS NOT INITIAL.
*          io_column->set_output_length( lv_length ).
*          CALL METHOD io_column->set_optimized
*            EXPORTING
*              value = space. "if_salv_c_bool_sap=>TRUE.
*
*        ENDIF.
      CATCH cx_salv_not_found .
    ENDTRY.

  ENDLOOP.

ENDFORM.                    " ALV_FIELDS
*&---------------------------------------------------------------------*
*& Form GET_COMPONENTS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_components .
  REFRESH gt_comp_tab.

  CALL FUNCTION 'GET_COMPONENT_LIST'
    EXPORTING
      program    = sy-repid
      fieldname  = 'GT_OUTPUT'
    TABLES
      components = gt_comp_tab[].


ENDFORM.                    "get_components
*&---------------------------------------------------------------------*
*&      Form  TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM toggle_functionality .

  LOOP AT SCREEN.
* Set the screen fields to display only
    IF  screen-group1 EQ gc_modif_id_dsp.
      screen-input = 0.
    ENDIF.
    IF r_alv = 'X'.
      IF screen-group1 = 'OLV'.
        screen-input = 1.
      ENDIF.
      IF screen-group1 = 'OFL'.
        screen-input = 0.
      ENDIF.
    ELSE.
      IF screen-group1 = 'OLV'.
        screen-input = 0.
      ENDIF.
      IF screen-group1 = 'OFL'.
        screen-input = 1.
      ENDIF.
    ENDIF.
    "-----------------------
    MODIFY   SCREEN.
  ENDLOOP.

ENDFORM.                    " TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_FILE
*&---------------------------------------------------------------------*
*       Output File
*----------------------------------------------------------------------*
FORM output_file .
  DATA: lv_wrbtr(15),
        lv_menge(15),
        ls_text TYPE text1024,
        ls_data TYPE ty_data.

  OPEN DATASET p_ofile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    MESSAGE e100(zm) WITH 'Unable to open output File.'  p_ofile ' ' ' '.
  ENDIF.
  CONCATENATE 'Company Code' 'Doc.#' 'Year' 'Document Type' 'Posting Date'
              'Document Date' 'Entry Date' 'Reference' 'Doc Header Text'
              'Session ID' 'Billing Document' 'G/L Account' 'Customer'
               'Cost Center' 'Order Number' 'Asset #' 'Sub Asset #'
               'Material' 'Material Description' 'Posting Key' 'Dr/Cr'
               'Amount' 'Tax Code' "'Quantity' 'Unit'
               'Item Text' 'Assignment' INTO ls_text SEPARATED BY '|'.
  TRANSFER ls_text TO p_ofile.
  LOOP AT gt_data INTO ls_data.
    WRITE ls_data-wrbtr TO lv_wrbtr DECIMALS 2.
*    WRITE ls_data-menge TO lv_menge DECIMALS 3.
    CONCATENATE  ls_data-bukrs ls_data-belnr ls_data-gjahr ls_data-blart
                 ls_data-budat ls_data-bldat ls_data-cpudt ls_data-xblnr
                 ls_data-bktxt ls_data-grpid ls_data-vbeln_vf ls_data-hkont
                 ls_data-kunnr ls_data-kostl ls_data-aufnr ls_data-anln1
                 ls_data-anln2 ls_data-matnr ls_data-maktx ls_data-bschl
                 ls_data-shkzg lv_wrbtr ls_data-mwskz "lv_menge ls_data-meins
                 ls_data-sgtxt ls_data-zuonr
   INTO ls_text SEPARATED BY '|'.
    "----------------------
    TRANSFER ls_text TO p_ofile.
  ENDLOOP.
  CLOSE DATASET p_ofile.
  IF sy-subrc = 0.
    WRITE : / 'File has been created, ' , p_ofile.
  ELSE.
    MESSAGE e100(zm) WITH 'Unable to close output File.' p_ofile ' ' ' '.
  ENDIF.
ENDFORM.                    " OUTPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_FILE_SUM
*&---------------------------------------------------------------------*
*       Download Summary data
*----------------------------------------------------------------------*
FORM output_file_sum .
  DATA: lv_wrbtr(15),
          lv_menge(15),
          ls_text TYPE text1024,
          ls_data TYPE ty_data_sum.

  OPEN DATASET p_ofile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    MESSAGE e100(zm) WITH 'Unable to open output File.'  p_ofile ' ' ' '.
  ENDIF.
  CONCATENATE 'Company Code' 'G/L Account' 'Material' 'Material Description'
              'Posting Date' 'Amount' INTO ls_text SEPARATED BY '|'.
  TRANSFER ls_text TO p_ofile.
  LOOP AT gt_data_sum INTO ls_data.
    WRITE ls_data-wrbtr TO lv_wrbtr DECIMALS 2.
*    WRITE ls_data-menge TO lv_menge DECIMALS 3.
    CONCATENATE  ls_data-bukrs ls_data-hkont ls_data-matnr ls_data-maktx
                 ls_data-budat lv_wrbtr INTO ls_text SEPARATED BY '|'.
    "----------------------
    TRANSFER ls_text TO p_ofile.
  ENDLOOP.
  CLOSE DATASET p_ofile.
  IF sy-subrc = 0.
    WRITE : / 'File has been created, ' , p_ofile.
  ELSE.
    MESSAGE e100(zm) WITH 'Unable to close output File.' p_ofile ' ' ' '.
  ENDIF.
ENDFORM.                    " OUTPUT_FILE_SUM
