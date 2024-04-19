REPORT  zlsdi008_ctrx_st_copa_bdc MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       February 2011                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will receive contrax fuel file and translate for BDC   *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Issue   By      Date     Description                                  *
*TR980  MKhan 2012/05/02  Add new characteristic field WWRSN-order     *
*                         reason                                       *
*74189  Gymana 2014/09/15 Adding invoice number to input file layout   *
************************************************************************

*Input File Format
TYPES:  BEGIN OF ty_ctrx,
          posnr(3)  TYPE c,
          budat     LIKE cest1-budat,
          vrgar     LIKE cest1-vrgar,
          kndnr     LIKE cest1-kndnr,
          vkorg     LIKE cest1-vkorg,
          vtweg     LIKE cest1-vtweg,
          bukrs     LIKE cest1-bukrs,
*          artnr     LIKE cest1-artnr,
          werks     LIKE cest1-werks,
          spart     LIKE cest1-spart,
          wwrat     LIKE ce11100-wwrat,
          wwbrn     LIKE ce11100-wwbrn,
          wwsno     LIKE ce11100-wwsno,
          prdha     LIKE ce11100-prdha,
          wwrsn     LIKE ce11100-wwrsn,         "TR980
          vvbvl     LIKE ce11100-vvbvl,
          vvbvl_me  LIKE ce11100-vvbvl_me,
          vvbrv     LIKE ce11100-vvbrv,
          inv_num(8)  TYPE C,                              "SDP74189
        END OF ty_ctrx.


DATA: msg(80)       TYPE          c,
      lv_input(400) TYPE          c,
      lv_rptyp(1)   TYPE          c,

      ls_ctrx       TYPE          ty_ctrx,
      gt_ctrx       LIKE TABLE OF ls_ctrx,

      lv_char(20)   TYPE          c,

      gv_bdcname    TYPE         apqi-groupid,

      ls_splits     TYPE          string,
      lt_splits     LIKE TABLE OF ls_splits,

      lv_string     TYPE          string,
      lv_num        TYPE          n,
      lv_int        TYPE          integer,
      lv_lines      TYPE          integer,
      lv_curdat     LIKE          sy-datum,

      ls_bdcdata    TYPE          bdcdata,
      gt_bdcdata    LIKE TABLE OF ls_bdcdata,

      lv_datarec    TYPE          string,
      lt_data       LIKE TABLE OF lv_datarec.

FIELD-SYMBOLS:  <curcol>      TYPE          ANY.

CONSTANTS:  delimtr(1) TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.
*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.
PARAMETERS: p_input   LIKE filenameci-fileextern OBLIGATORY.
PARAMETERS: p_ctrx   RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_ctrxst RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK a1.

*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE:  '/usr/sap/interfaces/' sy-sysid+0(3) '/LSDCNTXDP/'
                'zcontraxfuelPA.dat' INTO p_input.





*************************************************************************
*************************************************************************
START-OF-SELECTION.

  IF p_ctrx = 'X'.
    gv_bdcname = 'ZCO_CSF_CGS'.
  ELSE.
    gv_bdcname = 'ZCO_CSF_CST'.
  ENDIF.


  PERFORM read_input_file.
  PERFORM open_batch_session.
  PERFORM process_bdc.
  PERFORM close_session.

  WRITE:/ 'BDC Created'.

*----------------------------------------------------------------------*
*  This routine reads the tab-delimited input file sent by the
*  client community and splits it into its various components.
*----------------------------------------------------------------------*
FORM  read_input_file.
  OPEN DATASET p_input  FOR INPUT  IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH p_input msg.
    STOP.
  ENDIF.
  CLEAR lv_input.


  DO.
    READ DATASET p_input INTO lv_input.

    IF sy-subrc <> 0.           "Exit when file is completely read in
      EXIT.
    ENDIF.
    CLEAR lt_splits.
    SPLIT lv_input AT delimtr INTO TABLE lt_splits.

    LOOP AT lt_splits INTO ls_splits.
      ASSIGN COMPONENT sy-tabix
             OF STRUCTURE ls_ctrx
             TO <curcol>.
      MOVE ls_splits TO <curcol>.
    ENDLOOP.

    APPEND ls_ctrx TO gt_ctrx.
  ENDDO.



  CLOSE DATASET p_input.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessful close' p_input msg.
    STOP.
  ENDIF.
ENDFORM.                    "read_input_file


*----------------------------------------------------------------------*
*  This routine will create the BDC Data.
*----------------------------------------------------------------------*
FORM process_bdc.
  LOOP AT gt_ctrx INTO ls_ctrx.

    CLEAR gt_bdcdata.

    PERFORM bdc_screen USING 'RK1M1100' '100'.
    PERFORM bdc_field USING 'CEST1-BUDAT' ls_ctrx-budat.
    PERFORM bdc_field USING 'CEST1-PERDE' ls_ctrx-budat+4(2).
    PERFORM bdc_field USING 'CEST1-VRGAR' ls_ctrx-vrgar.
    PERFORM bdc_field USING 'BDC_OKCODE' '=KPDA'.

    PERFORM bdc_screen USING 'RK1M1100' '110'.
    PERFORM bdc_field USING 'CEST1-KNDNR' ls_ctrx-kndnr.
    PERFORM bdc_field USING 'CEST1-VKORG' ls_ctrx-vkorg.
    PERFORM bdc_field USING 'CEST1-VTWEG' ls_ctrx-vtweg.
    PERFORM bdc_field USING 'CEST1-BUKRS' ls_ctrx-bukrs.
    PERFORM bdc_field USING 'BDC_OKCODE' '=PSDA'.

    PERFORM bdc_screen USING 'RK1M1100' '120'.
    PERFORM bdc_field USING 'CEST1-WERKS' ls_ctrx-werks.
    PERFORM bdc_field USING 'CEST1-SPART' ls_ctrx-spart.
    PERFORM bdc_field USING 'BDC_OKCODE' '=NEXT'.


    PERFORM bdc_screen USING 'RK1M1100' '130'.
    PERFORM bdc_field USING 'CE11100-WWRAT' ls_ctrx-wwrat.
    PERFORM bdc_field USING 'CE11100-WWBRN' ls_ctrx-wwbrn.
    PERFORM bdc_field USING 'CE11100-WWSNO' ls_ctrx-wwsno.
    PERFORM bdc_field USING 'CE11100-WWRSN' ls_ctrx-wwrsn.   "TR980
    PERFORM bdc_field USING 'CE11100-PRDHA' ls_ctrx-prdha.
    PERFORM bdc_field USING 'CE11100-WWSUB' 'FL'.
    PERFORM bdc_field USING 'BDC_OKCODE' '=ANRE'.

    PERFORM bdc_screen USING 'RK1M1100' '130'.
    PERFORM bdc_field USING 'BDC_OKCODE' '=NEXT'.

    PERFORM bdc_screen USING 'RK1M1100' '150'.
    CLEAR lv_char.
    lv_char = ls_ctrx-vvbvl.
    SHIFT lv_char LEFT DELETING LEADING ' '.
    PERFORM bdc_field USING 'CE11100-VVBVL' lv_char.
    PERFORM bdc_field USING 'CE11100-VVBVL_ME' ls_ctrx-vvbvl_me.
    PERFORM bdc_field USING 'CE11100-VVUVL_ME' ls_ctrx-vvbvl_me.
    CLEAR lv_char.
    lv_char = ls_ctrx-vvbrv.
    SHIFT lv_char LEFT DELETING LEADING ' '.
    PERFORM bdc_field USING 'CE11100-VVBRV' lv_char.
    PERFORM bdc_field USING 'BDC_OKCODE' '=NEXT'.

    PERFORM bdc_screen USING 'RK1M1100' '151'.
    PERFORM bdc_field USING 'BDC_OKCODE' '=BUCH'.
    PERFORM insert_session.
  ENDLOOP.
ENDFORM.                    "process_bdc






*-----------------------------------------------------------------------
*     FORM OPEN_BATCH_SESSION
*-----------------------------------------------------------------------
* - This routine just simply opens up a new batch input session.
*-----------------------------------------------------------------------
FORM open_batch_session.

  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client            = sy-mandt
      group             = gv_bdcname
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
    WRITE:/ 'Error in open'.
  ENDIF.

ENDFORM.                    "OPEN_BATCH_SESSION


*-----------------------------------------------------------------------
*     FORM INSERT_SESSION
*-----------------------------------------------------------------------
* - This routine inserts the BDC data for one transaction into the
*   batch input session.
*-----------------------------------------------------------------------
FORM insert_session.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode          = 'KE21'
    TABLES
      dynprotab      = gt_bdcdata
    EXCEPTIONS
      internal_error = 1
      not_open       = 2
      queue_error    = 3
      tcode_invalid  = 4.
  IF sy-subrc <> 0.
    MESSAGE e013 WITH sy-subrc.
  ENDIF.

ENDFORM.                    "INSERT_SESSION


*-----------------------------------------------------------------------
*     FORM CLOSE_SESSION
*-----------------------------------------------------------------------
* - This routine simply closes the current batch input session.
*-----------------------------------------------------------------------
FORM close_session.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open    = 1
      queue_error = 2.
  IF sy-subrc <> 0.
    WRITE:/ 'ERROR IN CLOSE'.
  ENDIF.
ENDFORM.                    "CLOSE_SESSION



*-----------------------------------------------------------------------
*     FORM BDC_SCREEN
*-----------------------------------------------------------------------
*  Description:
*  - This routine adds an entry to the table BDCDATA with screen
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*
*  Parameters:
*      -->  PROGRAM - Program name of the screen
*           DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM bdc_screen USING program dynpro.

  CLEAR ls_bdcdata.
  ls_bdcdata-program = program.
  ls_bdcdata-dynpro = dynpro.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO gt_bdcdata.

ENDFORM.                    "BDC_SCREEN


*-----------------------------------------------------------------------
*     FORM BDC_FIELD
*-----------------------------------------------------------------------
*  Description:
*  - This routine adds an entry to the table BDCDATA with field
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*
*  Parameters:
*      -->  FNAM - name of the field on the screen
*           FVAL - value to be entered for that field on the
*                  screen
*-----------------------------------------------------------------------
FORM bdc_field USING fnam fval.

  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = fnam.
  ls_bdcdata-fval = fval.
  APPEND ls_bdcdata TO gt_bdcdata.

ENDFORM.                    "BDC_FIELD
