REPORT  zlsdc008_st_salesorder_trans  MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       February 2011                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will receive contrax storage and transporation revenue *
*  file and translate it for ORDERS05                                  *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Issue    By       Date     Description                                *
*TR804COG BTBOUNDY 20110510 Fix BSART Logic                            *
************************************************************************
* 2021/02/23 BIRUDURD COG changes to pass Aggregate Customer and       *
* exclusion customer from selection screen.                            *
*&---------------------------------------------------------------------*

TABLES: kna1." Added for COG
TYPES:  BEGIN OF ty_stfile,
          type(10)              TYPE c,
          year(4)               TYPE c,
          month(2)              TYPE c,
          pty_id(8)             TYPE c,
          rate_class(12)        TYPE c,
          st_code(6)            TYPE c,
          sc_code(2)            TYPE c,
          seasonal_class(4)     TYPE c,
          rate_type(4)          TYPE c,
          charge_type(4)        TYPE c,
          sr_usage(4)           TYPE c,
          st_sub_type(6)        TYPE c,
          non_rate_item_type(8) TYPE c,
          cycled_ind(1)         TYPE c,
          tier_step_level(2)    TYPE c,
          amount(16)            TYPE p DECIMALS 2,
          amount_uom(8)         TYPE c,
          volume(15)            TYPE c,
          volume_uom(8)         TYPE c,
          item_class(8)         TYPE c,
          exchange_rate(20)     TYPE c,
          ssegid(10)            TYPE c,
          srsubtype(10)         TYPE c,
          srusersup(10)         TYPE c,
          contrref(15)          TYPE c,
          shrtlnterm(10)        TYPE c,
          sr_num(8)             TYPE c,
          unit_rate(8)          TYPE c,
          unit_rate_uom(8)      TYPE c,
        END OF ty_stfile,

        BEGIN OF ty_orders,
          audat(8)    TYPE c,
          fkdat(8)    TYPE c,
          curcy(3)    TYPE c,
          bsart(4)    TYPE c,
          autlf(1)    TYPE c,
          vkorg(4)    TYPE c,
          vtweg(2)    TYPE c,
          spart(2)    TYPE c,
          parvw(3)    TYPE c,
          partn(17)   TYPE c,
          zuonr(18)   TYPE c,
          bstkd(35)   TYPE c,
          xblnr(16)   TYPE c,
          dwerk(4)    TYPE c,
          augru(3)    TYPE c,
          konda(2)    TYPE c,
          kdgrp(2)    TYPE c,
          prsdt(8)    TYPE c,
          kurrf(9)    TYPE c,
          kschdr(4)   TYPE c,
          kbethdr(11) TYPE c,
          matnr(18)   TYPE c,
          kwmeng(15)  TYPE c,
          vrkme(3)    TYPE c,
          kschl1(4)   TYPE c,
          kbetr1(11)  TYPE c,
          kschl2(4)   TYPE c,
          kbetr2(11)  TYPE c,
        END OF ty_orders.


DATA:   msg(80)       TYPE          c,
        lv_input(400) TYPE          c,
        lv_curdat     LIKE          sy-datum,
        lv_lines      TYPE          integer,
        lv_int        TYPE          integer,
        lv_rptyp(1)   TYPE          c,
        s_splits      TYPE          string,
        t_splits      LIKE TABLE OF s_splits,
        lv_string     TYPE          string,
        s_zlsdst01    TYPE          zlsdst01,
        s_zlsdc02     TYPE          zlsdc02,
        s_stfile      TYPE          ty_stfile,
        t_stfile      LIKE TABLE OF s_stfile,
        s_orders      TYPE          ty_orders,
        t_orders      LIKE TABLE OF s_orders,
        lv_datarec    TYPE          string,
        t_data        LIKE TABLE OF lv_datarec,
        lv_waers      LIKE          knvv-waers.

FIELD-SYMBOLS:  <curcol>      TYPE          ANY.

CONSTANTS:  delimtr(1) TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.
PARAMETERS: p_input   LIKE filenameci-fileextern OBLIGATORY,
            p_output  LIKE filenameci-fileextern OBLIGATORY.
SELECTION-SCREEN END OF BLOCK a1.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_autlf   TYPE autlf OBLIGATORY DEFAULT 'X',
*            p_vkorg   TYPE vkorg OBLIGATORY DEFAULT 'Z003', "SDP45168
            p_vtweg   TYPE vtweg OBLIGATORY DEFAULT 'Z0',
            p_spart   TYPE spart OBLIGATORY DEFAULT 'Z0',
            p_parvw   TYPE parvw OBLIGATORY DEFAULT 'AG',
            p_dwerk   TYPE dwerk OBLIGATORY DEFAULT 'GSTH',
            p_kunnr   TYPE kunag OBLIGATORY DEFAULT ''. "COG
SELECT-OPTIONS : s_kunnr for kna1-kunnr NO INTERVALS.   "COG
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK c1 WITH FRAME.
PARAMETERS: p_actual  RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_est     RADIOBUTTON GROUP rad1,
            p_rev     RADIOBUTTON GROUP rad1.

SELECTION-SCREEN END OF BLOCK c1.
*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE:  '/usr/sap/interfaces/' sy-sysid+0(3) '/DRCO0078/'
                'zcontraxstsalesorder.dat' INTO p_input,
                '/usr/sap/interfaces/' sy-sysid+0(3) '/DRCO0078/'
                'zcontraxstsoidoc.dat' INTO p_output.


*************************************************************************
*************************************************************************
START-OF-SELECTION.

  IF p_actual = 'X'.
    lv_rptyp = 'A'.
  ELSEIF p_est = 'X'.
    lv_rptyp = 'E'.
  ELSEIF p_rev = 'X'.
    lv_rptyp = 'R'.
  ELSE.
    lv_rptyp = 'U'.
  ENDIF.

  PERFORM read_input_file.
  PERFORM translate_file.
  SORT t_orders BY partn bsart dwerk augru konda kdgrp.

  PERFORM output_files.


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
    CLEAR t_splits.
    SPLIT lv_input AT delimtr INTO TABLE t_splits.

    LOOP AT t_splits INTO s_splits.
      ASSIGN COMPONENT sy-tabix
             OF STRUCTURE s_stfile
             TO <curcol>.
      MOVE s_splits TO <curcol>.
    ENDLOOP.

    APPEND s_stfile TO t_stfile.
  ENDDO.



  CLOSE DATASET p_input.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessful close' p_input msg.
    STOP.
  ENDIF.
ENDFORM.                    "read_input_file





*----------------------------------------------------------------------*
*  This routine trasnlates the file.
*----------------------------------------------------------------------*
FORM translate_file.
* Start of changes    COG
TYPES : BEGIN OF ty_pty,
          pty_id type char10,
        end of ty_pty.
DATA: lwa_pty type ty_pty,
      lt_pty type STANDARD TABLE OF ty_pty.

  LOOP AT s_kunnr.
    clear lwa_pty.
    lwa_pty-pty_id = s_kunnr-low.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT         = lwa_pty-pty_id
     IMPORTING
       OUTPUT        = lwa_pty-pty_id.
    append lwa_pty to lt_pty.
  ENDLOOP.
* End of changes    COG

  CLEAR: t_orders.

  LOOP AT t_stfile INTO s_stfile.
    CLEAR s_orders.
***************************
**Simple Logic
***************************


    s_orders-autlf = p_autlf.
*    s_orders-vkorg = p_vkorg. "SDP45168
    s_orders-vtweg = p_vtweg.
    s_orders-spart = p_spart.
    s_orders-parvw = p_parvw.
* Start of changes    COG
    SHIFT s_stfile-pty_id LEFT DELETING LEADING ' '.
    read TABLE lt_pty INTO lwa_pty with key pty_id = s_stfile-pty_id.
    IF sy-subrc = 0.
      CONCATENATE 'ST' s_stfile-pty_id INTO s_orders-partn.
    ELSE.
       s_orders-partn = p_kunnr.
    ENDIF.
* Start of changes    COG
    SHIFT s_stfile-srsubtype LEFT DELETING LEADING ' '.
    SHIFT s_stfile-srusersup LEFT DELETING LEADING ' '.
    CONCATENATE s_stfile-srsubtype s_stfile-srusersup INTO s_orders-zuonr.
    s_orders-bstkd = s_stfile-contrref.
    s_orders-dwerk = p_dwerk.
    SHIFT s_stfile-exchange_rate LEFT DELETING LEADING ' '.
    s_orders-kurrf = s_stfile-exchange_rate.

    CASE s_stfile-amount_uom.
      WHEN '$CDN'.
        s_orders-curcy = 'CAD'.
        s_orders-vkorg = 'Z003'.                            "SDP45168
      WHEN '$US'.
        s_orders-curcy = 'USD'.
        s_orders-vkorg = 'Z004'.                            "SDP45168
      WHEN OTHERS.
        "Lookup the pty_id to get currency
        SELECT SINGLE waers
          FROM knvv
          INTO lv_waers
          WHERE kunnr = s_orders-partn
            AND vkorg = 'Z003'
            AND vtweg = p_vtweg
            AND spart = p_spart
        .
        SHIFT lv_waers LEFT DELETING LEADING ' '.
        s_orders-curcy = lv_waers(3).
    ENDCASE.

    IF s_stfile-volume = 0.
      s_orders-kwmeng = '.001'.
    ELSE.
      s_orders-kwmeng = ABS( s_stfile-volume ).
    ENDIF.
    s_orders-vrkme = 'GV'.

    s_orders-kbetr1 = ABS( s_stfile-amount ).

    s_orders-kschl2 = 'ZPR0'.
    s_orders-kbetr2 = ABS( s_stfile-unit_rate ).






***************************
**Conditional on checkbox.
***************************
    IF lv_rptyp = 'E'.
      CONCATENATE s_stfile-year s_stfile-month '01' INTO lv_curdat.
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = lv_curdat
        IMPORTING
          last_day_of_month = lv_curdat.
      IF sy-subrc <> 0.
        WRITE:/ 'Invalid date: ', lv_curdat, '.'.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        EXIT.
      ENDIF.

      s_orders-audat = lv_curdat.
      s_orders-fkdat = lv_curdat.
      s_orders-xblnr = 'S&T ESTIMATE'.
      s_orders-augru = 'ZES'.
      s_orders-prsdt = lv_curdat.

    ELSEIF lv_rptyp = 'R'.
      IF s_stfile-month = '12'.
        s_stfile-month = '01'.
        lv_int = s_stfile-year.
        lv_int = lv_int + 1.
        s_stfile-year = lv_int.
      ELSE.
        lv_int = s_stfile-month.
        lv_int = lv_int + 1.
        s_stfile-month = lv_int.
        IF lv_int < 10.
          CONCATENATE '0' s_stfile-month INTO s_stfile-month.
        ENDIF.
      ENDIF.
      CONCATENATE s_stfile-year s_stfile-month '01' INTO lv_curdat.
      s_orders-audat = lv_curdat.
      s_orders-fkdat = lv_curdat.
      s_orders-xblnr = 'S&T REVERSAL'.
      s_orders-augru = 'ZRV'.
      s_orders-prsdt = lv_curdat.

    ELSE.
      lv_curdat      = sy-datum.
      s_orders-audat = lv_curdat.
      s_orders-fkdat = lv_curdat.
      s_orders-xblnr = 'S&T ACTUAL'.
      s_orders-augru = 'ZAC'.
      s_orders-prsdt = lv_curdat.

    ENDIF.





***************************
**Lookups
***************************
    CLEAR: s_zlsdst01, s_zlsdc02.
    SELECT SINGLE *
      FROM zlsdst01
      INTO s_zlsdst01
      WHERE c_nrttyp  = s_stfile-non_rate_item_type
        AND c_svctyp  = s_stfile-st_code
        AND c_ratecl  = s_stfile-rate_class
        AND c_seascl  = s_stfile-seasonal_class
        AND c_rtetyp  = s_stfile-rate_type
        AND c_chgtyp  = s_stfile-charge_type
        AND c_sruse   = s_stfile-sr_usage
        AND c_stsub   = s_stfile-st_sub_type
        AND c_sccode  = s_stfile-sc_code
        AND c_term    = s_stfile-shrtlnterm
    .

    SELECT SINGLE *
      FROM zlsdc02
      INTO s_zlsdc02
      WHERE c_ratecl  = s_stfile-rate_class
        AND c_svctyp  = s_stfile-st_code
        AND c_seascl  = s_stfile-seasonal_class
    .


*"TR804COG
*    IF s_stfile-amount = 0.
*      s_orders-bsart = 'ZNOP'.
*    ELSEIF s_stfile-amount < 0.
*      CASE s_zlsdst01-auart.
*        WHEN 'ZISS'.
*          s_orders-bsart = 'ZRET'.
*        WHEN OTHERS.
*          s_orders-bsart = s_zlsdst01-auart.
*      ENDCASE.
*    ELSE.
*      s_orders-bsart = s_zlsdst01-auart.
*    ENDIF.
*"TR804COG

    IF s_stfile-amount = 0.
      s_orders-bsart = 'ZNOP'.
    ELSEIF s_stfile-amount < 0.
      IF lv_rptyp = 'R'.
        "Reversal.
        s_orders-bsart = s_zlsdst01-auart.
      ELSE.
        "Actual/Estimate
        s_orders-bsart = s_zlsdst01-z_rev_otype.
      ENDIF.
    ELSE.
      "Greater than 0.
      IF lv_rptyp = 'R'.
        "Reversal.
        s_orders-bsart = s_zlsdst01-z_rev_otype.
      ELSE.
        "Actual/Estimate
        s_orders-bsart = s_zlsdst01-auart.
      ENDIF.
    ENDIF.

    s_orders-konda = s_zlsdc02-konda.
    s_orders-kdgrp = s_zlsdst01-kdgrp.

    IF s_zlsdst01-kschl = 'ZTAX'.
      s_orders-kschdr = 'ZTAX'.
      s_orders-kbethdr = ABS( s_stfile-amount ).
    ENDIF.

    s_orders-matnr = s_zlsdst01-matnr.
    s_orders-kschl1 = s_zlsdst01-kschl.

    APPEND s_orders TO t_orders.
  ENDLOOP.



ENDFORM.                    "translate_file



*----------------------------------------------------------------------*
*  This routine outputs the 3 files.
*----------------------------------------------------------------------*
FORM output_files.
  PERFORM open_outputfiles.



  CLEAR t_data.
  LOOP AT t_orders INTO s_orders.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_orders TO <curcol>.
      IF sy-subrc = 0.
        lv_string = <curcol>.
        IF sy-index = 1.
          lv_datarec = lv_string.
        ELSE.
          CONCATENATE lv_datarec lv_string INTO lv_datarec SEPARATED BY delimtr.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    lv_lines = lv_lines + 1.
    APPEND lv_datarec TO t_data.
  ENDLOOP.

  LOOP AT t_data INTO lv_datarec.
    TRANSFER lv_datarec TO p_output.
  ENDLOOP.





  PERFORM close_outputfiles.
ENDFORM.                    "output_files


*----------------------------------------------------------------------*
*  This routine opens the outputfiles.
*----------------------------------------------------------------------*
FORM open_outputfiles.

  OPEN DATASET p_output FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH p_output msg.
    STOP.
  ENDIF.

ENDFORM.                    "open_outputfiles

*----------------------------------------------------------------------*
*  This routine closesthe outputfiles.
*----------------------------------------------------------------------*
FORM close_outputfiles.
  CLOSE DATASET p_output.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessful close' p_output msg.
    STOP.
  ENDIF.
  WRITE:/ 'File Outputed Successfully to: ', p_output.

ENDFORM.                    "close_outputfiles
