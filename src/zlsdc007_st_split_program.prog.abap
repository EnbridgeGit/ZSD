REPORT  zlsdc007_st_split_program  MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       February 2011                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will receive contrax storage and transporation revenue *
*  files and split them into three unique files to be used for Sales   *
*  Order IDocs, Accounting BDC and Goods Movement IDOC.                *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Issue   By      Date    Description                                   *
************************************************************************


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
          amount(17)            TYPE c,
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
        END OF ty_stfile.



DATA:   msg(80)       TYPE          c,
        lv_input(400) TYPE          c,
        lv_lines      TYPE          integer,
        lv_string     TYPE          string,
        s_zlsdst01    TYPE          zlsdst01,
        s_stfile      TYPE          ty_stfile,
        t_input       LIKE TABLE OF s_stfile,
        t_fuel        LIKE TABLE OF s_stfile,
        t_acc         LIKE TABLE OF s_stfile,
        t_other       LIKE TABLE OF s_stfile,
        lv_datarec    TYPE          string,
        t_data        LIKE TABLE OF lv_datarec.

FIELD-SYMBOLS:  <curcol>      TYPE          ANY.

CONSTANTS:  delimtr(1) TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
PARAMETERS: p_infile  LIKE filenameci-fileextern OBLIGATORY.

SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE text-002.
PARAMETERS: p_fuel    LIKE filenameci-fileextern OBLIGATORY,
            p_acc     LIKE filenameci-fileextern OBLIGATORY,
            p_other   LIKE filenameci-fileextern OBLIGATORY.

SELECTION-SCREEN END OF BLOCK a2.

*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE: '/usr/sap/interfaces/' sy-sysid+0(3) '/DRCO0078/'
              'zbis231.chk' INTO p_infile,
              '/usr/sap/interfaces/' sy-sysid+0(3) '/DRCO0078/'
              'zcontraxstfuel.dat' INTO p_fuel,
              '/usr/sap/interfaces/' sy-sysid+0(3) '/DRCO0078/'
              'zcontraxstacc.dat' INTO p_acc,
              '/usr/sap/interfaces/' sy-sysid+0(3) '/DRCO0078/'
              'zcontraxstsalesorder.dat' INTO p_other.


*************************************************************************
*************************************************************************
START-OF-SELECTION.
  PERFORM read_input_file.
  PERFORM split_file.
  PERFORM output_files.


*----------------------------------------------------------------------*
*  This routine reads the tab-delimited input file sent by the
*  client community and splits it into its various components.
*----------------------------------------------------------------------*
FORM  read_input_file.
  OPEN DATASET p_infile  FOR INPUT  IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH p_infile msg.
    STOP.
  ENDIF.
  CLEAR t_input.


  DO.
    READ DATASET p_infile INTO lv_input.

    IF sy-subrc <> 0.           "Exit when file is completely read in
      EXIT.
    ENDIF.
    CLEAR s_stfile.
    SPLIT lv_input AT delimtr INTO
          s_stfile-type s_stfile-year s_stfile-month s_stfile-pty_id
          s_stfile-rate_class s_stfile-st_code s_stfile-sc_code s_stfile-seasonal_class
          s_stfile-rate_type s_stfile-charge_type s_stfile-sr_usage s_stfile-st_sub_type
          s_stfile-non_rate_item_type s_stfile-cycled_ind s_stfile-tier_step_level s_stfile-amount
          s_stfile-amount_uom s_stfile-volume s_stfile-volume_uom s_stfile-item_class
          s_stfile-exchange_rate s_stfile-ssegid s_stfile-srsubtype s_stfile-srusersup
          s_stfile-contrref s_stfile-shrtlnterm s_stfile-sr_num s_stfile-unit_rate
          s_stfile-unit_rate_uom.

    APPEND s_stfile TO t_input.
  ENDDO.



  CLOSE DATASET p_infile.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessfl close' p_infile msg.
    STOP.
  ENDIF.
ENDFORM.                    "read_input_file

*----------------------------------------------------------------------*
*  This routine splits the input file into 3 seperate files based on
*  a configuration table
*----------------------------------------------------------------------*
FORM split_file.

  CLEAR: t_fuel, t_acc, t_other.

  LOOP AT t_input INTO s_stfile.

    IF s_stfile-sr_usage = 'FUEL' OR s_stfile-st_sub_type = 'FUEL' or s_stfile-st_sub_type = 'INJFL' or s_stfile-st_sub_type = 'WTDFL'.
      APPEND s_stfile TO t_fuel.
    ELSE.

      CLEAR s_zlsdst01.
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

      IF s_zlsdst01 IS INITIAL.
        APPEND s_stfile TO t_acc.
      ELSEIF s_zlsdst01-kschl = 'ZTAX'.
        APPEND s_stfile TO t_other.
      ELSEIF s_zlsdst01-matnr = ''.
        APPEND s_stfile TO t_acc.
      ELSE.
        APPEND s_stfile TO t_other.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "split_file

*----------------------------------------------------------------------*
*  This routine outputs the 3 files.
*----------------------------------------------------------------------*
FORM output_files.
  PERFORM open_outputfiles.


****Fuel****
  CLEAR t_data.
  LOOP AT t_fuel INTO s_stfile.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_stfile TO <curcol>.
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
    TRANSFER lv_datarec TO p_fuel.
  ENDLOOP.





****Acc****
  CLEAR t_data.
  LOOP AT t_acc INTO s_stfile.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_stfile TO <curcol>.
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
    TRANSFER lv_datarec TO p_acc.
  ENDLOOP.





****Other****
  CLEAR t_data.
  LOOP AT t_other INTO s_stfile.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_stfile TO <curcol>.
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
    TRANSFER lv_datarec TO p_other.
  ENDLOOP.




  PERFORM close_outputfiles.
ENDFORM.                    "output_files


*----------------------------------------------------------------------*
*  This routine opens the outputfiles.
*----------------------------------------------------------------------*
FORM open_outputfiles.

  OPEN DATASET p_fuel FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH p_fuel msg.
    STOP.
  ENDIF.

  OPEN DATASET p_acc FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH p_acc msg.
    STOP.
  ENDIF.

  OPEN DATASET p_other FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH p_other msg.
    STOP.
  ENDIF.

ENDFORM.                    "open_outputfiles

*----------------------------------------------------------------------*
*  This routine closesthe outputfiles.
*----------------------------------------------------------------------*
FORM close_outputfiles.
  CLOSE DATASET p_fuel.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessfl close' p_fuel msg.
    STOP.
  ENDIF.
  WRITE:/ 'File Outputed Successfully to: ', p_fuel.

  CLOSE DATASET p_acc.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessfl close' p_acc msg.
    STOP.
  ENDIF.
  WRITE:/ 'File Outputed Successfully to: ', p_acc.

  CLOSE DATASET p_other.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessfl close' p_other msg.
    STOP.
  ENDIF.
  WRITE:/ 'File Outputed Successfully to: ', p_other.
ENDFORM.                    "close_outputfiles
