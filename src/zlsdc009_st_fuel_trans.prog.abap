REPORT  zlsdc009_st_fuel_trans  MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       February 2011                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will receive contrax storage and transporation fuel    *
*  file and translate for MBGMCR02 Idoc                                *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Issue     By      Date         Description                            *
*ACR-5357  KMB     15.12.2017   SAP support request -ve volume in idoc *
*INC0991490  KMB   23.10.2018   GJ and MJ changes                      *
************************************************************************
************************************************************************
*CHANGES:                                                              *
*INC0991490   SKAKUMANU 2018/10/15                                     *
*TR#  D30K929186    CHG0125116                                         *
*Desc:For Fuel Records, if GJ is passed as the Unit of Measure         *
*just use the volume as passed by the file.  If MJ, use existing logic,*
* and if any other UoM is passed throw exception and abend the program.*
************************************************************************
* 2021/03/15 NAGIRIR COG changes to pass Aggregate Customer and        *
*                        exclusion customer from selection.            *
************************************************************************

TYPES:  BEGIN OF ty_stfile,
          type(8)              TYPE c,
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
        END OF ty_stfile,

        BEGIN OF ty_mbgmcr,
          budat         TYPE d,
          bldat         TYPE d,
          xblnr(16)     TYPE c,
          frbnr(16)     TYPE c,
          u_name(12)    TYPE c,
          bktxt(25)     TYPE c,
          matnr(18)     TYPE c,
          werks_d(4)    TYPE c,
          lgort_d(4)    TYPE c,
          bwart(3)      TYPE c,
          erfmg(13)     TYPE c,
          erfme(3)      TYPE c,
          isocd_unit(3) TYPE c,
          sgtxt(30)     TYPE c,
          kostl(19)     TYPE c,
          saknr(10)      TYPE c,
          exbwr(13)     TYPE c,
        END OF ty_mbgmcr.


DATA:   msg(80)       TYPE          c,
        lv_input(400) TYPE          c,
        lv_curdat     LIKE          sy-datum,
        lv_lines      TYPE          integer,
        lv_int        TYPE          integer,
        lv_num        TYPE          n,
        lv_rptyp(1)   TYPE          c,

        s_splits      TYPE          string,
        t_splits      LIKE TABLE OF s_splits,
        lv_string     TYPE          string,

        s_stfile      TYPE          ty_stfile,
        t_stfile      LIKE TABLE OF s_stfile,

        s_mbew        TYPE          mbew,
        s_mbgmcr      TYPE          ty_mbgmcr,
        t_mbgmcr      LIKE TABLE OF s_mbgmcr,

        lv_datarec    TYPE          string,
        t_data        LIKE TABLE OF lv_datarec.

FIELD-SYMBOLS:  <curcol>      TYPE          any.

CONSTANTS:  delimtr(1) TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.
TABLES: KNA1." Added for COG
*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.
PARAMETERS: p_input   LIKE filenameci-fileextern OBLIGATORY,
            p_output  LIKE filenameci-fileextern OBLIGATORY.
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_matnr   LIKE mara-matnr OBLIGATORY DEFAULT 'NATGAS',
            p_werks   LIKE mseg-werks OBLIGATORY DEFAULT 'GSTH',
            p_lgort   LIKE mseg-lgort OBLIGATORY DEFAULT 'A001',
            p_erfme   LIKE mseg-erfme OBLIGATORY DEFAULT 'GJ1',
            p_isocd   TYPE isocd_unit OBLIGATORY DEFAULT 'GJ',
            p_saknr   TYPE saknr      OBLIGATORY DEFAULT '390679',
            p_kostl   LIKE mseg-kostl OBLIGATORY DEFAULT '20310',
            p_kunnr   TYPE kunnr OBLIGATORY.  " Added Customer for COG
SELECT-OPTIONS : s_kunnr2 for kna1-kunnr NO INTERVALS.   "COG
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
                'zcontraxstfuel.dat' INTO p_input,
                '/usr/sap/interfaces/' sy-sysid+0(3) '/DRCO0078/'
                'zcontraxstfuelidoc.dat' INTO p_output.


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
*    BOC SKAKUMANU for  INC0991490 D30K929186
    IF  s_stfile-sr_usage = 'FUEL' AND ( s_stfile-volume_uom ne 'GJ' AND s_stfile-volume_uom ne 'MJ' ).
      CLOSE DATASET p_input.
      IF sy-subrc NE '0'.
        MESSAGE e019 WITH 'unsuccessful close' p_input msg.
        STOP.
      ENDIF.
      MESSAGE text-001 TYPE 'E'.
      STOP.
    ENDIF.
*    EOC SKAKUMANU for  INC0991490 D30K929186
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
* Start of changes for COG
  TYPES : BEGIN OF ty_pty,
            pty_id type char10,
          end of ty_pty.
  DATA: lwa_pty type ty_pty,
        lt_pty type STANDARD TABLE OF ty_pty.

  LOOP AT s_kunnr2.
    clear lwa_pty.
    lwa_pty-pty_id = s_kunnr2-low.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = lwa_pty-pty_id
      IMPORTING
        OUTPUT = lwa_pty-pty_id.

    append lwa_pty to lt_pty.
  ENDLOOP.
* End of changes for COG
  CLEAR: t_mbgmcr.

  LOOP AT t_stfile INTO s_stfile.
    CLEAR s_mbgmcr.
***************************
**Simple Logic
***************************

    SHIFT s_stfile-srsubtype LEFT DELETING LEADING ' '.
    SHIFT s_stfile-srusersup LEFT DELETING LEADING ' '.
    CONCATENATE s_stfile-srsubtype s_stfile-srusersup INTO s_mbgmcr-xblnr.
    s_mbgmcr-frbnr  = s_stfile-contrref.
    s_mbgmcr-u_name = sy-uname.
    s_mbgmcr-bktxt  = 'S&T: CSF'.

    s_mbgmcr-matnr    = p_matnr.
    s_mbgmcr-werks_d  = p_werks.
    s_mbgmcr-lgort_d  = p_lgort.

    IF s_stfile-volume > 0.
      s_mbgmcr-bwart = '501'.
    ELSEIF s_stfile-volume < 0.
      s_mbgmcr-bwart = '502'.
    ENDIF.

    SHIFT s_stfile-volume LEFT DELETING LEADING ' '.

*BOC by KMB on 15.12.2017 on ACR-5357 SAP support request -ve volume in idoc will not be accepted
*    s_mbgmcr-erfmg      = s_stfile-volume.
    IF s_stfile-volume GE 0.
      s_mbgmcr-erfmg      = s_stfile-volume.
    ELSE.
      s_stfile-volume = s_stfile-volume * -1.
      CONDENSE s_stfile-volume.
      s_mbgmcr-erfmg      = s_stfile-volume.
    ENDIF.
*EOC by KMB on 15.12.2017 on ACR-5357 SAP support request -ve volume in idoc will not be accepted
    s_mbgmcr-erfme      = p_erfme.
    s_mbgmcr-isocd_unit = p_isocd.

    SHIFT s_stfile-pty_id LEFT DELETING LEADING ' '.
* Start of changes for  COG
*    CONCATENATE 'ST' s_stfile-pty_id INTO s_mbgmcr-sgtxt.
    CLEAR: lwa_pty.
    read TABLE lt_pty INTO lwa_pty with key pty_id = s_stfile-pty_id.
    IF sy-subrc = 0.
      CONCATENATE 'ST' s_stfile-pty_id INTO s_mbgmcr-sgtxt.
    ELSE.
      s_mbgmcr-sgtxt = p_kunnr.
    ENDIF.
* Start of changes for COG

    s_mbgmcr-kostl  = p_kostl.
    s_mbgmcr-saknr  = p_saknr.

    lv_int = strlen( s_mbgmcr-saknr ).

    WHILE lv_int < 10.
      CONCATENATE '0' s_mbgmcr-saknr INTO s_mbgmcr-saknr.
    ENDWHILE.







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

      s_mbgmcr-budat = lv_curdat.
      s_mbgmcr-bldat = lv_curdat.

*     BOC By KMB INC0991490 on 23.10.2018
*     if UOM is MJ existing logic would follow
      IF s_stfile-volume_uom = 'MJ'.
        s_mbgmcr-erfmg = s_stfile-volume / 1000.
*       If UOM is GJ, the value received from file would be passed directly
      ELSEIF s_stfile-volume_uom = 'GJ'.
        s_mbgmcr-erfmg = s_stfile-volume.
      ENDIF.
*     EOC By KMB INC0991490 on 23.10.2018

    ELSEIF lv_rptyp = 'R'.

      IF s_mbgmcr-bwart = '502'.
        s_mbgmcr-bwart = '501'.
      ELSEIF s_mbgmcr-bwart = '501'.
        s_mbgmcr-bwart = '502'.
      ENDIF.

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
      s_mbgmcr-budat = lv_curdat.
      s_mbgmcr-bldat = lv_curdat.

*     BOC By KMB INC0991490 on 23.10.2018
*     if UOM is MJ existing logic would follow
      IF s_stfile-volume_uom = 'MJ'.
        s_mbgmcr-erfmg = s_stfile-volume / 1000.
*       If UOM is GJ, the value received from file would be passed directly
      ELSEIF s_stfile-volume_uom = 'GJ'.
        s_mbgmcr-erfmg = s_stfile-volume.
      ENDIF.
*     EOC By KMB INC0991490 on 23.10.2018

    ELSE.
      lv_curdat      = sy-datum.

      s_mbgmcr-budat = lv_curdat.
      s_mbgmcr-bldat = lv_curdat.

*     BOC By KMB INC0991490 on 23.10.2018
*     if UOM is MJ existing logic would follow
      IF s_stfile-volume_uom = 'MJ'.
        s_mbgmcr-erfmg = s_stfile-volume / 1000.
*       If UOM is GJ, the value received from file would be passed directly
      ELSEIF s_stfile-volume_uom = 'GJ'.
        s_mbgmcr-erfmg = s_stfile-volume.
      ENDIF.
*     EOC By KMB INC0991490 on 23.10.2018

    ENDIF.





***************************
**Lookups
***************************
    CLEAR: s_mbew.

    SELECT SINGLE stprs
      FROM mbew
      INTO CORRESPONDING FIELDS OF s_mbew
      WHERE matnr = s_mbgmcr-matnr
        AND bwkey = s_mbgmcr-werks_d
    .

    lv_num = s_stfile-volume.
    lv_num = lv_num * s_mbew-stprs.
    lv_num = lv_num / 1000.
    s_mbgmcr-exbwr = lv_num.







***************************
**Append
***************************
    APPEND s_mbgmcr TO t_mbgmcr.
  ENDLOOP.



ENDFORM.                    "translate_file



*----------------------------------------------------------------------*
*  This routine outputs the 3 files.
*----------------------------------------------------------------------*
FORM output_files.
  PERFORM open_outputfiles.



  CLEAR t_data.
  LOOP AT t_mbgmcr INTO s_mbgmcr.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_mbgmcr TO <curcol>.
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
