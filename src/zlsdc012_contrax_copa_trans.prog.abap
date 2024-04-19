REPORT  zlsdc012_contrax_copa_trans MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       February 2011                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will receive contrax fuel file and translate for BDC   *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Issue   By      Date        Description                               *
*TR980   M Khan  2012/04/19  Add new filed WWRSN (Order reason) in bdc *
*                            file and fill it out.                     *
*74189   G Ymana 2014/09/12  Add field invoice number to file layout   *
*ACR244  gymana  2016/12/06  Add rebill_yr, rebill_mth, so_id to input *
*                            file layout                               *
************************************************************************
************************************************************************
*CHANGES:                                                              *
*INC0991490   SKAKUMANU 2018/10/15                                     *
*TR#  D30K929186       CHG0125116                                      *
*Desc:For Fuel Records, if GJ is passed as the Unit of Measure         *
*just use the volume as passed by the file.  If MJ, use existing logic,*
* and if any other UoM is passed throw exception and abend the program.*
*CHG0137170  AKMADASU  2019/02/25
*Contrax COPA Trans UOM Validation.
************************************************************************
* 2021/02/23 BIRUDURD COG changes to pass Aggregate Customer from      *
*                         selection screen
************************************************************************
*Input File Format
TYPES:  BEGIN OF ty_ctxfile,
          app_yr(4)             TYPE c,
          app_mth(2)            TYPE c,
          cust_id(8)            TYPE c,
          rate_cl(12)           TYPE c,
          serv_type(6)          TYPE c,
          serv_cl(2)            TYPE c,
          seas_cl(4)            TYPE c,
          rate_type(4)          TYPE c,
          charge_type(4)        TYPE c,
          sr_usage(4)           TYPE c,
          st_subtype(6)         TYPE c,
          non_rate_item_typ(8)  TYPE c,
          tier_step_lvl(2)      TYPE c,
          sector_size(1)        TYPE c,
          sector(6)             TYPE c,
          amount(16)            TYPE c,
          volume(18)            TYPE c,
          cust_type(1)          TYPE c,
          geca_code(6)          TYPE c,
          vol_uom(8)            TYPE c,
          sa_num(8)             TYPE c,
          inv_num(8)            TYPE c,
          rebill_yr(4)          TYPE c,     "ACR244
          rebill_mth(2)         TYPE c,     "ACR244
          so_id(4)              TYPE c,     "ACR244
        END OF ty_ctxfile.

*Output File Format
TYPES:  BEGIN OF ty_bdcout,
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
          inv_num(8) TYPE C,                                "SDP74189
        END OF ty_bdcout.


DATA: msg(80)       TYPE          c,
      lv_rptyp(1)   TYPE          c,
      ls_ctx        TYPE ty_ctxfile,
      lt_ctx        LIKE TABLE OF ls_ctx,
      ls_bdc        TYPE ty_bdcout,
      lt_bdc        LIKE TABLE OF ls_bdc,
      ls_mbew       TYPE          mbew,
      ls_zlsdbn002  TYPE          zlsdbn002,
      ls_zlsdc03    TYPE          zlsdc03,

      lv_string     TYPE          string,
      lv_num        TYPE          rke2_vvbrv,
      lv_int        TYPE          integer,
      lv_lines      TYPE          integer,
      lv_curdat     LIKE          sy-datum,

      lv_datarec    TYPE          string,
      lt_data       LIKE TABLE OF lv_datarec.

FIELD-SYMBOLS:  <curcol>      TYPE          ANY.

CONSTANTS:  delimtr(1) TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
            c_cr       TYPE c VALUE cl_abap_char_utilities=>cr_lf.
*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.
PARAMETERS: p_input   LIKE filenameci-fileextern OBLIGATORY,
            p_output  LIKE filenameci-fileextern OBLIGATORY.
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_vrgar   LIKE cest1-vrgar      OBLIGATORY DEFAULT 'B',
            p_vkorg   LIKE cest1-vkorg      OBLIGATORY DEFAULT 'Z002',
            p_vtweg   LIKE cest1-vtweg      OBLIGATORY DEFAULT 'Z0',
            p_bukrs   LIKE cest1-bukrs      OBLIGATORY DEFAULT 'UGL',
            p_artnr   LIKE cest1-artnr      OBLIGATORY DEFAULT 'NATGAS',
            p_spart   LIKE cest1-spart      OBLIGATORY DEFAULT 'Z0',
            p_vvbvl   LIKE ce11100-vvbvl_me OBLIGATORY DEFAULT 'GJ',
            p_kunnr   LIKE cest1-kndnr      OBLIGATORY DEFAULT ''. "COG

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK c1 WITH FRAME.
PARAMETERS: p_actual  RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_est     RADIOBUTTON GROUP rad1,
            p_rev     RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK c1.

*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE:  '/usr/sap/interfaces/' sy-sysid+0(3) '/LSDCNTXDP/'
                'zcontraxgsfuel.dat' INTO p_input,
                '/usr/sap/interfaces/' sy-sysid+0(3) '/LSDCNTXDP/'
                'zcontraxfuelPA.dat' INTO p_output.





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
  CLEAR ls_ctx.

  DO.
    READ DATASET p_input INTO ls_ctx.
    IF sy-subrc <> 0.           "Exit when file is completely read in
      EXIT.
    ENDIF.
    REPLACE c_cr WITH '' INTO  ls_ctx-sa_num.
*     BOC SKAKUMANU INC0991490 D30K929186
    IF ls_ctx-sr_usage = 'FUEL' AND ( ls_ctx-vol_uom ne 'GJ' AND ls_ctx-vol_uom NE 'MJ' ).
      CLOSE DATASET p_input.
      MESSAGE text-001 TYPE 'E'.
      STOP.
    ENDIF.
*     EOC SKAKUMANU INC0991490 D30K929186
    APPEND ls_ctx TO lt_ctx.
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
  CLEAR lt_bdc.

  LOOP AT lt_ctx INTO ls_ctx.
    CLEAR ls_bdc.


***************************
**Simple Logic
***************************
    ls_bdc-vrgar = p_vrgar.
* Start of Changes   COG
*    SHIFT ls_ctx-cust_id LEFT DELETING LEADING ' '.
*    CONCATENATE 'CX' ls_ctx-cust_id INTO ls_bdc-kndnr.
    ls_bdc-kndnr    = p_kunnr.
* End of Changes    COG
    ls_bdc-vkorg    = p_vkorg.
    ls_bdc-vtweg    = p_vtweg.
    ls_bdc-bukrs    = p_bukrs.
*    ls_bdc-artnr    = p_artnr.
    ls_bdc-spart    = p_spart.
*    ls_bdc-wwrat    = ls_ctx-rate_cl.
    ls_bdc-wwsno    = ls_ctx-sa_num+2(6).
    ls_bdc-vvbvl_me = p_vvbvl.
    ls_bdc-inv_num  = ls_ctx-inv_num.                       "SDP74189


***************************
**Conditional on checkbox.
***************************
    IF lv_rptyp = 'E'.
      ls_bdc-posnr = 'EST'.
      ls_bdc-wwrsn = 'ZES'.                  "TR980
      CONCATENATE ls_ctx-app_yr ls_ctx-app_mth '01' INTO lv_curdat.
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

      ls_bdc-budat = lv_curdat.
*     BOC SKAKUMANU INC0991490 D30K929186
*     if UOM is MJ existing logic would follow
      IF ls_ctx-vol_uom = 'MJ'.
*     EOC SKAKUMANU INC0991490 D30K929186
        ls_bdc-vvbvl = ls_ctx-volume / 1000.
*     BOC SKAKUMANU INC0991490 D30K929186
*       If UOM is GJ, the value received from file would be passed directly
      ELSEIF ls_ctx-vol_uom = 'GJ'.
        ls_bdc-vvbvl = ls_ctx-volume.
      endif.
*     EOC SKAKUMANU INC0991490 D30K929186

    ELSEIF lv_rptyp = 'R'.
      ls_bdc-posnr = 'REV'.
      ls_bdc-wwrsn = 'ZRV'.                  "TR980
      IF ls_ctx-app_mth = '12'.
        ls_ctx-app_mth = '01'.
        lv_int = ls_ctx-app_yr.
        lv_int = lv_int + 1.
        ls_ctx-app_yr = lv_int.
      ELSE.
        lv_int = ls_ctx-app_mth.
        lv_int = lv_int + 1.
        ls_ctx-app_mth = lv_int.
        IF lv_int < 10.
          CONCATENATE '0' ls_ctx-app_mth INTO ls_ctx-app_mth.
        ENDIF.
      ENDIF.
      CONCATENATE ls_ctx-app_yr ls_ctx-app_mth '01' INTO lv_curdat.
      ls_bdc-budat = lv_curdat.

      ls_ctx-volume = ls_ctx-volume * -1.
*     BOC SKAKUMANU INC0991490 D30K929186
*     if UOM is MJ existing logic would follow
      IF ls_ctx-vol_uom = 'MJ'.
*     EOC SKAKUMANU INC0991490 D30K929186
        ls_bdc-vvbvl = ls_ctx-volume / 1000.
*     BOC SKAKUMANU INC0991490 D30K929186
*       If UOM is GJ, the value received from file would be passed directly
      ELSEIF ls_ctx-vol_uom = 'GJ'.
        ls_bdc-vvbvl = ls_ctx-volume.
      endif.
*     EOC SKAKUMANU INC0991490 D30K929186
    ELSE.
      ls_bdc-posnr = 'ACT'.
      ls_bdc-wwrsn = 'ZAC'.                  "TR980
      lv_curdat      = sy-datum.
      ls_bdc-budat = lv_curdat.

*     BOC SKAKUMANU INC0991490 D30K929186
*     if UOM is MJ existing logic would follow
      IF ls_ctx-vol_uom = 'MJ'.
*     EOC SKAKUMANU INC0991490 D30K929186
        ls_bdc-vvbvl = ls_ctx-volume / 1000.
*     BOC SKAKUMANU INC0991490 D30K929186
*       If UOM is GJ, the value received from file would be passed directly
      ELSEIF ls_ctx-vol_uom = 'GJ'.
        ls_bdc-vvbvl = ls_ctx-volume.
      endif.
*     EOC SKAKUMANU INC0991490 D30K929186
    ENDIF.





***************************
**Lookups
***************************
    CLEAR ls_zlsdbn002.
    SELECT SINGLE werks vkbur
      FROM zlsdbn002
      INTO CORRESPONDING FIELDS OF ls_zlsdbn002
      WHERE towncode = ls_ctx-geca_code(2)
        AND municode = ls_ctx-geca_code+2(4)
    .

    if sy-subrc = 0.
      ls_bdc-werks = ls_zlsdbn002-werks.
      ls_bdc-wwbrn = ls_zlsdbn002-vkbur.
    else.
      ls_bdc-werks = 'GSTH'.
      ls_bdc-wwbrn = 'HAMI'.
    endif.


    CLEAR ls_zlsdc03.
    SELECT SINGLE prdha
      FROM zlsdc03
      INTO CORRESPONDING FIELDS OF ls_zlsdc03
      WHERE c_nrttyp = ls_ctx-non_rate_item_typ
        AND c_svctyp = ls_ctx-serv_type
        AND c_custyp = ls_ctx-cust_type
        AND c_rtetyp = ls_ctx-rate_type
        AND c_chgtyp = ls_ctx-charge_type
        AND c_sruse  = ls_ctx-sr_usage
        AND c_stsub  = ls_ctx-st_subtype
        AND c_sccode = ls_ctx-serv_cl
    .

    IF sy-subrc <> 0.
      CLEAR ls_zlsdc03.
      SELECT SINGLE prdha
        FROM zlsdc03
        INTO CORRESPONDING FIELDS OF ls_zlsdc03
        WHERE c_nrttyp = ls_ctx-non_rate_item_typ
          AND c_svctyp = ls_ctx-serv_type
          AND c_custyp = '*'
          AND c_rtetyp = ls_ctx-rate_type
          AND c_chgtyp = ls_ctx-charge_type
          AND c_sruse  = ls_ctx-sr_usage
          AND c_stsub  = ls_ctx-st_subtype
          AND c_sccode = '**'
      .
    ENDIF.

    ls_bdc-prdha = ls_zlsdc03-prdha.


    CLEAR: ls_mbew.

    SELECT SINGLE stprs
      FROM mbew
      INTO CORRESPONDING FIELDS OF ls_mbew
      WHERE matnr = p_artnr
        AND bwkey = ls_bdc-werks
    .

    lv_num = ls_ctx-volume.
**-- START OF CHANGES BY AKMADASU  CHG0137170
    if ls_ctx-VOL_UOM = 'MJ'.
      lv_num = lv_num / 1000.
    ENDIF.
*     lv_num = lv_num / 1000.
**-- END OF CHANGES BY AKMADASU  CHG0137170
    lv_num = lv_num * ls_mbew-stprs.
    lv_num = lv_num / 1000.

    ls_bdc-vvbrv = lv_num.


    SELECT SINGLE wwrat
      FROM zfc02
      INTO ls_bdc-wwrat
      WHERE c_ratecl = ls_ctx-rate_cl
    .


***************************
**Append
***************************
    APPEND ls_bdc TO lt_bdc.
  ENDLOOP.


ENDFORM.                    "translate_file


*----------------------------------------------------------------------*
*  This routine outputs the 3 files.
*----------------------------------------------------------------------*
FORM output_files.
  PERFORM open_outputfiles.

  CLEAR lt_data.
  LOOP AT lt_bdc INTO ls_bdc.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE ls_bdc TO <curcol>.
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
    APPEND lv_datarec TO lt_data.
  ENDLOOP.

  LOOP AT lt_data INTO lv_datarec.
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
