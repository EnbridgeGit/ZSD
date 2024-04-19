REPORT  zlsdi006_st_orderidoc  MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       February 2011                                           *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will take the input from the conversion program        *
*  and create an orders idoc                                           *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Issue   By      Date    Description                                   *
************************************************************************


TYPES:  BEGIN OF ty_orders,
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
        lv_count      TYPE          integer,
        lv_int        TYPE          integer,
        lv_string     TYPE          string,

        s_splits      TYPE          string,
        t_splits      LIKE TABLE OF s_splits,
        s_orders      TYPE          ty_orders,
        t_orders      LIKE TABLE OF s_orders,

        lv_iold       TYPE          string,
        lv_inew       TYPE          string,
        lv_hold       TYPE          string,
        lv_hnew       TYPE          string,

        s_edidc       TYPE          edidc,                  "Control record
        t_edidc       LIKE TABLE OF s_edidc,
        s_edidd       TYPE          edidd,                  "Data record (IDoc)
        t_edidd       LIKE TABLE OF s_edidd.

FIELD-SYMBOLS:  <curcol>      TYPE          ANY.

CONSTANTS:  delimtr(1) TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.
PARAMETERS: p_input   LIKE filenameci-fileextern OBLIGATORY.
SELECTION-SCREEN END OF BLOCK a1.

*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE:  '/usr/sap/interfaces/' sy-sysid+0(3) '/DRCO0078/'
                'zcontraxstsoidoc.dat' INTO p_input.


*************************************************************************
*************************************************************************
START-OF-SELECTION.

  PERFORM read_input_file.
  PERFORM build_control_rec.
  PERFORM build_data_rec.



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
             OF STRUCTURE s_orders
             TO <curcol>.
      MOVE s_splits TO <curcol>.
    ENDLOOP.

    APPEND s_orders TO t_orders.
  ENDDO.



  CLOSE DATASET p_input.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessful close' p_input msg.
    STOP.
  ENDIF.
ENDFORM.                    "read_input_file

*----------------------------------------------------------------------*
*  This routine builds the control record.
*----------------------------------------------------------------------*
FORM build_control_rec.

  DATA: own_logical_system TYPE tbdls-logsys.

  CLEAR: s_edidc.


  s_edidc-mestyp = 'ORDERS'.           "Message Type
  s_edidc-idoctp = 'ORDERS05'.         "Basic type
  s_edidc-cimtyp = 'ZORD05E'.          "Extension
  UNPACK '1' TO s_edidc-docnum.        "IDoc number #1

  "Get Logical system Name of Current System
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system             = own_logical_system
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 0.

  "Sender
  s_edidc-sndprt = 'LS'.                 "Partner type
  s_edidc-sndprn = 'CTRXST'.             "Partner #
  s_edidc-sndpor = own_logical_system.   "Sender port

  "Receiver
  s_edidc-rcvprt = 'LS'.               "Partner Type
  s_edidc-rcvprn = own_logical_system. "Partner #
  s_edidc-rcvpor = space.              "Receiver port

ENDFORM.                    "build_control_rec

*----------------------------------------------------------------------*
*  This routine builds the data record.
*----------------------------------------------------------------------*
FORM build_data_rec.

  DATA: s_e1edk01 TYPE e1edk01,
        s_z1or05e TYPE z1or05e,
        s_e1edk14 TYPE e1edk14,
        s_e1edk03 TYPE e1edk03,
        s_e1edk05 TYPE e1edk05,
        s_e1edka1 TYPE e1edka1,
        s_e1edk02 TYPE e1edk02,

        s_e1edp01 TYPE e1edp01,
        s_e1edp02 TYPE e1edp02,
        s_e1edp05 TYPE e1edp05,
        s_e1edp052 TYPE e1edp05,
        s_e1edp19 TYPE e1edp19.


  CLEAR: lv_hold, lv_hnew, lv_iold, lv_inew.

  SORT t_orders BY partn bsart dwerk augru konda kdgrp bstkd zuonr vkorg matnr.

  LOOP AT t_orders INTO s_orders.

    CONCATENATE s_orders-partn s_orders-bsart s_orders-dwerk s_orders-augru
                s_orders-konda s_orders-kdgrp s_orders-bstkd s_orders-zuonr s_orders-vkorg INTO lv_hnew.
    CONCATENATE s_orders-konda s_orders-kdgrp s_orders-matnr INTO lv_inew.

    IF lv_hnew <> lv_hold.
      "New Idoc.
      IF lv_hold IS NOT INITIAL.
        "Add the last item from previous IDOC and send it.
        PERFORM add_segment USING 'E1EDP01' s_e1edp01.
        PERFORM add_segment USING 'E1EDP02' s_e1edp02.
        PERFORM add_segment USING 'E1EDP05' s_e1edp05.
        IF s_e1edp052-betrg <> 0.
          PERFORM add_segment USING 'E1EDP05' s_e1edp052.
        ENDIF.
        PERFORM add_segment USING 'E1EDP19' s_e1edp19.

        PERFORM send_idoc.
      ENDIF.

      "Set as the current idoc.
      lv_hold = lv_hnew.

      "Clear the item.
      CLEAR lv_iold.


      "Build the header.

      CLEAR: t_edidd.
      CLEAR: s_e1edk01, s_z1or05e, s_e1edk14, s_e1edk03, s_e1edk05, s_e1edka1, s_e1edk02.

      s_e1edk01-curcy = s_orders-curcy.
      s_e1edk01-autlf = s_orders-autlf.
      s_e1edk01-augru = s_orders-augru.
      s_e1edk01-wkurs = s_orders-kurrf.
      PERFORM add_segment USING 'E1EDK01' s_e1edk01.

      s_z1or05e-konda = s_orders-konda.
      s_z1or05e-kdgrp = s_orders-kdgrp.
      PERFORM add_segment USING 'Z1OR05E' s_z1or05e.



      DEFINE segment_e1edk14.
        s_e1edk14-qualf = &1.
        s_e1edk14-orgid = &2.
        perform add_segment
         using 'E1EDK14' s_e1edk14.
      END-OF-DEFINITION.

      segment_e1edk14:
      '012' s_orders-bsart,'008' s_orders-vkorg, '006' s_orders-vtweg,
      '007' s_orders-spart,'005' s_orders-dwerk.



      DEFINE segment_e1edk03.
        s_e1edk03-iddat = &1.
        s_e1edk03-datum = &2.
        s_e1edk03-uzeit = sy-uzeit.
        perform add_segment
         using 'E1EDK03' s_e1edk03.
      END-OF-DEFINITION.

      segment_e1edk03:
      '012' s_orders-audat, '022' s_orders-audat, '029' s_orders-audat,
      '026' s_orders-fkdat, '035' s_orders-fkdat, '023' s_orders-prsdt.


      s_e1edk05-kschl = s_orders-kschdr.
      s_e1edk05-kobtr = s_orders-kbethdr.
      PERFORM add_segment USING 'E1EDK05' s_e1edk05.


      s_e1edka1-parvw = s_orders-parvw.
      s_e1edka1-partn = s_orders-partn.
      PERFORM add_segment USING 'E1EDKA1' s_e1edka1.


      DEFINE segment_e1edk02.
        s_e1edk02-qualf = &1.
        s_e1edk02-belnr = &2.
        perform add_segment
         using 'E1EDK02' s_e1edk02.
      END-OF-DEFINITION.

      segment_e1edk02:
      '017' s_orders-zuonr,'011' s_orders-augru, '001' s_orders-bstkd.



    ENDIF.



    IF lv_inew <> lv_iold.

      IF lv_iold IS NOT INITIAL.
        "Add the previous item to start a new one.
        PERFORM add_segment USING 'E1EDP01' s_e1edp01.
        PERFORM add_segment USING 'E1EDP02' s_e1edp02.
        PERFORM add_segment USING 'E1EDP05' s_e1edp05.
        IF s_e1edp052-betrg <> 0.
          PERFORM add_segment USING 'E1EDP05' s_e1edp052.
        ENDIF.
        PERFORM add_segment USING 'E1EDP19' s_e1edp19.
      ENDIF.

      CLEAR: s_e1edp01, s_e1edp02, s_e1edp05, s_e1edp052, s_e1edp19.
      lv_iold = lv_inew.

      s_e1edp01-matnr_external  = s_orders-matnr.
      s_e1edp01-menge           = s_orders-kwmeng.
      s_e1edp01-menee           = s_orders-vrkme.

      s_e1edp02-qualf = '001'.
      s_e1edp02-belnr = s_orders-bstkd.

      s_e1edp05-kschl = s_orders-kschl1.
      s_e1edp05-betrg = s_orders-kbetr1.

      s_e1edp052-kschl = s_orders-kschl2.
      s_e1edp052-betrg = s_orders-kbetr2.

      s_e1edp19-qualf = '002'.
      s_e1edp19-idtnr = s_orders-matnr.
      s_e1edp19-ktext = 'S&T'.

    ELSE.
      "Add the aditional information.
      s_e1edp01-menge = s_e1edp01-menge + s_orders-kwmeng.

      s_e1edp05-betrg = s_e1edp05-betrg + s_orders-kbetr1.

      s_e1edp052-betrg = s_e1edp052-betrg + s_orders-kbetr2.

    ENDIF.





  ENDLOOP.


  "Send the last idoc.
  IF lv_hold IS NOT INITIAL.
    "Add the last item.
    PERFORM add_segment USING 'E1EDP01' s_e1edp01.
    PERFORM add_segment USING 'E1EDP02' s_e1edp02.
    PERFORM add_segment USING 'E1EDP05' s_e1edp05.
    IF s_e1edp052-betrg <> 0.
      PERFORM add_segment USING 'E1EDP05' s_e1edp052.
    ENDIF.
    PERFORM add_segment USING 'E1EDP19' s_e1edp19.

    PERFORM send_idoc.
  ENDIF.
ENDFORM.                    "build_data_rec

*----------------------------------------------------------------------*
*  This routine adds a segment to the idoc data.
*----------------------------------------------------------------------*
FORM add_segment
  USING segnam LIKE edidd-segnam
        sdata.
  CLEAR s_edidd.
  UNPACK s_edidc-docnum TO s_edidd-docnum.
  s_edidd-segnam = segnam.
  s_edidd-sdata = sdata.
  APPEND s_edidd TO t_edidd.
ENDFORM.                    "ADD_SEGMENT

*----------------------------------------------------------------------*
*  This routine sends the idoc as an inbound idoc.
*----------------------------------------------------------------------*
FORM send_idoc.

  DATA: s_dc40  TYPE          edi_dc40, "Control Record
        t_dc40  LIKE TABLE OF s_dc40,   "Control Record
        s_dd40  TYPE          edi_dd40, "Data Record
        t_dd40  LIKE TABLE OF s_dd40.   "Data Record

  CLEAR: s_dc40, t_dc40, s_dd40, t_dd40.

  "Convert the Data record
  LOOP AT t_edidd INTO s_edidd.
    MOVE-CORRESPONDING s_edidd TO s_dd40.
    APPEND s_dd40 TO t_dd40.
  ENDLOOP.

  "Convert the IDOC control
  CALL FUNCTION 'IDOC_CONTROL_OUTBOUND_CONVERT'
    EXPORTING
      control_record   = s_edidc
      port_version     = '4'
    IMPORTING
      control_40       = s_dc40
    EXCEPTIONS
      conversion_error = 1
      OTHERS           = 2.
  APPEND s_dc40 TO t_dc40.

  CALL FUNCTION 'IDOC_INBOUND_ASYNCHRONOUS'
    DESTINATION 'NONE'
    TABLES
      idoc_control_rec_40 = t_dc40
      idoc_data_rec_40    = t_dd40.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    DESTINATION 'NONE'
    EXPORTING
      wait = 'X'.
  WRITE:/ 'Idoc created.'.

ENDFORM.                    "send_idoc
