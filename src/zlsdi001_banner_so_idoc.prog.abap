*&---------------------------------------------------------------------*
*& Report  ZLSDI001_BANNER_SO_IDOC
*&--------------------------------------------------------------------*
*&*********************************************************************
*  Author:      Mohammad T. Khan                                      *
*  Date:        November 2010.                                        *
*  Project:     Cost of Gas.                                          *
*  Issue Log:   TR804                                                 *
*  Description:                                                       *
*     - The purpose of this program is to summarize the data file     *
*       created by program ZLSDC002, move summerized data to IDOC     *
*       segments and create IDOC Basic Type ORDERS05 Extension ZORD05E*
*                                                                     *
*&--------------------------------------------------------------------*
*CHANGES****                                                          *
*&01/03/2013 SDP42280 Increased the lenght of Sale Order Counter from *
*                     9999 to 99999.                                  *
*&--------------------------------------------------------------------*
* 11/04/2016 JRHARTUNG D30K927430 ACR-2583 - Cap & Trade Enhancements *
* 12/04/2016 GYMANA    D30K927651 ACR-244 - Segment E1EDK14 Bug fix   *
*&--------------------------------------------------------------------*
*&--------------------------------------------------------------------*

REPORT  zlsdi001_banner_so_idocbb LINE-SIZE 170.
* Intput file format
DATA:  BEGIN OF sidoc_rec,
  audat   TYPE d, "Document Date
  fkdat   TYPE d, "Billing Date
  curcy(3)  TYPE c, "Order Currency
  bsart(4)  TYPE c,   "Sales Document Type
  autlf(1)  TYPE c,   "Complete Delivery Indicator
  vkorg(4)  TYPE c,       "Sales Organization
  vtweg(2)  TYPE c,   "Distribution Channel
  spart(2)  TYPE c,   "Division
  parvw(3)  TYPE c,   "Sold-To-Party (Customer)
  partn(17) TYPE c,   "Customer Number
  bstkd(35) TYPE c,   "PO Number
  dwerk(4)  TYPE c,   "Delivering Plant
  vkbur(4)  TYPE c,   "Sales Office
  augru(3)  TYPE c,   "Order Reason
  konda(2)  TYPE c,   "Price Group
  kdgrp(2)  TYPE c,   "Customer Group
 	prsdt	 	TYPE d,	"Pricing Date
       kvgr1(3)      TYPE c,      "Customer Group 1
  bzirk(4)  TYPE c,   "Sales District
  mabnr(18) TYPE c,   "Material Number
       kwmeng(15) TYPE c,   "Order Quantity
  vrkme(3)  TYPE c,   "Unit of Measure
  kschl(4)  TYPE c,       "Condition Type
  kbetr(11) TYPE c, "Condition Amount
  kdkg1(2)  TYPE c,   "Condition Group 1                    "D30K927430
  kvgr2(3)  TYPE c.   "Customer Group 2                     "D30K927430
DATA:  END OF sidoc_rec.

DATA:
    BEGIN OF intab OCCURS 0.
        INCLUDE STRUCTURE sidoc_rec AS srec.
DATA: END OF intab.

DATA:
    BEGIN OF held_rec.
        INCLUDE STRUCTURE sidoc_rec AS srec.
DATA: END OF held_rec.

DATA:
    BEGIN OF sumtab OCCURS 0,
        socount(5)  TYPE c,       "Sale Order Counter SDP42280
*       SOCOUNT(4)  TYPE C,       "Sale Order Counter SDP42280
        item_no(5)  TYPE c.       "Item Number        SDP42280
*        ITEM_NO(4)  TYPE C.       "Item Number       SDP42280
        INCLUDE STRUCTURE sidoc_rec AS srec.
DATA: END OF sumtab.

DATA: first_rec        TYPE c,
      w_item(5)        TYPE n,
      last_item(5)     TYPE n,
      w_rec_number(5)  TYPE n.                              "SDP42280
*      W_REC_NUMBER(4)  TYPE N.            "SDP42280

DATA: key_fields1      TYPE string,
      prev_key_fields1 TYPE string,
      hrec_skey        TYPE string,
      sumrec_skey      TYPE string,
      w_kwmeng         LIKE vbap-kwmeng,
      w_kbetr(8)       TYPE p DECIMALS 2.

*IDOC related working data.
DATA: control_record LIKE edidc,                   "Control record-IDoc
      own_logical_system TYPE tbdls-logsys,             "Logical system
      int_edidd TYPE TABLE OF edidd WITH HEADER LINE. "Data record-IDoc


****BBCHANGE***********************************
DATA:   lv_input(400) TYPE          c,
        s_splits      TYPE          string,
        t_splits      LIKE TABLE OF s_splits.

FIELD-SYMBOLS:  <curcol>      TYPE          any.

CONSTANTS:  delimtr(1) TYPE c VALUE
cl_abap_char_utilities=>horizontal_tab,
 gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP'.
****BBCHANGE***********************************


*------------------------  Selection Screen  -------------------------*
selection-screen begin of block b1 with frame TITLE text-002.
PARAMETER: r_server  RADIOBUTTON GROUP rad2 DEFAULT 'X'  USER-COMMAND cmd,
           r_local   RADIOBUTTON GROUP rad2.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
PARAMETER:
infile LIKE filenameci-fileextern MODIF ID srv. " DEFAULT
SELECTION-SCREEN END OF BLOCK box.
selection-screen begin of block b2 with frame TITLE text-003.
PARAMETER: p_lfile    TYPE        rfpdo-rfbifile DEFAULT 'H:\' MODIF ID lcl.
selection-screen END OF BLOCK b2.
selection-screen begin of block b3 with frame TITLE text-004.
PARAMETERS: p_fixed   RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_tab     RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK b3.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3)
              '/BANNER/zbannsalesorderidoc.dat' INTO infile.
"------------------------------------------
AT SELECTION-SCREEN OUTPUT.

  PERFORM toggle_functionality.
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lfile.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask      = ',DAT File,*.dat'
      static    = 'X'
    CHANGING
      file_name = p_lfile.
*---------------------------------------------------------------------*
*------------------------  Start of selection ------------------------*
*---------------------------------------------------------------------*
START-OF-SELECTION.
  IF r_server IS NOT INITIAL.
    PERFORM open_input_file.
    PERFORM build_intab_table.
  ELSE.
    PERFORM upload_file_pc.
  ENDIF.
  PERFORM build_sumtab_table.
  PERFORM send_inbound_idoc.
**start of temp code
*     WRITE: /1 '--------------------------------------------'.
*     WRITE: /1 '              CHANGED  RECORD               '.
*     WRITE: /1 '--------------------------------------------'.
*     LOOP AT INTAB.
*     WRITE: /1 INTAB.
*     ENDLOOP.
*
*     WRITE:  /1 SPACE.
*     WRITE: /1 '--------------------------------------------'.
*     WRITE: /1 '                OUTPUT RECORD               '.
*     WRITE: /1 '--------------------------------------------'.
*     LOOP AT SUMTAB.
*     WRITE: /1 SUMTAB.
*     ENDLOOP.
*STOP.
*end of temp code

  MESSAGE i100(zm) WITH text-100.

*---------------------------------------------------------------------*
*------------------------  OPEN_INPUT_FILE ---------------------------*
*---------------------------------------------------------------------*
FORM open_input_file.

  OPEN DATASET infile FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH infile.
  ENDIF.

ENDFORM.                    "OPEN_INPUT_FILE

*---------------------------------------------------------------------*
*------------------------  BUILD_INTAB_TABLE -------------------------*
*---------------------------------------------------------------------*

FORM build_intab_table.
**start of temp code
*     WRITE: /1 '--------------------------------------------'.
*     WRITE: /1 '                INPUT  RECORD               '.
*     WRITE: /1 '--------------------------------------------'.
*end of temp code

  DO.

****BBCHANGE***********************************
    IF p_fixed = 'X'.
      READ DATASET infile INTO sidoc_rec.
      IF sy-subrc <> '0'.
        EXIT.
      ENDIF.

    ELSE.

      READ DATASET infile INTO lv_input.

      IF sy-subrc <> 0.           "Exit when file is completely read in
        EXIT.
      ENDIF.
      CLEAR t_splits.
      SPLIT lv_input AT delimtr INTO TABLE t_splits.

      LOOP AT t_splits INTO s_splits.
        ASSIGN COMPONENT sy-tabix
               OF STRUCTURE sidoc_rec
               TO <curcol>.
        MOVE s_splits TO <curcol>.
      ENDLOOP.

    ENDIF.

****BBCHANGE***********************************
*    READ DATASET INFILE INTO SIDOC_REC.
*    IF SY-SUBRC <> '0'.
*      EXIT.
*    ENDIF.
**start of temp code
*    WRITE: /1 SIDOC_REC.
*end of temp code

    MOVE-CORRESPONDING sidoc_rec TO intab.
    APPEND intab.
    CLEAR  intab.
  ENDDO.
*  SORT INTAB BY BSART VKORG VTWEG SPART DWERK BZIRK KDGRP KONDA
*                PRSDT MABNR.
*  SORT INTAB BY BSART VKORG VTWEG SPART VKBUR DWERK BZIRK KDGRP KONDA
*       MABNR KVGR1 KDKG1.

* SORT INTAB BY BSART VKORG VTWEG SPART VKBUR DWERK BZIRK   "D30K927430
*                                       KDGRP KONDA KVGR1.  "D30K927430

  SORT intab BY bsart vkorg vtweg spart vkbur dwerk         "D30K927430
                bzirk kdgrp konda kvgr1 kvgr2.              "D30K927430

ENDFORM.                    "BUILD_INTAB_TABLE

*---------------------------------------------------------------------*
*------------------------  BUILD_SUMTAB_TABLE ------------------------*
*---------------------------------------------------------------------*
FORM build_sumtab_table.

  LOOP AT intab.

*    AT FIRST.
*      MOVE 'Y' TO FIRST_REC.
*    ENDAT.
*    CLEAR: KEY_FIELDS1.
*    CONCATENATE INTAB-BSART INTAB-VKORG INTAB-VTWEG INTAB-SPART
*                INTAB-VKBUR INTAB-DWERK INTAB-BZIRK INTAB-KDGRP
*                INTAB-KONDA INTAB-MABNR INTAB-KVGR1 INTAB-KDKG1
*                                              INTO KEY_FIELDS1.
*
*    IF KEY_FIELDS1 = PREV_KEY_FIELDS1.
*      PERFORM ADD_AMOUNT_TO_HELD_REC.
*    ELSE.
*      PERFORM INSERT_ROW_TO_SUMTAB.
*      PERFORM MOVE_DATA_TO_HELD_REC.
*    ENDIF.
*
*    MOVE KEY_FIELDS1 TO PREV_KEY_FIELDS1.
*
*    AT LAST.
*      PERFORM INSERT_ROW_TO_SUMTAB.
*    ENDAT.
*
*  ENDLOOP.
*ENDFORM.                    "BUILD_SUMTAB_TABLE
*
**---------------------------------------------------------------------*
**------------------------ MOVE_DATA_TO_HELD_REC-----------------------*
**---------------------------------------------------------------------*
*FORM MOVE_DATA_TO_HELD_REC.
*  CLEAR: HELD_REC.
*  MOVE-CORRESPONDING INTAB TO HELD_REC.
*  MOVE INTAB-KBETR  TO W_KBETR.
*  MOVE INTAB-KWMENG TO W_KWMENG.
*
*ENDFORM.                    "MOVE_DATA_TO_HELD_REC
**---------------------------------------------------------------------*
**------------------------ ADD_AMOUNT_TO_HELD_REC----------------------*
**---------------------------------------------------------------------*
*FORM ADD_AMOUNT_TO_HELD_REC.
*
*  ADD INTAB-KBETR  TO W_KBETR.
*  MOVE W_KBETR     TO HELD_REC-KBETR.
**$  SHIFT HELD_REC-KBETR RIGHT.
*
*  ADD INTAB-KWMENG TO W_KWMENG.
*  MOVE W_KWMENG    TO HELD_REC-KWMENG.
**$  SHIFT HELD_REC-KWMENG RIGHT.
*ENDFORM.                    "ADD_AMOUNT_TO_HELD_REC

*---------------------------------------------------------------------*
*------------------------ ADD_ROW_TO_SUMTAB---------------------------*
*---------------------------------------------------------------------*
*FORM INSERT_ROW_TO_SUMTAB.

*  IF FIRST_REC <> 'Y'.
*   CONCATENATE INTAB-BSART INTAB-VKORG INTAB-VTWEG         "D30K927430
*               INTAB-SPART INTAB-VKBUR INTAB-DWERK         "D30K927430
*               INTAB-BZIRK INTAB-KDGRP INTAB-KONDA         "D30K927430
*               INTAB-KVGR1 INTO HREC_SKEY.                 "D30K927430

    CONCATENATE intab-bsart intab-vkorg intab-vtweg         "D30K927430
                intab-spart intab-vkbur intab-dwerk         "D30K927430
                intab-bzirk intab-kdgrp intab-konda         "D30K927430
                intab-kvgr1 intab-kvgr2 INTO hrec_skey.     "D30K927430

*    CONCATENATE HELD_REC-BSART HELD_REC-VKORG HELD_REC-VTWEG
*                HELD_REC-SPART HELD_REC-VKBUR HELD_REC-DWERK
*                HELD_REC-BZIRK HELD_REC-KDGRP HELD_REC-KONDA
*                HELD_REC-KVGR1 INTO HREC_SKEY.
**                HELD_REC-PRSDT INTO HREC_SKEY.

    IF sumrec_skey = hrec_skey.
      w_item = last_item + 10.
      MOVE w_rec_number TO sumtab-socount.
    ELSE.
      w_rec_number = w_rec_number + 01.
      MOVE 10           TO: w_item, last_item.
    ENDIF.
    MOVE-CORRESPONDING intab TO sumtab.

    MOVE intab-kbetr  TO w_kbetr.        "NEXT 4 LINES NEW
    MOVE intab-kwmeng TO w_kwmeng.
    MOVE w_kwmeng     TO sumtab-kwmeng.
    MOVE w_kbetr      TO sumtab-kbetr.

*    MOVE-CORRESPONDING HELD_REC TO SUMTAB.
    MOVE w_rec_number TO sumtab-socount.
    MOVE w_item TO: sumtab-item_no, last_item.
    APPEND sumtab.
*   CONCATENATE SUMTAB-BSART SUMTAB-VKORG SUMTAB-VTWEG      "D30K927430
*               SUMTAB-SPART SUMTAB-VKBUR SUMTAB-DWERK      "D30K927430
*               SUMTAB-BZIRK SUMTAB-KDGRP SUMTAB-KONDA      "D30K927430
*               SUMTAB-KVGR1 INTO SUMREC_SKEY.              "D30K927430

    CONCATENATE sumtab-bsart sumtab-vkorg sumtab-vtweg      "D30K927430
                sumtab-spart sumtab-vkbur sumtab-dwerk      "D30K927430
                sumtab-bzirk sumtab-kdgrp sumtab-konda      "D30K927430
                sumtab-kvgr1 sumtab-kvgr2 INTO sumrec_skey. "D30K927430

*                SUMTAB-PRSDT INTO SUMREC_SKEY.
*  ENDIF.
    CLEAR: first_rec, w_kwmeng, w_kbetr.
  ENDLOOP.

ENDFORM.                    "INSERT_ROW_TO_SUMTAB

*---------------------------------------------------------------------*
*--------------------------SEND INBOUND IDOC--------------------------*
*---------------------------------------------------------------------*

FORM send_inbound_idoc.
  PERFORM init_control.
  PERFORM build_idoc.
  PERFORM idoc_send_async TABLES int_edidd USING control_record.
ENDFORM.               "SEND_INBOUND_IDOC

*---------------------------------------------------------------------*
*------------------------------BUILD_IDOC-----------------------------*
*---------------------------------------------------------------------*

FORM build_idoc.
  DATA: header1 TYPE e1edk01,             "Document header general Data
        header2 TYPE z1or05e,             "Doc Header Custom segment
        header3 TYPE e1edk14,             "Doc Hedr Organizational Data
        header4 TYPE e1edk03,             "Doc Header Date Segment
        header5 TYPE e1edka1,             "Doc Hedr Partner Information
        header6 TYPE e1edk02,             "Doc Header reference data
        header7 TYPE e1edk35,             "Doc Header Additional Data
        item1   TYPE e1edp01,             "Doc Item General Data
        item2   TYPE e1edp05,             "Doc Item Conditions
        item3   TYPE e1edp19,             "Doc Item Object Identificatn
        item4   TYPE e1edp35.             "Doc Item Additional Data
  DATA: prev_socount LIKE sumtab-socount VALUE '99999'.

  LOOP AT sumtab.
    IF sumtab-socount > prev_socount.
      PERFORM idoc_send_async TABLES int_edidd USING control_record.
      CLEAR   int_edidd.
      REFRESH int_edidd.
    ENDIF.
    IF sumtab-item_no = '00010'.                            "SDP42280
*    IF SUMTAB-ITEM_NO = '0010'.          "SDP42280
*Start of HEADER1
      header1-curcy = sumtab-curcy.
      header1-autlf = sumtab-autlf.
      header1-augru = sumtab-augru.
*                                    "CONTROL_RECORD-DOCNUM = IDOC #
      PERFORM add_segment
        TABLES int_edidd
         USING control_record-docnum 'E1EDK01' header1.
*End of HEADER1

*Start of HEADER2 (Custom Segment)
      header2-konda = sumtab-konda.
      header2-kdgrp = sumtab-kdgrp.
      header2-bzirk = sumtab-bzirk.
      PERFORM add_segment
        TABLES int_edidd
         USING control_record-docnum 'Z1OR05E' header2.
*End of HEADER2


*Start of HEADER3        (Using Macros)
      DEFINE segment_e1edk14.
        header3-qualf = &1.
        header3-orgid = &2.
        perform add_segment
         tables int_edidd
         using control_record-docnum 'E1EDK14' header3.
      END-OF-DEFINITION.

*ACR-244 D30K927651 G.Ymana Change mapping of segment 6 & 7
      segment_e1edk14:
      '005' sumtab-dwerk, '006' sumtab-spart, '007' sumtab-vtweg,
      '008' sumtab-vkorg, '012' sumtab-bsart, '016' sumtab-vkbur.
*End of HEADER3

*Start of HEADER4        (Using Macros)
      DEFINE segment_e1edk03.
        header4-iddat = &1.
        header4-datum = &2.
        header4-uzeit = sy-uzeit.
        perform add_segment
         tables int_edidd
         using control_record-docnum 'E1EDK03' header4.
      END-OF-DEFINITION.

      segment_e1edk03:
      '012' sumtab-audat, '022' sumtab-audat, '023' sumtab-prsdt,
      '026' sumtab-fkdat, '029' sumtab-audat, '035' sumtab-fkdat.
*End of HEADER4

*Start of HEADER5
      header5-parvw = sumtab-parvw.
      header5-partn = sumtab-partn.
      PERFORM add_segment
        TABLES int_edidd
         USING control_record-docnum 'E1EDKA1' header5.
*End of HEADER5

*Start of HEADER6        (Using Macros)
      DEFINE segment_e1edk02.
        header6-qualf = &1.
        header6-belnr = &2.
        perform add_segment
         tables int_edidd
         using control_record-docnum 'E1EDK02' header6.
      END-OF-DEFINITION.

      segment_e1edk02:
      '001' sumtab-bstkd, '011' sumtab-augru, '017' sumtab-bstkd.
*End of HEADER6

*Start of HEADER7        (Using Macros)
      header7-qualz = '001'.
      header7-cusadd = sumtab-kvgr1.
      PERFORM add_segment
       TABLES int_edidd
       USING control_record-docnum 'E1EDK35' header7.
*End of HEADER7

*Start of HEADER7        (Using Macros) - Customer Group 2  "D30K927430
      IF       ( sumtab-kvgr2    IS NOT INITIAL ).          "D30K927430
        CLEAR                       header7.                "D30K927430
        MOVE     '002'           TO header7-qualz.          "D30K927430
        MOVE     sumtab-kvgr2    TO header7-cusadd.         "D30K927430
        PERFORM  add_segment TABLES int_edidd               "D30K927430
                              USING control_record-docnum   "D30K927430
                                    'E1EDK35'               "D30K927430
                                    header7.                "D30K927430
      ENDIF.                                                "D30K927430
*End of HEADER7                                             "D30K927430

    ENDIF.

*Start of ITEM1
    item1-posex   = sumtab-item_no.
    item1-matnr_external = sumtab-mabnr.
    item1-menge   = sumtab-kwmeng.
    item1-menee   = sumtab-vrkme.
*    ITEM1-VRKME   = SUMTAB-VRKME.
    PERFORM add_segment
      TABLES int_edidd
       USING control_record-docnum 'E1EDP01' item1.
*End of ITEM1

*Start of ITEM2 (Child Segment)
    item2-kschl   = sumtab-kschl.
    item2-betrg   = sumtab-kbetr.
    PERFORM add_segment
      TABLES int_edidd
       USING control_record-docnum 'E1EDP05' item2.
*End of ITEM2

*Start of ITEM3 (Child Segment)
    item3-qualf = '002'.
    item3-idtnr = sumtab-mabnr.
    item3-ktext = 'GAS SALES'.
    PERFORM add_segment
      TABLES int_edidd
       USING control_record-docnum 'E1EDP19' item3.
*End of ITEM3

*Start of ITEM4 (Child Segment)
    item4-qualz  = '006'.
    item4-cusadd = sumtab-kdkg1.
    PERFORM add_segment
     TABLES int_edidd
     USING control_record-docnum 'E1EDP35' item4.
*End of ITEM4

    MOVE sumtab-socount TO prev_socount.
  ENDLOOP.

ENDFORM.                    "build_idoc
*---------------------------------------------------------------------*
*------------------------------ADD_SEGMENT----------------------------*
*---------------------------------------------------------------------*

FORM add_segment
  TABLES int_edidd STRUCTURE edidd
  USING docnum segnam LIKE edidd-segnam
        sdata.
  UNPACK docnum TO int_edidd-docnum.
  int_edidd-segnam = segnam.
  int_edidd-sdata = sdata.
  APPEND int_edidd.
ENDFORM.                    "add_segment
*
*---------------------------------------------------------------------*
*------------------------------INIT_CONTROL --------------------------*
*---------------------------------------------------------------------*

FORM init_control.
  control_record-mestyp = 'ORDERS'.           "Message Type
  control_record-idoctp = 'ORDERS05'.         "Basic type
  control_record-cimtyp = 'ZORD05E'.          "Extension
  UNPACK '1' TO control_record-docnum.        "IDoc number #1

*Specify Name of Current Logged-on System (client in R/3 System)
*For example: East SBX3 = Q02CLNT050
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system             = own_logical_system
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 0.
*
*Note: Both values OWN_LOGICAL_SYSTEM or 'SAPQ02' as sendr port work OK
  control_record-sndprt = 'LS'.                 "Partner type of sender
  control_record-sndprn = 'BANNER'.             "Partner # of Sender
  control_record-sndpor = own_logical_system.   "Sender port-SAP System
* CONTROL_RECORD-SNDPOR = 'SAPQ02'.         .   "Sender port-SAP System
*
*Note: Both values SPACE or 'SAPQ02' as receiver port work OK
  control_record-rcvprt = 'LS'.               "Partner Type of Receiver
  control_record-rcvprn = own_logical_system. "Partner # of Receiver
  control_record-rcvpor = space.              "Receiver port-SAP System
* CONTROL_RECORD-RCVPOR = 'SAPQ02'.           "Receiver port-SAP System
*
ENDFORM.                    "init_control

*---------------------------------------------------------------------*
*--------------------------IDOC_SEND_ASYNC----------------------------*
*---------------------------------------------------------------------*

FORM idoc_send_async
  TABLES int_edidd STRUCTURE edidd                  "Data record (IDoc)
   USING x_edidc STRUCTURE edidc.                "Control record (IDoc)

  DATA: tedi_dc40 TYPE TABLE OF edi_dc40 WITH HEADER LINE.
  DATA: tedi_dd40 TYPE TABLE OF edi_dd40 WITH HEADER LINE.
*
  REFRESH tedi_dd40 .
  LOOP AT int_edidd.
    MOVE-CORRESPONDING int_edidd TO tedi_dd40.
    APPEND tedi_dd40 .
  ENDLOOP.
*
  CALL FUNCTION 'IDOC_CONTROL_OUTBOUND_CONVERT'
    EXPORTING
      control_record   = x_edidc
      port_version     = '4'
    IMPORTING
      control_40       = tedi_dc40
    EXCEPTIONS
      conversion_error = 1
      OTHERS           = 2.
  APPEND tedi_dc40.

  CALL FUNCTION 'IDOC_INBOUND_ASYNCHRONOUS'
    DESTINATION 'NONE'
    TABLES
      idoc_control_rec_40 = tedi_dc40[]            "IDoc Control Record
      idoc_data_rec_40    = tedi_dd40[].              "IDoc Data Record

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    DESTINATION 'NONE'
    EXPORTING
      wait = 'X'.

ENDFORM.                    "idoc_send


*
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
    IF r_local = 'X'.
      IF screen-group1 = 'LCL'.
        screen-input = 1.
      ENDIF.
      IF screen-group1 = 'SRV'.
        screen-input = 0.
      ENDIF.
    ELSE.
      IF screen-group1 = 'LCL'.
        screen-input = 0.
      ENDIF.
      IF screen-group1 = 'SRV'.
        screen-input = 1.
      ENDIF.
    ENDIF.
    "-----------------------
    MODIFY   SCREEN.
  ENDLOOP.
ENDFORM.                    " TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE_PC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_file_pc .
  DATA:   lt_auszug TYPE STANDARD TABLE OF string.
  DATA:   lv_auszug_file TYPE string,
          lv_msg(100),
          lv_sep type char01.

  REFRESH lt_auszug[].
  lv_auszug_file = p_lfile.
  lv_sep = space.
  IF p_fixed is INITIAL.
     lv_sep = 'X'.
  ENDIF.
  CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename            = lv_auszug_file
        has_field_separator = lv_sep  "'X'
        filetype            = 'ASC'
      CHANGING
        data_tab            = intab[]
      EXCEPTIONS
        file_open_error     = 1
        file_read_error     = 2
        OTHERS              = 18.
  CASE sy-subrc.
    WHEN 1.
      MESSAGE e100(zm) WITH 'Unable to open' p_lfile '' ''.

    WHEN 2 OR 18.
      MESSAGE e100(zm) WITH 'Read error ' p_lfile '' ''.
  ENDCASE.
  "-----------
  SORT intab BY bsart vkorg vtweg spart vkbur dwerk
               bzirk kdgrp konda kvgr1 kvgr2.
ENDFORM.                    " UPLOAD_FILE_PC
