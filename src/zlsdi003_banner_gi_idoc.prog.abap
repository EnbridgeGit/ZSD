*&---------------------------------------------------------------------*
*& Report  ZLSDI003_BANNER_GI_IDOC
*&
*&---------------------------------------------------------------------*
*&**********************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        November 2010.                                         *
*  Project:     Cost of Gas.                                           *
*  Issue Log:   TR804                                                  *
*  Description:                                                        *
*     - The purpose of this program is to move translated data to IDOC *
*       segments and create IDOC Basic Type MBGMCR02.                  *
*                                                                      *
*&---------------------------------------------------------------------*
*CHANGES****                                                           *
*&5/12/2022   DADIM  D30K932173   CHG0247111 - Quantity field increased*
*                                                          to 18 digits*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

REPORT  zlsdi003_banner_gi_idoc MESSAGE-ID zm.

* Intput file format
DATA: BEGIN OF ogi_rec,
      budat	            TYPE d,     "POSTNG DATE
      bldat	            TYPE d,     "DOCUMENT DATE
      xblnr(16)           TYPE c,     "REF DOC NO
      frbnr(16)           TYPE c,	    "BILL OF LADING
      u_name(12)          TYPE c,     "PR UNAME
      bktxt(25)           TYPE c,     "HEADER TEXT
      matnr(18)           TYPE c,     "MATERIAL
      werks_d(4)          TYPE c,     "PLANT
      lgort_d(4)        TYPE c,     "STORAGE LOC
      bwart(3)            TYPE c,     "MOVEMENT TYPE
*Start of changes by DADIM for CHG0247111
*      ERFMG(13)           TYPE C,     "ENTRY QNT
      erfmg(18)           TYPE c,     "QTY
*End of changes by DADIM for CHG0247111
      erfme(3)            TYPE c,     "ENTRY UOM
      isocd(3)            TYPE c,     "ENTRY UOM ISO
      sgtxt(30)           TYPE c,	    "ITEM TEXT
      aufnr(12)           TYPE c,	    "ORDER ID
      kostl(10)           TYPE c,     "Cost Center
      saknr(10)           TYPE c.     "GL ACCOUNT
DATA: END OF ogi_rec.

*IDOC related working data.
DATA: control_record LIKE edidc,                   "Control record-IDoc
      own_logical_system TYPE tbdls-logsys,             "Logical system
      int_edidd TYPE TABLE OF edidd WITH HEADER LINE. "Data record-IDoc


*------------------------  Selection Screen  ---------------------------
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
PARAMETER:
infile LIKE filenameci-fileextern OBLIGATORY.
* DEFAULT '/usr/sap/interfaces/D30/BANNER/zbannowngi.dat' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK box.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3)
              '/BANNER/zbannowngi.dat' INTO infile.

*---------------------------------------------------------------------*
*------------------------  Start of selection ------------------------*
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM open_input_file.
  PERFORM send_inbound_idoc.

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
*--------------------------SEND INBOUND IDOC--------------------------*
*---------------------------------------------------------------------*

FORM send_inbound_idoc.
  PERFORM init_control.
  PERFORM build_idoc.
*  PERFORM IDOC_SEND_ASYNC TABLES INT_EDIDD USING CONTROL_RECORD.
ENDFORM.               "SEND_INBOUND_IDOC

*---------------------------------------------------------------------*
*------------------------------BUILD_IDOC-----------------------------*
*---------------------------------------------------------------------*

FORM build_idoc.
  DATA: header1 TYPE e1bp2017_gm_head_01,     "Material Doc Header Data
        header2 TYPE e1bp2017_gm_code,        "MMIM: New Key Assignment
        item1   TYPE e1bp2017_gm_item_create. "Material Document Item
*  DATA: PREV_SOCOUNT LIKE SUMTAB-SOCOUNT VALUE '9999'.

  DO.
    READ DATASET infile INTO ogi_rec.
    IF sy-subrc <> '0'.
      EXIT.
    ENDIF.
*Start of HEADER1
    CLEAR header1.
    header1-pstng_date     = ogi_rec-budat.
    header1-doc_date       = ogi_rec-bldat.
    header1-ref_doc_no     = ogi_rec-xblnr.
    header1-bill_of_lading = ogi_rec-frbnr.
    header1-pr_uname       = ogi_rec-u_name.
    header1-header_txt     = ogi_rec-bktxt.
*                                    "CONTROL_RECORD-DOCNUM = IDOC #
    PERFORM add_segment
      TABLES int_edidd
       USING control_record-docnum 'E1BP2017_GM_HEAD_01' header1.
*End of HEADER1

*Start of HEADER2
    CLEAR header2.
    header2-gm_code  = '03'.

    PERFORM add_segment
      TABLES int_edidd
       USING control_record-docnum 'E1BP2017_GM_CODE' header2.
*End of HEADER2

*Start of ITEM1
    CLEAR item1.
    item1-material      = ogi_rec-matnr.
    item1-plant         = ogi_rec-werks_d.
    item1-stge_loc      = ogi_rec-lgort_d.
    item1-move_type     = ogi_rec-bwart.
    item1-entry_qnt     = ogi_rec-erfmg.
    item1-entry_uom     = ogi_rec-erfme.
    item1-entry_uom_iso = ogi_rec-isocd.
    item1-item_text     = ogi_rec-sgtxt.
    item1-orderid       = ogi_rec-aufnr.
    item1-costcenter    = ogi_rec-kostl.
    item1-gl_account    = ogi_rec-saknr.

    PERFORM add_segment
      TABLES int_edidd
       USING control_record-docnum 'E1BP2017_GM_ITEM_CREATE' item1.
*End of ITEM1
    PERFORM idoc_send_async TABLES int_edidd USING control_record.
    CLEAR int_edidd.
    REFRESH int_edidd.
  ENDDO.

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
  control_record-mestyp = 'MBGMCR'.           "Message Type
  control_record-idoctp = 'MBGMCR02'.         "Basic type
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
