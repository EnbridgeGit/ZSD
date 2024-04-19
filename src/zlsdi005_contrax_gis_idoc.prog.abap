*&--------------------------------------------------------------------*
*& Report  ZLSDI005_CONTRAX_GIS_IDOC
*&
*&--------------------------------------------------------------------*
*&*********************************************************************
*  Author:      Mohammad T. Khan                                      *
*  Date:        Frbruary 2011.                                        *
*  Project:     Cost of Gas.                                          *
*  Issue Log:   TR804                                                 *
*  Description:                                                       *
*     - The purpose of this program is to move translated data to IDOC*
*       segments and create IDOC Basic Type MBGMCR02.                 *
*                                                                     *
*&--------------------------------------------------------------------*
*CHANGES****                                                          *
*&                                                                    *
*&--------------------------------------------------------------------*
*&--------------------------------------------------------------------*

REPORT  ZLSDI005_CONTRAX_GIS_IDOC MESSAGE-ID ZM.

* Intput file format
DATA: BEGIN OF gsg_rec,
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
      erfmg(13)           TYPE c,     "ENTRY QNT
      erfme(3)            TYPE c,     "ENTRY UOM
      isocd(3)            TYPE c,     "ENTRY UOM ISO
      sgtxt(30)           TYPE c,	    "ITEM TEXT
      kostl(19)           TYPE c,     "COST CENTER
      saknr(10)           TYPE c,	    "GL ACCOUNT
      exbwr(13)           TYPE c.     "AMOUNT IN LC
DATA: END OF gsg_rec.




****BBCHANGE***********************************
DATA:   lv_input(400) TYPE          c,
        s_splits      TYPE          string,
        t_splits      LIKE TABLE OF s_splits.

FIELD-SYMBOLS:  <curcol>      TYPE          ANY.

CONSTANTS:  delimtr(1) TYPE c VALUE
cl_abap_char_utilities=>horizontal_tab.
****BBCHANGE***********************************



*IDOC related working data.
DATA: control_record LIKE edidc,                   "Control record-IDoc
      own_logical_system TYPE tbdls-logsys,             "Logical system
      int_edidd TYPE TABLE OF edidd WITH HEADER LINE. "Data record-IDoc


*------------------------  Selection Screen  --------------------------
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
PARAMETER:
infile LIKE filenameci-fileextern OBLIGATORY.
****BBCHANGE***********************************
PARAMETERS: p_ctrx   RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_ctrxst RADIOBUTTON GROUP rad1.
****BBCHANGE***********************************
SELECTION-SCREEN END OF BLOCK box.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3)
              '/IFFI061/zcontraxfuelidoc.dat' INTO infile.

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

****BBCHANGE***********************************
    IF p_ctrx = 'X'.
      READ DATASET infile INTO gsg_rec.
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
               OF STRUCTURE gsg_rec
               TO <curcol>.
        MOVE s_splits TO <curcol>.
      ENDLOOP.

    ENDIF.

****BBCHANGE***********************************
*    READ DATASET INFILE INTO GSG_REC. "BBOrig
*    IF SY-SUBRC <> '0'. "BBOrig
*     EXIT. "BBOrig
*    ENDIF. "BBOrig




*Start of HEADER1
    CLEAR header1.
    header1-pstng_date     = gsg_rec-budat.
    header1-doc_date       = gsg_rec-bldat.
    header1-ref_doc_no     = gsg_rec-xblnr.
    header1-bill_of_lading = gsg_rec-frbnr.
    header1-pr_uname       = gsg_rec-u_name.
    header1-header_txt     = gsg_rec-bktxt.
*                                    "CONTROL_RECORD-DOCNUM = IDOC #
    PERFORM add_segment
      TABLES int_edidd
       USING control_record-docnum 'E1BP2017_GM_HEAD_01' header1.
*End of HEADER1

*Start of HEADER2
    CLEAR header2.
    header2-gm_code  = '01'.

    PERFORM add_segment
      TABLES int_edidd
       USING control_record-docnum 'E1BP2017_GM_CODE' header2.
*End of HEADER2

*Start of ITEM1
    CLEAR item1.
    item1-material      = gsg_rec-matnr.
    item1-plant         = gsg_rec-werks_d.
    item1-stge_loc      = gsg_rec-lgort_d.
    item1-move_type     = gsg_rec-bwart.
    item1-entry_qnt     = gsg_rec-erfmg.
    item1-entry_uom     = gsg_rec-erfme.
    item1-entry_uom_iso = gsg_rec-isocd.
    item1-item_text     = gsg_rec-sgtxt.
    item1-costcenter    = gsg_rec-kostl.
    item1-gl_account    = gsg_rec-saknr.
    item1-amount_lc     = gsg_rec-exbwr.

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
  IF p_ctrx = 'X'.
    "Contrax send port.
    control_record-sndprn = 'CTRXGS'.             "Partner # of Sender
  else.
    "ST send port.
    control_record-sndprn = 'CTRXST'.             "Partner # of Sender
  endif.
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
