*&--------------------------------------------------------------------*
*& Report  ZLSDI007_BANNER_RR_IDOC
*&--------------------------------------------------------------------*
*&*********************************************************************
*  Author:      Mohammad T. Khan                                      *
*  Date:        April 2011.                                           *
*  Project:     Cost of Gas.                                          *
*  Issue Log:   TR804                                                 *
*  Description:                                                       *
*     - The purpose of this program is to use the data file created   *
*       by program ZLSDC007, move data to IDOC segments and create    *
*       IDOC Basic Type ORDERS05 Extension ZORD05E                    *
*                                                                     *
*&--------------------------------------------------------------------*
*CHANGES****                                                          *
*& 2013/02/22 - Removed all references to SOCOUNT. Using SOCOUNT, pgm *
*&     gymana   could only handle up to 9999 sales order idocs. We now*
*&   SDP41538   have files going thru larger than 10,000 recs. The pgm*
*&              was generating one idoc per order anyways so SOCOUNT  *
*&              was unnecessary.                                      *
*& 12/04/2016 GYMANA D30K927651 ACR-244 - Segment E1EDK14 Bug fix     *
*&--------------------------------------------------------------------*
*&--------------------------------------------------------------------*

REPORT  ZLSDI007_BANNER_RR_IDOC LINE-SIZE 170.
* Intput file format
DATA:  BEGIN OF SIDOC_REC,
  AUDAT   TYPE D, "Document Date
  FKDAT   TYPE D, "Billing Date
  CURCY(3)  TYPE C, "Order Currency
  BSART(4)  TYPE C,   "Sales Document Type
  AUTLF(1)  TYPE C,   "Complete Delivery Indicator
  VKORG(4)  TYPE C,       "Sales Organization
  VTWEG(2)  TYPE C,   "Distribution Channel
  SPART(2)  TYPE C,   "Division
  PARVW(3)  TYPE C,   "Sold-To-Party (Customer)
  PARTN(17) TYPE C,   "Customer Number
  BSTKD(35) TYPE C,   "PO Number
  DWERK(4)  TYPE C,   "Delivering Plant
  VKBUR(4)  TYPE C,   "Sales Office
  AUGRU(3)  TYPE C,   "Order Reason
  KONDA(2)  TYPE C,   "Price Group
  KDGRP(2)  TYPE C,   "Customer Group
 	PRSDT	 	TYPE D,	"Pricing Date
       KVGR1(3)      TYPE C,      "Customer Group 1
  BZIRK(4)  TYPE C,   "Sales District
  MABNR(18) TYPE C,   "Material Number
       KWMENG(15) TYPE C,   "Order Quantity
  VRKME(3)  TYPE C,   "Unit of Measure
  KSCHL(4)  TYPE C,       "Condition Type
  KBETR(11) TYPE C, "Condition Amount
       KDKG1(2)      TYPE C.      "Condition Group 1
DATA:  END OF SIDOC_REC.

*DATA:
*    BEGIN OF INTAB OCCURS 0.
*        INCLUDE STRUCTURE SIDOC_REC AS SREC.
*DATA: END OF INTAB.

DATA:
    BEGIN OF SUMTAB OCCURS 0,
*        SOCOUNT(4)  TYPE C,        "Sale Order Counter         SDP41538
        ITEM_NO(4)  TYPE C.       "Item Number
        INCLUDE STRUCTURE SIDOC_REC AS SREC.
DATA: END OF SUMTAB.

DATA: W_REC_NUMBER(4) TYPE N,
      W_ITEM(4)       TYPE N VALUE '10'.
*IDOC related working data.
DATA: CONTROL_RECORD LIKE EDIDC,                   "Control record-IDoc
      OWN_LOGICAL_SYSTEM TYPE TBDLS-LOGSYS,             "Logical system
      INT_EDIDD TYPE TABLE OF EDIDD WITH HEADER LINE. "Data record-IDoc


*------------------------  Selection Screen  -------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETER:
INFILE LIKE FILENAMECI-FILEEXTERN OBLIGATORY. " DEFAULT
SELECTION-SCREEN END OF BLOCK BOX.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
            '/BANNER/zbannrrorderidoc.dat' INTO INFILE.

*---------------------------------------------------------------------*
*------------------------  Start of selection ------------------------*
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM OPEN_INPUT_FILE.
*  PERFORM BUILD_INTAB_TABLE.
  PERFORM BUILD_SUMTAB_TABLE.
  PERFORM SEND_INBOUND_IDOC.

  MESSAGE I100(ZM) WITH TEXT-100.

*---------------------------------------------------------------------*
*------------------------  OPEN_INPUT_FILE ---------------------------*
*---------------------------------------------------------------------*
FORM OPEN_INPUT_FILE.

  OPEN DATASET INFILE FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC NE 0.
    MESSAGE E006(ZM) WITH INFILE.
  ENDIF.

ENDFORM.                    "OPEN_INPUT_FILE

*---------------------------------------------------------------------*
*------------------------  BUILD_SUMTAB_TABLE ------------------------*
*---------------------------------------------------------------------*

FORM BUILD_SUMTAB_TABLE.

  DO.
    READ DATASET INFILE INTO SIDOC_REC.
    IF SY-SUBRC <> '0'.
      EXIT.
    ENDIF.

    MOVE-CORRESPONDING SIDOC_REC TO SUMTAB.
*    W_REC_NUMBER = W_REC_NUMBER + 01.                         "SDP41538
*    MOVE W_REC_NUMBER TO SUMTAB-SOCOUNT.                      "SDP41538
    MOVE W_ITEM       TO SUMTAB-ITEM_NO.
    APPEND SUMTAB.
    CLEAR  SUMTAB.
  ENDDO.
*  SORT SUMTAB BY SOCOUNT BSART VKORG VTWEG SPART DWERK BZIRK  "SDP41538
*                 KDGRP KONDA PRSDT MABNR.                     "SDP41538
  SORT SUMTAB BY BSART VKORG VTWEG SPART DWERK BZIRK KDGRP     "SDP41538
                 KONDA PRSDT MABNR.                            "SDP41538

ENDFORM.                    "BUILD_INTAB_TABLE

*---------------------------------------------------------------------*
*------------------------  BUILD_SUMTAB_TABLE ------------------------*
*---------------------------------------------------------------------*
*FORM BUILD_SUMTAB_TABLE.
*
*  LOOP AT INTAB.
*    MOVE-CORRESPONDING INTAB TO SUMTAB.
*    W_ITEM = W_ITEM + 10.
*    MOVE W_REC_NUMBER TO SUMTAB-SOCOUNT.
*    W_REC_NUMBER = W_REC_NUMBER + 01.
*
*    MOVE W_REC_NUMBER TO SUMTAB-SOCOUNT.
*    MOVE W_ITEM       TO SUMTAB-ITEM_NO.
*    APPEND SUMTAB.
*    CLEAR  SUMTAB.
*  ENDLOOP.
*ENDFORM.                    "BUILD_SUMTAB_TABLE

*---------------------------------------------------------------------*
*--------------------------SEND INBOUND IDOC--------------------------*
*---------------------------------------------------------------------*

FORM SEND_INBOUND_IDOC.
  PERFORM INIT_CONTROL.
  PERFORM BUILD_IDOC.
*  PERFORM IDOC_SEND_ASYNC TABLES INT_EDIDD                    "SDP41538
*    USING CONTROL_RECORD.                                     "SDP41538
ENDFORM.               "SEND_INBOUND_IDOC

*---------------------------------------------------------------------*
*------------------------------BUILD_IDOC-----------------------------*
*---------------------------------------------------------------------*

FORM BUILD_IDOC.
  DATA: HEADER1 TYPE E1EDK01,             "Document header general Data
        HEADER2 TYPE Z1OR05E,             "Doc Header Custom segment
        HEADER3 TYPE E1EDK14,             "Doc Hedr Organizational Data
        HEADER4 TYPE E1EDK03,             "Doc Header Date Segment
        HEADER5 TYPE E1EDKA1,             "Doc Hedr Partner Information
        HEADER6 TYPE E1EDK02,             "Doc Header reference data
        HEADER7 TYPE E1EDK35,             "Doc Header Additional Data
        ITEM1   TYPE E1EDP01,             "Doc Item General Data
        ITEM2   TYPE E1EDP05,             "Doc Item Conditions
        ITEM3   TYPE E1EDP19,             "Doc Item Object Identificatn
        ITEM4   TYPE E1EDP35.             "Doc Item Additional Data
*  DATA: PREV_SOCOUNT LIKE SUMTAB-SOCOUNT VALUE '9999'.        "SDP41538

  LOOP AT SUMTAB.
* Moved to end of loop - gymana (SDP41538)
*    IF SUMTAB-SOCOUNT > PREV_SOCOUNT.
*      PERFORM IDOC_SEND_ASYNC TABLES INT_EDIDD USING CONTROL_RECORD.
*      CLEAR   INT_EDIDD.
*      REFRESH INT_EDIDD.
*    ENDIF.
    IF SUMTAB-ITEM_NO = '0010'.
*Start of HEADER1
      HEADER1-CURCY = SUMTAB-CURCY.
      HEADER1-AUTLF = SUMTAB-AUTLF.
      HEADER1-AUGRU = SUMTAB-AUGRU.
*                                    "CONTROL_RECORD-DOCNUM = IDOC #
      PERFORM ADD_SEGMENT
        TABLES INT_EDIDD
         USING CONTROL_RECORD-DOCNUM 'E1EDK01' HEADER1.
*End of HEADER1

*Start of HEADER2 (Custom Segment)
      HEADER2-KONDA = SUMTAB-KONDA.
      HEADER2-KDGRP = SUMTAB-KDGRP.
      HEADER2-BZIRK = SUMTAB-BZIRK.
      PERFORM ADD_SEGMENT
        TABLES INT_EDIDD
         USING CONTROL_RECORD-DOCNUM 'Z1OR05E' HEADER2.
*End of HEADER2


*Start of HEADER3        (Using Macros)
      DEFINE SEGMENT_E1EDK14.
        HEADER3-QUALF = &1.
        HEADER3-ORGID = &2.
        PERFORM ADD_SEGMENT
         TABLES INT_EDIDD
         USING CONTROL_RECORD-DOCNUM 'E1EDK14' HEADER3.
      END-OF-DEFINITION.

* ACR-244 D30K927651 Change Segment 6 & 7 mapping
      SEGMENT_E1EDK14:
      '005' SUMTAB-DWERK, '006' SUMTAB-SPART, '007' SUMTAB-VTWEG,
      '008' SUMTAB-VKORG, '012' SUMTAB-BSART, '016' SUMTAB-VKBUR.
*End of HEADER3

*Start of HEADER4        (Using Macros)
      DEFINE SEGMENT_E1EDK03.
        HEADER4-IDDAT = &1.
        HEADER4-DATUM = &2.
        HEADER4-UZEIT = SY-UZEIT.
        PERFORM ADD_SEGMENT
         TABLES INT_EDIDD
         USING CONTROL_RECORD-DOCNUM 'E1EDK03' HEADER4.
      END-OF-DEFINITION.

      SEGMENT_E1EDK03:
      '012' SUMTAB-AUDAT, '022' SUMTAB-AUDAT, '023' SUMTAB-PRSDT,
      '026' SUMTAB-FKDAT, '029' SUMTAB-AUDAT, '035' SUMTAB-FKDAT.
*End of HEADER4

*Start of HEADER5
      HEADER5-PARVW = SUMTAB-PARVW.
      HEADER5-PARTN = SUMTAB-PARTN.
      PERFORM ADD_SEGMENT
        TABLES INT_EDIDD
         USING CONTROL_RECORD-DOCNUM 'E1EDKA1' HEADER5.
*End of HEADER5

*Start of HEADER6        (Using Macros)
      DEFINE SEGMENT_E1EDK02.
        HEADER6-QUALF = &1.
        HEADER6-BELNR = &2.
        PERFORM ADD_SEGMENT
         TABLES INT_EDIDD
         USING CONTROL_RECORD-DOCNUM 'E1EDK02' HEADER6.
      END-OF-DEFINITION.

      SEGMENT_E1EDK02:
      '001' SUMTAB-BSTKD, '011' SUMTAB-AUGRU, '017' SUMTAB-BSTKD.
*End of HEADER6

*Start of HEADER7        (Using Macros)
        HEADER7-QUALZ = '001'.
        HEADER7-CUSADD = SUMTAB-KVGR1.
        PERFORM ADD_SEGMENT
         TABLES INT_EDIDD
         USING CONTROL_RECORD-DOCNUM 'E1EDK35' HEADER7.
*End of HEADER7
    ENDIF.

*Start of ITEM1
    ITEM1-POSEX   = SUMTAB-ITEM_NO.
    ITEM1-MATNR_EXTERNAL = SUMTAB-MABNR.
    ITEM1-MENGE   = SUMTAB-KWMENG.
    ITEM1-MENEE   = SUMTAB-VRKME.
*    ITEM1-VRKME   = SUMTAB-VRKME.
    PERFORM ADD_SEGMENT
      TABLES INT_EDIDD
       USING CONTROL_RECORD-DOCNUM 'E1EDP01' ITEM1.
*End of ITEM1

*Start of ITEM2 (Child Segment)
    ITEM2-KSCHL   = SUMTAB-KSCHL.
    ITEM2-BETRG   = SUMTAB-KBETR.
    PERFORM ADD_SEGMENT
      TABLES INT_EDIDD
       USING CONTROL_RECORD-DOCNUM 'E1EDP05' ITEM2.
*End of ITEM2

*Start of ITEM3 (Child Segment)
    ITEM3-QUALF = '002'.
    ITEM3-IDTNR = SUMTAB-MABNR.
    ITEM3-KTEXT = 'GAS SALES'.
    PERFORM ADD_SEGMENT
      TABLES INT_EDIDD
       USING CONTROL_RECORD-DOCNUM 'E1EDP19' ITEM3.
*End of ITEM3

*Start of ITEM4 (Child Segment)
    ITEM4-QUALZ  = '006'.
    ITEM4-CUSADD = SUMTAB-KDKG1.
        PERFORM ADD_SEGMENT
         TABLES INT_EDIDD
         USING CONTROL_RECORD-DOCNUM 'E1EDP35' ITEM4.
*End of ITEM4

*    MOVE SUMTAB-SOCOUNT TO PREV_SOCOUNT.                      "SDP41538

    PERFORM IDOC_SEND_ASYNC TABLES INT_EDIDD                   "SDP41538
      USING CONTROL_RECORD.                                    "SDP41538
    CLEAR   INT_EDIDD.                                         "SDP41538
    REFRESH INT_EDIDD.                                         "SDP41538
  ENDLOOP.

ENDFORM.                    "build_idoc
*---------------------------------------------------------------------*
*------------------------------ADD_SEGMENT----------------------------*
*---------------------------------------------------------------------*

FORM ADD_SEGMENT
  TABLES INT_EDIDD STRUCTURE EDIDD
  USING DOCNUM SEGNAM LIKE EDIDD-SEGNAM
        SDATA.
  UNPACK DOCNUM TO INT_EDIDD-DOCNUM.
  INT_EDIDD-SEGNAM = SEGNAM.
  INT_EDIDD-SDATA = SDATA.
  APPEND INT_EDIDD.
ENDFORM.                    "add_segment
*
*---------------------------------------------------------------------*
*------------------------------INIT_CONTROL --------------------------*
*---------------------------------------------------------------------*

FORM INIT_CONTROL.
  CONTROL_RECORD-MESTYP = 'ORDERS'.           "Message Type
  CONTROL_RECORD-IDOCTP = 'ORDERS05'.         "Basic type
  CONTROL_RECORD-CIMTYP = 'ZORD05E'.          "Extension
  UNPACK '1' TO CONTROL_RECORD-DOCNUM.        "IDoc number #1

*Specify Name of Current Logged-on System (client in R/3 System)
*For example: East SBX3 = Q02CLNT050
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      OWN_LOGICAL_SYSTEM             = OWN_LOGICAL_SYSTEM
    EXCEPTIONS
      OWN_LOGICAL_SYSTEM_NOT_DEFINED = 1
      OTHERS                         = 0.
*
*Note: Both values OWN_LOGICAL_SYSTEM or 'SAPQ02' as sendr port work OK
  CONTROL_RECORD-SNDPRT = 'LS'.                 "Partner type of sender
  CONTROL_RECORD-SNDPRN = 'BANNER'.             "Partner # of Sender
  CONTROL_RECORD-SNDPOR = OWN_LOGICAL_SYSTEM.   "Sender port-SAP System
* CONTROL_RECORD-SNDPOR = 'SAPQ02'.         .   "Sender port-SAP System
*
*Note: Both values SPACE or 'SAPQ02' as receiver port work OK
  CONTROL_RECORD-RCVPRT = 'LS'.               "Partner Type of Receiver
  CONTROL_RECORD-RCVPRN = OWN_LOGICAL_SYSTEM. "Partner # of Receiver
  CONTROL_RECORD-RCVPOR = SPACE.              "Receiver port-SAP System
* CONTROL_RECORD-RCVPOR = 'SAPQ02'.           "Receiver port-SAP System
*
ENDFORM.                    "init_control

*---------------------------------------------------------------------*
*--------------------------IDOC_SEND_ASYNC----------------------------*
*---------------------------------------------------------------------*

FORM IDOC_SEND_ASYNC
  TABLES INT_EDIDD STRUCTURE EDIDD                  "Data record (IDoc)
   USING X_EDIDC STRUCTURE EDIDC.                "Control record (IDoc)

  DATA: TEDI_DC40 TYPE TABLE OF EDI_DC40 WITH HEADER LINE.
  DATA: TEDI_DD40 TYPE TABLE OF EDI_DD40 WITH HEADER LINE.
*
  REFRESH TEDI_DD40 .
  LOOP AT INT_EDIDD.
    MOVE-CORRESPONDING INT_EDIDD TO TEDI_DD40.
    APPEND TEDI_DD40 .
  ENDLOOP.
*
  CALL FUNCTION 'IDOC_CONTROL_OUTBOUND_CONVERT'
    EXPORTING
      CONTROL_RECORD   = X_EDIDC
      PORT_VERSION     = '4'
    IMPORTING
      CONTROL_40       = TEDI_DC40
    EXCEPTIONS
      CONVERSION_ERROR = 1
      OTHERS           = 2.
  APPEND TEDI_DC40.

  CALL FUNCTION 'IDOC_INBOUND_ASYNCHRONOUS'
    DESTINATION 'NONE'
    TABLES
      IDOC_CONTROL_REC_40 = TEDI_DC40[]            "IDoc Control Record
      IDOC_DATA_REC_40    = TEDI_DD40[].              "IDoc Data Record

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    DESTINATION 'NONE'
    EXPORTING
      WAIT = 'X'.

ENDFORM.                    "idoc_send


*
