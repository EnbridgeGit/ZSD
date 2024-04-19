*&---------------------------------------------------------------------*
*& Report  ZLSDI009_CONTRAX_RR_IDOC                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&**********************************************************************
*  Author:      Glenn Ymana                                            *
*  Date:        May 26, 2016                                           *
*  Project:     Contrax Rate Splitting                                 *
*  Issue Log:   ACR244                                                 *
*  Description:                                                        *
*     - The purpose of this program is to fill out data in the data    *
*       segments of IDOC Basic Type ORDERS05 Extension ZORD05E using   *
*       the file created by program ZLSDC015_CONTRAX_RR_TRANSLATE.     *
*                                                                      *
*&---------------------------------------------------------------------*
*CHANGES****                                                           *
*& 12/04/2016 GYMANA D30K927651 ACR-244  - Segment E1EDK14 Bug fix     *
*&                                                                     *
*& 03/17/2017 GYMANA D30K928077 ACR-3950 - Add PRSDT as header key     *
*&                                            field.                   *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

REPORT  ZLSDI009_CONTRAX_RR_IDOC LINE-SIZE 260.

* Intput file format
DATA:  BEGIN OF IDOCREC,
        AUDAT                 TYPE D,  "Document Date
        FKDAT                 TYPE D,  "Billing Date
        CURCY(3)              TYPE C,    "Order Currency
        BSART(4)              TYPE C,    "Sales Document Type
        AUTLF(1)              TYPE C,    "Complete Delivery Indicator
        VKORG(4)              TYPE C,    "Sales Organization
        VTWEG(2)              TYPE C,    "Distribution Channel
        SPART(2)              TYPE C,    "Division
        PARVW(3)              TYPE C,    "Sold-To-Party (Customer)
        PARTN(17)             TYPE C,    "Customer Number
        ZUONR(18)             TYPE C,    "Assignment
        BSTKD(35)             TYPE C,    "PO Number
        DWERK(4)              TYPE C,    "Delivering Plant
        VKBUR(4)              TYPE C,    "Sales Office
        AUGRU(3)              TYPE C,    "Order Reason
        KONDA(2)              TYPE C,    "Price Group
        KDGRP(2)              TYPE C,  "Customer Group
        PRSDT                 TYPE D,  "Pricing Date
        KVGR1(3)              TYPE C,    "Customer Group 1
        BZIRK(4)              TYPE C,  "Sales District
*        KSCHDR(4)             TYPE C,    "Header Condition
*        KBETHDR(11)           TYPE C,    "Header Condition Amount
   MABNR(18)           TYPE C,  "Material Number
   KWMENG(15)          TYPE C,  "Order Quantity
   VRKME(3)          TYPE C,  "Unit of Measure
   KSCHL(4)          TYPE C,     "Condition Type
        KBETR(11)          TYPE C,  "Condition Amount
        KDKG1(2)              TYPE C.    "Material Pricing Group
DATA: END OF IDOCREC.

DATA:
    BEGIN OF INTAB OCCURS 0,
        PARTN(17)             TYPE C,    "Customer Number
        BSART(4)              TYPE C,    "Sales Document Type
        DWERK(4)              TYPE C,    "Delivering Plant
        VKBUR(4)              TYPE C,    "Sales Office
        AUGRU(3)              TYPE C,    "Order Reason
        ZUONR(18)             TYPE C,    "Assignment
        KONDA(2)              TYPE C,    "Price Group
        KDGRP(2)              TYPE C,  "Customer Group
        MABNR(18)             TYPE C,  "Material Number
        KVGR1(3)              TYPE C,    "Customer Group 1
        KDKG1(2)              TYPE C,    "Material Pricing Group
        KBETR                 LIKE BSEG-WRBTR,   "Amount
        KWMENG                LIKE BSEG-MENGE, 	"Quantity
        AUDAT                 TYPE D,  "Document Date
        FKDAT                 TYPE D,  "Billing Date
        CURCY(3)              TYPE C,    "Order Currency
        AUTLF(1)              TYPE C,    "Complete Delivery Indicator
        VKORG(4)              TYPE C,    "Sales Organization
        VTWEG(2)              TYPE C,    "Distribution Channel
        SPART(2)              TYPE C,    "Division
        PARVW(3)              TYPE C,    "Sold-To-Party (Customer)
        BSTKD(35)             TYPE C,    "PO Number
        PRSDT                 TYPE D,  "Pricing Date
        BZIRK(4)              TYPE C,  "Sales District
        VRKME(3)              TYPE C,  "Unit of Measure
        KSCHL(4)              TYPE C,     "Condition Type
    END OF INTAB.

DATA:
    BEGIN OF SUMTAB OCCURS 0,
        PARTN(17)             TYPE C,    "Customer Number
        BSART(4)              TYPE C,    "Sales Document Type
        DWERK(4)              TYPE C,    "Delivering Plant
        VKBUR(4)              TYPE C,    "Sales Office
        AUGRU(3)              TYPE C,    "Order Reason
        ZUONR(18)             TYPE C,    "Assignment
        KONDA(2)              TYPE C,    "Price Group
        KDGRP(2)              TYPE C,  "Customer Group
        KVGR1(3)              TYPE C,    "Customer Group 1
        KDKG1(2)              TYPE C,    "Material Pricing Group
        MABNR(18)             TYPE C,  "Material Number
        KBETR                 LIKE BSEG-WRBTR,   "Amount
        KWMENG                LIKE BSEG-MENGE, 	"Quantity
        AUDAT                 TYPE D,  "Document Date
        FKDAT                 TYPE D,  "Billing Date
        CURCY(3)              TYPE C,    "Order Currency
        AUTLF(1)              TYPE C,    "Complete Delivery Indicator
        VKORG(4)              TYPE C,    "Sales Organization
        VTWEG(2)              TYPE C,    "Distribution Channel
        SPART(2)              TYPE C,    "Division
        PARVW(3)              TYPE C,    "Sold-To-Party (Customer)
        BSTKD(35)             TYPE C,    "PO Number
        PRSDT                 TYPE D,  "Pricing Date
        BZIRK(4)              TYPE C,  "Sales District
        VRKME(3)              TYPE C,  "Unit of Measure
        KSCHL(4)              TYPE C,     "Condition Type
      END OF SUMTAB.
*        INCLUDE STRUCTURE INTAB.
*        INCLUDE STRUCTURE INTAB AS SREC.
*DATA: END OF SUMTAB.

DATA: INTAB_LINE  LIKE LINE OF INTAB,
      SUMTAB_LINE LIKE LINE OF SUMTAB.

DATA: W_ITEMNO(6) TYPE N.
*IDOC related working data.
DATA: CONTROL_RECORD LIKE EDIDC,                   "Control record-IDoc
      OWN_LOGICAL_SYSTEM TYPE TBDLS-LOGSYS,             "Logical system
      INT_EDIDD TYPE TABLE OF EDIDD WITH HEADER LINE. "Data record-IDoc

*------------------------  Selection Screen  ---------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETER:
INFILE LIKE FILENAMECI-FILEEXTERN OBLIGATORY. " DEFAULT
SELECTION-SCREEN END OF BLOCK BOX.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
            '/CONTRAX/zcontraxrridoc.dat' INTO INFILE.

*---------------------------------------------------------------------*
*------------------------  Start of selection ------------------------*
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM OPEN_INPUT_FILE.
  PERFORM BUILD_INTAB_TABLE.
  PERFORM BUILD_SUMTAB_TABLE.

*  STOP.                    "TEMP
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
*------------------------  BUILD_INTAB_TABLE -------------------------*
*---------------------------------------------------------------------*

FORM BUILD_INTAB_TABLE.
**start of temp code
*     WRITE: /1 '--------------------------------------------'.
*     WRITE: /1 '                INTAB  RECORD               '.
*     WRITE: /1 '--------------------------------------------'.
*end of temp code

  DO.
    READ DATASET INFILE INTO IDOCREC.
    IF SY-SUBRC <> '0'.
      EXIT.
    ENDIF.

    MOVE-CORRESPONDING IDOCREC TO INTAB.
    APPEND INTAB.
    CLEAR  INTAB.
  ENDDO.
*  SORT INTAB BY PARTN BSART DWERK VKBUR AUGRU ZUONR KONDA KDGRP
*                MABNR KVGR1 KDKG1.
  SORT INTAB BY PARTN BSART DWERK VKBUR AUGRU ZUONR KONDA  "ACR3950
                KDGRP KVGR1 KDKG1 PRSDT MABNR.             "ACR3950
**start of temp code

*   LOOP AT INTAB.
*    WRITE: /1      INTAB-PARTN, INTAB-BSART, INTAB-DWERK, INTAB-AUGRU,
*      INTAB-ZUONR, INTAB-KONDA, INTAB-KDGRP, INTAB-MABNR, INTAB-KBETR,
*      INTAB-KWMENG, INTAB-AUDAT, INTAB-FKDAT, INTAB-CURCY,
*      INTAB-AUTLF, INTAB-VKORG, INTAB-VTWEG, INTAB-SPART, INTAB-PARVW,
*      INTAB-BSTKD, INTAB-VKBUR, INTAB-PRSDT, INTAB-KVGR1, INTAB-BZIRK,
*      INTAB-VRKME, INTAB-KSCHL, INTAB-KDKG1.
*
*   ENDLOOP.
*end of temp code

ENDFORM.                    "BUILD_INTAB_TABLE

*---------------------------------------------------------------------*
*------------------------  BUILD_SUM_TABLE ---------------------------*
*---------------------------------------------------------------------*

FORM BUILD_SUMTAB_TABLE.
**start of temp code
*     WRITE: /1 '--------------------------------------------'.
*     WRITE: /1 '               SUMTAB  RECORD               '.
*     WRITE: /1 '--------------------------------------------'.
*end of temp code

   LOOP AT INTAB.
        MOVE INTAB TO INTAB_LINE.
*        AT END OF MABNR.                     "ACR3950  "Code commented by JOOKONTR for CHG0165696
*        AT END OF KDKG1.                    "ACR3950
*           SUM.                                        "Code commented by JOOKONTR for CHG0165696
           MOVE-CORRESPONDING INTAB_LINE TO SUMTAB.
           MOVE INTAB-KWMENG  TO  SUMTAB-KWMENG.
           MOVE INTAB-KBETR   TO  SUMTAB-KBETR.

           APPEND SUMTAB.
           CLEAR  SUMTAB.
*       ENDAT.                                          "Code commented by JOOKONTR for CHG0165696
  ENDLOOP.
*  SORT SUMTAB BY PARTN BSART DWERK VKBUR AUGRU ZUONR KONDA KDGRP KVGR1.
  SORT SUMTAB BY PARTN BSART DWERK VKBUR AUGRU ZUONR KONDA  "ACR3950
                 KDGRP KVGR1 KDKG1 PRSDT MABNR.             "ACR3950

**start of temp code
* LOOP AT SUMTAB.
*    WRITE: /1      SUMTAB-PARTN, SUMTAB-BSART, SUMTAB-DWERK,
*SUMTAB-AUGRU,
*      SUMTAB-ZUONR, SUMTAB-KONDA, SUMTAB-KDGRP, SUMTAB-MABNR,
*SUMTAB-KBETR,
*      SUMTAB-KWMENG, SUMTAB-AUDAT, SUMTAB-FKDAT, SUMTAB-CURCY,
*      SUMTAB-AUTLF, SUMTAB-VKORG, SUMTAB-VTWEG, SUMTAB-SPART,
*SUMTAB-PARVW,
*      SUMTAB-BSTKD, SUMTAB-VKBUR, SUMTAB-PRSDT, SUMTAB-KVGR1,
*SUMTAB-BZIRK,
*      SUMTAB-VRKME, SUMTAB-KSCHL, SUMTAB-KDKG1.
*   ENDLOOP.
*end of temp code

ENDFORM.                    "BUILD_SUMTAB_TABLE
*---------------------------------------------------------------------*
*--------------------------SEND INBOUND IDOC--------------------------*
*---------------------------------------------------------------------*

FORM SEND_INBOUND_IDOC.
  PERFORM INIT_CONTROL.
  PERFORM BUILD_IDOC.
*  PERFORM IDOC_SEND_ASYNC TABLES INT_EDIDD USING CONTROL_RECORD.
ENDFORM.               "SEND_INBOUND_IDOC

*---------------------------------------------------------------------*
*------------------------------BUILD_IDOC-----------------------------*
*---------------------------------------------------------------------*

FORM BUILD_IDOC.
  DATA: HEADER1 TYPE E1EDK01,             "Doc Header general Data
        HEADER2 TYPE Z1OR05E,             "Doc Header Custom segment
        HEADER3 TYPE E1EDK14,             "Doc Hdr Organizational Data
        HEADER4 TYPE E1EDK03,             "Document Header Date Segment
        HEADER6 TYPE E1EDKA1,             "Doc Hedr Partner Information
        HEADER7 TYPE E1EDK02,             "Doc header reference data
        HEADER8 TYPE E1EDK35,             "Doc Header Additional Data
        ITEM1   TYPE E1EDP01,             "Document Item General Data
        ITEM2   TYPE E1EDP05,             "Document Item Conditions
        ITEM3   TYPE E1EDP19,             "Doc Item Obj Identification
        ITEM4   TYPE E1EDP35.             "Doc Item Additional Data
* DATA: PREV_SOCOUNT LIKE SUMTAB-SOCOUNT VALUE '9999'.

LOOP AT SUMTAB.
        MOVE SUMTAB TO SUMTAB_LINE.
*Start of HEADER1
*   AT NEW KDGRP.
*    AT NEW KVGR1.
     AT NEW PRSDT.                       "ACR3950
      HEADER1-CURCY = SUMTAB_LINE-CURCY.
      HEADER1-AUTLF = SUMTAB_LINE-AUTLF.
      HEADER1-AUGRU = SUMTAB_LINE-AUGRU.
      PERFORM ADD_SEGMENT
        TABLES INT_EDIDD
         USING CONTROL_RECORD-DOCNUM 'E1EDK01' HEADER1.
*Note: CONTROL_RECORD-DOCNUM = IDOC #

*End of HEADER1

*Start of HEADER2 (Custom Segment)
      HEADER2-KONDA = SUMTAB_LINE-KONDA.
      HEADER2-KDGRP = SUMTAB_LINE-KDGRP.
      HEADER2-BZIRK = SUMTAB_LINE-BZIRK.
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

*ACR-244 D30K927651 Change segment 6 & 7 mapping
      SEGMENT_E1EDK14:
      '008' SUMTAB_LINE-VKORG, '006' SUMTAB_LINE-SPART,
      '007' SUMTAB_LINE-VTWEG, '012' SUMTAB_LINE-BSART,
      '005' SUMTAB_LINE-DWERK, '016' SUMTAB_LINE-VKBUR.
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
      '012' SUMTAB_LINE-AUDAT, '022' SUMTAB_LINE-AUDAT,
      '023' SUMTAB_LINE-PRSDT, '026' SUMTAB_LINE-FKDAT,
      '029' SUMTAB_LINE-AUDAT, '035' SUMTAB_LINE-FKDAT.
*End of HEADER4


**Start of HEADER5
*      HEADER5-KSCHL = SUMTAB_LINE-KSCHDR.
*      HEADER5-KOBTR = SUMTAB_LINE-KBETHDR.
*      PERFORM ADD_SEGMENT
*        TABLES INT_EDIDD
*         USING CONTROL_RECORD-DOCNUM 'E1EDK05' HEADER5.
**End of HEADER5

*Start of HEADER6
      HEADER6-PARVW = SUMTAB_LINE-PARVW.
      HEADER6-PARTN = SUMTAB_LINE-PARTN.
      PERFORM ADD_SEGMENT
        TABLES INT_EDIDD
         USING CONTROL_RECORD-DOCNUM 'E1EDKA1' HEADER6.
*End of HEADER6

*Start of HEADER7
      DEFINE SEGMENT_E1EDK02.
        HEADER7-QUALF = &1.
        HEADER7-BELNR = &2.
        PERFORM ADD_SEGMENT
         TABLES INT_EDIDD
         USING CONTROL_RECORD-DOCNUM 'E1EDK02' HEADER7.
      END-OF-DEFINITION.

      SEGMENT_E1EDK02:
      '017' SUMTAB_LINE-ZUONR, '011' SUMTAB_LINE-AUGRU,
      '001' SUMTAB_LINE-BSTKD.
*End of HEADER7

*Start of HEADER8
      HEADER8-QUALZ  = '001'.
      HEADER8-CUSADD = SUMTAB_LINE-KVGR1.
      PERFORM ADD_SEGMENT
        TABLES INT_EDIDD
         USING CONTROL_RECORD-DOCNUM 'E1EDK35' HEADER8.
*End of HEADER8
    ENDAT.   "KDGRP.


*Start of ITEM1
    W_ITEMNO = W_ITEMNO + 1.
    ITEM1-POSEX   = W_ITEMNO.
    ITEM1-MATNR_EXTERNAL = SUMTAB_LINE-MABNR.
    ITEM1-MENGE   = SUMTAB_LINE-KWMENG.
    ITEM1-MENEE   = SUMTAB_LINE-VRKME.
    ITEM1-WERKS   = SUMTAB_LINE-DWERK.
    PERFORM ADD_SEGMENT
      TABLES INT_EDIDD
       USING CONTROL_RECORD-DOCNUM 'E1EDP01' ITEM1.
*End of ITEM1

*Start of ITEM2 (Child Segment)
 IF SUMTAB_LINE-KSCHL <> SPACE.
    ITEM2-KSCHL   = SUMTAB_LINE-KSCHL.
    ITEM2-BETRG   = SUMTAB_LINE-KBETR.
    PERFORM ADD_SEGMENT
      TABLES INT_EDIDD
       USING CONTROL_RECORD-DOCNUM 'E1EDP05' ITEM2.
 ENDIF.
*End of ITEM2

*Start of ITEM3 (Child Segment)
    ITEM3-QUALF   = '003'.
    ITEM3-IDTNR   = SUMTAB_LINE-MABNR.
    ITEM3-KTEXT   = 'Gas Sales'. "It should be moved to translaton prog
    PERFORM ADD_SEGMENT
      TABLES INT_EDIDD
       USING CONTROL_RECORD-DOCNUM 'E1EDP19' ITEM3.
*End of ITEM3

*Start of ITEM4 (Child Segment)
IF SUMTAB_LINE-KDKG1 <> SPACE.
    ITEM4-QUALZ   = '006'.
    ITEM4-CUSADD  = SUMTAB_LINE-KDKG1.
    PERFORM ADD_SEGMENT
      TABLES INT_EDIDD
       USING CONTROL_RECORD-DOCNUM 'E1EDP35' ITEM4.
ENDIF.
*End of ITEM4

*    MOVE SUMTAB-SOCOUNT TO PREV_SOCOUNT.
*    AT END OF KDGRP.
*     AT END OF KVGR1.                      "ACR3950
     AT END of PRSDT.                       "ACR3950
        CLEAR W_ITEMNO.
        PERFORM IDOC_SEND_ASYNC TABLES INT_EDIDD USING CONTROL_RECORD.
        CLEAR INT_EDIDD.
        REFRESH INT_EDIDD.
     ENDAT.

     AT LAST.
        IF W_ITEMNO > 0.
        PERFORM IDOC_SEND_ASYNC TABLES INT_EDIDD USING CONTROL_RECORD.
        CLEAR INT_EDIDD.
        REFRESH INT_EDIDD.
        ENDIF.
     ENDAT.

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
  CONTROL_RECORD-SNDPRN = 'CTRXGS'.             "Partner # of Sender
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
