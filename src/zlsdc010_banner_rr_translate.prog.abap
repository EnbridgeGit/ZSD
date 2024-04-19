*&--------------------------------------------------------------------*
*& Report  ZLSDC007_BANNER_RR_TRANSLATE
*&
*&--------------------------------------------------------------------*
*&*********************************************************************
*  Author:      Mohammad T. Khan                                      *
*  Date:        April, 2011                                           *
*  Issue Log:   TR804                                                 *
*  Description:                                                       *
*    -The purpose of this program is to translate the RateRider data  *
*     using custom mapping table ZLSDBN001, ZLSDBN002, ZLSDBN003 and  *
*     convert the data into rate rider Idoc format.                   *
*                                                                     *
*&--------------------------------------------------------------------*
*CHANGES****                                                          *
*&12/04/2016 GYMANA D30K927653 ACR-244 - remove Dist Channel selection*
*&                                       screen field (p_vtweg)       *                                                *
*&--------------------------------------------------------------------*

REPORT  ZLSDC007_BANNER_RR_TRANSLATE NO STANDARD PAGE HEADING
LINE-COUNT 65 LINE-SIZE 80 MESSAGE-ID ZM.

TABLES: ZLSDBN001,        " Banner GL Code Mapping
        ZLSDBN002,        " Banner Organization Mapping
        ZLSDBN003.        " Banner Rate Class Mapping

* Input file format
DATA:  BEGIN OF SALE_REC,
       PROCESS_DATE       TYPE D,         "Process Date
       SERV_TYPE(4)       TYPE C,         "Serv Type
       SERV_CAT(4)        TYPE C,
       GL_CLASSCD(4)      TYPE C,
       SERV_CLASS(2)      TYPE C,
       TOWN_CD(2)         TYPE C,
       MUNIC_CD(4)        TYPE C,
       BUDGET_IND(1)      TYPE C,
       CUST_NUM(16)       TYPE N,
       TRANS_AMT_SIGN(1)  TYPE C,
       TRANS_AMT(13)      TYPE C,
       CUST_CHRG_SIGN(1)  TYPE C,
       CUST_CHRG(11)      TYPE C,         "Budget Amount
       CONS_SIGN(1)       TYPE C,
       CONS(13)           TYPE C,
       NO_OF_CUSTS(6)     TYPE N,
       EFF_DATE           TYPE D,
       RATE_CLASS(4)      TYPE C.
DATA:  END OF SALE_REC.

* Output file format
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
       KVGR1(3)	TYPE C,       "Customer Group 1
  BZIRK(4)  TYPE C,   "Sales District
  MABNR(18) TYPE C,   "Material Number
  KWMENG(15)  TYPE C,   "Order Quantity
  VRKME(3)  TYPE C,   "Unit of Measure
  KSCHL(4)  TYPE C,       "Condition Type
  KBETR(11) TYPE C, "Condition Amount
       KDKG1(2)      TYPE C.  "Condition Group 1
DATA:  END OF SIDOC_REC.

DATA: W_GLCODE LIKE ZLSDBN001-GLCODE,
      W_AMOUNT(13) TYPE C,
      W_DOLLAR     TYPE P LENGTH 13 DECIMALS 2.

CONSTANTS: TRUE  TYPE BOOLEAN VALUE 'X'.

*---------------------------------------------------------------------*
*------------------------  Selection Screen  --------------------------
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETER:
P_VKORG LIKE VBAK-VKORG DEFAULT 'Z001' OBLIGATORY,        "Sales Org
P_SPART LIKE VBAK-SPART DEFAULT 'Z0'   OBLIGATORY,        "Division
P_KUNNR LIKE KUAGV-KUNNR DEFAULT 'BANNER' OBLIGATORY,     "Customer #
P_WAERK LIKE VBAK-WAERK  DEFAULT 'CAD' OBLIGATORY,        "SD Doc. Curr
P_PARVW(2)   TYPE C      DEFAULT 'AG'  OBLIGATORY,        "SoldTo Party
P_ISOCOD(2)  TYPE C      DEFAULT 'CR' OBLIGATORY.         "ISO Code
PARAMETER:
INFILE LIKE FILENAMECI-FILEEXTERN OBLIGATORY,             "Input File
OUTFILE LIKE FILENAMECI-FILEEXTERN OBLIGATORY.            "Output File

SELECTION-SCREEN END OF BLOCK BOX.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
              '/BANNER/zbis100rrorder.dat' INTO INFILE.

  CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
              '/BANNER/zbannrrorderidoc.dat' INTO OUTFILE.

***********************************************************************
*                START-OF-SELECTION.                                  *
***********************************************************************
START-OF-SELECTION.
  OPEN DATASET INFILE FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC NE 0.
    MESSAGE E006(ZM) WITH INFILE.
  ENDIF.

  OPEN DATASET OUTFILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC NE 0.
    MESSAGE E006(ZM) WITH OUTFILE.
  ENDIF.

  DO.
    READ DATASET INFILE INTO SALE_REC.
    IF SY-SUBRC <> '0'.
      EXIT.
    ENDIF.

    PERFORM RECORD_MAPPING.
  ENDDO.
*Program ended Message
  MESSAGE I100(ZM) WITH TEXT-100.

***********************************************************************
*                    RECORD_MAPPING.                                  *
***********************************************************************
FORM RECORD_MAPPING.

  CLEAR W_GLCODE.
  MOVE: SALE_REC-GL_CLASSCD   TO   W_GLCODE.

  IF SALE_REC-CONS = '00000000000.000'.
    SIDOC_REC-KWMENG = ABS( '00000000001.000' ).
  ELSE.
    SIDOC_REC-KWMENG = ABS( SALE_REC-CONS ).
  ENDIF.

*Mapping from Input File Fields
  MOVE: SALE_REC-EFF_DATE      TO  SIDOC_REC-PRSDT,
        SALE_REC-SERV_CLASS    TO  SIDOC_REC-KVGR1,
        SALE_REC-PROCESS_DATE  TO  SIDOC_REC-AUDAT,
        SALE_REC-PROCESS_DATE  TO  SIDOC_REC-FKDAT.
*         SALE_REC-CONS_SIGN     TO  SIDOC_REC-QSIGN.

*Mapping from Selection Screen Fields
  MOVE: P_WAERK   TO  SIDOC_REC-CURCY,
        'X'       TO  SIDOC_REC-AUTLF,
        P_VKORG   TO  SIDOC_REC-VKORG,
        P_SPART   TO  SIDOC_REC-SPART,
        P_PARVW   TO  SIDOC_REC-PARVW,
        P_KUNNR   TO  SIDOC_REC-PARTN,
        P_ISOCOD  TO  SIDOC_REC-VRKME.

  CONCATENATE SALE_REC-RATE_CLASS '/' SALE_REC-SERV_CLASS '/' P_KUNNR
              INTO SIDOC_REC-BSTKD.

*Mapping for Field AUGRU.
    MOVE 'ZAC' TO SIDOC_REC-AUGRU.

*Mapping for Field BSART.
  IF SALE_REC-CONS_SIGN = '-'.
     MOVE 'ZRRR'  TO SIDOC_REC-BSART.
  ELSE.
    SELECT SINGLE AUART INTO ZLSDBN001-AUART
      FROM ZLSDBN001
     WHERE GLCODE = W_GLCODE.
     IF SY-SUBRC = 0.
        MOVE ZLSDBN001-AUART TO SIDOC_REC-BSART.
     ENDIF.
  ENDIF.

*Mapping for Field KBERT.
  IF SALE_REC-TRANS_AMT_SIGN = '-'.
     W_AMOUNT = SALE_REC-TRANS_AMT * -1.
  ELSE.
     W_AMOUNT = SALE_REC-TRANS_AMT.
  ENDIF.

  IF SIDOC_REC-BSART = 'ZRRR'.
     W_AMOUNT = W_AMOUNT * -1.
     MOVE W_AMOUNT+2(11) TO SIDOC_REC-KBETR.
  ELSE.
     MOVE W_AMOUNT+2(11) TO SIDOC_REC-KBETR.
  ENDIF.

*Mapping from ZLSDBN001 Table.
    SELECT SINGLE KDGRP MATNR KSCHL KDKG1
    INTO (ZLSDBN001-KDGRP, ZLSDBN001-MATNR, ZLSDBN001-KSCHL,
                                            ZLSDBN001-KDKG1)
    FROM ZLSDBN001
    WHERE GLCODE   = W_GLCODE.
    IF SY-SUBRC = 0.
      MOVE ZLSDBN001-KDGRP TO SIDOC_REC-KDGRP.
      MOVE ZLSDBN001-MATNR TO SIDOC_REC-MABNR.
      MOVE ZLSDBN001-KSCHL TO SIDOC_REC-KSCHL.
      MOVE ZLSDBN001-KDKG1 TO SIDOC_REC-KDKG1.
    ENDIF.

*Mapping from ZLSDBN002 Table.
    SELECT SINGLE *
     FROM ZLSDBN002
     WHERE TOWNCODE   = SALE_REC-TOWN_CD
       AND MUNICODE = SALE_REC-MUNIC_CD.
    IF SY-SUBRC = 0.
      MOVE: ZLSDBN002-WERKS TO SIDOC_REC-DWERK,
            ZLSDBN002-VKBUR TO SIDOC_REC-VKBUR,
            ZLSDBN002-BZIRK TO SIDOC_REC-BZIRK,
            ZLSDBN002-VTWEG TO SIDOC_REC-VTWEG.         "D30K927653
    ELSE.                                               "D30K927653
      SIDOC_REC-VTWEG = 'Z0'.                           "D30K927653
    ENDIF.

*Mapping from ZLSDBN003 Table.
    SELECT SINGLE KONDA INTO ZLSDBN003-KONDA
      FROM ZLSDBN003
     WHERE RATECLS = SALE_REC-RATE_CLASS.
    IF SY-SUBRC = 0.
      MOVE ZLSDBN003-KONDA TO SIDOC_REC-KONDA.
    ENDIF.

*Create a Record on the Output file
    MOVE SIDOC_REC-KBETR TO W_DOLLAR.
    IF W_DOLLAR <> 0.
      TRANSFER SIDOC_REC TO OUTFILE.
    ENDIF..
    CLEAR SIDOC_REC.

  ENDFORM.                    "REST_OF_THE_MAPPING
END-OF-SELECTION.
