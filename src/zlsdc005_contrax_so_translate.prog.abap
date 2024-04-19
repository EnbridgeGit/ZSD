*&---------------------------------------------------------------------*
*& Report  ZLSDC005_CONTRAX_SO_TRANSLATE
*&
*&---------------------------------------------------------------------*
*&**********************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        January  2011.                                         *
*  Issue Log:   TR804                                                  *
*  Description:                                                        *
*     - This program will receive sales file zcontraxgssalesorder.dat  *
*       Data will be translated using custome tables and an output file*
*       will be created for the data segments of Basic Type ORDER05    *
*&---------------------------------------------------------------------*
*CHANGES****                                                           *
*& 2016/12/04 GYMANA ACR244 D30K927657 - Add Price Date logic for      *
*&                                       pricing date.                 *
*& 2017/01/13 GYMANA ACR244 D30K927657 - Changed program logic to stop *
*&                                       using variant UOM value and   *
*&                                       start using file UOM value    *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* 2021/02/23 BIRUDURD COG changes to pass Aggregate Customer from      *
*                         selection screen                             *
*&---------------------------------------------------------------------*

REPORT  ZLSDC005_CONTRAX_SO_TRANSLATE NO STANDARD PAGE HEADING
        MESSAGE-ID ZM.

TABLES: ZLSDC01,     "Contrax Gas Sales Order Mapping
        ZLSDC02,     "Contrax Rate Class Mapping
        ZLSDBN002.   "Banner Organization Mapping

* Input file format
DATA:  BEGIN OF CTRX,
        APP_YR(4)             TYPE C,    "Year transaction applied
        APP_MTH(2)            TYPE C,    "Month transaction applied
        CUST_ID(8)            TYPE C,    "Customer number
        RATE_CL(12)           TYPE C,    "Rate class
        SERV_TYPE(6)          TYPE C,    "Service type
        SERV_CL(2)            TYPE C,    "Service class
        SEAS_CL(4)            TYPE C,    "Seasonal class
        RATE_TYPE(4)          TYPE C,     "Rate type
        CHARGE_TYPE(4)        TYPE C,     "Charge type
        SR_USAGE(4)           TYPE C,     "SR USAGE
        ST_SUBTYPE(6)         TYPE C,     "ST SUBTYPE
        NON_RATE_ITEM_TYP(8)  TYPE C,     "NON-RATE ITEM TYPE
        TIER_STEP_LVL(2)      TYPE C,     "TIER STEP LEVEL
        SECTOR_SIZE(1)        TYPE C,     "SECTOR SIZE
        SECTOR(6)             TYPE C,     "SECTOR
        AMOUNT(16)            TYPE C,     "TRANSACTION AMOUNT
        VOLUME(18)            TYPE C,     "TRANSACTION VOLUME
        CUST_TYPE(1)          TYPE C,     "CUSTOMER TYPE
        GECA_CODE(6)          TYPE C,     "GEOGRAPHIC AREA
        VOL_UOM(8)            TYPE C,     "VOLUME UNIT OF MEASURE
        SA_NUM(8)             TYPE C,     "CONTRACT NUMBER
        INV_NUM(8)            TYPE C,     "INVOICE #
        REBILL_YR(4)          TYPE C,     "REBILL YEAR         ACR244
        REBILL_MTH(2)         TYPE C,     "REBILL MONTH        ACR244
        SO_ID(4)              TYPE C,     "SERVICE OFFERING ID ACR244
        CUST_ID1(10)           TYPE C.     " Customer ID added by shegar
DATA: END OF CTRX.

*{   INSERT         S01K900875                                        3
DATA:   GT_CTRX               LIKE STANDARD TABLE OF ctrx.
*}   INSERT
* Output file format
DATA:  BEGIN OF IDOCREC,
        AUDAT            TYPE D,  "Document Date
   FKDAT             TYPE D,  "Billing Date
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
        MABNR(18)             TYPE C,  "Material Number
        KWMENG(15)            TYPE C,  "Order Quantity
        VRKME(3)              TYPE C,  "Unit of Measure
        KSCHL(4)              TYPE C,     "Condition Type
        KBETR(11)             TYPE C,  "Condition Amount
*{   REPLACE        S01K900875                                        2
*\        KDKG1(2)              TYPE C.    "Material Pricing Group
        KDKG1(2)              TYPE C,    "Material Pricing Group
        KVGR2(3)              TYPE C,    "Customer Group 2
*}   REPLACE
        CUST_ID(10)           TYPE C.     " Customer ID added by shegar
DATA: END OF IDOCREC.

DATA: W_DATE_IN  LIKE SY-DATUM,
      W_DATE_OUT LIKE SY-DATUM,
      W_TOWNCODE LIKE ZLSDBN002-TOWNCODE,
      W_MUNICODE LIKE ZLSDBN002-MUNICODE,
      W_AMOUNT(12)     TYPE P DECIMALS 2.

*------------------------  Selection Screen  ---------------------------
*
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-000.
*SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-001.
PARAMETER:
P_WAERK LIKE VBAK-WAERK  DEFAULT 'CAD' OBLIGATORY,      "SD Doc. Curr
P_AUTLF TYPE C           DEFAULT  'X'  OBLIGATORY,    "Del Complete Ind
P_VKORG LIKE VBAK-VKORG DEFAULT 'Z002' OBLIGATORY,      "Sales Org
*P_VTWEG LIKE VBAK-VTWEG DEFAULT 'Z0'   OBLIGATORY,      "Dist Chanel
P_SPART LIKE VBAK-SPART DEFAULT 'Z0'   OBLIGATORY,      "Division
P_PARVW(2) TYPE C       DEFAULT 'AG'  OBLIGATORY,
*P_VRKME LIKE T006-ISOCODE  DEFAULT 'CR' OBLIGATORY,   "ACR-244
P_BSTKD LIKE BVBAPKOM-BSTKD DEFAULT 'CONTRAX GAS SALES',
P_KUNNR LIKE VBAK-KUNNR DEFAULT ''  OBLIGATORY. " Customer "COG
SELECT-OPTIONS: S_MATNR FOR ZLSDC01-MATNR OBLIGATORY.   "Material #

SELECTION-SCREEN END OF BLOCK BOX2.
* Input/Output Files
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-002.
PARAMETERS:
INFILE LIKE FILENAMECI-FILEEXTERN OBLIGATORY,
OUTFILE LIKE FILENAMECI-FILEEXTERN OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX3.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-003.
PARAMETERS:     P_ACT RADIOBUTTON GROUP RBCR,            "Actual
                P_EST RADIOBUTTON GROUP RBCR,            "Estimate
                P_REV RADIOBUTTON GROUP RBCR.            "Reversal
*{   INSERT         S01K900875                                        1
SELECTION-SCREEN  BEGIN OF LINE.
PARAMETERS:       P_CTDSS AS CHECKBOX. "Cap & Trade Deliv/Stor Split
SELECTION-SCREEN  COMMENT 03(38) TEXT-S01.
SELECTION-SCREEN  END   OF LINE.
*}   INSERT
SELECTION-SCREEN END OF BLOCK BOX4.
SELECTION-SCREEN END OF BLOCK BOX1.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE:
  '/usr/sap/interfaces/' SY-SYSID(3) '/CONTRAX/zcontraxgssalesorder.dat'
                                                           INTO INFILE,
  '/usr/sap/interfaces/' SY-SYSID(3) '/CONTRAX/zcontraxsoidoc.dat'
                                                           INTO OUTFILE.
  S_MATNR  = 'I'.
  S_MATNR-OPTION = 'EQ'.
  S_MATNR-LOW    = '000000000000901695'.
  APPEND S_MATNR.
  S_MATNR-LOW    = '000000000000901718'.
  APPEND S_MATNR.
  S_MATNR-LOW    = '000000000000902903'.
  APPEND S_MATNR.
  S_MATNR-LOW    = '000000000000901729'.
  APPEND S_MATNR.
  S_MATNR-LOW    = '000000000000901740'.
  APPEND S_MATNR.

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

*{   REPLACE        S01K900875                                        1
*\  DO.
*\    READ DATASET INFILE INTO CTRX.
*\    IF SY-SUBRC <> '0'.
*\      EXIT.
*\    ENDIF.

  CLEAR    GT_CTRX[].

  DO.

    READ DATASET INFILE INTO CTRX.
    IF SY-SUBRC <> '0'.
      EXIT.
    ENDIF.
     ctrx-cust_id1 = ctrx-cust_id.     " Added by shegar
     CTRX-CUST_ID = P_KUNNR.                         "COG Change
    APPEND   CTRX                        TO GT_CTRX.

  ENDDO.

  IF   ( ( P_CTDSS                       IS NOT INITIAL ) AND
         ( GT_CTRX[]                     IS NOT INITIAL )     ).

    PERFORM  F_CAP_AND_TRADE_SPLIT   TABLES GT_CTRX.

  ENDIF.

  CLEAR                                     CTRX.
  LOOP AT  GT_CTRX                     INTO CTRX.

*}   REPLACE

*Move Data from Variant Screen Fields
  MOVE: P_WAERK   TO  IDOCREC-CURCY,
        P_AUTLF   TO  IDOCREC-AUTLF,
        P_VKORG   TO  IDOCREC-VKORG,
*        P_VTWEG   TO  IDOCREC-VTWEG,
        P_SPART   TO  IDOCREC-SPART,
        P_PARVW   TO  IDOCREC-PARVW,
*        P_VRKME   TO  IDOCREC-VRKME,                      "ACR-244
        P_BSTKD   TO  IDOCREC-BSTKD.
"-------
*Move Data from input file fields
  MOVE: CTRX-SA_NUM       TO  IDOCREC-ZUONR,
        CTRX-CUST_TYPE    TO  IDOCREC-KVGR1.
        IDOCREC-KWMENG  = ABS( CTRX-VOLUME ).
    IF IDOCREC-KWMENG = 0.
       MOVE '.001' TO IDOCREC-KWMENG.
    ENDIF.

* Process UOM from file.                                   "ACR-244
                                                           "ACR-244
 CLEAR IDOCREC-VRKME.                                      "ACR-244
 IDOCREC-VRKME   = CTRX-VOL_UOM.                           "ACR-244
 IF CTRX-VOL_UOM = 'GJ'.                                   "ACR-244
    IDOCREC-VRKME   = 'GV'.                                "ACR-244
 ELSEIF CTRX-VOL_UOM = 'M3'.                               "ACR-244
    IDOCREC-VRKME   = 'CR'.                                "ACR-244
 ELSEIF CTRX-VOL_UOM = 'MJ'.                               "ACR-244
    IDOCREC-VRKME   = 'GV'.                                "ACR-244
    IDOCREC-KWMENG  = ABS( CTRX-VOLUME / 1000 ).           "ACR-244
    IF IDOCREC-KWMENG = 0.                                 "ACR-244
       MOVE '.001' TO IDOCREC-KWMENG.                      "ACR-244
    ENDIF.                                                 "ACR-244
 ELSEIF IDOCREC-VRKME = ' '.                               "ACR-244
    IDOCREC-VRKME   = 'CR'.                                "ACR-244
 ENDIF.                                                    "ACR-244

*Calculate and Move Data

 MOVE CTRX-AMOUNT TO W_AMOUNT.

 IF CTRX-VOLUME = 0  AND  W_AMOUNT = 0.
    IDOCREC-KBETR   = ABS( CTRX-AMOUNT ).
 ELSEIF CTRX-VOLUME = 0  AND  W_AMOUNT < 0.
    IDOCREC-KBETR   = ABS( CTRX-AMOUNT ).
 ELSEIF CTRX-VOLUME = 0  AND  W_AMOUNT > 0.
    IDOCREC-KBETR   = ABS( CTRX-AMOUNT ).

 ELSEIF CTRX-VOLUME > 0  AND  W_AMOUNT = 0.
    IDOCREC-KBETR   = ABS( CTRX-AMOUNT ).
 ELSEIF CTRX-VOLUME > 0  AND  W_AMOUNT < 0.
    IDOCREC-KBETR   = CTRX-AMOUNT+5(11).
 ELSEIF CTRX-VOLUME > 0  AND  W_AMOUNT > 0.
    IDOCREC-KBETR   = ABS( CTRX-AMOUNT ).

 ELSEIF CTRX-VOLUME < 0  AND  W_AMOUNT = 0.
    IDOCREC-KBETR   = ABS( CTRX-AMOUNT ).
 ELSEIF CTRX-VOLUME < 0  AND  W_AMOUNT < 0.
    IDOCREC-KBETR   = ABS( CTRX-AMOUNT ).
 ELSEIF CTRX-VOLUME < 0  AND  W_AMOUNT > 0.
    IDOCREC-KBETR   = CTRX-AMOUNT+5(11).
 ENDIF.

* IF CTRX-VOLUME = 0  AND  W_AMOUNT <> 0.
*    IDOCREC-KBETR   = ABS( CTRX-AMOUNT ).
* ELSEIF CTRX-VOLUME > 0  AND  W_AMOUNT <> 0.
*    IDOCREC-KBETR   = ABS( CTRX-AMOUNT ).
* ELSEIF CTRX-VOLUME > 0  AND  W_AMOUNT < 0.
*    IDOCREC-KBETR   = CTRX-AMOUNT.
* ELSEIF CTRX-VOLUME < 0  AND  W_AMOUNT > 0.
*    IDOCREC-KBETR   = CTRX-AMOUNT.
* ELSEIF CTRX-VOLUME < 0  AND  W_AMOUNT < 0.
*    IDOCREC-KBETR   = ABS( CTRX-AMOUNT ).
* ENDIF.

* Start of changes    COG
*    CONCATENATE 'CX' CTRX-CUST_ID INTO IDOCREC-PARTN.
    IDOCREC-PARTN = P_KUNNR.
      IDOCREC-CUST_ID = CTRX-CUST_ID1 . "Added by shegar since sales order should be every custoemr from flat file.
* End of changes      COG
    CONDENSE IDOCREC-PARTN NO-GAPS.
    CONCATENATE CTRX-APP_YR CTRX-APP_MTH '01' INTO W_DATE_IN.

* ACR-244 D30K927657 Remove IDOCREC-PRSDT from Act/Est/Rev logic
 IF P_ACT  = 'X'.                                     "Actual
    MOVE SY-DATUM TO: IDOCREC-AUDAT, IDOCREC-FKDAT.
    MOVE 'ZAC'    TO  IDOCREC-AUGRU.
 ELSE.
    PERFORM GET_LAST_DAY_OF_MONTH USING W_DATE_IN CHANGING W_DATE_OUT.
    IF P_EST  = 'X'.                                   "Estimate
       MOVE W_DATE_OUT TO: IDOCREC-AUDAT, IDOCREC-FKDAT.
       MOVE 'ZES'     TO  IDOCREC-AUGRU.
    ELSE.
       W_DATE_OUT = W_DATE_OUT + 1.
       MOVE W_DATE_OUT TO: IDOCREC-AUDAT, IDOCREC-FKDAT.
       MOVE 'ZRV'      TO  IDOCREC-AUGRU.
    ENDIF.
 ENDIF.

* ACR-244 D30K927657 New PRSDT logic
 IF CTRX-REBILL_YR IS NOT INITIAL AND
    CTRX-REBILL_MTH IS NOT INITIAL.
    CONCATENATE CTRX-REBILL_YR CTRX-REBILL_MTH '01' INTO W_DATE_IN.
    PERFORM GET_LAST_DAY_OF_MONTH USING W_DATE_IN CHANGING W_DATE_OUT.
    MOVE W_DATE_OUT TO IDOCREC-PRSDT.
 ELSE.
    CONCATENATE CTRX-APP_YR CTRX-APP_MTH '01' INTO W_DATE_IN.
    PERFORM GET_LAST_DAY_OF_MONTH USING W_DATE_IN CHANGING W_DATE_OUT.
    MOVE W_DATE_OUT TO IDOCREC-PRSDT.
 ENDIF.

*Move Data from DB Tables
    CLEAR: ZLSDC01.
    SELECT SINGLE * FROM ZLSDC01
     WHERE C_NRTTP      = CTRX-NON_RATE_ITEM_TYP
       AND ( C_SVCTYP   = CTRX-SERV_TYPE   OR C_SVCTYP  = '******' )
       AND ( C_CUSTTYP  = CTRX-CUST_TYPE   OR C_CUSTTYP = '*' )
       AND ( C_RTETYP   = CTRX-RATE_TYPE   OR C_RTETYP  = '****' )
       AND ( C_CHGTYP   = CTRX-CHARGE_TYPE OR C_CHGTYP  = '****' )
       AND ( C_SRUSE    = CTRX-SR_USAGE    OR C_SRUSE   = '****' )
       AND ( C_STSUB    = CTRX-ST_SUBTYPE  OR C_STSUB   = '******' )
       AND ( C_SCCODE   = CTRX-SERV_CL     OR C_SCCODE  = '**' ).
    IF SY-SUBRC = 0.
       MOVE ZLSDC01-KDGRP   TO IDOCREC-KDGRP.
       MOVE ZLSDC01-KDKG1   TO IDOCREC-KDKG1.
       MOVE ZLSDC01-KSCHL   TO IDOCREC-KSCHL.
       IF ZLSDC01-KSCHL = 'ZPB0' OR
          ZLSDC01-KSCHL = 'ZPB1' OR
          ZLSDC01-KSCHL = 'ZPB4'.
          MOVE ZLSDC01-MATNR TO IDOCREC-MABNR.
          IF ZLSDC01-MATNR IN S_MATNR.
             MOVE SPACE  TO IDOCREC-VRKME.
*            MOVE 'EA'  TO IDOCREC-VRKME.
          ENDIF.
       ENDIF.

       IF P_ACT  = 'X'  OR  P_EST  = 'X'.           "Actual or Estimate
          IF CTRX-VOLUME = 0  AND  W_AMOUNT > 0
                              AND ZLSDC01-MATNR = 'NATGAS'.
             MOVE ZLSDC01-Z_DB_MEM TO IDOCREC-BSART.
          ELSEIF CTRX-VOLUME = 0  AND  W_AMOUNT < 0
                              AND ZLSDC01-MATNR = 'NATGAS'.
             MOVE ZLSDC01-Z_CR_MEM TO IDOCREC-BSART.
          ELSEIF  CTRX-VOLUME = 0  AND  W_AMOUNT > 0.
             MOVE ZLSDC01-AUART TO IDOCREC-BSART.
          ELSEIF  CTRX-VOLUME = 0  AND  W_AMOUNT < 0.
             MOVE ZLSDC01-Z_REV_OTYPE TO IDOCREC-BSART.
          ELSEIF CTRX-VOLUME > 0  AND  W_AMOUNT < 0.
             MOVE ZLSDC01-AUART TO IDOCREC-BSART.
          ELSEIF CTRX-VOLUME < 0  AND  W_AMOUNT > 0.
             MOVE ZLSDC01-Z_REV_OTYPE TO IDOCREC-BSART.
          ELSEIF CTRX-VOLUME > 0  AND  W_AMOUNT > 0.
             MOVE ZLSDC01-AUART TO IDOCREC-BSART.
          ELSEIF CTRX-VOLUME < 0  AND  W_AMOUNT < 0.
             MOVE ZLSDC01-Z_REV_OTYPE TO IDOCREC-BSART.
          ENDIF.
       ELSE.
          IF CTRX-VOLUME = 0  AND  W_AMOUNT > 0
                              AND ZLSDC01-MATNR = 'NATGAS'.
             MOVE ZLSDC01-Z_CR_MEM TO IDOCREC-BSART.
          ELSEIF CTRX-VOLUME = 0  AND  W_AMOUNT < 0
                              AND ZLSDC01-MATNR = 'NATGAS'.
             MOVE ZLSDC01-Z_DB_MEM TO IDOCREC-BSART.
          ELSEIF CTRX-VOLUME = 0  AND  W_AMOUNT > 0.
             MOVE ZLSDC01-Z_REV_OTYPE TO IDOCREC-BSART.
          ELSEIF CTRX-VOLUME = 0  AND  W_AMOUNT < 0.
             MOVE ZLSDC01-AUART TO IDOCREC-BSART.
          ELSEIF CTRX-VOLUME > 0  AND  W_AMOUNT < 0.
             MOVE ZLSDC01-Z_REV_OTYPE TO IDOCREC-BSART.
          ELSEIF CTRX-VOLUME < 0  AND  W_AMOUNT > 0.
             MOVE ZLSDC01-AUART TO IDOCREC-BSART.
          ELSEIF CTRX-VOLUME > 0  AND  W_AMOUNT > 0.
             MOVE ZLSDC01-Z_REV_OTYPE TO IDOCREC-BSART.
          ELSEIF CTRX-VOLUME < 0  AND  W_AMOUNT < 0.
             MOVE ZLSDC01-AUART TO IDOCREC-BSART.
          ENDIF.
       ENDIF.
       IF IDOCREC-BSART = 'ZCR' OR IDOCREC-BSART = 'ZDR'.
          MOVE 'ZPB0'  TO IDOCREC-KSCHL.
       ENDIF.
    ENDIF.

    SELECT SINGLE KONDA INTO ZLSDC02-KONDA
      FROM ZLSDC02
     WHERE C_RATECL = CTRX-RATE_CL
       AND ( C_SVCTYP = CTRX-SERV_TYPE  OR C_SVCTYP = '******' )
       AND ( C_SEASCL = CTRX-SEAS_CL    OR C_SEASCL = '****' ).
    IF SY-SUBRC = 0.
       MOVE ZLSDC02-KONDA TO IDOCREC-KONDA.
    ENDIF.
*
    MOVE CTRX-GECA_CODE(2)    TO  W_TOWNCODE.
    MOVE CTRX-GECA_CODE+2(4)  TO  W_MUNICODE.
    CLEAR: ZLSDBN002-VKBUR, ZLSDBN002-WERKS.

    SELECT SINGLE * FROM ZLSDBN002
     WHERE TOWNCODE = W_TOWNCODE
       AND MUNICODE = W_MUNICODE.
    IF SY-SUBRC = 0.
       MOVE ZLSDBN002-VKBUR TO IDOCREC-VKBUR.
       MOVE ZLSDBN002-WERKS TO IDOCREC-DWERK.
       MOVE ZLSDBN002-BZIRK TO IDOCREC-BZIRK.
       IDOCREC-VTWEG = zlsdbn002-vtweg.
    else.
       IDOCREC-VTWEG = 'Z0'.
    ENDIF.
*{   INSERT         S01K900875                                        3
    IF     ( CTRX-SECTOR                 EQ 'EXEMPT' ).
      CLEAR                                 IDOCREC-KVGR2.
      MOVE   'E'                         TO IDOCREC-KVGR2.
    ELSEIF ( CTRX-SECTOR                 EQ 'NONEXE' ).
      CLEAR                                 IDOCREC-KVGR2.
      MOVE   'N'                         TO IDOCREC-KVGR2.
    ENDIF.
*}   INSERT

    MOVE CTRX-AMOUNT TO W_AMOUNT.
    IF W_AMOUNT <> 0.
      TRANSFER IDOCREC TO OUTFILE.
    ENDIF.

*{   REPLACE        S01K900875                                        2
*\  ENDDO.
    CLEAR  CTRX.
  ENDLOOP.
*}   REPLACE
  CLOSE DATASET: INFILE, OUTFILE.

  MESSAGE I100(ZM) WITH TEXT-100.

***********************************************************************
FORM GET_LAST_DAY_OF_MONTH USING W_DATE_IN CHANGING VALUE(W_DATE_OUT).

CALL FUNCTION 'FKK_LAST_DAY_OF_MONTH'
   EXPORTING
     DAY_IN = W_DATE_IN
   IMPORTING
     LAST_DAY_OF_MONTH = W_DATE_OUT
   EXCEPTIONS
     DAY_IN_NO_DATE = 1.
ENDFORM.

END-OF-SELECTION.

*{   INSERT         S01K900875                                        1
*&---------------------------------------------------------------------*
*&      Form  F_CAP_AND_TRADE_SPLIT
*&---------------------------------------------------------------------*
*       Cap and Trade Delivery / Storage Split
*----------------------------------------------------------------------*
FORM F_CAP_AND_TRADE_SPLIT
  TABLES   CT_CTRX                STRUCTURE CTRX.

  FIELD-SYMBOLS: <FS_CTRX>             LIKE CTRX.

  DATA:    BEGIN OF LS_EXEMPT,
             CUST_ID(8)                TYPE C,
             SA_NUM(8)                 TYPE C,
             FL_FOUND                  TYPE FLAG,
           END   OF LS_EXEMPT.

  DATA:    LT_EXEMPT LIKE STANDARD TABLE OF LS_EXEMPT.

  FIELD-SYMBOLS: <FS1>                 LIKE CTRX.

  IF   ( ( P_CTDSS                       IS INITIAL ) OR
         ( CT_CTRX[]                     IS INITIAL )    ).
    RETURN.
  ENDIF.

  CLEAR    LT_EXEMPT[].

  SORT     CT_CTRX             ASCENDING BY CUST_ID SA_NUM.

  LOOP AT  CT_CTRX                ASSIGNING <FS_CTRX>.
    IF     ( <FS_CTRX>-NON_RATE_ITEM_TYP EQ 'CTCREDIT' ).
      CLEAR                                 LS_EXEMPT.
      MOVE   <FS_CTRX>-CUST_ID           TO LS_EXEMPT-CUST_ID.
      MOVE   <FS_CTRX>-SA_NUM            TO LS_EXEMPT-SA_NUM.
      MOVE   ABAP_TRUE                   TO LS_EXEMPT-FL_FOUND.
      APPEND LS_EXEMPT                   TO LT_EXEMPT.
    ENDIF.
  ENDLOOP.

  SORT     LT_EXEMPT           ASCENDING BY CUST_ID SA_NUM.
  DELETE   ADJACENT DUPLICATES         FROM LT_EXEMPT
                                  COMPARING CUST_ID SA_NUM.

  LOOP AT    CT_CTRX              ASSIGNING <FS_CTRX>.

    IF   ( ( <FS_CTRX>-CUST_ID           EQ LS_EXEMPT-CUST_ID ) AND
           ( <FS_CTRX>-SA_NUM            EQ LS_EXEMPT-SA_NUM  )     ).
    ELSE.
      CLEAR                                 LS_EXEMPT.
      READ   TABLE LT_EXEMPT           INTO LS_EXEMPT
                                   WITH KEY CUST_ID = <FS_CTRX>-CUST_ID
                                            SA_NUM  = <FS_CTRX>-SA_NUM
                              BINARY SEARCH.
      IF     ( SY-SUBRC NE 0 ).
        CLEAR                               LS_EXEMPT.
        MOVE   <FS_CTRX>-CUST_ID         TO LS_EXEMPT-CUST_ID.
        MOVE   <FS_CTRX>-SA_NUM          TO LS_EXEMPT-SA_NUM.
      ENDIF.
    ENDIF.

    CLEAR                                   <FS_CTRX>-SECTOR.
    IF     ( LS_EXEMPT-FL_FOUND          IS INITIAL ).
      MOVE   'NONEXE'                    TO <FS_CTRX>-SECTOR.
    ELSE.
      MOVE   'EXEMPT'                    TO <FS_CTRX>-SECTOR.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_CAP_AND_TRADE_SPLIT
*}   INSERT
