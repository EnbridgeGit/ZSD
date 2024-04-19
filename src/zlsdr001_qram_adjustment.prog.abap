*&--------------------------------------------------------------------*
*& Report  ZLSDR001_QRAM_ADJUSTMENT                                   *
*&                                                                    *
*&--------------------------------------------------------------------*
* AUTHOR:      Mohammad T. Khan                                       *
* DATE:        February 2011.                                         *
* PROJECT:     Cost of Gas.                                           *
* ISSUE LOG:   TR804                                                  *
* DESCRIPTION:                                                        *
* This program will adjust the cost of sales posting when price change*
* has occurred and necessary entries to the Cost of Sales need to be  *
* adjusted. The program will find the deliveries posted by the user & *
* retrieve the standard cost from the prior periods. The standard cost*
* will be compared to the current released price on material master   *
* and an FI BDC Session will be created with the adjustment entries.  *
*&--------------------------------------------------------------------*

REPORT  ZLSDR001_QRAM_ADJUSTMENT.
 TYPE-POOLS: SLIS.

TABLES:
   VBAK,       "Sales Document: Header Data
   VBAP,       "Sales Document: Item Data
   VBFA.       "Sales Document Flow

DATA: BEGIN OF MBEWH_TAB OCCURS 1,
        BWKEY LIKE MBEWH-BWKEY,
        STPRS LIKE MBEWH-STPRS,
      END OF MBEWH_TAB.

DATA: BEGIN OF VBFA_TAB OCCURS 1,
        VBELV   LIKE VBFA-VBELV,
        POSNV   LIKE VBFA-POSNV,
        VBELN_J LIKE VBFA-VBELN,
      END OF VBFA_TAB.

DATA: BEGIN OF ITAB OCCURS 1,
        VBELN      LIKE VBAK-VBELN,
        AUDAT      LIKE VBAK-AUDAT,
        AUART      LIKE VBAK-AUART,
        WERKS      LIKE VBAP-WERKS,
        POSNR      LIKE VBAP-POSNR,
        MATNR_VBAP LIKE VBAP-MATNR,
        VBELN_R    LIKE VBFA-VBELN,
        RFMNG      LIKE VBFA-RFMNG,
        RFWRT      LIKE VBFA-RFWRT,
        MATNR_VBFA LIKE VBFA-MATNR,
        BWART      LIKE VBFA-BWART,
      END OF ITAB.

DATA: BEGIN OF REPTAB OCCURS 1,
        VBELN      LIKE VBAK-VBELN,
        AUDAT      LIKE VBAK-AUDAT,
        AUART      LIKE VBAK-AUART,
        WERKS      LIKE VBAP-WERKS,
        POSNR      LIKE VBAP-POSNR,
        MATNR_VBAP LIKE VBAP-MATNR,
        VBELN_R    LIKE VBFA-VBELN,
        VBELN_J    LIKE VBFA-VBELN,
        RFMNG      LIKE VBFA-RFMNG,
        RFWRT      LIKE VBFA-RFWRT,
        STPRS      LIKE MBEWH-STPRS,
        ADJ_COST   LIKE MBEWH-STPRS,
        ADJ_DIFF   LIKE MBEWH-STPRS,
      END OF REPTAB.

* Batch input data
DATA: BEGIN OF BDCDATA OCCURS 1.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

DATA: W_TCODE       LIKE TSTC-TCODE VALUE 'FB01',
      W_AMOUNT(13)  TYPE C,
      NEXT_ACCT     LIKE BSEG-HKONT,
      NEXT_KEY      LIKE RF05A-NEWBS,
      W_HEAD01(60)  TYPE C,
      W_HEAD02(60)  TYPE C,
      COST_COMP_IND TYPE C.

CONSTANTS: TRUE    TYPE BOOLEAN VALUE 'X',
           FLASE   TYPE BOOLEAN VALUE '-',
           UNKNOWN TYPE BOOLEAN VALUE ' '.

***********************************************************************
*                      SELECTION SCREEN                               *
***********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
   S_VBELN  FOR  VBAK-VBELN OBLIGATORY,                    "Sales Docu
   S_AUDAT  FOR  VBAK-AUDAT DEFAULT SY-DATUM OBLIGATORY.   "System Date
PARAMETERS:
   P_MATNR  LIKE  VBAP-MATNR  DEFAULT 'NATGAS' OBLIGATORY.  "Material #
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
PARAMETERS:
   P_LFMON  LIKE MBEWH-LFMON DEFAULT SY-DATUM+4(2) OBLIGATORY, "Curr PP
   P_LFGJA  LIKE MBEWH-LFGJA DEFAULT SY-DATUM(4) OBLIGATORY.   "Curr PY
SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
PARAMETERS:
   BOX_BDC  AS CHECKBOX USER-COMMAND RAD,
   P_BUKRS LIKE BKPF-BUKRS DEFAULT 'UGL',                 "Company Code
   P_BLDAT LIKE BKPF-BLDAT DEFAULT SY-DATUM,             "Document Date
   P_BUDAT LIKE BKPF-BUDAT DEFAULT SY-DATUM,              "Posting Date
   P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM+0(4),         "Current Year
   P_MONAT LIKE BKPF-MONAT DEFAULT SY-DATUM+4(2),       "Current Period
   P_BLART LIKE BKPF-BLART DEFAULT 'SA',                      "Doc Type
   P_WAERS LIKE BKPF-WAERS DEFAULT 'CAD',                     "Currency
   P_XBLNR LIKE BKPF-XBLNR DEFAULT 'COGS ADJ',          "Reference Doc#
   P_BKTXT LIKE BKPF-BKTXT DEFAULT 'COGS ADJUSTMENT',  "Doc Header Text
   P_MWSKZ LIKE BSEG-MWSKZ DEFAULT 'O0',                      "Tax Code
   P_COGADJ LIKE BSEG-HKONT DEFAULT '0000399998',       "GLAcct-COG ADJ
   P_OFFSET  LIKE BSEG-HKONT DEFAULT '0000399998',       "GLAcct-Offset
   P_GRUPID LIKE APQI-GROUPID DEFAULT 'ZCOG_QRAM'.    "BDC Session name

SELECTION-SCREEN END OF BLOCK BOX3.
SELECTION-SCREEN END OF BLOCK BOX.

***********************************************************************
*                    AT SELECTION-SCREEN OUTPUT                       *
***********************************************************************
AT SELECTION-SCREEN OUTPUT.
IF BOX_BDC <> 'X'.
   LOOP AT SCREEN.
   IF SCREEN-NAME = 'P_BUKRS'  OR SCREEN-NAME = 'P_BLDAT'  OR
      SCREEN-NAME = 'P_BUDAT'  OR SCREEN-NAME = 'P_GJAHR'  OR
      SCREEN-NAME = 'P_MONAT'  OR SCREEN-NAME = 'P_BLART'  OR
      SCREEN-NAME = 'P_WAERS'  OR SCREEN-NAME = 'P_XBLNR'  OR
      SCREEN-NAME = 'P_BKTXT'  OR SCREEN-NAME = 'P_COGADJ' OR
      SCREEN-NAME = 'P_OFFSET' OR SCREEN-NAME = 'P_GRUPID' OR
      SCREEN-NAME = 'P_MWSKZ'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
   ENDIF.
ENDLOOP.
ENDIF.

***********************************************************************
*                      START-OF-SELECTION                             *
***********************************************************************
START-OF-SELECTION.

PERFORM GET_ONE_TIME_DATA.
PERFORM GET_DB_DATA.
PERFORM BUILD_REPORT_TABLE.
IF BOX_BDC = 'X' AND COST_COMP_IND = 'X'.
   PERFORM CREATE_BDC_SESSION.
ENDIF.
PERFORM DISPLAY_ALV_GRID_DATA.

***********************************************************************
*                    SELECT_ONE_TIME_DATA                             *
***********************************************************************
*Select data from MBEWH table & if no row found, STOP program execution
FORM GET_ONE_TIME_DATA.
 SELECT BWKEY STPRS INTO TABLE MBEWH_TAB
   FROM MBEWH
  WHERE MATNR = P_MATNR
    AND LFGJA = P_LFGJA
    AND LFMON = P_LFMON.

 IF SY-SUBRC <> 0.
    WRITE: /1 TEXT-101.
    STOP.
 ENDIF.
 SORT MBEWH_TAB BY BWKEY.

**Select data from VBFA table
SELECT VBELV POSNV VBELN INTO TABLE VBFA_TAB
  FROM VBFA
  WHERE  VBELV  IN  S_VBELN
    AND  VBTYP_N = 'J'.
 SORT VBFA_TAB BY VBELV POSNV VBELN_J.
ENDFORM.

*
***********************************************************************
*                             GET_DB_DATA                             *
***********************************************************************
FORM GET_DB_DATA.

SELECT VBAK~VBELN VBAK~AUDAT VBAK~AUART VBAP~WERKS VBAP~POSNR
      VBAP~MATNR VBFA~VBELN VBFA~RFMNG VBFA~RFWRT VBFA~MATNR VBFA~BWART
  INTO TABLE ITAB
  FROM    ( (  VBAK INNER JOIN VBAP
                    ON VBAP~VBELN = VBAK~VBELN )
                    INNER JOIN VBFA
                    ON VBFA~VBELV = VBAK~VBELN AND
                       VBFA~POSNV = VBAP~POSNR )
  WHERE  VBAK~VBELN IN S_VBELN
    AND  VBAK~AUDAT IN S_AUDAT
    AND  VBAP~MATNR =  P_MATNR
    AND  VBFA~VBTYP_N = 'R'.

 IF SY-SUBRC <> 0.
    WRITE: /1 'NO DATA SELECTED'.
    STOP.
 ENDIF.
ENDFORM.

***********************************************************************
*                        BUILD_REPORT_TABLE.                          *
***********************************************************************
FORM BUILD_REPORT_TABLE.

 LOOP AT ITAB.
      MOVE-CORRESPONDING ITAB TO REPTAB.
      IF ITAB-BWART = '653'.
         REPTAB-RFMNG = ITAB-RFMNG * -1.
         REPTAB-RFWRT = ITAB-RFWRT * -1.
      ENDIF.
      READ TABLE MBEWH_TAB WITH KEY BWKEY = ITAB-WERKS.
      IF SY-SUBRC = 0.
         MOVE MBEWH_TAB-STPRS  TO  REPTAB-STPRS.
         REPTAB-ADJ_COST = ITAB-RFMNG * ( MBEWH_TAB-STPRS / 1000 ).
         IF ITAB-BWART = '653'.
            REPTAB-ADJ_COST = REPTAB-ADJ_COST * -1.
         ENDIF.
         REPTAB-ADJ_DIFF = REPTAB-RFWRT - REPTAB-ADJ_COST.
         IF REPTAB-ADJ_DIFF <> 0.
            MOVE 'X' TO COST_COMP_IND.
         ENDIF.
      ENDIF.

      READ TABLE VBFA_TAB WITH KEY VBELV = ITAB-VBELN
                                   POSNV = ITAB-POSNR.
      IF SY-SUBRC = 0.
         MOVE VBFA_TAB-VBELN_J  TO  REPTAB-VBELN_J.
      ENDIF.
      APPEND REPTAB.
      CLEAR  REPTAB.
 ENDLOOP.

ENDFORM.

***********************************************************************
*                        CREATE_BDC_SESSION.                          *
***********************************************************************
FORM CREATE_BDC_SESSION.

DATA: FIRST_TIME VALUE 'Y'.
  PERFORM OPEN_BATCH_SESSION.

LOOP AT REPTAB WHERE ADJ_DIFF <> 0.
  MOVE REPTAB-ADJ_DIFF TO W_AMOUNT.
  IF REPTAB-ADJ_DIFF < 0.
     PERFORM PUT_SIGN_IN_FRONT CHANGING W_AMOUNT.
  ENDIF.

  IF FIRST_TIME = 'Y'.
     CLEAR FIRST_TIME.
     PERFORM BDC_SCREEN USING 'SAPMF05A' '100'.
     PERFORM BDC_FIELD  USING 'BKPF-BLDAT' P_BLDAT.
     PERFORM BDC_FIELD  USING 'BKPF-BLART' P_BLART.
     PERFORM BDC_FIELD  USING 'BKPF-BUKRS' P_BUKRS.
     PERFORM BDC_FIELD  USING 'BKPF-BUDAT' P_BLDAT.
     PERFORM BDC_FIELD  USING 'BKPF-MONAT' P_MONAT.
     PERFORM BDC_FIELD  USING 'BKPF-WAERS' P_WAERS.
     PERFORM BDC_FIELD  USING 'BKPF-XBLNR' P_XBLNR.
     PERFORM BDC_FIELD  USING 'BKPF-BKTXT' P_BKTXT.
  ENDIF.

  IF REPTAB-ADJ_DIFF < 0.
     PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '50'.
     PERFORM BDC_FIELD  USING 'RF05A-NEWKO' P_OFFSET.
     MOVE '40' TO NEXT_KEY.
     MOVE P_COGADJ  TO  NEXT_ACCT.
  ELSE.
     PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '40'.
     PERFORM BDC_FIELD  USING 'RF05A-NEWKO' P_COGADJ.
     MOVE '50' TO NEXT_KEY.
     MOVE P_OFFSET  TO  NEXT_ACCT.
  ENDIF.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/00'.

  PERFORM BDC_SCREEN USING 'SAPMF05A'    '300'.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR'  W_AMOUNT. "REPTAB-ADJ_DIFF.
*  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'  P_MWSKZ.
  PERFORM BDC_FIELD  USING 'DKACB-FMORE'  ' '.
  PERFORM BDC_FIELD  USING 'BSEG-SGTXT'  REPTAB-VBELN_R.
  PERFORM BDC_FIELD  USING 'RF05A-NEWBS' NEXT_KEY.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' NEXT_ACCT.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/00'.
  PERFORM BDC_SCREEN USING 'SAPMF05A'   '300'.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR'  W_AMOUNT. "REPTAB-ADJ_DIFF.
*  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'  P_MWSKZ.
  PERFORM BDC_FIELD  USING 'DKACB-FMORE'  ' '.
  PERFORM BDC_FIELD  USING 'BSEG-SGTXT'  REPTAB-VBELN_R.
ENDLOOP.

  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/03'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '700'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.

  PERFORM INSERT_SESSION.
  PERFORM CLOSE_SESSION.

ENDFORM.

*-----------------------------------------------------------------------
*     FORM OPEN_BATCH_SESSION
*-----------------------------------------------------------------------
FORM OPEN_BATCH_SESSION.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT            = SY-MANDT
            GROUP             = P_GRUPID
            KEEP              = 'X'
            USER              = SY-UNAME
       EXCEPTIONS
            GROUP_INVALID     = 1
            GROUP_IS_LOCKED   = 2
            HOLDDATE_INVALID  = 3
            INTERNAL_ERROR    = 4
            QUEUE_ERROR       = 5
            RUNNING           = 6
            SYSTEM_LOCK_ERROR = 7
            USER_INVALID      = 8.

  IF SY-SUBRC <> 0.
    MESSAGE E004(ZS) WITH P_GRUPID.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM INSERT_SESSION
*-----------------------------------------------------------------------
FORM INSERT_SESSION.

  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = W_TCODE
       TABLES
            DYNPROTAB      = BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4.
  IF SY-SUBRC <> 0.
    MESSAGE E013(ZS) WITH SY-SUBRC.
  ENDIF.

ENDFORM.
*-----------------------------------------------------------------------
*     FORM CLOSE_SESSION
*-----------------------------------------------------------------------
FORM CLOSE_SESSION.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN
            QUEUE_ERROR.
  IF SY-SUBRC = 0.
    MESSAGE I003(ZS) WITH P_GRUPID.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM BDC_SCREEN
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM = PROGRAM.
  BDCDATA-DYNPRO = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.

*-----------------------------------------------------------------------
*     FORM BDC_FIELD
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.

  DATA: TVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.
*----------------------------------------------------------------------*
*       FORM PUT_SIGN_IN_FRONT.                                        *
*----------------------------------------------------------------------*

FORM PUT_SIGN_IN_FRONT CHANGING VALUE.
  DATA: TEXT1(1) TYPE C.

  SEARCH VALUE FOR '-'.
  IF SY-SUBRC = 0 AND SY-FDPOS <> 0.
    SPLIT VALUE AT '-' INTO VALUE TEXT1.
    CONDENSE VALUE.
*    CONCATENATE '-' VALUE INTO VALUE.
  ELSE.
    CONDENSE VALUE.
  ENDIF.
ENDFORM.
***********************************************************************
*                        DISPLAY_ALV_GRID_DATA                        *
***********************************************************************
FORM DISPLAY_ALV_GRID_DATA.

DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      FC_STR   TYPE SLIS_FIELDCAT_ALV,
      LAYOUT   TYPE SLIS_LAYOUT_ALV,
      TITLE    TYPE LVC_TITLE,
      REPID    LIKE SY-REPID,
      VARIANT  LIKE DISVARIANT,
      SORT     TYPE SLIS_T_SORTINFO_ALV,
      SORT_STR TYPE SLIS_SORTINFO_ALV.

  CONCATENATE TEXT-004 P_LFMON TEXT-005 P_LFGJA INTO W_HEAD01
                                         SEPARATED BY SPACE.

  MOVE TEXT-CLT  TO W_HEAD02+0(7).
  MOVE SY-SYSID  TO W_HEAD02+8(5).
  MOVE SY-MANDT  TO W_HEAD02+14(4).
  MOVE TEXT-DTE  TO W_HEAD02+21(5).
  WRITE SY-DATUM TO W_HEAD02+27(10).
  MOVE TEXT-TME  TO W_HEAD02+40(5).
  WRITE SY-UZEIT TO W_HEAD02+46(10).
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.
  VARIANT-REPORT = REPID.
*  VARIANT-VARIANT = PVARIANT.
  REFRESH FIELDCAT.
  CLEAR:  FIELDCAT, FC_STR.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'REPTAB'
       I_INCLNAME             = REPID
    CHANGING
       CT_FIELDCAT            = FIELDCAT
    EXCEPTIONS
       INCONSISTENT_INTERFACE = 1
       PROGRAM_ERROR          = 2
       OTHERS                 = 3.

* update field catalog (hide/reposition/etc)
LOOP AT FIELDCAT INTO FC_STR.

CASE FC_STR-FIELDNAME.
     WHEN 'POSNR'.
           FC_STR-KEY    = ' '.               " Key columns-not first
     WHEN 'VBELN_J'.
          FC_STR-SELTEXT_L = TEXT-C08.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
           FC_STR-KEY    = ' '.               " Key columns-not first
     WHEN 'VBELN_R'.
          FC_STR-SELTEXT_L = TEXT-C09.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
           FC_STR-KEY    = ' '.               " Key columns-not first
     WHEN 'ADJ_COST'.
          FC_STR-SELTEXT_L = TEXT-C13.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'ADJ_DIFF'.
          FC_STR-SELTEXT_L = TEXT-C14.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN OTHERS.
** fc_str-no_out = 'X'.           " hide column
ENDCASE.

MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.

* Display ALV
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
           IT_FIELDCAT  = FIELDCAT
           IS_LAYOUT    = LAYOUT
           I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
          I_CALLBACK_PROGRAM      = repid
          I_SAVE       = 'A'
          IS_VARIANT   = variant
          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = REPTAB
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.

ENDFORM.

*************************************************************
*                        TOP OF PAGE                        *
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*1- Heading Line: Type H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- Heading Line: Type H
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'H'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*3- Action Line:  Type A
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.
ENDFORM.
