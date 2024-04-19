*&---------------------------------------------------------------------*
*& Report  ZLSDR002_IDOC_RECONCILIATION
*&
*&---------------------------------------------------------------------*
*  Author:      Mohammad T. Khan                                       *
*  Date:        April, 2011.                                           *
*  Issue Log:   TR804                                                  *
*  Description:                                                        *
*    - This program will retrieve the idoc numbers for a partner system*
* posted on a particular day and list all follow-up documents (sales   *
* order/deliveries/material documents/billing documents/GL Documents)  *
* that relate to the idoc number.                                      *
*----------------------------------------------------------------------*
*CHANGES****                                                           *
*&                                                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

REPORT  ZLSDR002_IDOC_RECONCILIATION.
TYPE-POOLS: SLIS.

TABLES: EDIDC,      "Control record (IDoc)
        EDIDS,      "Status Record (IDoc)
        VBAK,       "Sales Document: Header Data
        MSEG,       "Document Segment: Material
        VBFA,       "Sales Document Flow
        BSID.       "Accounting: Secondary Index for Customers

DATA: BEGIN OF ITAB OCCURS 1,
      DOCNUM	LIKE	EDIDC-DOCNUM,
      CREDAT	LIKE	EDIDC-CREDAT,
      CRETIM	LIKE	EDIDC-CRETIM,
      UNAME   LIKE   EDIDS-UNAME,
      SNDPRN	LIKE	EDIDC-SNDPRN,
      STATUS	LIKE	EDIDS-STATUS,
      STAPA1	LIKE	EDIDS-STAPA1,
      STAPA2	LIKE	EDIDS-STAPA2,
      STAPA3	LIKE	EDIDS-STAPA3,
      STAMID	LIKE	EDIDS-STAMID,
      STAMNO	LIKE	EDIDS-STAMNO,
      END OF ITAB.

DATA: BEGIN OF REPTAB OCCURS 1,
      DOCNUM	LIKE	EDIDC-DOCNUM,
      CREDAT	LIKE	EDIDC-CREDAT,
      CRETIM	LIKE	EDIDC-CRETIM,
      UNAME   LIKE   EDIDS-UNAME,
      SNDPRN	LIKE	EDIDC-SNDPRN,
      STATUS	LIKE	EDIDS-STATUS,
      STAPA1	LIKE	EDIDS-STAPA1,
      STAPA2	LIKE	EDIDS-STAPA2,
      STAPA3	LIKE	EDIDS-STAPA3,
      STAMID	LIKE	EDIDS-STAMID,
      STAMNO	LIKE	EDIDS-STAMNO,
      KUNNR	LIKE	VBAK-KUNNR,
      NETWR	LIKE	VBAK-NETWR,
      VBELN1	LIKE	VBFA-VBELN,
      ERFMG	LIKE	MSEG-ERFMG,
      ERFME	LIKE	MSEG-ERFME,
      MENGE	LIKE	MSEG-MENGE,
      MEINS	LIKE	MSEG-MEINS,
      VBELN2	LIKE	VBFA-VBELN,
      BELNR	LIKE	BSID-BELNR,
      END OF REPTAB.

DATA: W_HEAD01(60)  TYPE C,
      W_HEAD02(60)  TYPE C,
      ES_VARIANT    LIKE DISVARIANT,
      IS_VARIANT    LIKE DISVARIANT,
      W_STAPA2(10)  TYPE N.

*Data Fields to Restrict ranges
TYPE-POOLS: SSCR.
DATA: L_RESTRICT TYPE SSCR_RESTRICT,
      L_OPT_LIST TYPE SSCR_OPT_LIST,
      L_ASS TYPE SSCR_ASS.
***********************************************************************
*                      SELECTION SCREEN                               *
***********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
*SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-101.
SELECT-OPTIONS:
  S_CREDAT FOR EDIDC-CREDAT DEFAULT SY-DATUM OBLIGATORY,
  S_CRETIM FOR EDIDC-CRETIM,
  S_SNDPRN FOR EDIDC-SNDPRN DEFAULT 'BANNER' OBLIGATORY,
  S_STAMN2 FOR EDIDS-STAMNO DEFAULT '260'.
PARAMETERS:
  P_STAMD1 LIKE EDIDS-STAMID DEFAULT 'B1',
  P_STAMN1 LIKE EDIDS-STAMNO DEFAULT '501',
  P_STAMD2 LIKE EDIDS-STAMID DEFAULT 'V1',
  P_FYEAR  LIKE MSEG-LFBJA DEFAULT SY-DATUM+0(4) OBLIGATORY,
  P_COCODE LIKE MSEG-BUKRS DEFAULT 'UGL' OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS:
  BOX_SDOC  AS CHECKBOX,           "Sales Documents
  BOX_MDOC  AS CHECKBOX,           "Material Documents
  BOX_BDOC  AS CHECKBOX.           "Billing/GL Documents
SELECTION-SCREEN SKIP.
PARAMETERS PVARIANT LIKE DISVARIANT-VARIANT.           "Display Variant
*SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN END OF BLOCK BOX.

INITIALIZATION.
  CLEAR L_OPT_LIST.
  L_OPT_LIST-NAME = 'S_STAMN2'.
  L_OPT_LIST-OPTIONS-EQ = 'X'.
  APPEND L_OPT_LIST TO L_RESTRICT-OPT_LIST_TAB.

  CLEAR L_ASS.
  L_ASS-KIND = 'S'.
  L_ASS-NAME = 'S_STAMN2'.
  L_ASS-SG_MAIN = 'I'.
  L_ASS-OP_MAIN = 'S_STAMN2'.
  APPEND L_ASS TO L_RESTRICT-ASS_TAB.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      RESTRICTION = L_RESTRICT
    EXCEPTIONS
      OTHERS      = 1.

  S_STAMN2-SIGN   = 'I'.
  S_STAMN2-OPTION = 'EQ'.
  S_STAMN2-LOW    = '261'.
  APPEND S_STAMN2.

***********************************************************************
*                      AT SELECTION-SCREEN OUTPUT                     *
***********************************************************************
AT SELECTION-SCREEN OUTPUT.
   LOOP AT SCREEN.
        IF SCREEN-NAME = 'P_STAMD1' OR SCREEN-NAME = 'P_STAMD2' OR
           SCREEN-NAME = 'P_STAMN1' OR SCREEN-NAME = 'P_STAMN2'.
           SCREEN-INPUT = 0.
           MODIFY SCREEN.
        ENDIF.
   ENDLOOP.
***********************************************************************
*                  SELECT DISPLAY VARIANT                             *
***********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PVARIANT.
  IS_VARIANT-REPORT = 'ZLSDR002_IDOC_RECONCILIATION'.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT          = IS_VARIANT
*           I_TABNAME_HEADER    =
*           I_TABNAME_ITEM      =
*           IT_DEFAULT_FIELDCAT =
            I_SAVE              = 'A'
        IMPORTING
*           E_EXIT              =
            ES_VARIANT          = ES_VARIANT
       EXCEPTIONS
            NOT_FOUND           = 1
            PROGRAM_ERROR       = 2
            OTHERS              = 3.
  PVARIANT = ES_VARIANT-VARIANT.

***********************************************************************
*                      START-OF-SELECTION                             *
***********************************************************************
START-OF-SELECTION.
PERFORM GET_DB_DATA.
PERFORM DISPLAY_ALV_GRID_DATA.

***********************************************************************
*                             GET_DB_DATA                             *
***********************************************************************
FORM GET_DB_DATA.
SELECT EDIDC~DOCNUM EDIDC~CREDAT EDIDC~CRETIM EDIDS~UNAME EDIDC~SNDPRN
       EDIDS~STATUS EDIDS~STAPA1 EDIDS~STAPA2 EDIDS~STAPA3 EDIDS~STAMID
       EDIDS~STAMNO
  INTO TABLE ITAB
  FROM    ( EDIDC INNER JOIN EDIDS
                 ON EDIDC~DOCNUM = EDIDS~DOCNUM )
  WHERE EDIDC~CREDAT  IN S_CREDAT
    AND EDIDC~CRETIM  IN S_CRETIM
    AND EDIDC~SNDPRN  IN S_SNDPRN
    AND ( ( EDIDS~STAMID = P_STAMD1 AND EDIDS~STAMNO = P_STAMN1 ) OR
        ( EDIDS~STAMID = P_STAMD2 AND EDIDS~STAMNO IN S_STAMN2 ) ).

 IF SY-SUBRC <> 0.
    WRITE: /1 'NO DATA SELECTED'.
    STOP.
 ENDIF.

 LOOP AT ITAB.
      MOVE-CORRESPONDING ITAB TO REPTAB.
      MOVE ITAB-STAPA2 TO W_STAPA2.
*Sale Order Selection
      IF BOX_SDOC  = 'X'.
         IF ITAB-STAPA1 = '0' OR ITAB-STAPA1 = SPACE.
            MOVE 'N/A' TO: REPTAB-KUNNR, REPTAB-NETWR.
         ELSE.
            SELECT SINGLE KUNNR NETWR INTO (VBAK-KUNNR, VBAK-NETWR)
              FROM VBAK
             WHERE VBELN = W_STAPA2.
             IF SY-SUBRC = 0.
                MOVE VBAK-KUNNR TO REPTAB-KUNNR.
                MOVE VBAK-NETWR TO REPTAB-NETWR.
             ENDIF.
         ENDIF.
      ENDIF.

*Material Document Selection
      IF BOX_MDOC  = 'X'.
         SELECT SINGLE VBELN INTO VBFA-VBELN
           FROM VBFA
          WHERE VBELV = W_STAPA2
            AND VBTYP_N = 'R'.          "Goods Movement
         IF SY-SUBRC = 0.
            MOVE VBFA-VBELN TO REPTAB-VBELN1.
*            SELECT SINGLE ERFMG ERFME MENGE MEINS
*              INTO (MSEG-ERFMG, MSEG-ERFME, MSEG-MENGE, MSEG-MEINS)
*              FROM MSEG
             SELECT MEINS SUM( MENGE ) ERFME SUM( ERFMG )
               INTO (MSEG-MEINS, MSEG-MENGE, MSEG-ERFME, MSEG-ERFMG)
               FROM MSEG
             WHERE MBLNR = VBFA-VBELN
               AND MJAHR = P_FYEAR
               GROUP BY MEINS ERFME.
            IF SY-SUBRC = 0.
               MOVE: MSEG-ERFMG  TO  REPTAB-ERFMG,
                     MSEG-ERFME  TO  REPTAB-ERFME,
                     MSEG-MENGE  TO  REPTAB-MENGE,
                     MSEG-MEINS  TO  REPTAB-MEINS.
            ENDIF.
            ENDSELECT.
         ENDIF.
      ENDIF.

*Billing Document Selection
      IF BOX_BDOC  = 'X'.
         CLEAR VBFA-VBELN.
         SELECT SINGLE VBELN INTO VBFA-VBELN
           FROM VBFA
          WHERE VBELV = W_STAPA2
            AND VBTYP_N IN ('M','O').
         IF SY-SUBRC = 0.
            MOVE VBFA-VBELN TO REPTAB-VBELN2.
            SELECT SINGLE BELNR INTO BSID-BELNR
              FROM BSID
             WHERE GJAHR = P_FYEAR
               AND BUKRS = P_COCODE
               AND VBELN = VBFA-VBELN.
            IF SY-SUBRC = 0.
               MOVE BSID-BELNR TO REPTAB-BELNR.
            ENDIF.
         ENDIF.
      ENDIF.
      APPEND REPTAB.
      CLEAR  REPTAB.
 ENDLOOP.
 SORT REPTAB BY DOCNUM CREDAT SNDPRN.
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

DATA: W_DATE_FROM(10) TYPE C,
      W_DATE_TO(10)   TYPE C.

CONCATENATE S_CREDAT-LOW(4) '/' S_CREDAT-LOW+4(2) '/' S_CREDAT-LOW+6(2)
                                                    INTO W_DATE_FROM.
CONCATENATE S_CREDAT-HIGH(4) '/' S_CREDAT-HIGH+4(2) '/'
                                 S_CREDAT-HIGH+6(2) INTO W_DATE_TO.
*
CONCATENATE TEXT-004 W_DATE_FROM TEXT-005 W_DATE_TO
            INTO W_HEAD01 SEPARATED BY SPACE.

  MOVE TEXT-CLT  TO W_HEAD02+0(7).
  MOVE SY-SYSID  TO W_HEAD02+8(5).
  MOVE SY-MANDT  TO W_HEAD02+14(4).
  MOVE TEXT-DTE  TO W_HEAD02+21(5).
  WRITE SY-DATUM TO W_HEAD02+27(10).
  MOVE TEXT-TME  TO W_HEAD02+40(5).
  WRITE SY-UZEIT TO W_HEAD02+46(10).
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-COUNTFNAME = 'BPMNG'.
  LAYOUT-ZEBRA = 'X'.
  VARIANT-REPORT = REPID.
  VARIANT-VARIANT = PVARIANT.
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

*Key Column Not First
DEFINE HIDE_COLUMN.
LOOP AT FIELDCAT INTO FC_STR.
     CASE FC_STR-FIELDNAME.
          WHEN &1.
          FC_STR-KEY    = ' '.               " Key columns-not first
     ENDCASE.
     MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.
END-OF-DEFINITION.

     HIDE_COLUMN:
     'DOCNUM', 'VBELN1', 'VBELN2', 'BELNR'.

*Sumup Column
DEFINE SUM_COLUMN.
LOOP AT FIELDCAT INTO FC_STR.
     CASE FC_STR-FIELDNAME.
          WHEN &1.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     ENDCASE.
     MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.
END-OF-DEFINITION.

      SUM_COLUMN:
     'NETWR', 'ERFMG', 'MENGE'.

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

*3- Heading Line: Type H
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'H'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- Action Line:  Type A
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.
ENDFORM.
