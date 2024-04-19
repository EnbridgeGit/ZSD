*&---------------------------------------------------------------------*
*& Report  ZLSDC004_CONTRAX_GASSALE_SPLIT
*&
*&---------------------------------------------------------------------*
*&**********************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        November 2010.                                         *
*  Issue Log:   TR804                                                  *
*  Description:                                                        *
*     - This program will receive the receive the contrax gas sales    *
*       revenue files and split into three unique files to be used for *
*       Sales Order IDocs, Accounting BDCs, and Goods Movement IDocs  *
*&---------------------------------------------------------------------*
*CHANGES****                                                           *
*& ACR-244    Contrax Rate Splitting Project                           *
*& GYMANA     Changed to accept new file layout and logic to handle    *
*& 2016/05/26 6 new sales order types and split out Rate Rider records *
*&---------------------------------------------------------------------*

REPORT  ZLSDC004_CONTRAX_GASSALE_SPLIT NO STANDARD PAGE HEADING
LINE-COUNT 65 LINE-SIZE 80 MESSAGE-ID ZM.

TABLES: ZLSDC01.     "Contrax Gas Sales Order Mapping
* Input/Output file format
DATA:  BEGIN OF CREC,
        APP_YR(4)             TYPE C,     "Year transaction applied
        APP_MTH(2)            TYPE C,     "Month transaction applied
        CUST_ID(8)            TYPE C,     "Customer number
        RATE_CL(12)           TYPE C,     "Rate class
        SERV_TYPE(6)          TYPE C,     "Service type
        SERV_CL(2)            TYPE C,     "Service class
        SEAS_CL(4)            TYPE C,     "Seasonal class
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
        INV_NUM(8)            TYPE C,     "INVOICE # Ticket 65368
        REBILL_YR(4)          TYPE C,     "REBILL YEAR         ACR244
        REBILL_MTH(2)         TYPE C,     "REBILL MONTH        ACR244
        SO_ID(4)              TYPE C.     "SERVICE OFFERING ID ACR244

DATA: END OF CREC.

DATA: INREC(400).

* CONSTANTS: W_DMTR Type X VALUE '09'.
CONSTANTS: W_DMTR TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

*------------------------  Selection Screen  ---------------------------
* Input File
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_NRTTP FOR ZLSDC01-C_NRTTP OBLIGATORY.   "Non Rate Type
SELECTION-SCREEN SKIP.
PARAMETER: INFILE LIKE FILENAMECI-FILEEXTERN OBLIGATORY.
SELECTION-SCREEN SKIP.
* Output Files
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
PARAMETERS:
OUTFILE1 LIKE FILENAMECI-FILEEXTERN OBLIGATORY,
OUTFILE2 LIKE FILENAMECI-FILEEXTERN OBLIGATORY,
OUTFILE3 LIKE FILENAMECI-FILEEXTERN OBLIGATORY,
OUTFILE4 LIKE FILENAMECI-FILEEXTERN OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN END OF BLOCK BOX.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE:
  '/usr/sap/interfaces/' SY-SYSID+0(3) '/CONTRAX/ctrx_act.chk'
                                                         INTO INFILE,
  '/usr/sap/interfaces/' SY-SYSID(3) '/CONTRAX/zctrxactsalesorder.dat'
                                                         INTO OUTFILE1,
  '/usr/sap/interfaces/' SY-SYSID(3) '/CONTRAX/zctrxactgsacc.dat'
                                                         INTO OUTFILE2,
  '/usr/sap/interfaces/' SY-SYSID(3) '/CONTRAX/zctrxactfuel.dat'
                                                         INTO OUTFILE3,
  '/usr/sap/interfaces/' SY-SYSID(3) '/CONTRAX/zctrxactrrider.dat'
                                                         INTO OUTFILE4.

***********************************************************************
START-OF-SELECTION.

  OPEN DATASET INFILE FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC NE 0.
    MESSAGE E006(ZM) WITH INFILE.
  ENDIF.

  OPEN DATASET OUTFILE1 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC NE 0.
    MESSAGE E006(ZM) WITH OUTFILE1.
  ENDIF.

  OPEN DATASET OUTFILE2 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC NE 0.
    MESSAGE E006(ZM) WITH OUTFILE2.
  ENDIF.

  OPEN DATASET OUTFILE3 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC NE 0.
    MESSAGE E006(ZM) WITH OUTFILE3.
  ENDIF.

  OPEN DATASET OUTFILE4 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC NE 0.
    MESSAGE E006(ZM) WITH OUTFILE4.
  ENDIF.

  DO.
    CLEAR: CREC, ZLSDC01-MATNR.
    READ DATASET INFILE INTO INREC.
    IF SY-SUBRC <> '0'.
      EXIT.
    ENDIF.
    SPLIT INREC AT W_DMTR INTO CREC-APP_YR CREC-APP_MTH
    CREC-CUST_ID CREC-RATE_CL CREC-SERV_TYPE
    CREC-SERV_CL CREC-SEAS_CL CREC-RATE_TYPE CREC-CHARGE_TYPE
    CREC-SR_USAGE CREC-ST_SUBTYPE CREC-NON_RATE_ITEM_TYP
    CREC-TIER_STEP_LVL CREC-SECTOR_SIZE CREC-SECTOR CREC-AMOUNT
    CREC-VOLUME CREC-CUST_TYPE CREC-GECA_CODE CREC-VOL_UOM CREC-SA_NUM
    CREC-INV_NUM CREC-REBILL_YR CREC-REBILL_MTH CREC-SO_ID.
*&--Start of code Added by JOOKONTR CHG0165696
    IF CREC-REBILL_MTH+1(1) EQ '' AND CREC-REBILL_MTH+0(1) ne '0' AND CREC-REBILL_MTH ne space.
      CONCATENATE '0' CREC-REBILL_MTH INTO CREC-REBILL_MTH.
    ENDIF.
*&--End of code Added by JOOKONTR CHG0165696
* ACR244 - All Rate Rider records will be split off into outfile4
    IF CREC-RATE_TYPE = 'COMR' OR CREC-NON_RATE_ITEM_TYP IN S_NRTTP.
      TRANSFER CREC TO OUTFILE4 LENGTH 148.
    ELSE.
      IF CREC-SR_USAGE   = 'FUEL' OR CREC-NON_RATE_ITEM_TYP = 'FUELADJ'.
        TRANSFER CREC TO OUTFILE3 LENGTH 148.
      ELSE.
        SELECT SINGLE MATNR KSCHL INTO (ZLSDC01-MATNR, ZLSDC01-KSCHL)
          FROM ZLSDC01
         WHERE C_NRTTP   = CREC-NON_RATE_ITEM_TYP
        AND ( C_SVCTYP   = CREC-SERV_TYPE   OR C_SVCTYP  = '******' )
        AND ( C_CUSTTYP  = CREC-CUST_TYPE   OR C_CUSTTYP = '*' )
        AND ( C_RTETYP   = CREC-RATE_TYPE   OR C_RTETYP  = '****' )
        AND ( C_CHGTYP   = CREC-CHARGE_TYPE OR C_CHGTYP  = '****' )
        AND ( C_SRUSE    = CREC-SR_USAGE    OR C_SRUSE   = '****' )
        AND ( C_STSUB    = CREC-ST_SUBTYPE  OR C_STSUB   = '******' )
        AND ( C_SCCODE   = CREC-SERV_CL     OR C_SCCODE  = '**' ).

        IF SY-SUBRC <> 0.
          TRANSFER CREC TO OUTFILE2 LENGTH 148.
        ELSE.
          IF ZLSDC01-MATNR <> SPACE.
            TRANSFER CREC TO OUTFILE1 LENGTH 148.
          ELSE.
            TRANSFER CREC TO OUTFILE2 LENGTH 148.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDDO.
  CLOSE DATASET: INFILE, OUTFILE1, OUTFILE2, OUTFILE3, OUTFILE4.

  MESSAGE I100(ZM) WITH TEXT-100.

END-OF-SELECTION.
