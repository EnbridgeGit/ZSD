*&---------------------------------------------------------------------*
*& Report  ZLSDC003_BANNER_OWN_USE
*&
*&---------------------------------------------------------------------*
*&**********************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        January  2011.                                         *
*  Issue Log:   TR804                                                  *
*  Description:                                                        *
*     - This program will receive the file and translate Billing Data  *
*  from the Banner billing system into the data required to fill the   *
*  Goods Issue IDOC.  Program will create flat file with the translated*
*  data.                                               *
*&---------------------------------------------------------------------*
*CHANGES****                                                           *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZLSDC003_BANNEROWNUSE MESSAGE-ID ZM.

TABLES: ZFB04,    "Banner Company Used Gas Lookup
        MBEW.     "Material Valuation

* Input file format
DATA:  BEGIN OF OWN_REC,
       PROCESS_DATE       TYPE D,
       SERV_TYPE(4)       TYPE C,
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
       CUST_CHRG(11)      TYPE C,
       CONS_SIGN(1)       TYPE C,
       CONS(13)           TYPE C,
       NO_OF_CUSTS(6)     TYPE N,
       EFF_DATE           TYPE D,
       RATE_CLASS(4)      TYPE C.
DATA:  END OF OWN_REC.

* Output file format
DATA: BEGIN OF OGI_REC,
      BUDAT	            TYPE D,     "POSTNG DATE
      BLDAT	            TYPE D,     "DOCUMENT DATE
      XBLNR(16)           TYPE C,     "REF DOC NO
      FRBNR(16)           TYPE C,	    "BILL OF LADING
      U_NAME(12)          TYPE C,     "PR UNAME
      BKTXT(25)           TYPE C,     "HEADER TEXT
      MATNR(18)           TYPE C,     "MATERIAL
      WERKS_D(4)          TYPE C,     "PLANT
      LGORT_D(4)        TYPE C,     "STORAGE LOC
      BWART(3)            TYPE C,     "MOVEMENT TYPE
      ERFMG(13)           TYPE C,     "ENTRY QNT
      ERFME(3)            TYPE C,     "ENTRY UOM
      ISOCD_UNIT(3)       TYPE C,     "ENTRY UOM ISO
      SGTXT(30)           TYPE C,	    "ITEM TEXT
      AUFNR(12)           TYPE C,	    "ORDER ID
      KOSTL(10)           TYPE C,     "Cost Center
      SAKNR(10)           TYPE C.     "GL ACCOUNT
DATA: END OF OGI_REC.

DATA: W_TRANS_AMT LIKE  MBEW-STPRS,
      W_ERFMG     LIKE  MSEG-ERFMG,
      W_CUST_NUM  LIKE  OWN_REC-CUST_NUM.

*------------------------  Selection Screen  ---------------------------
* Input File
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETERS:
  P_XBLNR  LIKE  BKPF-XBLNR DEFAULT 'BANNER: OWN USE'  OBLIGATORY,
  P_FRBNR  LIKE  MKPF-FRBNR DEFAULT 'OWN USE'          OBLIGATORY,
  P_BKTXT  LIKE  BKPF-BKTXT DEFAULT 'BANNER OWN USE'   OBLIGATORY,
*  P_SGTXT  LIKE  BSEG-SGTXT DEFAULT 'BANNER INTERFACE' OBLIGATORY,
  P_MATNR  LIKE  BSEG-MATNR DEFAULT 'NATGAS'           OBLIGATORY,
  P_LGORT  LIKE  MARD-LGORT DEFAULT 'A001'             OBLIGATORY,
  P_ERFME  LIKE  MSEG-ERFME DEFAULT 'GJ1'              OBLIGATORY,
  P_ISOCD(3) TYPE C         DEFAULT 'GJ'               OBLIGATORY,
  P_KOSTL  LIKE ZFB04-KOSTL DEFAULT '0000020310'       OBLIGATORY.
SELECTION-SCREEN SKIP.
* Output Files
PARAMETER: INFILE LIKE FILENAMECI-FILEEXTERN OBLIGATORY,
           OUTFILE LIKE FILENAMECI-FILEEXTERN OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
            '/BANNER/zbis100owngi.dat' INTO INFILE.
CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
            '/BANNER/zbannowngi.dat' INTO OUTFILE.

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
    CLEAR: OWN_REC, OGI_REC.
    READ DATASET INFILE INTO OWN_REC.
    IF SY-SUBRC <> '0'.
      EXIT.
    ENDIF.

 IF OWN_REC-TRANS_AMT <> '0000000000.00'.
*Move data from input file.
    MOVE: OWN_REC-PROCESS_DATE   TO  OGI_REC-BUDAT,
          OWN_REC-PROCESS_DATE   TO  OGI_REC-BLDAT,
          SY-UNAME               TO  OGI_REC-U_NAME.
    IF OWN_REC-GL_CLASSCD = 'OWNS' OR OWN_REC-GL_CLASSCD = 'CMPS'.
       MOVE 'GSTH'       TO  OGI_REC-WERKS_D.
    ELSE.
       MOVE 'GNTH'       TO  OGI_REC-WERKS_D.
    ENDIF.

    IF OWN_REC-GL_CLASSCD = 'OWNS' OR OWN_REC-GL_CLASSCD = 'OWNC'.
       MOVE '0000452005' TO  OGI_REC-SAKNR.
    ELSE.
       MOVE '0000302900' TO  OGI_REC-SAKNR.
    ENDIF.

    IF OGI_REC-SAKNR = '0000452005'.
       IF OWN_REC-CONS_SIGN = '+'.
           MOVE '261'  TO  OGI_REC-BWART.
       ELSE.
           MOVE '262'  TO  OGI_REC-BWART.
       ENDIF.
    ELSEIF OGI_REC-SAKNR = '0000302900'.
       IF OWN_REC-CONS_SIGN = '+'.
          MOVE '201'   TO  OGI_REC-BWART.
       ELSE.
          MOVE '202'   TO  OGI_REC-BWART.
       ENDIF.
    ENDIF.

*Move Variant Fields
    MOVE: P_XBLNR    TO   OGI_REC-XBLNR,
          P_FRBNR    TO   OGI_REC-FRBNR,
          P_BKTXT    TO   OGI_REC-BKTXT,
          P_MATNR    TO   OGI_REC-MATNR,
          P_LGORT    TO   OGI_REC-LGORT_D,
          P_ERFME    TO   OGI_REC-ERFME,
          P_ISOCD    TO   OGI_REC-ISOCD_UNIT.
   CONCATENATE OWN_REC-CUST_NUM+2(7) '-' OWN_REC-CUST_NUM+9(7)
                                         INTO  W_CUST_NUM.

   CONCATENATE P_BKTXT W_CUST_NUM INTO  OGI_REC-SGTXT
               SEPARATED BY SPACE.

*Get data from table MBEW
    CLEAR MBEW-STPRS.
    SELECT SINGLE STPRS INTO MBEW-STPRS FROM MBEW
     WHERE MATNR = P_MATNR
       AND BWKEY = OGI_REC-WERKS_D.
       IF SY-SUBRC = 0.
          MOVE OWN_REC-TRANS_AMT TO  W_TRANS_AMT.
          W_ERFMG = W_TRANS_AMT / ( MBEW-STPRS / 1000 ).
          MOVE W_ERFMG TO OGI_REC-ERFMG.
       ENDIF.

    IF OWN_REC-GL_CLASSCD = 'CMPS'  OR  OWN_REC-GL_CLASSCD = 'CMPN'.
       MOVE P_KOSTL  TO  OGI_REC-KOSTL.
    ELSE.
       CLEAR ZFB04-OAUFNR.
       SELECT SINGLE OAUFNR INTO ZFB04-OAUFNR
         FROM ZFB04
        WHERE Z_BCUST = OWN_REC-CUST_NUM.
           IF SY-SUBRC = 0.
              MOVE ZFB04-OAUFNR TO OGI_REC-AUFNR.
           ENDIF.
    ENDIF.

*Insert Record to Output file
       TRANSFER OGI_REC TO OUTFILE.
 ENDIF.
ENDDO.

  MESSAGE I100(ZM) WITH TEXT-100.
END-OF-SELECTION.
