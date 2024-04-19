*&---------------------------------------------------------------------*
*& Report  ZLSDC006_CONTRAX_GIS_TRANSLATE
*&
*&---------------------------------------------------------------------*
*&**********************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        February 2011.                                         *
*  Issue Log:   TR804                                                  *
*  Description:                                                        *
*     - This program will receive sales file zcontraxgssalesorder.dat  *
*       Data will be translated using custome tables and an output file*
*       will be created for the data segments of Basic Type MBGMCR02   *
*&---------------------------------------------------------------------*
*CHANGES****
*& 2014/09/15 - Added invoice number to input file layout.
*& SDP74189
*& GYMANA
*& 2016/12/06 - Added Rebill_yr, Rebill_mth, and SO_id to input file
*& ACR-244      layout.  These will not be used in this program.
*& GYMANA
*& akmadasu
*&---------------------------------------------------------------------*
************************************************************************
*CHANGES:                                                              *
*INC0991490   SKAKUMANU 2018/10/12                                     *
*TR#  D30K929186   CHG0125116                                          *
*Desc:For Fuel Records, if GJ is passed as the Unit of Measure         *
*just use the volume as passed by the file.  If MJ, use existing logic,*
* and if any other UoM is passed throw exception and abend the program.*
* 14/02/2019 akmadasu added new condtions for for GJ and MJ UOM
************************************************************************
* 2021/02/23 BIRUDURD COG changes to pass Aggregate Customer from      *
*                         selection screen                             *
*&---------------------------------------------------------------------*

REPORT  ZLSDC006_CONTRAX_GIS_TRANSLATE NO STANDARD PAGE HEADING
LINE-COUNT 65 LINE-SIZE 80 MESSAGE-ID ZM.

TABLES: MBEW,        "Material Valuation
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
        INV_NUM(8)            TYPE C,     "INVOICE NUMBER    SDP74189
        REBILL_YR(4)          TYPE C,     "REBILL YEAR         ACR244
        REBILL_MTH(2)         TYPE C,     "REBILL MONTH        ACR244
        SO_ID(4)              TYPE C.     "SERVICE OFFERING ID ACR244
DATA: END OF CTRX.

* Output file format
DATA:  BEGIN OF IDOCREC,
        BUDAT            TYPE D,  "Document Date
   BLDAT             TYPE D,  "Billing Date
        XBLNR(16)             TYPE C,    "Ref Doc #
        FRBNR(16)             TYPE C,    "Bill of lading
        UNAME(12)             TYPE C,    "PR Uname
        BKTXT(25)             TYPE C,    "Header Text
   MATNR(18)           TYPE C,  "Material Number
   DWERK(4)          TYPE C,  "Plant
   LGORT(4)          TYPE C,  "Storage Location
   BWART(3)              TYPE C,  "Movement Type
   ERFMG(13)           TYPE C,  "Entry Qty
   ERFME(3)          TYPE C,  "Entry UOM
   ISOCD(3)          TYPE C,  "ISO - UOM
   SGTXT(30)           TYPE C,  "Item Text
   KOSTL(19)           TYPE C,  "Cost Center
   SAKNR(10)           TYPE C,  "GL Account
        EXBWR(13)             TYPE C.    "Amount in LC
DATA: END OF IDOCREC.

DATA: W_DATE_IN  LIKE SY-DATUM,
      W_DATE_OUT LIKE SY-DATUM,
      W_TOWNCODE LIKE ZLSDBN002-TOWNCODE,
      W_MUNICODE LIKE ZLSDBN002-MUNICODE.

DATA: W_CTRX_AMT  LIKE  MBEW-STPRS,
      W_ERFMG     LIKE  MSEG-ERFMG,
      W_EXBWR     LIKE  MBEW-STPRS.
.

*------------------------  Selection Screen  ---------------------------
*
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-000.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-001.
PARAMETER:
P_MATNR LIKE MARA-MATNR DEFAULT 'NATGAS' OBLIGATORY,   "Material #
P_LGORT LIKE MSEG-LGORT DEFAULT 'A001'   OBLIGATORY,   "Storage Loctn
P_XBLNR LIKE BKPF-XBLNR DEFAULT 'CTRX GS: CSF' OBLIGATORY, "Ref Doc #
P_FRBNR LIKE MKPF-FRBNR DEFAULT 'CTRX GS: CSF' OBLIGATORY, "Bill of LAD
P_ERFME LIKE MSEG-ERFME DEFAULT 'GJ1'    OBLIGATORY,   "Entry UOM
P_ISOCD(2) TYPE C       DEFAULT 'GJ'     OBLIGATORY,   "ISO - UOM
P_HKONT LIKE BSEG-HKONT DEFAULT '390686' OBLIGATORY,   "ACCT #
P_KOSTL LIKE MSEG-KOSTL DEFAULT '20310'  OBLIGATORY,   "Cost Center
P_KUNNR LIKE BSEG-KUNNR DEFAULT '' OBLIGATORY. "Customer COG

SELECTION-SCREEN END OF BLOCK BOX2.
* Input/Output Files
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-002.
PARAMETERS:
INFILE LIKE FILENAMECI-FILEEXTERN OBLIGATORY,
OUTFILE LIKE FILENAMECI-FILEEXTERN OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX3.
SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-003.
PARAMETERS:     P_ACT RADIOBUTTON GROUP RBCR,            "Actual
                P_EST RADIOBUTTON GROUP RBCR,            "Estimate
                P_REV RADIOBUTTON GROUP RBCR.            "Reversal
SELECTION-SCREEN END OF BLOCK BOX4.
SELECTION-SCREEN END OF BLOCK BOX1.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE:
  '/usr/sap/interfaces/' SY-SYSID(3) '/LSDCNTXDP/zcontraxgsfuel.dat'
                                                           INTO INFILE,
  '/usr/sap/interfaces/' SY-SYSID(3) '/LSDCNTXDP/zcontraxfuelidoc.dat'
                                                           INTO OUTFILE.
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
    READ DATASET INFILE INTO CTRX.
    IF SY-SUBRC <> '0'.
      EXIT.
    ENDIF.

*Move Data from File and Variant Screen Fields
    MOVE: P_XBLNR     TO  IDOCREC-XBLNR,
          P_FRBNR     TO  IDOCREC-FRBNR,
          P_MATNR     TO  IDOCREC-MATNR,
          P_LGORT     TO  IDOCREC-LGORT,
          P_ERFME     TO  IDOCREC-ERFME,
          P_ISOCD     TO  IDOCREC-ISOCD,
          P_KOSTL     TO  IDOCREC-KOSTL,
          P_HKONT     TO  IDOCREC-SAKNR.
*        CTRX-AMOUNT+3(13) TO  IDOCREC-EXBWR.
*        IF IDOCREC-EXBWR < 0.
*           IDOCREC-EXBWR = IDOCREC-EXBWR * -1.
*           SHIFT IDOCREC-EXBWR RIGHT.
*        ENDIF.
*Calculate and Move Data Plus System Fields.
    MOVE SY-UNAME   TO  IDOCREC-UNAME.
* Start of changes    COG
*    CONCATENATE 'CX' CTRX-CUST_ID CTRX-SA_NUM ':CSF'
*                 INTO: IDOCREC-BKTXT, IDOCREC-SGTXT.
    CONCATENATE 'CX' P_KUNNR CTRX-SA_NUM ':CSF'
                 INTO: IDOCREC-BKTXT, IDOCREC-SGTXT.
* End of changes    COG
    CONDENSE IDOCREC-BKTXT NO-GAPS.
    CONDENSE IDOCREC-SGTXT NO-GAPS.

    IF P_ACT  = 'X' OR P_EST  = 'X'.
      IF CTRX-VOLUME < 0.
        MOVE '502'  TO IDOCREC-BWART.
      ELSE.
        MOVE '501'  TO IDOCREC-BWART.
      ENDIF.
    ELSE.
      IF CTRX-VOLUME < 0.
        MOVE '501'  TO IDOCREC-BWART.
      ELSE.
        MOVE '502'  TO IDOCREC-BWART.
      ENDIF.
    ENDIF.

    CONCATENATE CTRX-APP_YR CTRX-APP_MTH '01' INTO W_DATE_IN.
    IF P_ACT  = 'X'.                                     "Actual
      MOVE SY-DATUM TO: IDOCREC-BUDAT, IDOCREC-BLDAT.
    ELSE.
      PERFORM GET_LAST_DAY_OF_MONTH USING W_DATE_IN CHANGING W_DATE_OUT.
      IF P_EST  = 'X'.                                   "Estimate
        MOVE W_DATE_OUT TO: IDOCREC-BUDAT, IDOCREC-BLDAT.
      ELSE.
        W_DATE_OUT = W_DATE_OUT + 1.                    "Reversal
        MOVE W_DATE_OUT TO: IDOCREC-BUDAT, IDOCREC-BLDAT.
      ENDIF.
    ENDIF.

*Get Data from DB Tables
    MOVE CTRX-GECA_CODE(2)    TO  W_TOWNCODE.
    MOVE CTRX-GECA_CODE+2(4)  TO  W_MUNICODE.
    CLEAR: ZLSDBN002-VKBUR, ZLSDBN002-WERKS.

    SELECT SINGLE * FROM ZLSDBN002
     WHERE TOWNCODE = W_TOWNCODE
       AND MUNICODE = W_MUNICODE.
    IF SY-SUBRC = 0.
      MOVE ZLSDBN002-WERKS TO IDOCREC-DWERK.
    ELSE.
      MOVE 'GSTH'          TO IDOCREC-DWERK.
    ENDIF.

*Get Standard Cost
    CLEAR MBEW-STPRS.
    SELECT SINGLE STPRS INTO MBEW-STPRS
      FROM MBEW
     WHERE MATNR = IDOCREC-MATNR
       AND BWKEY = IDOCREC-DWERK.
    IF SY-SUBRC = 0.
**-- start of changes by akmadasu
*       W_EXBWR  = ( CTRX-VOLUME / 1000 ) * ( MBEW-STPRS / 1000 ).
*       IDOCREC-EXBWR = ABS( W_EXBWR ).
*     if UOM is MJ existing logic would follow
      IF ctrx-vol_uom = 'MJ'.
        W_EXBWR  = ( CTRX-VOLUME / 1000 ) * ( MBEW-STPRS / 1000 ).
        IDOCREC-EXBWR = ABS( W_EXBWR ).
      ELSEIF ctrx-vol_uom = 'GJ'.
        W_EXBWR  = ( CTRX-VOLUME ) * ( MBEW-STPRS / 1000 ).
        IDOCREC-EXBWR = ABS( W_EXBWR ).
      ELSE.
        CLOSE DATASET: INFILE, OUTFILE.
        MESSAGE text-004 TYPE 'E'.
        STOP.
      endif.
**-- end of changes by akmadasu
    ENDIF.
*   BOC SKAKUMANU INC0991490 D30K929186
    IF ctrx-sr_usage = 'FUEL'.
*     if UOM is MJ existing logic would follow
      IF ctrx-vol_uom = 'MJ'.
*   EOC SKAKUMANU INC0991490 D30K929186
        IDOCREC-ERFMG = ABS( CTRX-VOLUME / 1000 ).
*   BOC SKAKUMANU INC0991490 D30K929186
*       If UOM is GJ, the value received from file would be passed directly
      ELSEIF ctrx-vol_uom = 'GJ'.
        IDOCREC-ERFMG = ABS( CTRX-VOLUME ).
*       For any UOM other than GJ or MJ
      ELSE.
        CLOSE DATASET: INFILE, OUTFILE.
        MESSAGE text-004 TYPE 'E'.
        STOP.
      ENDIF.
    ENDIF.
*   EOC SKAKUMANU INC0991490 D30K929186

    TRANSFER IDOCREC TO OUTFILE.
    CLEAR IDOCREC.

  ENDDO.
  CLOSE DATASET: INFILE, OUTFILE.

  MESSAGE I100(ZM) WITH TEXT-100.

***********************************************************************
FORM GET_LAST_DAY_OF_MONTH USING W_DATE_IN CHANGING VALUE(W_DATE_OUT).

  CALL FUNCTION 'FKK_LAST_DAY_OF_MONTH'
    EXPORTING
      DAY_IN            = W_DATE_IN
    IMPORTING
      LAST_DAY_OF_MONTH = W_DATE_OUT
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1.

*   W_DATE_OUT = W_DATE_OUT + 1.

ENDFORM.

END-OF-SELECTION.
