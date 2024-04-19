*&---------------------------------------------------------------------*
*& Report  ZLSDC013_CONTRAX_DP_SPLIT
*&
*&---------------------------------------------------------------------*
*&**********************************************************************
*  Author:      Brian Boudny                                           *
*  Date:        August 2011.                                           *
*  Issue Log:   TR804                                                  *
*  Description:                                                        *
*     - This program will receive the contrax DP                       *
*       revenue files and split into two unique files to be used for   *
*       accounting BDCs, and Goods Movement IDocs                      *
*&---------------------------------------------------------------------*
*CHANGES****                                                           *
*& 2014/09/12 - Add invoice number to input file                       *
*& SDP74189                                                            *
*& GYMANA                                                              *
*& 2016/05/30 - Split out Sales order records using Non Rate ITem Type *
*& ACR244                                                              *
*& GYMANA                                                              *
*&---------------------------------------------------------------------*

REPORT  zlsdc013_contrax_dp_split NO STANDARD PAGE HEADING
LINE-COUNT 65 LINE-SIZE 80 MESSAGE-ID zm.

TABLES: zlsdc01.     "Contrax Gas Sales Order Mapping
* Input/Output file format
DATA:  BEGIN OF crec,
        app_yr(4)             TYPE c,    "Year transaction applied
        app_mth(2)            TYPE c,    "Month transaction applied
        cust_id(8)            TYPE c,    "Customer number
        rate_cl(12)           TYPE c,    "Rate class
        serv_type(6)          TYPE c,    "Service type
        serv_cl(2)            TYPE c,    "Service class
        seas_cl(4)            TYPE c,    "Seasonal class
        rate_type(4)          TYPE c,     "Rate type
        charge_type(4)        TYPE c,     "Charge type
        sr_usage(4)           TYPE c,     "SR USAGE
        st_subtype(6)         TYPE c,     "ST SUBTYPE
        non_rate_item_typ(8)  TYPE c,     "NON-RATE ITEM TYPE
        tier_step_lvl(2)      TYPE c,     "TIER STEP LEVEL
        sector_size(1)        TYPE c,     "SECTOR SIZE
        sector(6)             TYPE c,     "SECTOR
        amount(16)            TYPE c,     "TRANSACTION AMOUNT
        volume(18)            TYPE c,     "TRANSACTION VOLUME
        cust_type(1)          TYPE c,     "CUSTOMER TYPE
        geca_code(6)          TYPE c,     "GEOGRAPHIC AREA
        vol_uom(8)            TYPE c,     "VOLUME UNIT OF MEASURE
        sa_num(8)             TYPE c,     "CONTRACT NUMBER
        inv_num(8)            TYPE c,     "INVOICE #           SDP74189
        rebill_yr(4)          TYPE c,     "REBILL YEAR         ACR244
        rebill_mth(2)         TYPE c,     "REBILL MONTH        ACR244
        so_id(4)              TYPE c.     "SERVICE OFFERING ID ACR244
DATA: END OF crec.

DATA: inrec(400).

* CONSTANTS: W_DMTR Type X VALUE '09'.
CONSTANTS: w_dmtr TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.

*------------------------  Selection Screen  ---------------------------
* Input File
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
SELECT-OPTIONS: S_NRTTP FOR ZLSDC01-C_NRTTP OBLIGATORY.         "ACR244
SELECTION-SCREEN SKIP.
PARAMETER: infile LIKE filenameci-fileextern OBLIGATORY.
SELECTION-SCREEN SKIP.
* Output Files
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
PARAMETERS:
outfile2 LIKE filenameci-fileextern OBLIGATORY,
outfile3 LIKE filenameci-fileextern OBLIGATORY,
outfile4 LIKE filenameci-fileextern OBLIGATORY.                 "ACR244
SELECTION-SCREEN END OF BLOCK box2.
SELECTION-SCREEN END OF BLOCK box.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE:
  '/usr/sap/interfaces/' sy-sysid+0(3) '/LSDCNTXDP/dp_act.chk'
                                                        INTO infile,
  '/usr/sap/interfaces/' sy-sysid(3) '/LSDCNTXDP/zcontraxdpaccact.dat'
                                                        INTO outfile2,
  '/usr/sap/interfaces/' sy-sysid(3) '/LSDCNTXDP/zcontraxdpfuel.dat'
                                                        INTO outfile3,
* ACR244
  '/usr/sap/interfaces/' sy-sysid(3) '/LSDCNTXDP/zcontraxdpsordact.dat'
                                                        INTO outfile4.
*  S_NRTTP  = 'I'.
*  S_NRTTP-OPTION = 'EQ'.
*  S_NRTTP-LOW    = '00901695'.
*  APPEND S_NRTTP.
*  S_NRTTP-LOW    = '00901718'.
*  APPEND S_NRTTP.
*  S_NRTTP-LOW    = '00902903'.
*  APPEND S_NRTTP.
*  S_NRTTP-LOW    = '00901729'.
*  APPEND S_NRTTP.
*  S_NRTTP-LOW    = '00901740'.
*  APPEND S_NRTTP.


***********************************************************************
START-OF-SELECTION.

  OPEN DATASET infile FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH infile.
  ENDIF.


  OPEN DATASET outfile2 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH outfile2.
  ENDIF.

  OPEN DATASET outfile3 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH outfile3.
  ENDIF.

  OPEN DATASET outfile4 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH outfile3.
  ENDIF.

  DO.
    CLEAR: crec, zlsdc01-matnr.
    READ DATASET infile INTO inrec.
    IF sy-subrc <> '0'.
      EXIT.
    ENDIF.
    SPLIT inrec AT w_dmtr INTO crec-app_yr crec-app_mth
    crec-cust_id crec-rate_cl crec-serv_type
    crec-serv_cl crec-seas_cl crec-rate_type crec-charge_type
    crec-sr_usage crec-st_subtype crec-non_rate_item_typ
    crec-tier_step_lvl crec-sector_size crec-sector crec-amount
    crec-volume crec-cust_type crec-geca_code crec-vol_uom
    crec-sa_num crec-inv_num crec-rebill_yr crec-rebill_mth. "ACR244

*   acr-244 - so_id field not included in DP file but is needed in
*   revenue programs shared with DP (ZFFII030, ZLSDC006, ZLSDC012)
*   All output files will contain the so_id field.
    MOVE '0000' to crec-so_id.

    IF crec-non_rate_item_typ in S_NRTTP.                    "ACR244
      TRANSFER crec TO outfile4 LENGTH 148.                  "ACR244
    ELSE.                                                    "ACR244
      IF crec-sr_usage   = 'FUEL' OR crec-non_rate_item_typ = 'FUELADJ'.
        TRANSFER crec TO outfile3 LENGTH 148.                "ACR244
      ELSE.
        TRANSFER crec TO outfile2 LENGTH 148.                "ACR244
      ENDIF.
    ENDIF.                                                   "ACR244
  ENDDO.
  CLOSE DATASET: infile, outfile2, outfile3, outfile4.       "ACR244

  MESSAGE i100(zm) WITH text-100.

END-OF-SELECTION.
