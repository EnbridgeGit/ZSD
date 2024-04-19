*&---------------------------------------------------------------------*
*& Report  ZLSDC001_BANNER_GL_SPLIT
*&
*&---------------------------------------------------------------------*
*&**********************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        November 2010.                                         *
*  Issue Log:   TR804                                                  *
*  Description:                                                        *
*     - The purpose of this program is to split the billing file into 4*
*       files to translate data for appropriate Sales/Inventory/General*
*       Ledger Postings.                                               *
*  NOTE: If an input record doesn't map to any of the 4 output records *
*        then it will go to zbis100oth.dat file as default.            *
*&---------------------------------------------------------------------*
*CHANGES****                                                           *
*&04/12/2018 AKMADASU  CHG0131594 - Update Banner Splits Interface     *                                                         *
*&---------------------------------------------------------------------*
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 01-10-2019   KMB         D30K930191  CHG0161501 - Banner Split       *
*                                      program Change                  *
************************************************************************



REPORT  zlsdc001_banner_gl_split NO STANDARD PAGE HEADING
LINE-COUNT 65 LINE-SIZE 80 MESSAGE-ID zm.

* Input/Output file format

DATA:  BEGIN OF bann_file,
       process_date       TYPE d,
       serv_type(4)       TYPE c,
       serv_cat(4)        TYPE c,
       gl_classcd(4)      TYPE c,
       serv_class(2)      TYPE c,
       town_cd(2)         TYPE c,
       munic_cd(4)        TYPE c,
       budget_ind(1)      TYPE c,
       cust_num(16)       TYPE n,
       trans_amt_sign(1)  TYPE c,
       trans_amt(13)      TYPE c,
       cust_chrg_sign(1)  TYPE c,
       cust_chrg(11)      TYPE c,
       cons_sign(1)       TYPE c,
       cons(13)           TYPE c,
       no_of_custs(6)     TYPE n,
       eff_date           TYPE d,
       rate_class(4)      TYPE c,
       adj_code(4)        TYPE c.
DATA:  END OF bann_file.

DATA:
  rrso_check_01(11) TYPE c VALUE 'GAS GAS ',     "RRSale file condition
  rrso_check_02(11) TYPE c VALUE 'GASPGAS ',     "RRSale file condition
  sale_check_01(9) TYPE c VALUE 'GAS GAS G',       "Sale file condition
  sale_check_02(9) TYPE c VALUE 'GAS GAS U',       "Sale file condition
  sale_check_03(9) TYPE c VALUE 'GASPGAS G',       "Sale file condition
  sale_check_04(9) TYPE c VALUE 'GASPGAS U',       "Sale file condition
  sale_check_05(11) VALUE 'GAS GAS CFC',
  sale_check_06(11) VALUE 'GASPGAS CCR',
  sale_check_07(11) VALUE 'GAS GAS CCR',
  sale_check_08(11) VALUE 'GASPGAS CFC',
  invn_check_01(9) TYPE c VALUE 'GAS GAS O',  "Inventory file condition
  invn_check_02(9) TYPE c VALUE 'GASPGAS O',  "Inventory file condition
  invn_check_03(9) TYPE c VALUE 'CHRTGAS O',  "Inventory file condition
invn_check_04(12) TYPE c VALUE 'GAS GAS CMPS',"Inventory file condition
invn_check_05(12) TYPE c VALUE 'GAS GAS CMPN',"Inventory file condition
  gleg_check_01(8) TYPE c VALUE '        ',   "Gen.Ledgr file condition
  gleg_check_02(8) TYPE c VALUE '    NONE',   "Gen.Ledgr file condition
  gleg_check_03(8) TYPE c VALUE 'GAS NONE',   "Gen.Ledgr file condition
  gleg_check_04(8) TYPE c VALUE 'GASPNONE',   "Gen.Ledgr file condition
  gleg_check_05(8) TYPE c VALUE 'PROSNONE',   "Gen.Ledgr file condition
  gleg_check_06(8) TYPE c VALUE 'RENTNONE',   "Gen.Ledgr file condition
  gleg_check_07(11) TYPE c VALUE 'GASPGAS RCR',   "Gen.Ledgr file condition
  gleg_check_08(11) TYPE c VALUE 'GASPGAS RFC',   "Gen.Ledgr file condition
  gleg_check_09(11) TYPE c VALUE 'GAS GAS RCR',   "Gen.Ledgr file condition
  gleg_check_10(11) TYPE c VALUE 'GAS GAS RFC'.   "Gen.Ledgr file condition


DATA: gv_file3 TYPE xfeld,
      gv_output_file TYPE xfeld.
*------------------------  Selection Screen  --------------------------
* Input File
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
PARAMETER: infile LIKE filenameci-fileextern OBLIGATORY.
SELECTION-SCREEN SKIP.
* Output Files
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
PARAMETERS:
outfile1 LIKE filenameci-fileextern OBLIGATORY,    "DEFAULT
outfile2 LIKE filenameci-fileextern OBLIGATORY,    "DEFAULT
outfile3 LIKE filenameci-fileextern OBLIGATORY,    "DEFAULT
outfile4 LIKE filenameci-fileextern OBLIGATORY.    "DEFAULT
SELECTION-SCREEN END OF BLOCK box2.
SELECTION-SCREEN END OF BLOCK box.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE:
  '/usr/sap/interfaces/' sy-sysid+0(3) '/BANNER/zbis100.chk' INTO infile,
  '/usr/sap/interfaces/' sy-sysid(3) '/BANNER/zbis100salesorder.dat'
                                                           INTO outfile1,
  '/usr/sap/interfaces/' sy-sysid(3) '/BANNER/zbis100owngi.dat'
                                                           INTO outfile2,
  '/usr/sap/interfaces/' sy-sysid(3) '/BANNER/zbis100oth.dat'
                                                           INTO outfile3,
  '/usr/sap/interfaces/' sy-sysid(3) '/BANNER/zbis100rrorder.dat'
                                                           INTO outfile4.

***********************************************************************
START-OF-SELECTION.
  OPEN DATASET infile FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH infile.
  ENDIF.

  OPEN DATASET outfile1 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH outfile1.
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
    MESSAGE e006(zm) WITH outfile4.
  ENDIF.

  DO.
    CLEAR bann_file.
    READ DATASET infile INTO bann_file.
    IF sy-subrc <> '0'.
      EXIT.
    ENDIF.
*Note: The data for RateRider file OUTFILE4 should be separated first,
*      because some of its data can be qualified for OUTFILE3 as well.
    CLEAR: gv_file3,
           gv_output_file.
    IF (  bann_file+8(11) = gleg_check_07 OR
          bann_file+8(11) = gleg_check_08 OR
          bann_file+8(11) = gleg_check_09 OR
          bann_file+8(11) = gleg_check_10 ).
          gv_output_file = 'X'.
          gv_file3 = 'X'.
          TRANSFER bann_file TO outfile3 LENGTH 107.
    ELSEIF ( bann_file+8(11) = sale_check_06 or
             bann_file+8(11) = sale_check_07 or
             bann_file+8(11) = sale_check_08 ) and
           gv_output_file is INITIAL.
**-- START OF CHANGES BY AKMADASU FOR CHG0131594
*           TRANSFER bann_file TO outfile1 LENGTH 103.
           TRANSFER bann_file TO outfile3 LENGTH 103.
**-- END OF CHANGES BY AKMADASU FOR CHG0131594
           gv_output_file = 'X'.
    ELSEIF   ( bann_file+8(8) = rrso_check_01 OR
              bann_file+8(8) = rrso_check_02 ) AND
              bann_file+18(1) = 'R'            AND
              bann_file+103(4) <> 'BDAW' AND
              bann_file+103(4) <> 'BDCT' AND
              bann_file+103(4) <> 'BDBT' AND
              bann_file+103(4) <> 'BDCR' AND
              bann_file+103(4) <> 'BDRW' AND
*BOC by KMB on 01.10.2019 CHG0161501 - Banner Split program Change
              bann_file+103(4) <> 'BDTB' AND
*EOC by KMB on 01.10.2019 CHG0161501 - Banner Split program Change
              gv_output_file IS INITIAL .
              TRANSFER bann_file TO outfile4 LENGTH 103.
      gv_output_file = 'X'.
    ELSEIF bann_file+103(4) <> 'BDAW' AND
           bann_file+103(4) <> 'BDCT' AND
           bann_file+103(4) <> 'BDBT' AND
           bann_file+103(4) <> 'BDCR' AND
           bann_file+103(4) <> 'BDRW' AND
*BOC by KMB on 01.10.2019 CHG0161501 - Banner Split program Change
           bann_file+103(4) <> 'BDTB' AND
*EOC by KMB on 01.10.2019 CHG0161501 - Banner Split program Change
       ( bann_file+8(9) = sale_check_01 OR
         bann_file+8(9) = sale_check_02 OR
         bann_file+8(9) = sale_check_03 OR
         bann_file+8(9) = sale_check_04 ) AND
       gv_output_file IS INITIAL .
      TRANSFER bann_file TO outfile1 LENGTH 103.
      gv_output_file = 'X'.
    ELSEIF bann_file+8(11) = sale_check_05 AND "or
           "bann_file+8(12) = sale_check_06 AND
           gv_output_file is INITIAL.
**-- START OF CHANGES BY AKMADASU FOR CHG0131594
*           TRANSFER bann_file TO outfile1 LENGTH 103.
           TRANSFER bann_file TO outfile3 LENGTH 103.
**-- END OF CHANGES BY AKMADASU FOR CHG0131594
           gv_output_file = 'X'.
    ELSEIF bann_file+103(4) <> 'BDAW' AND
           bann_file+103(4) <> 'BDCT' AND
           bann_file+103(4) <> 'BDBT' AND
           bann_file+103(4) <> 'BDCR' AND
           bann_file+103(4) <> 'BDRW' AND
*BOC by KMB on 01.10.2019 CHG0161501 - Banner Split program Change
           bann_file+103(4) <> 'BDTB' AND
*EOC by KMB on 01.10.2019 CHG0161501 - Banner Split program Change
       ( bann_file+8(9)  = invn_check_01 OR
         bann_file+8(9)  = invn_check_02 OR
         bann_file+8(9)  = invn_check_03 OR
         bann_file+8(12) = invn_check_04 OR
         bann_file+8(12) = invn_check_05 ) AND
       gv_output_file IS INITIAL.
      TRANSFER bann_file TO outfile2 LENGTH 103.
      gv_output_file = 'X'.
    ELSEIF gv_output_file IS INITIAL AND
       (  bann_file+8(8) = gleg_check_01 OR
       bann_file+8(8) = gleg_check_02 OR
       bann_file+8(8) = gleg_check_03 OR
       bann_file+8(8) = gleg_check_04 OR
       bann_file+8(8) = gleg_check_05 OR
       bann_file+8(8) = gleg_check_06 ).
      gv_file3 = 'X'.
      TRANSFER bann_file TO outfile3 LENGTH 107.
      gv_output_file = 'X'.
    ELSE.
      gv_file3 = 'X'.
      TRANSFER bann_file TO outfile3 LENGTH 107.
      gv_output_file = 'X'.
    ENDIF.
    IF bann_file+103(4) <> 'BDAW' AND
       bann_file+103(4) <> 'BDCT' AND
       bann_file+103(4) <> 'BDBT' AND
       bann_file+103(4) <> 'BDCR' AND
       bann_file+103(4) <> 'BDRW' AND
*BOC by KMB on 01.10.2019 CHG0161501 - Banner Split program Change
       bann_file+103(4) <> 'BDTB' AND
*EOC by KMB on 01.10.2019 CHG0161501 - Banner Split program Change
       gv_file3 IS INITIAL        AND
       gv_output_file IS INITIAL.
      TRANSFER bann_file TO outfile3 LENGTH 107.
      gv_output_file = 'X'.
    ENDIF.
  ENDDO.

  MESSAGE i100(zm) WITH text-100.

END-OF-SELECTION.
