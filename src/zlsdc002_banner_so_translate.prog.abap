*&--------------------------------------------------------------------*
*& Report  ZLSDC002_BANNER_SO_TRANSLATE
*&
*&--------------------------------------------------------------------*
*&*********************************************************************
*  Author:      Mohammad T. Khan                                      *
*  Date:        November 2010.                                        *
*  Issue Log:   TR804                                                 *
*  Description:                                                       *
*    -The purpose of this program is to translate the billing data    *
*     using custom mapping table ZLSDBN001, ZLSDBN002, ZLSDBN003,ZFB08*
*     & convert the data into sale order Idoc format.                 *
*                                                                     *
*&--------------------------------------------------------------------*
*CHANGES****                                                          *
*&                                                                    *
*&--------------------------------------------------------------------*

REPORT  zlsdc002_banner_so_translate NO STANDARD PAGE HEADING
LINE-COUNT 65 LINE-SIZE 80 MESSAGE-ID zm.

TABLES: zlsdbn001,        " Banner GL Code Mapping
        zlsdbn002,        " Banner Organization Mapping
        zlsdbn003,        " Banner Rate Class Mapping
        zfb08.            " Banner Base Charge Mapping

* Input file format
DATA:  BEGIN OF sale_rec,
       process_date       TYPE d,         "Process Date
       serv_type(4)       TYPE c,         "Serv Type
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
       cust_chrg(11)      TYPE c,         "Budget Amount
       cons_sign(1)       TYPE c,
       cons(13)           TYPE c,
       no_of_custs(6)     TYPE n,
       eff_date           TYPE d,
       rate_class(4)      TYPE c.
DATA:  END OF sale_rec.

* Output file format
DATA:  BEGIN OF sidoc_rec,
  audat   TYPE d, "Document Date
  fkdat   TYPE d, "Billing Date
  curcy(3)  TYPE c, "Order Currency
  bsart(4)  TYPE c,   "Sales Document Type
  autlf(1)  TYPE c,   "Complete Delivery Indicator
  vkorg(4)  TYPE c,       "Sales Organization
  vtweg(2)  TYPE c,   "Distribution Channel
  spart(2)  TYPE c,   "Division
  parvw(3)  TYPE c,   "Sold-To-Party (Customer)
  partn(17) TYPE c,   "Customer Number
  bstkd(35) TYPE c,   "PO Number
  dwerk(4)  TYPE c,   "Delivering Plant
  vkbur(4)  TYPE c,   "Sales Office
  augru(3)  TYPE c,   "Order Reason
  konda(2)  TYPE c,   "Price Group
  kdgrp(2)  TYPE c,   "Customer Group
 	prsdt	 	TYPE d,	"Pricing Date
  kvgr1(3)  TYPE c,       "Customer Group 1
  bzirk(4)  TYPE c,   "Sales District
  mabnr(18) TYPE c,   "Material Number
  kwmeng(15)  TYPE c,   "Order Quantity
  vrkme(3)  TYPE c,   "Unit of Measure
  kschl(4)  TYPE c,       "Condition Type
  kbetr(11) TYPE c, "Condition Amount
  kdkg1(2)  TYPE c,  "Condition Group 1
  kvgr2(3).
DATA:  END OF sidoc_rec.

*Data Fields to Restrict ranges
TYPE-POOLS: sscr.

DATA: l_restrict TYPE sscr_restrict,
      l_opt_list TYPE sscr_opt_list,
      l_ass TYPE sscr_ass.

DATA: w_glcode LIKE zfb08-glcode,
      w_dollar TYPE p LENGTH 13 DECIMALS 2,
      w_consum TYPE p LENGTH 13 DECIMALS 3.

*      W_SECOND_REC(1) TYPE C.

CONSTANTS: true  TYPE boolean VALUE 'X'.

*---------------------------------------------------------------------*
*------------------------  Selection Screen  --------------------------
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
PARAMETER:
p_vkorg LIKE vbak-vkorg DEFAULT 'Z001' OBLIGATORY,        "Sales Org
*P_VTWEG LIKE VBAK-VTWEG DEFAULT 'Z0'   OBLIGATORY,        "Dist Chanel
p_spart LIKE vbak-spart DEFAULT 'Z0'   OBLIGATORY,        "Division
p_kunnr LIKE kuagv-kunnr DEFAULT 'BANNER' OBLIGATORY,     "Customer #
p_waerk LIKE vbak-waerk  DEFAULT 'CAD' OBLIGATORY,        "SD Doc. Curr
p_parvw(2)   TYPE c      DEFAULT 'AG'  OBLIGATORY,        "SoldTo Party
p_isocod(2)  TYPE c      DEFAULT 'CR' OBLIGATORY.         "ISO Code
SELECT-OPTIONS:
p_glcode FOR zlsdbn001-glcode DEFAULT 'GABD' OBLIGATORY.  "G/L ClasCode
PARAMETER:
infile LIKE filenameci-fileextern OBLIGATORY,             "Input File
outfile LIKE filenameci-fileextern OBLIGATORY,            "Output File
act_ban  RADIOBUTTON GROUP rbcr,                     "Actual Banner
unb_ban  RADIOBUTTON GROUP rbcr,                     "Unbilled Banner
unb_rev  RADIOBUTTON GROUP rbcr.                     "Unbilled Reversal

SELECTION-SCREEN END OF BLOCK box.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3)
              '/BANNER/zbis100salesorder.dat' INTO infile.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3)
              '/BANNER/zbannsalesorderidoc.dat' INTO outfile.

  CLEAR l_opt_list.
  l_opt_list-name = 'P_GLCODE'.
  l_opt_list-options-eq = 'X'.
  APPEND l_opt_list TO l_restrict-opt_list_tab.

  CLEAR l_ass.
  l_ass-kind = 'S'.
  l_ass-name = 'P_GLCODE'.
  l_ass-sg_main = 'I'.
  l_ass-op_main = 'P_GLCODE'.
  APPEND l_ass TO l_restrict-ass_tab.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction = l_restrict
    EXCEPTIONS
      OTHERS      = 1.

  p_glcode-sign   = 'I'.
  p_glcode-option = 'EQ'.
  p_glcode-low    = 'GATD'.
  APPEND p_glcode.

  p_glcode-low    = 'GBSD'.
  APPEND p_glcode.

  p_glcode-low    = 'GBTD'.
  APPEND p_glcode.

  p_glcode-low    = 'GBZD'.
  APPEND p_glcode.

  p_glcode-low    = 'GUSD'.
  APPEND p_glcode.

  p_glcode-low    = 'UTSD'.
  APPEND p_glcode.
  CLEAR  p_glcode.

***********************************************************************
*                START-OF-SELECTION.                                  *
***********************************************************************
START-OF-SELECTION.
  OPEN DATASET infile FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH infile.
  ENDIF.

  OPEN DATASET outfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH outfile.
  ENDIF.

  DO.
    READ DATASET infile INTO sale_rec.
    IF sy-subrc <> '0'.
      EXIT.
    ENDIF.

    PERFORM first_record_mapping.
    PERFORM rest_of_the_mapping.
    IF sale_rec-gl_classcd IN p_glcode.
*      MOVE 'Y' TO W_SECOND_REC.
      PERFORM second_record_mapping.
      PERFORM rest_of_the_mapping.
*      CLEAR W_SECOND_REC.
    ENDIF.
  ENDDO.
*Program ended Message
  MESSAGE i100(zm) WITH text-100.

***********************************************************************
*              FIRST_RECORD_MAPPING.                                  *
***********************************************************************
FORM first_record_mapping.

  CLEAR w_glcode.
  MOVE: sale_rec-gl_classcd   TO   w_glcode.
  sidoc_rec-kbetr = abs( sale_rec-trans_amt ).

  MOVE sale_rec-cons TO w_consum.
  IF w_consum = 0.
    sidoc_rec-kwmeng = abs( '00000000000.001' ).
    MOVE 'CR'  TO sidoc_rec-vrkme.
  ELSE.
    sidoc_rec-kwmeng = abs( sale_rec-cons ).
    MOVE p_isocod  TO  sidoc_rec-vrkme.
  ENDIF.

ENDFORM.                    "FIRST_RECORD_MAPPING

***********************************************************************
*              SECOND_RECORD_MAPPING.                                 *
***********************************************************************
FORM second_record_mapping.

  sidoc_rec-kwmeng = abs( '00000000000.001' ).
  MOVE 'EA'  TO sidoc_rec-vrkme.
  sidoc_rec-kbetr  = abs( sale_rec-cust_chrg ).
  MOVE sale_rec-cust_chrg_sign TO sale_rec-trans_amt_sign.
  CLEAR w_glcode.
  SELECT SINGLE bc_glcode INTO w_glcode
    FROM zfb08
   WHERE glcode = sale_rec-gl_classcd.
  IF sy-subrc <> 0.
    MOVE  '****'        TO w_glcode.
  ENDIF.

ENDFORM.                    "SECOND_RECORD_MAPPING

***********************************************************************
*               REST_OF_THE_MAPPING.                                  *
***********************************************************************
FORM rest_of_the_mapping.

  DATA: ls_zlsdbn004 TYPE zlsdbn004.

*Mapping from Input File Fields
  IF sale_rec-eff_date = space OR sale_rec-eff_date = '00000000'.
    MOVE  sale_rec-process_date  TO  sidoc_rec-prsdt.
  ELSE.
    MOVE  sale_rec-eff_date      TO  sidoc_rec-prsdt.
  ENDIF.
  MOVE: sale_rec-serv_class    TO  sidoc_rec-kvgr1,
        sale_rec-process_date  TO  sidoc_rec-audat,
        sale_rec-process_date  TO  sidoc_rec-fkdat.
*         SALE_REC-CONS_SIGN     TO  SIDOC_REC-QSIGN.

*Mapping from Selection Screen Fields
  MOVE: p_waerk   TO  sidoc_rec-curcy,
        'X'       TO  sidoc_rec-autlf,
        p_vkorg   TO  sidoc_rec-vkorg,
*        P_VTWEG   TO  SIDOC_REC-VTWEG,
        p_spart   TO  sidoc_rec-spart,
        p_parvw   TO  sidoc_rec-parvw,
        p_kunnr   TO  sidoc_rec-partn.
*        P_ISOCOD  TO  SIDOC_REC-VRKME.

  CONCATENATE sale_rec-rate_class '/' sale_rec-serv_class '/' p_kunnr
              INTO sidoc_rec-bstkd.

*Mapping for Field AUGRU.
  IF act_ban = 'X'.
    MOVE 'ZAC' TO sidoc_rec-augru.
  ELSEIF unb_ban = 'X'.
    MOVE 'ZES' TO sidoc_rec-augru.
  ELSE.
    MOVE 'ZRV' TO sidoc_rec-augru.
  ENDIF.

*Mapping for Field BSART.
  CASE true.
    WHEN act_ban.
      SELECT SINGLE auart INTO zlsdbn001-auart
        FROM zlsdbn001
       WHERE glcode = w_glcode.
      IF sy-subrc = 0.
        IF sale_rec-trans_amt_sign = '+'.
          MOVE zlsdbn001-auart TO sidoc_rec-bsart.
        ELSE.
          IF zlsdbn001-auart = 'ZISS'.
            MOVE 'ZRET'  TO sidoc_rec-bsart.
          ELSEIF zlsdbn001-auart = 'ZDPI'.
            MOVE 'ZDPR'  TO sidoc_rec-bsart.
          ELSEIF zlsdbn001-auart = 'ZRRI'.
            MOVE 'ZRRR'  TO sidoc_rec-bsart.
          ELSEIF zlsdbn001-auart = 'ZBTI'.
            MOVE 'ZBTR'  TO sidoc_rec-bsart.
          ELSE.
            MOVE '????' TO sidoc_rec-bsart.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN unb_ban.
      IF sale_rec-trans_amt_sign = '+'.
        MOVE 'ZUNI'  TO sidoc_rec-bsart.
      ELSE.
        MOVE 'ZUNR'  TO sidoc_rec-bsart.
      ENDIF.
    WHEN unb_rev.
      IF sale_rec-trans_amt_sign = '+'.
        MOVE 'ZUNR'  TO sidoc_rec-bsart.
      ELSE.
        MOVE 'ZUNI'  TO sidoc_rec-bsart.
      ENDIF.
  ENDCASE.

*Mapping from ZLSDBN001 Table.
  SELECT SINGLE kdgrp matnr kschl kdkg1
  INTO (zlsdbn001-kdgrp, zlsdbn001-matnr, zlsdbn001-kschl,
                                          zlsdbn001-kdkg1)
  FROM zlsdbn001
  WHERE glcode   = w_glcode.
  IF sy-subrc = 0.
    MOVE zlsdbn001-kdgrp TO sidoc_rec-kdgrp.
    MOVE zlsdbn001-matnr TO sidoc_rec-mabnr.
    MOVE zlsdbn001-kschl TO sidoc_rec-kschl.
    MOVE zlsdbn001-kdkg1 TO sidoc_rec-kdkg1.
  ENDIF.

*Mapping from ZLSDBN002 Table.
  SELECT SINGLE *
   FROM zlsdbn002
   WHERE towncode   = sale_rec-town_cd
     AND municode = sale_rec-munic_cd.
  IF sy-subrc = 0.
    MOVE: zlsdbn002-werks TO sidoc_rec-dwerk,
          zlsdbn002-vkbur TO sidoc_rec-vkbur,
          zlsdbn002-bzirk TO sidoc_rec-bzirk,
          zlsdbn002-vtweg to sidoc_rec-vtweg.
  ELSE.
    sidoc_rec-vtweg = 'Z0'.
  ENDIF.

*Mapping from ZLSDBN003 Table.
  SELECT SINGLE konda INTO zlsdbn003-konda
    FROM zlsdbn003
   WHERE ratecls = sale_rec-rate_class.
  IF sy-subrc = 0.
    MOVE zlsdbn003-konda TO sidoc_rec-konda.
  ENDIF.
  "-------
  CLEAR: ls_zlsdbn004.
  SELECT SINGLE * FROM zlsdbn004 INTO ls_zlsdbn004
    WHERE ct_glcode = sale_rec-gl_classcd.
  IF sy-subrc = 0.
    sidoc_rec-kvgr2 = 'N'.
  ELSE.
    sidoc_rec-kvgr2 = space.
  ENDIF.
*Create a Record on the Output file
  MOVE sidoc_rec-kbetr TO w_dollar.
  IF w_dollar <> 0.
    TRANSFER sidoc_rec TO outfile.
  ENDIF..
  CLEAR sidoc_rec.

ENDFORM.                    "REST_OF_THE_MAPPING

END-OF-SELECTION.
