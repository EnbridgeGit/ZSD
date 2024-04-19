*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_EKKO) LIKE  EKKO STRUCTURE  EKKO
*"             VALUE(I_TRTYP)
*"             VALUE(I_CI_EKKO) LIKE  EKKO_CI STRUCTURE  EKKO_CI
*"             VALUE(I_BSTYP) LIKE  EKKO-BSTYP
*"             VALUE(I_NO_SCREEN)
*"             VALUE(I_LFA1) LIKE  LFA1 STRUCTURE  LFA1
*"             VALUE(I_LFM1) LIKE  LFM1 STRUCTURE  LFM1
*"             VALUE(I_KEKKO) LIKE  EKKO STRUCTURE  EKKO
*"             VALUE(I_AEKKO) LIKE  EKKO STRUCTURE  EKKO
*"             VALUE(I_REKKO) LIKE  EKKO STRUCTURE  EKKO
*"             VALUE(I_EKKO_OLD) LIKE  EKKO STRUCTURE  EKKO OPTIONAL
*"             VALUE(I_VORGA) LIKE  T160-VORGA
*"       TABLES
*"              TEKPO STRUCTURE  BEKPO OPTIONAL
*"              TEKET STRUCTURE  BEKET OPTIONAL
*"              TEKKN STRUCTURE  EKKNU OPTIONAL
*"              TKOMV STRUCTURE  KOMV OPTIONAL
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&  Include           ZXM06U36
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
*Modification details                                                  *
*Version No    : 01                                                    *
*Date          : 22/09/2011                                            *
*Modified by   : RBHATT                                                *
*Correction No : EE008                                                 *
*Transport No  : D30K917672  SL:LMM:MM: EE008-MM_PO_CUSTOM FIELDS_UG   *
*Description   : To add new custom field 'Service Receipt Confirmer'   *
*                [CI_EKKODB-ZZARIBA_APPROVER] & 'Authorization Code'   *
*                [CI_EKKODB-ZZ_PO_AUTH_CODE]in the additional tab in   *
*                the PO transaction ME21N, ME22N, ME23N                *
*-----------------------------------------------------------------------
* 01/04/2021 | MEDISETA | S01K900755 | Master agreement requirement
* ---------------------------------------------------------------------*
* 13-APR-2022  DADIM      D30K932132  CHG0246647 - MSA Field Optional  *
*                                     Requirement for G05 only         *
* 28-APR-2022  DADIM      D30K932163  CHG0248493 - NAESB Dropping off  *
*                                     Gas Supply Agreements in SAP     *
************************************************************************

* BEGIN OF PANUSURI 12/09/2011, SE035 PO CREATE BADI
*Move from DB to *EKKO
zekko-zzariba_approver = i_ci_ekko-zzariba_approver.
ekko-zzariba_approver  = i_ci_ekko-zzariba_approver.

*Modify or Display.
gf_trtyp = i_trtyp.
* END OF PANUSURI

* BEGIN OF ADDITION BY RBHATT
* Change log EE008
zekko-zz_po_auth_code = i_ci_ekko-zz_po_auth_code.
ekko-zz_po_auth_code  = i_ci_ekko-zz_po_auth_code.
zekko-ebeln           = i_ekko-ebeln.
* Change log EE008
* END OF ADDITION BY RBHATT

*COG
zekko-zzcondayqty = i_ci_ekko-zzcondayqty.
ekko-zzcondayqty  = i_ci_ekko-zzcondayqty.

zekko-zzconprice  = i_ci_ekko-zzconprice.
ekko-zzconprice   = i_ci_ekko-zzconprice.

* Begin of insert by MEDISETA TR S01K900755
*ekko-zzmsa = i_ci_ekko-zzmsa.
*zekko-zzmsa = i_ci_ekko-zzmsa.
ekko-zzekgrp = i_ci_ekko-zzekgrp.
zekko-zzekgrp = i_ci_ekko-zzekgrp.
ekko-zztrloc1 = i_ci_ekko-zztrloc1.
zekko-zztrloc1 = i_ci_ekko-zztrloc1.
ekko-zztrloc2 = i_ci_ekko-zztrloc2.
zekko-zztrloc2 = i_ci_ekko-zztrloc2.
ekko-zztrloc3 = i_ci_ekko-zztrloc3.
zekko-zztrloc3 = i_ci_ekko-zztrloc3.
ekko-zztrloc4 = i_ci_ekko-zztrloc4.
zekko-zztrloc4 = i_ci_ekko-zztrloc4.
ekko-zzparty = i_ci_ekko-zzparty.
zekko-zzparty = i_ci_ekko-zzparty.
gf_bukrs    = i_ekko-bukrs.
gf_bsart    = i_ekko-bsart.
gf_todate   = i_ekko-kdate.
gf_fromdate = i_ekko-kdatb.
gf_lifnr    = i_ekko-lifnr.
gf_ekorg    = i_ekko-ekorg.
*Start of changes by DADIM for CHG0246647
gf_ekgrp    = i_ekko-ekgrp.
IF sy-tcode NE 'ME35' AND sy-tcode NE 'ME35L' AND sy-tcode NE 'ME35K'.  "Added by DADIM for CHG0248493
  zekko-zzparty_agmt_id = i_ci_ekko-zzparty_agmt_id.
ENDIF.                                                        "Added by DADIM for CHG0248493
*End of changes by DADIM for CHG0246647

SELECT SINGLE eikto FROM lfm1 INTO gf_eikto
  WHERE lifnr = gf_lifnr AND ekorg = gf_ekorg.

IF i_ekko-zzparty_agmt_id IS NOT INITIAL.
  SELECT SINGLE *
         FROM zmmt_mastagree
    WHERE zzparty_agmt_id = i_ekko-zzparty_agmt_id.
  IF sy-subrc = 0.
  ENDIF.
ENDIF.
IF ekko-zzekgrp IS NOT INITIAL.
  SELECT SINGLE *
         FROM t024
    WHERE ekgrp = ekko-zzekgrp.
  IF sy-subrc = 0.
  ENDIF.
ENDIF.

IF ekko-zztrloc1 IS NOT INITIAL.
  SELECT SINGLE *
         FROM zmmt_locmast
    WHERE zztrloc = ekko-zztrloc1.
  IF sy-subrc = 0.
  ENDIF.
ENDIF.
* End of insert by MEDISETA TR S01K900755
