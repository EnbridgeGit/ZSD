*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_UCOMM) TYPE  SY-UCOMM
*"  EXPORTING
*"     VALUE(E_CI_UPDATE) LIKE  SY-CALLD
*"     VALUE(E_UCOMM) LIKE  SY-UCOMM
*"  CHANGING
*"     VALUE(E_CI_EKKO) LIKE  EKKO_CI STRUCTURE  EKKO_CI OPTIONAL
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&  Include           ZXM06U37
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
*-----------------------------------------------------------------------
*Modification details
*Version No    : 01
*Date          : 23/09/2011
*Modified by   : VKOMMERA
*Correction No : EE005
*Description   : When the user enter authorization code check if the
*                authorization code is already exists in the system.
*                if yes provide an error message.
*                Also trigger an RFC connect to SRM system to get the
*                authorization code status(shopping card).
*                if the shopping card status s not in approved status
*                then provide an error message.
*
*
*-----------------------------------------------------------------------
* 01/04/2021 | MEDISETA | S01K900755 | Master agreement requirement
************************************************************************
** Define Constants
CONSTANTS : co_x        TYPE char1         VALUE 'X'.


IF ekko-zzariba_approver NE zekko-zzariba_approver.

  e_ci_ekko-zzariba_approver = ekko-zzariba_approver.
  e_ci_update = co_x.

ENDIF.

* Begin of addition by RBHATT
* Begin of Change log EE009

IF ekko-zz_po_auth_code NE zekko-zz_po_auth_code.

  e_ci_ekko-zz_po_auth_code = ekko-zz_po_auth_code.
  e_ci_update               = co_x.

ENDIF.

IF ekko-zzcondayqty <> zekko-zzcondayqty.
  e_ci_ekko-zzcondayqty = ekko-zzcondayqty.
  e_ci_update = 'X'.
ENDIF.

IF ekko-zzconprice <> zekko-zzconprice.
  e_ci_ekko-zzconprice = ekko-zzconprice.
  e_ci_update = 'X'.
ENDIF.
* Begin of insert by MEDISETA TR S01K900755
IF ekko-zzparty_agmt_id <> zekko-zzparty_agmt_id.
  e_ci_ekko-zzparty_agmt_id = ekko-zzparty_agmt_id.
  e_ci_update = 'X'.
ENDIF.
IF ekko-zzekgrp <> zekko-zzekgrp.
  e_ci_ekko-zzekgrp = ekko-zzekgrp.
  e_ci_update = 'X'.
ENDIF.
IF ekko-zztrloc1 <> zekko-zztrloc1.
  e_ci_ekko-zztrloc1 = ekko-zztrloc1.
  e_ci_update = 'X'.
ENDIF.
IF ekko-zztrloc2 <> zekko-zztrloc2.
  e_ci_ekko-zztrloc2 = ekko-zztrloc2.
  e_ci_update = 'X'.
ENDIF.
IF ekko-zztrloc3 <> zekko-zztrloc3.
  e_ci_ekko-zztrloc3 = ekko-zztrloc3.
  e_ci_update = 'X'.
ENDIF.
IF ekko-zztrloc4 <> zekko-zztrloc4.
  e_ci_ekko-zztrloc4 = ekko-zztrloc4.
  e_ci_update = 'X'.
ENDIF.
IF ekko-zzparty <> zekko-zzparty.
  e_ci_ekko-zzparty = ekko-zzparty.
  e_ci_update = 'X'.
ENDIF.
* End ofinsert by MEDISETA TR S01K900755
