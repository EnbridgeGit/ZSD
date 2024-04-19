*&---------------------------------------------------------------------*
*&  Include           ZXM06TOP
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Include:   ZXM06TOP                                                 *
*  Func Grp:  XM06                                                     *
*  Author:    Brian Boundy                                             *
*  Date:      November 29, 2010                                        *
*  Track #:   TR872 Release 1                                          *
*                                                                      *
*  Description:                                                        *
*     - Enhancement MM06E005 - user exits to maintain customer         *
*                              fields in purchase orders               *
*                                                                      *
*     This enhancement is used to maintain data in the custom          *
*     subscreen - purchase order.                                      *
*                                                                      *
************************************************************************
*------------------------ CHANGE LOG ----------------------------------*
*  Date     TR # By      Description                                   *
* --------- ---- ------- --------------------------------------------- *
* 29-Nov-10 0872 BTBOUND D30K916097 - ARIBA R1 - Initial development   *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    :  004                                                 *
* Date          :  04-Jun-2014                                         *
* Modified By   :  JRHARTUNG                                           *
* Correction No :  D30K923669                                          *
* Description   :  Create segment Z1ARBA1 and populate with            *
*                  references to PO item attachments                   *
*----------------------------------------------------------------------*
* 01/04/2021 | MEDISETA | S01K900755 | Master agreement requirement
* ---------------------------------------------------------------------*
* 13-APR-2022  DADIM       D30K932132  CHG0246647 - MSA Field Optional *
*                                      Requirement for G05 only        *
************************************************************************

TABLES: ekko,
        ekpo,
        zvar,
        zmmt_mastagree,     " I - TR S01K900755
        t024,
        zmmt_locmast.

TYPES:  ty_wa_z1arba1    TYPE z1arba1,
        ty_it_z1arba1    TYPE STANDARD TABLE OF z1arba1.

DATA:   gwa_zvar         TYPE zvar,
        git_zvar         TYPE STANDARD TABLE OF zvar.

DATA:   zekko            TYPE ekko,
        zekpo            TYPE ekpo,
        gf_aktyp         TYPE aktyp,
        gf_trtyp         TYPE trtyp,
        gf_bukrs         TYPE ekko-bukrs,   " I - TR S01K900755
        gf_bsart         TYPE ekko-bsart,   " I - TR S01K900755
        v_msa_txt        TYPE char20,      " I - TR S01K900755
        t_dynfld         LIKE dynpread OCCURS 2 WITH HEADER LINE," I - TR S01K900755
        gf_todate        TYPE ekko-kdatb,
        gf_fromdate      TYPE ekko-kdatb,
        gf_lifnr         TYPE ekko-lifnr,
        gf_ekorg         TYPE lfm1-ekorg,
        gf_eikto         TYPE lfm1-eikto,
        zztrlocalias2    TYPE zmmt_locmast-zztrlocalias,
        zztrlocalias3    TYPE zmmt_locmast-zztrlocalias,
        zztrlocalias4    TYPE zmmt_locmast-zztrlocalias.
DATA: BEGIN OF gs_locmast,
      zztrloc TYPE zmmt_locmast-zztrloc,
      zztrlocalias TYPE zmmt_locmast-zztrlocalias,
      zzconvndid  TYPE zmmt_locmast-zzconvndid,
      zzparty  TYPE zmmt_locmast-zzparty,
      zzactflg  TYPE zmmt_locmast-zzactflg,
      zzfrequsdflg  TYPE zmmt_locmast-zzfrequsdflg,
      END OF gs_locmast.
*Start of change by DADIM for CHG0246647
DATA : gf_ekgrp TYPE ekko-ekgrp,
       gt_zvar  TYPE TABLE OF zvar,
       gs_zvar  TYPE zvar.
CONSTANTS : gc_program TYPE programm  VALUE 'ZXM06O01',
            gc_varname TYPE z_varname VALUE 'EKGRP'.
*End of change by DADIM for CHG0246647
