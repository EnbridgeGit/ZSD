*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(SEGMENT) LIKE  EDIDD STRUCTURE  EDIDD OPTIONAL
*"             VALUE(DVTCOMAG) OPTIONAL
*"             VALUE(DXMESCOD) LIKE  EDIDC-MESCOD OPTIONAL
*"             VALUE(CONTRL) LIKE  EDIDC STRUCTURE  EDIDC OPTIONAL
*"       TABLES
*"              DXBDCDATA STRUCTURE  BDCDATA OPTIONAL
*"              DXVBAP OPTIONAL
*"              DXVBEP OPTIONAL
*"              DYVBEP OPTIONAL
*"              DXVBADR OPTIONAL
*"              DYVBADR OPTIONAL
*"              DXVBPA STRUCTURE  VBPAVB OPTIONAL
*"              DXVBUV OPTIONAL
*"              DD_FLAG_P OPTIONAL
*"              DXKOMV OPTIONAL
*"              DXVEKP OPTIONAL
*"              DYVEKP OPTIONAL
*"       CHANGING
*"             VALUE(DXVBAK) OPTIONAL
*"             VALUE(DD_FLAG_K) OPTIONAL
*"       EXCEPTIONS
*"              USER_ERROR
*"----------------------------------------------------------------------

***********************************************************************
* This UserExit is for BANNER Sale Order & CONTRAX Gas Sale interfaces*
* The data Structure set up in this exit will be used to fill screen  *
* data in the next EXIT for the following fields:                     *
* VBKD-KONDA: Price Group                                             *
* VBKD-KDGRP: Customer Group                                          *
* VBKD-BZIRK: Sales District                                          *
* VBKD-KURRF: Currency Exchange Rate                                  *
***********************************************************************

DATA: BEGIN OF control_rec.           "Control Record
        INCLUDE STRUCTURE edidc.
DATA: END OF control_rec.

DATA: lv_int  TYPE integer.

DATA: BEGIN OF newseg,                "Custom Segment
      konda LIKE vbkd-konda,
      kdgrp LIKE vbkd-kdgrp,
      bzirk LIKE vbkd-bzirk.
DATA: END OF newseg.
DATA: zzdxvbak LIKE xvbak.
DATA: w_e1edk03 LIKE e1edk03.

MOVE contrl TO control_rec.

***********************************************************************
*             Changes for BANNER interface                            *
***********************************************************************

IF control_rec-sndprn = 'BANNER' OR  control_rec-sndprn = 'CTRXGS'
                                 OR control_rec-sndprn = 'CTRXST'
                                 OR control_rec-sndprn = 'ENTGS'.
  IF control_rec-cimtyp = 'ZORD05E'.    "Extension
    IF segment-segnam = 'Z1OR05E'.
      MOVE: segment-sdata TO newseg.
      zzdxvbak = dxvbak.

      lv_int = STRLEN( newseg-konda ).
      WHILE lv_int < 2.
        CONCATENATE ' ' newseg-konda INTO newseg-konda.
        lv_int = lv_int + 1.
      ENDWHILE.

      lv_int = STRLEN( newseg-kdgrp ).
      WHILE lv_int < 2.
        CONCATENATE ' ' newseg-kdgrp INTO newseg-kdgrp.
        lv_int = lv_int + 1.
      ENDWHILE.

      lv_int = STRLEN( newseg-bzirk ).
      WHILE lv_int < 4.
        CONCATENATE ' ' newseg-bzirk  INTO newseg-bzirk .
        lv_int = lv_int + 1.
      ENDWHILE.

      CONCATENATE newseg-konda newseg-kdgrp newseg-bzirk
                  INTO zzdxvbak-bname RESPECTING BLANKS.
      dxvbak = zzdxvbak.
    ENDIF.
  ENDIF.

  IF segment-segnam = 'E1EDK03'.
    MOVE segment-sdata TO w_e1edk03.
    IF w_e1edk03-iddat = '012'.
      MOVE dxvbak          TO zzdxvbak.
      MOVE w_e1edk03-datum TO zzdxvbak-audat.
      MOVE zzdxvbak        TO dxvbak.
    ENDIF.
  ENDIF.

ENDIF.

***********************************************************************
*           Changes for CONTRAX interface                             *
***********************************************************************
DATA: BEGIN OF w_e1edk02,             "Custom Segment
      qualf(3)   TYPE  c,
      belnr(35)  TYPE  c.
DATA: END OF w_e1edk02.

DATA: w_e1edk01 LIKE e1edk01.

IF control_rec-sndprn = 'CTRXGS' OR control_rec-sndprn = 'CTRXST'.         "in DEV CTRXGX
  IF segment-segnam = 'E1EDK02'.
    MOVE segment-sdata TO w_e1edk02.
    IF w_e1edk02-qualf = '011'.
      MOVE dxvbak          TO zzdxvbak.
      MOVE w_e1edk02-belnr TO zzdxvbak-xblnr.
      MOVE zzdxvbak        TO dxvbak.
    ENDIF.
    IF w_e1edk02-qualf = '017'.
      MOVE dxvbak          TO zzdxvbak.
      MOVE w_e1edk02-belnr TO zzdxvbak-zuonr.
      MOVE zzdxvbak        TO dxvbak.
    ENDIF.
  ENDIF.
ENDIF.

IF control_rec-sndprn = 'CTRXST'.
  IF segment-segnam = 'E1EDK01'.
    MOVE segment-sdata    TO w_e1edk01.
    MOVE dxvbak           TO zzdxvbak.
    MOVE w_e1edk01-wkurs  TO zzdxvbak-ihrez.
    MOVE zzdxvbak         TO dxvbak.
  ENDIF.
ENDIF.
********CIS to SAP ERP Changes - SAHMAD - 2021-02-26
IF control_rec-sndprn = 'BANNER'.
  IF segment-segnam = 'E1EDK02'.
    MOVE segment-sdata TO w_e1edk02.
    "Map Reconkey from IDoc to Assignment (Sales order)
    IF w_e1edk02-qualf = '017'.
      MOVE dxvbak          TO zzdxvbak.
      MOVE w_e1edk02-belnr TO zzdxvbak-zuonr.
      MOVE zzdxvbak        TO dxvbak.
    ENDIF.
  ENDIF.
ENDIF.
********
