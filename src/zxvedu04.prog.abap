*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(DXVBAK) OPTIONAL
*"             VALUE(DVTCOMAG) OPTIONAL
*"             VALUE(DLAST_DYNPRO) OPTIONAL
*"             VALUE(DXMESCOD) LIKE  EDIDC-MESCOD OPTIONAL
*"       TABLES
*"              DXBDCDATA STRUCTURE  BDCDATA OPTIONAL
*"              DXVBAP OPTIONAL
*"              DXVBEP OPTIONAL
*"              DYVBEP OPTIONAL
*"              DXVBADR OPTIONAL
*"              DYVBADR OPTIONAL
*"              DXVBPA STRUCTURE  VBPAVB OPTIONAL
*"              DXVBUV OPTIONAL
*"              DIDOC_DATA STRUCTURE  EDIDD OPTIONAL
*"              DXKOMV OPTIONAL
*"              DXVEKP OPTIONAL
*"              DYVEKP OPTIONAL
*"       EXCEPTIONS
*"              USER_ERROR
*"----------------------------------------------------------------------

***********************************************************************
*This User-Exit fill out following screen fields for BANNER interface *
* VBKD-KONDA: Price Group                                             *
* VBKD-KDGRP: Customer Group                                          *
* VBKD-BZIRK: Sales District                                          *
*                                                                     *
*This User-Exit fill out following screen fields for CONTRAX interface*
* VBKD-ZUONR: Assignment                                              *
* VBAK-XBLNR: Reference Document Number                               *
* VBKD-KURRF: Currency Exchange Rate                                  *
* VBAK-AUDAT: Doc Date                                                *
* VBKD-BSTDK: Doc Date                                                *
* REV45A-KETDAT: Doc Date                                             *
***********************************************************************
DATA:
zzxvbak LIKE xvbak,
field01(2)  TYPE c,
field02(2)  TYPE c,
field03(4)  TYPE c,
h_flag,
lv_sndprn TYPE edi_sndprn,
ls_edidd TYPE edidd.

***********************************************************************
*                     CODE FOR BANNER INTERFACE                       *
***********************************************************************

MOVE dxvbak   TO  zzxvbak.
IF h_flag = space.
*******CIS to SAP Interface *********
  "-----Get Sender Partner id
  IF didoc_data[] IS NOT INITIAL.
    READ TABLE didoc_data INTO ls_edidd INDEX 1.
    SELECT SINGLE sndprn INTO lv_sndprn FROM edidc
                    WHERE docnum = ls_edidd-docnum.
  ENDIF.
***********
*   IF ZZXVBAK-VKORG = 'Z001'.          "BANNER Interface
  IF dlast_dynpro = '4002' AND dxbdcdata-fnam = 'VBKD-PRSDT'.
    MOVE zzxvbak-bname+0(2) TO field01.
    MOVE zzxvbak-bname+2(2) TO field02.
    MOVE zzxvbak-bname+4(4) TO field03.
*      ENDIF.

    IF field01 <> space.
      CLEAR dxbdcdata.
      dxbdcdata-fnam = 'VBKD-KONDA'.
      dxbdcdata-fval = field01.
      APPEND dxbdcdata.
      CLEAR dxbdcdata.
    ENDIF.

    IF field02 <> space.
      CLEAR dxbdcdata.
      dxbdcdata-fnam = 'VBKD-KDGRP'.
      dxbdcdata-fval = field02.
      APPEND dxbdcdata.
      CLEAR dxbdcdata.
    ENDIF.

    IF field03 <> space.
      CLEAR dxbdcdata.
      dxbdcdata-fnam = 'VBKD-BZIRK'.
      dxbdcdata-fval = field03.
      APPEND dxbdcdata.
      CLEAR dxbdcdata.
    ENDIF.


*   ENDIF.

***********************************************************************
*                     CODE FOR CONTRAX INTERFACE                      *
***********************************************************************

    dxbdcdata-fnam = 'BDC_OKCODE'.            "GO BACK
    dxbdcdata-fval = 'BACK'.
    APPEND dxbdcdata.
    CLEAR dxbdcdata.

    dxbdcdata-program = 'SAPMV45A'.           "SCREEN 4001
    dxbdcdata-dynpro  = '4001'.
    dxbdcdata-dynbegin  = 'X'.
    APPEND dxbdcdata.
    CLEAR dxbdcdata.

    IF zzxvbak-audat <> space.
      dxbdcdata-fnam = 'RV45A-KETDAT'.
      dxbdcdata-fval = zzxvbak-audat.
      APPEND dxbdcdata.
      CLEAR dxbdcdata.

      dxbdcdata-fnam = 'VBKD-BSTDK'.
      dxbdcdata-fval = zzxvbak-audat.
      APPEND dxbdcdata.
      CLEAR dxbdcdata.
    ENDIF.

    dxbdcdata-fnam = 'BDC_OKCODE'.             "GOTO ACCOUNTING TAB
    dxbdcdata-fval = 'KBUC'.
    APPEND dxbdcdata.
    CLEAR dxbdcdata.

    dxbdcdata-program = 'SAPMV45A'.            "SCREEN 4002
    dxbdcdata-dynpro  = '4002'.
    dxbdcdata-dynbegin  = 'X'.
    APPEND dxbdcdata.
    CLEAR dxbdcdata.


    IF zzxvbak-xblnr <> space.
      dxbdcdata-fnam = 'VBAK-XBLNR'.  "Ref Doc # field
      dxbdcdata-fval = zzxvbak-xblnr. "Ref Doc # Value
      APPEND dxbdcdata.
      CLEAR  dxbdcdata.
    ENDIF.

    IF zzxvbak-zuonr <> space.
      dxbdcdata-fnam = 'VBAK-ZUONR'.  "Assignmnet field
      dxbdcdata-fval = zzxvbak-zuonr. "Assignment value
      APPEND dxbdcdata.
    ENDIF.
    "----BANNER (CIS) IDoc has IHREZ field populated.
    "----It should not map to KURRF field.
    IF lv_sndprn <> 'BANNER'.
      IF zzxvbak-ihrez <> space.
        dxbdcdata-fnam = 'VBKD-KURRF'.
        dxbdcdata-fval = zzxvbak-ihrez.
        APPEND dxbdcdata.
        CLEAR  dxbdcdata.
      ENDIF.
    ENDIF.


    dxbdcdata-fnam = 'BDC_OKCODE'.             "GOTO Sales TAB
    dxbdcdata-fval = 'KKAU'.
    APPEND dxbdcdata.
    CLEAR dxbdcdata.

    dxbdcdata-program = 'SAPMV45A'.            "SCREEN 4002
    dxbdcdata-dynpro  = '4002'.
    dxbdcdata-dynbegin  = 'X'.
    APPEND dxbdcdata.
    CLEAR dxbdcdata.

    IF zzxvbak-audat <> space.
      dxbdcdata-fnam = 'VBAK-AUDAT'.
      dxbdcdata-fval = zzxvbak-audat.
      APPEND dxbdcdata.
      CLEAR dxbdcdata.
    ENDIF.


  ENDIF.
***********************************************************************
  h_flag = 'X'.
ENDIF.
*
