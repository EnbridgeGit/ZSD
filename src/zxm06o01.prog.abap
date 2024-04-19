*----------------------------------------------------------------------*
***INCLUDE ZXM06O01 .
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 13-APR-2022 DADIM        D30K932132 CHG0246647 - MSA Field Optional  *
*                                     Requirement for G05 only         *
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  STATUS_0201  OUTPUT
*&---------------------------------------------------------------------*

MODULE status_0201 OUTPUT.
* Begin of insert TR  S01K900755 by  BIRUDURD
  IF sy-tcode = 'ME31L' OR sy-tcode = 'ME32L' OR sy-tcode = 'ME33L' .
    v_msa_txt = 'NAESB Agreement'.
    SELECT SINGLE zztrlocalias
               FROM zmmt_locmast
               INTO zztrlocalias2
              WHERE zztrloc = ekko-zztrloc2.
    IF sy-subrc = 0.
    ENDIF.
    SELECT SINGLE zztrlocalias
             FROM zmmt_locmast
             INTO zztrlocalias3
            WHERE zztrloc = ekko-zztrloc3.
    IF sy-subrc = 0.
    ENDIF.
    SELECT SINGLE zztrlocalias
           FROM zmmt_locmast
           INTO zztrlocalias4
          WHERE zztrloc = ekko-zztrloc4.
    IF sy-subrc = 0.
    ENDIF.
    IF gf_bsart = 'ZLOC' OR  gf_bsart = 'ZDP'.
      LOOP AT SCREEN.
        IF screen-group2 = 'GRP'.
          screen-required = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF gf_bsart = 'ZDP'.
      LOOP AT SCREEN.
        IF screen-group2 = 'ZDP'.
          screen-required = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSEIF sy-tcode = 'ME31K' OR sy-tcode = 'ME32K' OR sy-tcode = 'ME33K'.
    v_msa_txt = 'Master Agreement'.
    IF gf_bsart NE 'ZK' AND gf_bsart NE 'ZJ' .
      LOOP AT SCREEN.
        IF screen-group1 = 'NA'.
          screen-invisible = '1'.
          screen-active = '0'.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1 = 'TB'.
          screen-invisible = '1'.
          screen-active = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
    LOOP AT SCREEN.
      IF screen-group1 = 'LOC'.
        screen-invisible = '1'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
*Start of change By DADIM for CHG0246647
    CLEAR : gt_zvar, gs_zvar.
    SELECT * FROM zvar INTO TABLE gt_zvar
           WHERE programm = gc_program AND
                 varname  = gc_varname.
    READ TABLE gt_zvar INTO gs_zvar WITH KEY value1 = gf_ekgrp.
    IF sy-subrc = 0.
      READ TABLE gt_zvar INTO gs_zvar WITH KEY value1 = ekko-zzekgrp.
      IF sy-subrc = 0.
        LOOP AT SCREEN.
          IF screen-group2 = 'GRP'.
            screen-required = '0'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ELSEIF ekko-zzekgrp = ''.
        LOOP AT SCREEN.
          IF screen-group2 = 'GRP'.
            screen-required = '0'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ELSE.
        LOOP AT SCREEN.
          IF screen-group2 = 'GRP'.
            screen-required = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
*End of change By DADIM for CHG0246647
  ENDIF.

  IF gf_trtyp = 'A'.
    LOOP AT SCREEN.
      screen-input = 0. " I suppose the field are designed for input/output

      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
*      screen-input = 1. " I suppose the field are designed for input/output
      IF screen-group2 NE 'DIS'.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_0101  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  STATUS_0111  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0111 OUTPUT.


  IF gf_trtyp = 'A'.
    LOOP AT SCREEN.
      screen-input = 0. " I suppose the field are designed for input/output
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      screen-input = 1. " I suppose the field are designed for input/output
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_0111  OUTPUT
