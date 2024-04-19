"Name: \PR:SAPFV50C\FO:LIEFERKOPF_AUFBAUEN\SE:END\EI
ENHANCEMENT 0 Z_SAPFV50C.
*Feb 2011 btboundy
  move cvbak-VDATU to likp-wadat_ist.
*----SAHMAD - August 13, 2021 - CHG0223869
DATA:  lv_year(4)  TYPE n,
       lv_mth(3)   TYPE n,
       lv_kunnr type vbak-kunnr,
       lv_frye1 type t001b-frye1,
       lv_frpe1 type t001b-frpe1,
       lv_perclose_flag(1) VALUE 'N',
       lv_pgidate type ZLSD_PGIDATE-pgidate.

lv_kunnr = '*'.

SELECT SINGLE pgidate INTO lv_pgidate
  FROM ZLSD_PGIDATE
  WHERE vkorg = cvbak-vkorg
    AND kunnr = cvbak-kunnr.

IF sy-subrc <> 0.
SELECT SINGLE pgidate INTO lv_pgidate
  FROM ZLSD_PGIDATE
  WHERE vkorg = cvbak-vkorg
    AND kunnr = lv_kunnr.
ENDIF.

IF lv_pgidate IS NOT INITIAL.
    lv_year = cvbak-vdatu+0(4).
    lv_mth  = cvbak-vdatu+4(2).

   SELECT SINGLE frye1 frpe1 FROM t001b
      INTO (lv_frye1, lv_frpe1)
     WHERE rrcty = '0'
       AND bukrs = 'ZCIS'
       AND mkoar = 'M'.
    IF sy-subrc = 0.
      IF lv_year = lv_frye1.
        IF lv_mth < lv_frpe1.
          MOVE 'Y' TO lv_perclose_flag.
        ENDIF.
      ELSEIF lv_year < lv_frye1.
        MOVE 'Y' TO lv_perclose_flag.
      ENDIF.
    ENDIF.
    "if period is closed then maintain first day of the open period
    IF lv_perclose_flag = 'Y'.
        CONCATENATE lv_frye1 lv_frpe1+1(2) '01'
           INTO likp-wadat_ist.
    ENDIF.
ENDIF.
*----End of CHG0223869
ENDENHANCEMENT.
