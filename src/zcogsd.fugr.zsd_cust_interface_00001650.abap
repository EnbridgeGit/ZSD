FUNCTION ZSD_CUST_INTERFACE_00001650.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_POSTAB) LIKE  RFPOS STRUCTURE  RFPOS
*"  EXPORTING
*"     VALUE(E_POSTAB) LIKE  RFPOS STRUCTURE  RFPOS
*"----------------------------------------------------------------------
* OCT 13 2021 birudurd To get Customer on FBL3N output for CIS data.
*-------------- Initialize Output by using the following line ----------
  E_POSTAB = I_POSTAB.

  TABLES:BSEG.

  DATA: W_KUNNR TYPE KUNNR,
        W_KIDNO TYPE KIDNO,
        W_VKORG TYPE VBRK-VKORG,
        lv_assign type ZLSD_PGIDATE-ASSIGNMENT_FI .

  CLEAR:W_KUNNR , W_KIDNO, W_VKORG, LV_ASSIGN.

  IF E_POSTAB-KIDNO IS NOT INITIAL.
    SELECT SINGLE VKORG KUNAG
      INTO (W_VKORG , W_KUNNR)
      FROM VBRK WHERE VBELN = E_POSTAB-KIDNO.
    IF SY-SUBRC = 0.
      SELECT SINGLE ASSIGNMENT_FI INTO lv_assign
        FROM ZLSD_PGIDATE
        WHERE vkorg = W_VKORG
          AND kunnr = W_KUNNR.
      IF lv_assign IS NOT INITIAL.
        E_POSTAB-ZZKUNNR = W_KUNNR.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
