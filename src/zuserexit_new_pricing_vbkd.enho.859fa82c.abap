"Name: \PR:SAPMV45A\FO:USEREXIT_NEW_PRICING_VBKD\SE:BEGIN\EI
ENHANCEMENT 0 ZUSEREXIT_NEW_PRICING_VBKD.

data: lv_knprs type knprs.
check  vbkd-posnr is NOT INITIAL.
  IF vbkd-kdkg1 <> *vbkd-kdkg1.
     select SINGLE knprs INTO lv_knprs from ZSD_PRICEUPDATE
       WHERE auart = vbak-auart.
     IF sy-subrc = 0.
        new_pricing = lv_knprs.
     else.
        new_pricing = 'C'.
     ENDIF.
 ENDIF.

ENDENHANCEMENT.
