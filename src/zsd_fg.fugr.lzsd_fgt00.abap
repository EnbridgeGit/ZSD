*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_PRICEUPDATE.................................*
DATA:  BEGIN OF STATUS_ZSD_PRICEUPDATE               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_PRICEUPDATE               .
CONTROLS: TCTRL_ZSD_PRICEUPDATE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSD_PRICEUPDATE               .
TABLES: ZSD_PRICEUPDATE                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
