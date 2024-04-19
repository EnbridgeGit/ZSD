*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLSDCIS04.......................................*
DATA:  BEGIN OF STATUS_ZLSDCIS04                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLSDCIS04                     .
CONTROLS: TCTRL_ZLSDCIS04
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLSDCIS04                     .
TABLES: ZLSDCIS04                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
