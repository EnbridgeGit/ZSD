*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLSDCIS01_COPY..................................*
DATA:  BEGIN OF STATUS_ZLSDCIS01_COPY                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLSDCIS01_COPY                .
CONTROLS: TCTRL_ZLSDCIS01_COPY
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLSDCIS01_COPY                .
TABLES: ZLSDCIS01_COPY                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
