*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLSDE01.........................................*
DATA:  BEGIN OF STATUS_ZLSDE01                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLSDE01                       .
CONTROLS: TCTRL_ZLSDE01
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLSDE01                       .
TABLES: ZLSDE01                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
