class ZCLMM_SUBCONTRATACAO definition
  public
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_BADI_J1B_SUBCONTRACTING .
protected section.
private section.
ENDCLASS.



CLASS ZCLMM_SUBCONTRATACAO IMPLEMENTATION.


  METHOD if_badi_j1b_subcontracting~set_valuepostingstring.

    CONSTANTS: lc_true          VALUE 'X',
               lc_mvt541(3)     VALUE '541',
               lc_mvt542(3)     VALUE '542',
               lc_mvt543(3)     VALUE '543',
               lc_mvt544(3)     VALUE '544',
               lc_mvtzzz(3)     VALUE 'ZZZ',
               lc_mvtzzy(3)     VALUE 'ZZY',
               lc_z41(3)        VALUE 'Z41',
               lc_z42(3)        VALUE 'Z42',
               lc_zzz(3)        VALUE 'ZZZ',
               lc_zzy(3)        VALUE 'ZZY',
               lc_bustw_wa04(4) VALUE 'WA04',
               lc_bustw_wa01(4) VALUE 'WA01'.

    valuepostingstring = materialdocumentitem-bustw.
*    IF materialdocumentitem-mwskz <> space.
    CASE materialdocumentitem-bwart.
      WHEN lc_mvt541 OR lc_z41.
        valuepostingstring = lc_bustw_wa04.
      WHEN lc_mvt542 OR lc_z42.
        valuepostingstring = lc_bustw_wa04.
        isreturnsitem      = lc_true.
      WHEN lc_mvtzzz OR lc_zzz.
        valuepostingstring = lc_bustw_wa04.
      WHEN lc_mvtzzy OR lc_zzy.
        valuepostingstring = lc_bustw_wa04.
        isreturnsitem      = lc_true.
      WHEN lc_mvt543.
        valuepostingstring = lc_bustw_wa01.
      WHEN lc_mvt544.
        valuepostingstring = lc_bustw_wa01.
    ENDCASE.
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
