class ZCLMM_SUBCON_BUSTW definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces J1B_SUBCON_BUSTW_INF .
protected section.
private section.
ENDCLASS.



CLASS ZCLMM_SUBCON_BUSTW IMPLEMENTATION.


  METHOD j1b_subcon_bustw_inf~set_bustw.

    IF ( is_mseg-bwart = '541' OR
         is_mseg-bwart = '542' OR
         is_mseg-bwart = 'Z41' OR
         is_mseg-bwart = 'Z42' ) AND
          is_mseg-mwskz <> space.
      o_bustw = 'WA04'.
    ELSE.
      o_bustw = is_mseg-bustw.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
