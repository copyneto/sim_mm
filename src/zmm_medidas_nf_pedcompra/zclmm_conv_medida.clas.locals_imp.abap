CLASS lcl_ZI_MM_CONV_MEDIDA DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR zi_mm_conv_medida
        RESULT result.
ENDCLASS.

CLASS lcl_ZI_MM_CONV_MEDIDA IMPLEMENTATION.
  METHOD get_global_authorizations.
    RETURN.
  ENDMETHOD.
ENDCLASS.
