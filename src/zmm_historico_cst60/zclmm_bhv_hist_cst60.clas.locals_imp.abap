CLASS lcl_Historico DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Historico RESULT result.
    METHODS validatcsticms FOR DETERMINE ON SAVE
      IMPORTING keys FOR historico~validatcsticms.

ENDCLASS.

CLASS lcl_Historico IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD validatCstIcms.
    RETURN.
  ENDMETHOD.

ENDCLASS.
