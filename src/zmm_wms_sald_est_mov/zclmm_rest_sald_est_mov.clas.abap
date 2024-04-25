class ZCLMM_REST_SALD_EST_MOV definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_rest
      IMPORTING
        !io_server     TYPE REF TO if_http_server
      RETURNING
        VALUE(ro_rest) TYPE REF TO zifca_rest .
ENDCLASS.



CLASS ZCLMM_REST_SALD_EST_MOV IMPLEMENTATION.


  METHOD GET_REST.

***************************************************************************
    " VARIABLES
***************************************************************************
    DATA:
      lv_class_name     TYPE seoclsname,
      lv_request_method TYPE string.

***************************************************************************
    " APPEND REQUEST METHOD TO BASE CLASS
***************************************************************************
    lv_request_method = io_server->request->get_header_field( '~request_method' ).

    CONCATENATE 'ZCLMM_REST_SALD_EST_MOV_' lv_request_method INTO lv_class_name.

***************************************************************************
    " RETURN CLASS OBJECT
***************************************************************************
    TRY.
        CREATE OBJECT ro_rest
        TYPE (lv_class_name)
        EXPORTING
        io_request   = io_server->request
        io_response  = io_server->response.

***************************************************************************
        " ERRORS
***************************************************************************
      CATCH cx_sy_create_object_error.
    ENDTRY.

  ENDMETHOD.


  METHOD IF_HTTP_EXTENSION~HANDLE_REQUEST.

***************************************************************************
    " VARIABLES
***************************************************************************
    DATA:
      lo_rest_class TYPE REF TO zifca_rest,
      lo_error         TYPE REF TO cx_root,
      lv_reason      TYPE string.

***************************************************************************
    " GET THE CLASS OBJECT
***************************************************************************
    TRY.

        lo_rest_class ?= get_rest( io_server = server ).

***************************************************************************
        " EXECUTE THE RETRIEVED CLASS
***************************************************************************
        lo_rest_class->handle_request( ).

***************************************************************************
        " ERROR
***************************************************************************
      CATCH cx_root INTO lo_error.

        lv_reason = lo_error->get_text( ).
        server->response->set_status( code = 500
        reason = lv_reason ).

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
