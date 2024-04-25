CLASS zclmm_rest_doc_rem_rec_post DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zifca_rest .

    METHODS constructor
      IMPORTING
        !io_request  TYPE REF TO if_http_request
        !io_response TYPE REF TO if_http_response .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclmm_rest_doc_rem_rec_post IMPLEMENTATION.


  METHOD constructor.

    me->zifca_rest~go_response = io_response.
    me->zifca_rest~go_request = io_request.

  ENDMETHOD.


  METHOD zifca_rest~handle_request.

***************************************************************************
    " VARIABLES AND OBJECTS
***************************************************************************
    DATA:
      lt_tab_cpi       TYPE TABLE OF zsmm_doc_rem_rec,
      lr_deserializer  TYPE REF TO cl_trex_json_deserializer,
      lv_string_writer TYPE REF TO cl_sxml_string_writer,
      lv_json_body     TYPE string,
      ls_struc_cpi     TYPE zsmm_doc_rem_rec,
      lv_xstring       TYPE xstring.

    CREATE OBJECT lr_deserializer.

***************************************************************************
    " JSON TO ABAP DATA
***************************************************************************
    lv_json_body = me->zifca_rest~go_request->get_cdata( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
      json = lv_json_body
      CHANGING
      data = ls_struc_cpi
      ).

    CHECK  ls_struc_cpi IS NOT INITIAL.

    APPEND ls_struc_cpi TO lt_tab_cpi.

***************************************************************************
    " CREATE OBJECT
***************************************************************************

    NEW zclmm_modify_wms_tables( is_doc_rem_rec = ls_struc_cpi  )->fill_changes_return(  ).

***************************************************************************
    " CONVERT INPUT TO JSON STRING
***************************************************************************
    lv_string_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    CALL TRANSFORMATION id SOURCE array = lt_tab_cpi RESULT XML lv_string_writer.
    lv_xstring = lv_string_writer->get_output( ).

***************************************************************************
    " RETURN CREATED OBJECT AS RESPONSE (CONVENTION)
***************************************************************************
    me->zifca_rest~go_response->set_data( data = lv_xstring ).

  ENDMETHOD.


  METHOD zifca_rest~set_response.

    CALL METHOD me->zifca_rest~go_response->set_data
      EXPORTING
        data = is_data.

  ENDMETHOD.
ENDCLASS.
