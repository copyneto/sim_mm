class ZCLMM_REST_RETO_CON_INV_POST definition
  public
  final
  create public .

public section.

  interfaces ZIFCA_REST .

  methods CONSTRUCTOR
    importing
      !IO_REQUEST type ref to IF_HTTP_REQUEST
      !IO_RESPONSE type ref to IF_HTTP_RESPONSE .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLMM_REST_RETO_CON_INV_POST IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zifca_rest~go_response = io_response.
    me->zifca_rest~go_request = io_request.

  ENDMETHOD.


  METHOD zifca_rest~handle_request.

***************************************************************************
    " VARIABLES AND OBJECTS
***************************************************************************
    DATA:
      lt_tab_cpi       TYPE TABLE OF zsmm_reto_cont_inv,
      lr_deserializer  TYPE REF TO cl_trex_json_deserializer,
      lv_string_writer TYPE REF TO cl_sxml_string_writer,
      lv_json_body     TYPE string,
      lv_json_ret      TYPE string,
      ls_struc_cpi     TYPE zsmm_reto_cont_inv,
      lv_xstring       TYPE xstring.

    CREATE OBJECT lr_deserializer.

***************************************************************************
    " JSON TO ABAP DATA
***************************************************************************
    lv_json_body = me->zifca_rest~go_request->get_cdata( ).

    NEW zclca_monitor_cpi(  )->started_process(
             EXPORTING
                  iv_metodo = 'POST'
                  iv_processo = 'ZMM_RETOR_CONTAGEM_INV_WMS'
                  iv_json = lv_json_body
             IMPORTING
                  et_return = DATA(lt_ret)  ).

    IF lt_ret IS INITIAL.
      COMMIT WORK.
    ENDIF.

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

    DATA(lt_return) = NEW zclmm_reto_con_inv( is_reto_cont_inv = ls_struc_cpi  )->process(  ).

***************************************************************************
    " CONVERT INPUT TO JSON STRING
***************************************************************************
    lv_string_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    CALL TRANSFORMATION id SOURCE array = lt_tab_cpi RESULT XML lv_string_writer.
    lv_xstring = lv_string_writer->get_output( ).

    CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
      EXPORTING
        im_xstring  = lv_xstring
        im_encoding = 'UTF-8'
      IMPORTING
        ex_string   = lv_json_ret.

    REFRESH: lt_ret.

    NEW zclca_monitor_cpi(  )->save_log(
          EXPORTING
               iv_metodo = 'POST'
               iv_processo = 'ZMM_RETOR_CONTAGEM_INV_WMS'
               iv_json  = lv_json_body
               iv_json_retorno = lv_json_ret
               it_return = CORRESPONDING #( lt_return )
          IMPORTING
               et_return = lt_ret ).

    IF lt_ret IS INITIAL.
      COMMIT WORK.
    ENDIF.

***************************************************************************
    " RETURN CREATED OBJECT AS RESPONSE (CONVENTION)
***************************************************************************
    me->zifca_rest~go_response->set_data( data = lv_xstring ).

  ENDMETHOD.


  METHOD ZIFCA_REST~SET_RESPONSE.

    CALL METHOD me->zifca_rest~go_response->set_data
      EXPORTING
        data = is_data.

  ENDMETHOD.
ENDCLASS.
