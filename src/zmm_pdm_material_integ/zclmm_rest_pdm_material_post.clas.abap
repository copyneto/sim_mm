CLASS zclmm_rest_pdm_material_post DEFINITION
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

    CONSTANTS:
      gc_process TYPE ze_processo VALUE 'ZMM_PDM_MATERIAL',
      gc_e       TYPE char1 VALUE 'E',
      gc_s       TYPE char1 VALUE 'S',
      gc_200     TYPE i VALUE 200.


    DATA:
      go_cpi       TYPE REF TO zclca_monitor_cpi.
    METHODS start_log
      IMPORTING
        iv_json    TYPE string
        iv_method  TYPE ze_method_api
        iv_process TYPE ze_processo.

    METHODS finish_log
      IMPORTING
        iv_json     TYPE string
        iv_json_ret TYPE string
        iv_method   TYPE string
        iv_process  TYPE ze_processo
        it_return   TYPE bapireturn_t.

ENDCLASS.



CLASS zclmm_rest_pdm_material_post IMPLEMENTATION.


  METHOD zifca_rest~handle_request.

***************************************************************************
    " VARIABLES AND OBJECTS
***************************************************************************
    DATA:
      lt_tab_cpi       TYPE TABLE OF zsmm_pdm_material_int,
      lr_deserializer  TYPE REF TO cl_trex_json_deserializer,
      lv_string_writer TYPE REF TO cl_sxml_string_writer,
      lv_json_body     TYPE string,
      lv_method        TYPE string,
      lv_json_ret      TYPE string,
      ls_struc_cpi     TYPE  zsmm_pdm_material_int,
      lv_xstring       TYPE xstring,
      lv_code          TYPE i,
      lv_reason        TYPE string.

    CREATE OBJECT lr_deserializer.

***************************************************************************
    " JSON TO ABAP DATA
***************************************************************************
    lv_json_body = me->zifca_rest~go_request->get_cdata( ).
    lv_method = me->zifca_rest~go_request->get_method( ).


    start_log(
        iv_json = lv_json_body
        iv_method = CONV #( lv_method  )
        iv_process = gc_process
    ).

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

    DATA(lt_return) = NEW zclmm_pdm_material( ls_struc_cpi )->execute(  ).

***************************************************************************
    " CONVERT INPUT TO JSON STRING
***************************************************************************
    lv_string_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    CALL TRANSFORMATION id SOURCE return = lt_return RESULT XML lv_string_writer.
    lv_xstring = lv_string_writer->get_output( ).

    CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
      EXPORTING
        im_xstring  = lv_xstring
        im_encoding = 'UTF-8'
      IMPORTING
        ex_string   = lv_json_ret.

    finish_log(
        iv_json = lv_json_body
        iv_json_ret = lv_json_ret
        iv_method = lv_method
        iv_process = gc_process
        it_return = CORRESPONDING #( lt_return )
    ).

***************************************************************************
    " RETURN CREATED OBJECT AS RESPONSE (CONVENTION)
***************************************************************************
    me->zifca_rest~go_response->set_header_field( name = CONV #( TEXT-002 ) value = CONV #( TEXT-003 )  ).
    me->zifca_rest~go_response->set_status( code = gc_200
                                                                         reason = COND #( WHEN NOT line_exists( lt_return[ type = gc_e ] ) THEN CONV string( TEXT-004 ) ELSE CONV string( TEXT-005 ) ) ).
    me->zifca_rest~go_response->set_data( data = lv_xstring ).

  ENDMETHOD.


  METHOD zifca_rest~set_response.

    CALL METHOD me->zifca_rest~go_response->set_data
      EXPORTING
        data = is_data.

  ENDMETHOD.


  METHOD constructor.

    me->zifca_rest~go_response = io_response.
    me->zifca_rest~go_request = io_request.

    go_cpi = NEW zclca_monitor_cpi(  )->get_instance(  ).

  ENDMETHOD.

  METHOD start_log.


    go_cpi->started_process(
                EXPORTING
                    iv_processo  = iv_process
                    iv_metodo =  iv_method
                    iv_json     = iv_json
                IMPORTING
                    et_return = DATA(lt_return)
     ).

    IF lt_return IS INITIAL.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD finish_log.

    go_cpi->save_log(
                        EXPORTING
                            iv_processo  =  iv_process
                            iv_metodo = CONV #( iv_method )
                            iv_json_retorno     = iv_json_ret
                            iv_json = iv_json
                            it_return =  VALUE bapiret2_t( ( id = 'ZBP_LOG_API_BP'
                                                                           number = 001
                                                                           type = COND #( WHEN NOT line_exists( it_return[ type = gc_e ] ) THEN gc_s  ELSE gc_e )
                                                                           message = COND #( WHEN NOT line_exists( it_return[ type = gc_e ] ) THEN CONV string( TEXT-004 ) ELSE CONV string( TEXT-005 ) ) ) )
                        IMPORTING
                            et_return = DATA(lt_return)
             ).

    IF lt_return IS INITIAL.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
