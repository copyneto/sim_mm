CLASS zclmm_base_terc_pdf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclmm_base_terc_pdf IMPLEMENTATION.


  METHOD if_rap_query_provider~select.
    DATA: lt_tab                   TYPE TABLE OF zi_mm_base_terc_pdf.
    DATA: lt_master_keys           TYPE cl_somu_form_services=>ty_gt_key.
    DATA: lv_content               TYPE  xstring.
    DATA: lo_cl_somu_form_services TYPE REF TO cl_somu_form_services,
          lt_keys                  TYPE cl_somu_form_services=>ty_gt_key.


    TRY.
        "Requested data
        IF io_request->is_data_requested(  ).

          "Paginacao
          DATA(lv_offset) = io_request->get_paging( )->get_offset( ).
          DATA(lv_page_size) = io_request->get_paging( )->get_page_size( ).
          DATA(lv_max_rows) = COND #( WHEN lv_page_size = if_rap_query_paging=>page_size_unlimited
                                      THEN 0 ELSE lv_page_size )  .

          "Recupera filtros
          TRY.
              TRY.
                  DATA(lt_parameters) = io_request->get_parameters( ).
*                  DATA(lt_filters) = io_request->get_filter( )->get_as_ranges( ). "#EC CI_CONV_OK
                CATCH cx_rap_query_filter_no_range INTO DATA(lo_ex_filter).
                  DATA(lv_exp_msg) = lo_ex_filter->get_longtext( ).
              ENDTRY.
              "Busca os parametros da custom entity
*              DATA(lv_salesorder)     =   VALUE #( lt_parameters[ parameter_name =  'P_HANDLINGUNITEXTERNALID' ]-value OPTIONAL ).
              "Cria instancia
              DATA(lo_controller) = zclmm_base_terc_pdf_imp=>get_instance( ).

*              lt_tab = VALUE #( ( stream_data = lo_controller->build( lt_parameters ) ) ).
              lt_tab = VALUE #( ( stream_data = lo_controller->build( lt_parameters ) ) ).
              io_response->set_total_number_of_records( 1 ).

*  " -------------- Send the response back to UI------------
              io_response->set_data( lt_tab ).

            CATCH cx_rap_query_filter_no_range INTO DATA(lv_range).
              DATA(lv_msg) = lv_range->get_text( ).
          ENDTRY.


        ENDIF.
      CATCH cx_rap_query_provider.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
