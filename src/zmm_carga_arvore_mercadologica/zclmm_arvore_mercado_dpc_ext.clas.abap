CLASS zclmm_arvore_mercado_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zclmm_arvore_mercado_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /iwbep/if_mgw_appl_srv_runtime~create_stream
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclmm_arvore_mercado_dpc_ext IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_stream.
    DATA ls_file TYPE zclmm_arvore_mercado_mpc_ext=>ts_cargaarvoremerctype.

    CALL METHOD cl_bcs_convert=>xstring_to_string
      EXPORTING
        iv_xstr   = is_media_resource-value
        iv_cp     = '1209'
      RECEIVING
        rv_string = DATA(lv_content).

    ls_file-filename = iv_slug.
    DATA(lo_carga_arvore) = NEW zclmm_arvore_mercadologica( iv_slug ).

    lo_carga_arvore->upload_arquivo( lv_content ).
    lo_carga_arvore->criar_arvore( RECEIVING rt_return = DATA(lt_return) ).

    IF line_exists( lt_return[ type = zclmm_arvore_mercadologica=>if_xo_const_message~error ] ). "#EC CI_STDSEQ
      DATA(lo_message) = Me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
      lo_message->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message.
    ENDIF.

    CALL METHOD copy_data_to_ref
      EXPORTING
        is_data = ls_file
      CHANGING
        cr_data = er_entity.
  ENDMETHOD.
ENDCLASS.
