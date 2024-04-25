"! <p><strong>Classe log criação da árvore mercadológica</strong></p>
"! <p><strong>Autor:</strong>Enio Rafael de Jesus</p>
"! <p><strong>Data:</strong>14/12/2023</p>
CLASS zclmm_arvore_mercadologica_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      "! Filtros
      BEGIN OF ty_filters,
        guid TYPE if_rap_query_filter=>tt_range_option,
      END OF ty_filters,

      "! Tipo de tabela para as mensagens de erro/sucesso
      ty_t_messages TYPE STANDARD TABLE OF zc_mm_arvore_mercadoria_log WITH DEFAULT KEY.

    INTERFACES if_rap_query_filter .
    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Logica para buscar dados
    "! @parameter et_messages |Retorna mensagens
    METHODS
      build
        EXPORTING
          et_messages TYPE ty_t_messages.

    "! Configura os filtros que serão utilizados no relatório
    "! @parameter it_filters | Filtros do Aplicativo
    METHODS
      set_filters
        IMPORTING
          it_filters TYPE if_rap_query_filter=>tt_name_range_pairs.

    "! Obter criticidade da mensagem de erro
    "! iv_msgty | Tipo de mensagem
    "! rv_type | Tipo da mensagem
    METHODS
      get_criticality_type
        IMPORTING
          iv_msgty       TYPE sy-msgty
        RETURNING
          VALUE(rv_type) TYPE i.

    DATA gs_filter TYPE ty_filters.
ENDCLASS.



CLASS zclmm_arvore_mercadologica_log IMPLEMENTATION.

  METHOD if_rap_query_filter~get_as_ranges.
    RETURN.
  ENDMETHOD.

  METHOD if_rap_query_filter~get_as_sql_string.
    RETURN.
  ENDMETHOD.

  METHOD if_rap_query_filter~get_as_tree.
    RETURN.
  ENDMETHOD.

  METHOD if_rap_query_provider~select.
    TRY.
        CHECK io_request->is_data_requested( ).
      CATCH cx_rfc_dest_provider_error  INTO DATA(lo_ex_dest).
        DATA(lv_error_msg) = lo_ex_dest->get_longtext( ).
        RETURN.
    ENDTRY.

    DATA(lv_top)      = io_request->get_paging( )->get_page_size( ).
    DATA(lv_skip)     = io_request->get_paging( )->get_offset( ).
    DATA(lv_max_rows) = COND #( WHEN lv_top = if_rap_query_paging=>page_size_unlimited THEN 0 ELSE lv_top ).

    "@ Recupera e seta filtros de seleção
    TRY.
        me->set_filters( EXPORTING it_filters = io_request->get_filter( )->get_as_ranges( ) )."#EC CI_CONV_OK
      CATCH cx_rap_query_filter_no_range INTO DATA(lo_ex_filter).
        lv_error_msg = lo_ex_filter->get_longtext( ).
    ENDTRY.

    me->build(
      IMPORTING
        et_messages = DATA(lt_result)
    ).

    "@ Controla paginação (Adiciona registros de 20 em 20 )
    DATA(lt_result_page) = lt_result[].
    lt_result_page = VALUE #( FOR ls_result IN lt_result FROM ( lv_skip + 1 ) TO ( lv_skip + lv_max_rows ) ( ls_result ) ).

    "@ Exibe registros
    io_response->set_total_number_of_records( CONV #( lines( lt_result[] ) ) ).
    io_response->set_data( lt_result_page[] ).
  ENDMETHOD.

  METHOD build.
    DATA lt_messages TYPE TABLE OF balm.
    DATA lt_messages_header TYPE TABLE OF balhdr.

    SELECT Uuid
      FROM zi_mm_arvore_merc
INTO TABLE @DATA(lt_log_header)
     WHERE Uuid IN @gs_filter-guid
     ORDER BY Uuid.

    LOOP AT lt_log_header ASSIGNING FIELD-SYMBOL(<fs_log_header>).
      DATA(lv_external_number) = <fs_log_header>-uuid.

      CALL FUNCTION 'APPL_LOG_READ_DB'
        EXPORTING
          object          = zclmm_arvore_mercadologica=>gc_log_object
          subobject       = zclmm_arvore_mercadologica=>gc_log_subobject
          external_number = lv_external_number
        TABLES
          messages        = lt_messages
          header_data     = lt_messages_header.

      SORT lt_messages BY lognumber DESCENDING msgnumber ASCENDING."#EC CI_SORTLOOP
      LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<fs_msg>)."#EC CI_NESTED
        DATA(ls_message_header) = lt_messages_header[ lognumber = <FS_msg>-lognumber ]."#EC CI_STDSEQ

        MESSAGE ID <fs_msg>-msgid TYPE <fs_msg>-msgty NUMBER <fs_msg>-msgno WITH <fs_msg>-msgv1 <fs_msg>-msgv2 <fs_msg>-msgv3 <fs_msg>-msgv4
        INTO DATA(lv_message).

        et_messages = VALUE #( BASE et_messages
            ( Guid      = <fs_log_header>-Uuid
              MessageId  = <fs_msg>-lognumber && <fs_msg>-msgnumber
              mensagem   = lv_message
              Tipo       = <fs_msg>-msgty
              StatusCriticality = me->get_criticality_type( <fs_msg>-msgty )
              Criado_em  = <fs_msg>-time_stmp
              Criado_por = ls_message_header-aluser
            )
        ).

      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

  METHOD set_filters.
    TRY.
        gs_filter-guid = it_filters[ name = 'GUID' ]-range."#EC CI_STDSEQ
      CATCH cx_root INTO DATA(lo_root).
        DATA(lv_error_text) = lo_root->get_longtext( ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_criticality_type.
    CASE iv_msgty.
      WHEN zclmm_arvore_mercadologica=>if_xo_const_message~error.
        rv_type = 1.
      WHEN zclmm_arvore_mercadologica=>if_xo_const_message~success.
        rv_type = 3.
      WHEN zclmm_arvore_mercadologica=>if_xo_const_message~warning.
        rv_type = 2.
      WHEN OTHERS.
        rv_type = 0.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
