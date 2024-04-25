"! <p><strong>Classe para integração com CPI p/ envio de documentos de recebimento</strong></p>
"! <p><strong>Autor:</strong>Enio Rafael de Jesus</p>
"! <p><strong>Data:</strong>20/11/2023</p>
CLASS zclmm_wms_integrar_recebimento DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      "! Estrutura CPI - Payload header
      BEGIN OF ty_payload_header,
        ztipo_doc                  TYPE string,
        accesskey                  TYPE string,
        vbeln                      TYPE string,
        erdat                      TYPE string,
        erzet                      TYPE string,
        deliverydocumentbysupplier TYPE string,
        lgort                      TYPE string,
        shippingpoint              TYPE string,
        supplier                   TYPE string,
        receivingplant             TYPE string,
        shiptoparty                TYPE string,
      END OF ty_payload_header,

      "! Estrutura CPI - Payload items
      BEGIN OF ty_payload_item,
        posnr                      TYPE string,
        werks                      TYPE string,
        lgort                      TYPE string,
        material                   TYPE string,
        deliverydocumentitemtext   TYPE string,
        materialgroup              TYPE string,
        menge                      TYPE string,
        meins                      TYPE string,
        referencesdocumentcategory TYPE string,
        referencesdocument         TYPE string,
        referencesdocumentitem     TYPE string,
      END OF ty_payload_item,

      ty_t_payload_item TYPE TABLE OF ty_payload_item WITH DEFAULT KEY,

      "! Estrutura CPI - Payload principal
      BEGIN OF ty_payload,
        header TYPE ty_payload_header,
        item   TYPE ty_t_payload_item,
      END OF ty_payload,

      ty_t_payload         TYPE TABLE OF ty_payload WITH DEFAULT KEY,
      ty_t_doc_recebimento TYPE TABLE OF zc_mm_wms_integrar_recebimento WITH DEFAULT KEY.

    CONSTANTS:
      "! Status de integração
      BEGIN OF gc_Status_integracao,
        nao_enviado      TYPE ze_status_int  VALUE '00', "Enviar documento
        enviado          TYPE ze_status_int  VALUE '01', "Documento enviado
        retorno          TYPE ze_status_int  VALUE '02', "Retorno integração
        finalizado       TYPE ze_status_int  VALUE '03', "Processo Finalizado
        erro_processo    TYPE ze_status_int  VALUE '04', "Erro na execução do processo
        erro_integracao  TYPE ze_status_int  VALUE '05', "Erro integração
        retorno_eliminar TYPE ze_status_int  VALUE '06', "Retorno eliminar
        nao_enviar       TYPE ze_status_int  VALUE '07', "Não Enviar
      END OF gc_status_integracao,

      "! Tipo de documento
      gc_documento_entrada TYPE ze_tipo_doc VALUE 'E',

      "! Constantes para integração CPI
      BEGIN OF gc_cpi,
        processo    TYPE ze_processo   VALUE 'ZMM_ENVIO_RECEBIMENTO_WMS' ##NO_TEXT,
        method_post TYPE ze_method_api VALUE 'POST' ##NO_TEXT,
        erro        TYPE char20        VALUE 'Erro (E)' ##NO_TEXT,
        successo    TYPE char20        VALUE 'Sucesso (S)' ##NO_TEXT,
      END OF gc_cpi.

    "! Executar integração
    "! ir_documento | Lista de documentos
    "! ir_datadocumento | Data de seleção
    "! ir_tipodocumento | Tipos de documento
    "! ir_centro        | Centros
    "! iv_test          | (X) Modo teste ( ) Modo produtivo
    METHODS exec_integracao
      IMPORTING
        ir_documento     TYPE rsis_t_range
        ir_datadocumento TYPE rsis_t_range
        ir_tipodocumento TYPE rsis_t_range
        ir_statusintegr  TYPE rsis_t_Range
        ir_centro        TYPE rsis_t_range
        iv_test          TYPE char1
      RETURNING
        VALUE(rt_return) TYPE ty_t_doc_recebimento.

    "! Retornar parâmetros da tabela
    "! er_empresa | Parâmetros empresa
    "! er_centro | Parâmetros centro
    METHODS get_parameters
      EXPORTING
        er_empresa TYPE rsis_t_range
        er_centro  TYPE rsis_t_range.

    "! Retornar status
    "! rt_status | Status default
    METHODS get_default_status
      RETURNING
        VALUE(rt_status) TYPE rsis_t_range.

    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Leitura de filtros para aplicativo fiori
    "! it_filters | Lista de seleções
    METHODS get_filters
      IMPORTING
        it_filters TYPE if_rap_query_filter=>tt_name_range_pairs.

    "! Mapear cds entity para tabela de banco
    "! is_entity | Valores da entity (ZI_MM_Integracao_Doc_Remessa)
    METHODS map_entity_to_table
      IMPORTING
        is_entity TYPE any.

    DATA gr_documento       TYPE rsis_t_range.
    DATA gr_datadocumento   TYPE rsis_t_range.
    DATA gr_tipodocumento   TYPE rsis_t_range.
    DATA gr_statusintegr    TYPE rsis_t_Range.
    DATA gr_Centro          TYPE rsis_t_range.
    DATA gr_empresa         TYPE rsis_t_range.
    DATA gv_test_mode       TYPE abap_bool.
    DATA gt_docs_integracao TYPE TABLE OF ztmm_doc_rem_rec.

ENDCLASS.



CLASS zclmm_wms_integrar_recebimento IMPLEMENTATION.


  METHOD if_rap_query_provider~select.
    TRY.
        IF io_request->is_data_requested(  ).

          DATA(lv_offset) = io_request->get_paging( )->get_offset( ).
          DATA(lv_page_size) = io_request->get_paging( )->get_page_size( ).
          DATA(lv_max_rows) = COND #( WHEN lv_page_size = if_rap_query_paging=>page_size_unlimited THEN 0 ELSE lv_page_size ) .

          TRY.
              TRY.
                  DATA(lt_filters) = io_request->get_filter( )->get_as_ranges( ). "#EC CI_CONV_OK
                CATCH cx_rap_query_filter_no_range INTO DATA(lo_ex_filter).
                  DATA(lv_exp_msg) = lo_ex_filter->get_longtext( ).
              ENDTRY.

              IF NOT lt_filters IS INITIAL.

                me->get_filters( lt_filters ).
                me->get_default_status( ).
                me->get_parameters( ).

                CALL METHOD me->exec_integracao
                  EXPORTING
                    ir_documento     = me->gr_documento
                    ir_datadocumento = me->gr_datadocumento
                    ir_tipodocumento = me->gr_tipodocumento
                    ir_statusintegr  = me->gr_statusintegr
                    ir_centro        = me->gr_centro
                    iv_test          = me->gv_test_mode
                  RECEIVING
                    rt_return        = DATA(lt_return).

              ENDIF.

              io_response->set_total_number_of_records( lines( lt_return ) ).
              io_response->set_data( lt_return ).

            CATCH cx_rap_query_filter_no_range INTO DATA(lv_range).
              DATA(lv_msg) = lv_range->get_text( ).
          ENDTRY.

        ENDIF.
      CATCH cx_rap_query_provider.
    ENDTRY.
  ENDMETHOD.


  METHOD get_filters.
    LOOP AT it_filters ASSIGNING FIELD-SYMBOL(<fs_filters>).

      CASE <fs_filters>-name.
        WHEN 'DOCUMENTO'.
          gr_documento     = CORRESPONDING #( <fs_filters>-range ).
        WHEN 'DATADOCUMENTO'.
          gr_datadocumento = CORRESPONDING #( <fs_filters>-range ).
        WHEN 'TIPODOCUMENTO'.
          gr_tipodocumento = CORRESPONDING #( <fs_filters>-range ).
        WHEN 'CENTRO'.
          gr_centro        = CORRESPONDING #( <fs_filters>-range ).
        WHEN 'MODOTESTE'.
          gv_test_mode     = VALUE #( <fs_filters>-range[ 1 ]-low OPTIONAL ).
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.


  METHOD map_entity_to_table.
    DATA(ls_entity) = CORRESPONDING ZI_MM_Integracao_Doc_Remessa( is_entity ).

    UPDATE ztmm_doc_rem_rec
     SET Zstatus_Integracao = ls_entity-ZstatusIntegracao
         Id                 = ls_entity-Id
         Znumber            = ls_entity-Znumber
         Type               = ls_entity-Type
         Message            = ls_entity-Message
     WHERE
         vbeln              = ls_entity-Vbeln.

    COMMIT WORK.

*
*    ls_table-vbeln              = ls_entity-Vbeln.
*    ls_table-posnr              = ls_entity-posnr.
*    ls_table-werks              = ls_entity-werks.
*    ls_table-lgort              = ls_entity-lgort.
*    ls_table-ztipo_doc          = ls_entity-ztipodoc.
*    ls_table-zstatus_integracao = ls_entity-ZstatusIntegracao.
*    ls_table-menge              = ls_entity-menge.
*    ls_table-zqtrec             = ls_entity-zqtrec.
*    ls_table-zqtpen             = ls_entity-zqtpen.
*    ls_table-meins              = ls_entity-meins.
*    ls_table-cancel             = ls_entity-cancel.
*    ls_table-mblnr              = ls_entity-mblnr.
*    ls_table-mjahr              = ls_entity-mjahr.
*    ls_table-move_stloc         = ls_entity-movestloc.
*    ls_table-ernam              = ls_entity-ernam.
*    ls_table-erdat              = ls_entity-erdat.
*    ls_table-erzet              = ls_entity-erzet.
*    ls_table-aenam              = ls_entity-aenam.
*    ls_table-aedat              = ls_entity-aedat.
*    ls_table-aezet              = ls_entity-aezet.
*    ls_table-type               = ls_entity-type.
*    ls_table-id                 = ls_entity-id.
*    ls_table-znumber            = ls_entity-znumber.
*    ls_table-message            = ls_entity-message.
*
*    APPEND ls_table TO me->gt_docs_integracao.
*    CLEAR ls_table.

  ENDMETHOD.

  METHOD get_parameters.
    CONSTANTS:
      gc_modulo        TYPE ztca_param_mod-modulo VALUE 'MM',
      gc_chave1        TYPE ztca_param_par-chave1 VALUE 'INTEGRACAO_WMS',
      gc_chave2        TYPE ztca_param_par-chave1 VALUE 'ENVIO_RECEBIMENTO_DOCUMENTOS',
      gc_chave_empresa TYPE ztca_param_par-chave1 VALUE 'EMPRESA',
      gc_chave_centro  TYPE ztca_param_par-chave1 VALUE 'CENTRO'.

    DATA(lo_param) = NEW zclca_tabela_parametros( ).

    TRY.
        lo_param->m_get_range(
          EXPORTING
            iv_modulo = gc_modulo
            iv_chave1 = gc_chave1
            iv_chave2 = gc_chave2
            iv_chave3 = gc_chave_empresa
          IMPORTING
            et_range  = me->gr_empresa
        ).

        lo_param->m_get_range(
          EXPORTING
            iv_modulo = gc_modulo
            iv_chave1 = gc_chave1
            iv_chave2 = gc_chave2
            iv_chave3 = gc_chave_centro
          IMPORTING
            et_range  = me->gr_centro
        ).
      CATCH zcxca_tabela_parametros.
    ENDTRY.

    er_centro  = me->gr_centro.
    er_empresa = me->gr_empresa.
  ENDMETHOD.

  METHOD get_default_status.
    rt_status = VALUE #(
      ( sign = 'I' option = 'EQ' low = zclmm_wms_integrar_recebimento=>gc_status_integracao-nao_enviado     )
      ( sign = 'I' option = 'EQ' low = zclmm_wms_integrar_recebimento=>gc_status_integracao-erro_integracao )
    ).

    me->gr_statusintegr = rt_status.
  ENDMETHOD.

  METHOD exec_integracao.
    DATA ls_payload TYPE ty_payload.

    SELECT
        Vbeln,
        posnr,
        werks,
        lgort,
        ztipodoc,
        ZstatusIntegracao,
        menge,
        zqtrec,
        zqtpen,
        meins,
        cancel,
        mblnr,
        mjahr,
        movestloc,
        ernam,
        erdat,
        erzet,
        aenam,
        aedat,
        aezet,
        type,
        id,
        znumber,
        message,
        accesskey,
        deliverydocumentbysupplier,
        shippingpoint,
        supplier,
        receivingplant,
        shiptoparty,
        material,
        deliverydocumentitemtext,
        materialgroup,
        Vgtyp,
        Vgbel,
        Vgpos
      FROM zi_mm_integracao_doc_remessa
      INTO TABLE @DATA(lt_doc_remessa)
     WHERE vbeln             IN @ir_documento
       AND erdat             IN @ir_datadocumento
       AND ztipodoc          IN @ir_tipodocumento
       AND Werks             IN @gr_centro
       AND Cancel            EQ @abap_False
       AND ZstatusIntegracao IN @ir_statusintegr.

    IF lt_doc_remessa IS INITIAL.
      rt_return = VALUE #( (
        documento = '-'
        tipo      = gc_cpi-erro
        mensagem  = TEXT-002
      ) ).
      RETURN.
    ENDIF.

    DATA(lo_cpi) = NEW zclca_cpi( ).

    SORT lt_doc_remessa BY vbeln.
    LOOP AT lt_doc_remessa ASSIGNING FIELD-SYMBOL(<fs_doc_remessa>)
     GROUP BY ( vbeln = <fs_doc_remessa>-vbeln )
     ASSIGNING FIELD-SYMBOL(<fs_group_documents>).

      LOOP AT GROUP <fs_group_documents> ASSIGNING FIELD-SYMBOL(<fs_group_document>).
        ls_payload-header-ztipo_doc      = <fs_group_document>-ZtipoDoc.
        ls_payload-header-accesskey      = <fs_group_document>-accesskey.
        ls_payload-header-vbeln          = <fs_group_document>-vbeln.
        ls_payload-header-erdat          = <fs_group_document>-Erdat.
        ls_payload-header-erzet          = <fs_group_document>-Erzet.
        ls_payload-header-deliverydocumentbysupplier = <fs_group_document>-DeliveryDocumentBySupplier.
        ls_payload-header-lgort          = <fs_group_document>-Lgort.
        ls_payload-header-shippingpoint  = <fs_group_document>-ShippingPoint.
        ls_payload-header-supplier       = <fs_group_document>-Supplier.
        ls_payload-header-receivingplant = <fs_group_document>-ReceivingPlant.
        ls_payload-header-shiptoparty    = <fs_group_document>-ShipToParty.

        APPEND VALUE #(
          posnr                      = <fs_group_document>-Posnr
          werks                      = <fs_group_document>-Werks
          lgort                      = <fs_group_document>-lgort
          material                   = <fs_group_document>-material
          deliverydocumentitemtext   = <fs_group_document>-DeliveryDocumentItemText
          materialgroup              = <fs_group_document>-materialgroup
          menge                      = <fs_group_document>-menge
          meins                      = <fs_group_document>-meins
          referencesdocumentcategory = <fs_group_document>-Vgtyp
          referencesdocument         = <fs_group_document>-vgbel
          referencesdocumentitem     = <fs_group_document>-vgpos
        ) TO ls_payload-item.

      ENDLOOP.

      DATA(lo_cpi_monitor) = NEW zclca_monitor_cpi( ).
      DATA(lv_payload_str) = lo_cpi->conv_data_to_json( iv_data = ls_payload ).

      IF ( iv_test = abap_false ).

        CALL METHOD lo_cpi->send
          EXPORTING
            iv_processo  = gc_cpi-processo
            iv_metodo    = gc_cpi-method_post
            is_structure = ls_payload
          IMPORTING
            ev_result    = DATA(lv_result)
            et_return    = DATA(lt_return).

        IF ( lt_return IS NOT INITIAL ).
          <fs_group_document>-ZstatusIntegracao = gc_status_integracao-erro_integracao.
          <fs_group_document>-Id                = '00'.
          <fs_group_document>-Znumber           = '000'.
          <fs_group_document>-Type              = gc_cpi-erro.
          <fs_group_document>-Message           = lv_result.

          APPEND VALUE #(
            documento     = ls_payload-header-vbeln
            datadocumento = <fs_group_document>-Erdat
            tipodocumento = <fs_group_document>-ZtipoDoc
            centro        = <fs_group_document>-Werks
            tipo          = gc_cpi-erro
            mensagem      = lv_result
          ) TO rt_return.
        ELSE.
          <fs_group_document>-ZstatusIntegracao = gc_status_integracao-enviado.
          <fs_group_document>-Id                = '00'.
          <fs_group_document>-Znumber           = '000'.
          <fs_group_document>-Type              = gc_cpi-successo.
          <fs_group_document>-Message           = lv_result.

          APPEND VALUE #(
            documento     = <fs_group_document>-Vbeln
            datadocumento = <fs_group_document>-Erdat
            tipodocumento = <fs_group_document>-ZtipoDoc
            centro        = <fs_group_document>-Werks
            tipo          = gc_cpi-successo
            mensagem      = TEXT-003
          ) TO rt_return.
        ENDIF.

        me->map_entity_to_table( <fs_group_document> ).

        lo_cpi_monitor->started_process(
          EXPORTING
            iv_processo  = gc_cpi-processo
            iv_metodo    = gc_cpi-method_post
            iv_chave_ref = CONV #( <fs_group_documents>-vbeln )
            iv_json      = lv_payload_str
        ).

        lo_cpi_monitor->save_log(
          EXPORTING
            iv_processo     = gc_cpi-processo
            iv_metodo       = gc_cpi-method_post
            iv_json_retorno = lv_result
            iv_json         = lv_payload_str
            it_return       = lt_return
        ).

      ELSE.
        APPEND VALUE #(
          documento     = <fs_group_document>-Vbeln
          datadocumento = <fs_group_document>-Erdat
          tipodocumento = <fs_group_document>-ZtipoDoc
          centro        = <fs_group_document>-Werks
          tipo          = gc_cpi-successo
          mensagem      = TEXT-001
        ) TO rt_return.
      ENDIF.

      CLEAR ls_payload.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
