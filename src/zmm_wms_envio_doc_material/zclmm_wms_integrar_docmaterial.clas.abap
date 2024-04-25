"! <p><strong>Classe para integração com CPI p/ envio de documentos de materiais</strong></p>
"! <p><strong>Autor:</strong>Enio Rafael de Jesus</p>
"! <p><strong>Data:</strong>24/11/2023</p>
CLASS zclmm_wms_integrar_docmaterial DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      "! Estrutura CPI - Payload header
      BEGIN OF ty_payload_header,
        id        TYPE string,
        chvnfe    TYPE string,
        vbeln     TYPE string,
        erdat     TYPE string,
        erzet     TYPE string,
        verur     TYPE string,
        vstel     TYPE string,
        traty     TYPE string,
        traid     TYPE string,
        lifnr     TYPE string,
        name_org1 TYPE string,
      END OF ty_payload_header,

      "! Estrutura CPI - Payload items
      BEGIN OF ty_payload_item,
        posnr  TYPE string,
        erdat  TYPE string,
        erzet  TYPE string,
        werks  TYPE string,
        matnr  TYPE string,
        lgort  TYPE string,
        zqtenv TYPE string,
        meins  TYPE string,
        arktxt TYPE string,
        vgbel  TYPE string,
      END OF ty_payload_item,

      ty_t_payload_item TYPE TABLE OF ty_payload_item WITH DEFAULT KEY,

      "! Estrutura CPI - Payload principal
      BEGIN OF ty_payload,
        header TYPE ty_payload_header,
        item   TYPE ty_t_payload_item,
      END OF ty_payload,

      "! Tipo tabela payload principal
      ty_t_payload      TYPE TABLE OF ty_payload WITH DEFAULT KEY,
      "! Tipo tabela documentos de materiais
      ty_t_doc_material TYPE TABLE OF zc_mm_wms_integrar_doc_mat WITH DEFAULT KEY.

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
        processo    TYPE ze_processo   VALUE 'ZMM_ENVIO_DOCMATERIAL_WMS' ##NO_TEXT,
        method_post TYPE ze_method_api VALUE 'POST' ##NO_TEXT,
        erro        TYPE char20        VALUE 'Erro (E)' ##NO_TEXT,
        successo    TYPE char20        VALUE 'Sucesso (S)' ##NO_TEXT,
      END OF gc_cpi.

    "! Executar integração
    "! ir_documento | Lista de documentos
    "! ir_datadocumento | Data de seleção
    "! ir_tipodocumento | Tipos de documento
    "! ir_centro        | Centros
    "! ir_empresa       | Empresas
    "! iv_test          | (X) Modo teste ( ) Modo produtivo
    METHODS exec_integracao
      IMPORTING
        ir_documento     TYPE rsis_t_range
        ir_datadocumento TYPE rsis_t_range
        ir_tipodocumento TYPE rsis_t_range
        ir_statusintegr  TYPE rsis_t_Range
        ir_centro        TYPE rsis_t_range
        ir_empresa       TYPE rsis_t_range
        iv_test          TYPE char1
      RETURNING
        VALUE(rt_return) TYPE ty_t_doc_material.

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
    DATA gr_statusintegr    TYPE rsis_t_range.
    DATA gr_tipodocumento   TYPE rsis_t_range.
    DATA gr_Centro          TYPE rsis_t_range.
    DATA gr_empresa         TYPE rsis_t_range.
    DATA gv_test_mode       TYPE abap_bool.
    DATA gt_docs_integracao TYPE TABLE OF ztmm_doc_mat.

ENDCLASS.



CLASS zclmm_wms_integrar_docmaterial IMPLEMENTATION.


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
                me->get_parameters( ).

                CALL METHOD me->exec_integracao
                  EXPORTING
                    ir_documento     = me->gr_documento
                    ir_datadocumento = me->gr_datadocumento
                    ir_tipodocumento = me->gr_tipodocumento "VALUE #( ( sign = 'I' option = 'EQ' low = gc_documento_entrada ) )
                    ir_statusintegr  = me->gr_statusintegr
                    ir_centro        = me->gr_centro
                    ir_empresa       = me->gr_empresa
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
*        WHEN 'STATUSINTEGRACAO'.
*          gr_statusintegr  = CORRESPONDING #( <fs_filters>-range ).
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
    DATA ls_table TYPE ztmm_doc_mat.
    DATA(ls_entity) = CORRESPONDING zi_mm_integracao_docs_mat( is_entity ).

    UPDATE ztmm_doc_mat
     SET Zstatus_Integracao = ls_entity-ZstatusIntegracao
         ZId                = ls_entity-ZId
         Znumber            = ls_entity-Znumber
         Type               = ls_entity-Type
         zMessage           = ls_entity-zMessage
     WHERE
         Mblnr              = ls_entity-Mblnr AND
         Mjahr              = ls_entity-Mjahr.

    COMMIT WORK.


*    ls_table-Mblnr              = ls_entity-Mblnr            .
*    ls_table-Mjahr              = ls_entity-Mjahr            .
*    ls_table-Zeile              = ls_entity-Zeile            .
*    ls_table-ztipo_doc          = ls_entity-ZtipoDoc         .
*    ls_table-zstatus_integracao = ls_entity-ZstatusIntegracao.
*    ls_table-Zorigem            = ls_entity-Zorigem          .
*    ls_table-Matnr              = ls_entity-Matnr            .
*    ls_table-Bwart              = ls_entity-Bwart            .
*    ls_table-Shkzg              = ls_entity-Shkzg            .
*    ls_table-Vbeln_Im           = ls_entity-VbelnIm          .
*    ls_table-Vbelp_Im           = ls_entity-VbelpIm          .
*    ls_table-Werks              = ls_entity-Werks            .
*    ls_table-Lgort              = ls_entity-Lgort            .
*    ls_table-Umwrk              = ls_entity-Umwrk            .
*    ls_table-Umlgo              = ls_entity-Umlgo            .
*    ls_table-Erfmg              = ls_entity-Erfmg            .
*    ls_table-Erfme              = ls_entity-Erfme            .
*    ls_table-Zqtenv             = ls_entity-Zqtenv           .
*    ls_table-Meins              = ls_entity-Meins            .
*    ls_table-Smbln              = ls_entity-Smbln            .
*    ls_table-Sjahr              = ls_entity-Sjahr            .
*    ls_table-Smblp              = ls_entity-Smblp            .
*    ls_table-Xnull              = ls_entity-Xnull            .
*    ls_table-Ernam              = ls_entity-Ernam            .
*    ls_table-Erdat              = ls_entity-Erdat            .
*    ls_table-Erzet              = ls_entity-Erzet            .
*    ls_table-Aenam              = ls_entity-Aenam            .
*    ls_table-Aedat              = ls_entity-Aedat            .
*    ls_table-Aezet              = ls_entity-Aezet            .
*    ls_table-Type               = ls_entity-Type             .
*    ls_table-Zid                = ls_entity-Zid              .
*    ls_table-Znumber            = ls_entity-Znumber          .
*    ls_table-Zmessage           = ls_entity-Zmessage         .
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
        ( sign = 'I' option = 'EQ' low = zclmm_wms_integrar_docmaterial=>gc_status_integracao-nao_enviado     )
        ( sign = 'I' option = 'EQ' low = zclmm_wms_integrar_docmaterial=>gc_status_integracao-erro_integracao )
      ).

    me->gr_statusintegr = rt_status.
  ENDMETHOD.

  METHOD exec_integracao.
    DATA ls_payload TYPE ty_payload.

    SELECT
        Mblnr,
        Mjahr,
        Zeile,
        ZtipoDoc,
        ZstatusIntegracao,
        Zorigem,
        Matnr,
        Bwart,
        Shkzg,
        VbelnIm,
        VbelpIm,
        Werks,
        Lgort,
        Umwrk,
        Umlgo,
        Erfmg,
        Erfme,
        Zqtenv,
        Meins,
        Smbln,
        Sjahr,
        Smblp,
        Xnull,
        Ernam,
        Erdat,
        Erzet,
        Aenam,
        Aedat,
        Aezet,
        Type,
        Zid,
        Znumber,
        Zmessage,
        ShippingPoint,
        Supplier,
        SupplierName,
        MeansOfTransportType,
        MeansOfTransport,
        StorageLocation,
        ActualDeliveryQuantity,
        BaseUnit,
        DeliveryDocumentItemText,
        ReferenceSDDocument,
        MaterialName
    FROM zi_mm_integracao_docs_mat
  WHERE mblnr             IN @ir_documento
    AND erdat             IN @ir_datadocumento
    AND ztipodoc          IN @ir_tipodocumento
    AND Werks             IN @gr_centro
    AND CompanyCode       IN @gr_empresa
    AND ZstatusIntegracao IN @ir_statusintegr
    AND Zqtenv            IS NOT INITIAL
   INTO TABLE @DATA(lt_doc_material).

    IF lt_doc_material IS INITIAL.
      rt_return = VALUE #( (
        documento = '-'
        tipo      = gc_cpi-erro
        mensagem  = TEXT-002
      ) ).
      RETURN.
    ENDIF.

    DATA(lo_cpi) = NEW zclca_cpi( ).

    SORT lt_doc_material BY Mblnr Mjahr Zeile.
    LOOP AT lt_doc_material ASSIGNING FIELD-SYMBOL(<fs_doc_material>)
     GROUP BY ( Mblnr = <fs_doc_material>-mblnr Mjahr = <fs_doc_material>-Mjahr )
     ASSIGNING FIELD-SYMBOL(<fs_group_documents>).

      LOOP AT GROUP <fs_group_documents> ASSIGNING FIELD-SYMBOL(<fs_group_document>).
        ls_payload-header-id        = <fs_group_document>-ZtipoDoc. "<fs_group_document>-Mblnr && <fs_group_document>-Mjahr && <fs_group_document>-Zeile.
        ls_payload-header-chvnfe    = <fs_group_document>-Mblnr && <fs_group_document>-Mjahr.
        ls_payload-header-vbeln     = <fs_group_document>-VbelnIm.
        ls_payload-header-erdat     = <fs_group_document>-Erdat.
        ls_payload-header-erzet     = <fs_group_document>-Erzet.
        ls_payload-header-verur     = <fs_group_document>-VbelnIm.
        ls_payload-header-vstel     = <fs_group_document>-ShippingPoint.
        ls_payload-header-traty     = <fs_group_document>-MeansOfTransportType.
        ls_payload-header-traid     = <fs_group_document>-MeansOfTransport.
        ls_payload-header-lifnr     = <fs_group_document>-Supplier.
        ls_payload-header-name_org1 = <fs_group_document>-SupplierName.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = <fs_group_document>-Meins
          IMPORTING
            output         = <fs_group_document>-Meins
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.

        IF sy-subrc IS NOT INITIAL.
          <fs_group_document>-Meins = <fs_group_document>-Meins.
        ENDIF.

        APPEND VALUE #(
          posnr  = <fs_group_document>-Zeile
          erdat  = <fs_group_document>-Erdat
          erzet  = <fs_group_document>-Erzet
          werks  = <fs_group_document>-Werks
          matnr  = <fs_group_document>-Matnr
          lgort  = <fs_group_document>-Lgort
          zqtenv = |{ <fs_group_document>-Zqtenv NUMBER = USER }|
          meins  = <fs_group_document>-Meins
          arktxt = <fs_group_document>-MaterialName
          vgbel  = <fs_group_document>-ReferenceSDDocument
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
          <fs_group_document>-Zid               = '00'.
          <fs_group_document>-Znumber           = '000'.
          <fs_group_document>-Type              = gc_cpi-erro.
          <fs_group_document>-Zmessage          = lv_result.

          APPEND VALUE #(
            documento     = <fs_group_document>-Mblnr
            datadocumento = <fs_group_document>-Erdat
            tipodocumento = <fs_group_document>-ZtipoDoc
            centro        = <fs_group_document>-Werks
            tipo          = gc_cpi-erro
            mensagem      = lv_result
          ) TO rt_return.
        ELSE.
          <fs_group_document>-ZstatusIntegracao = gc_status_integracao-enviado.
          <fs_group_document>-Zid               = '00'.
          <fs_group_document>-Znumber           = '000'.
          <fs_group_document>-Type              = gc_cpi-successo.
          <fs_group_document>-Zmessage          = TEXT-003.

          APPEND VALUE #(
            documento     = <fs_group_document>-Mblnr
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
            iv_json      = lv_payload_str
            iv_chave_ref = <fs_group_documents>-mblnr && <fs_group_documents>-mjahr
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
          documento     = <fs_group_document>-Mblnr
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
