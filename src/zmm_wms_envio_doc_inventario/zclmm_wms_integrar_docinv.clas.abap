"! <p><strong>Classe para integração com CPI p/ envio de documentos de inventário</strong></p>
"! <p><strong>Autor:</strong>Enio Rafael de Jesus</p>
"! <p><strong>Data:</strong>01/12/2023</p>
CLASS zclmm_wms_integrar_docinv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_xo_const_message.

    TYPES:
      "! Estrutura CPI - Payload header
      BEGIN OF ty_payload_header,
        ztipo_doc TYPE string,
        iblnr     TYPE string,
        gjahr     TYPE string,
        werks     TYPE string,
        lgort     TYPE string,
        bldat     TYPE string,
      END OF ty_payload_header,

      "! Estrutura CPI - Payload items
      BEGIN OF ty_payload_item,
        zeile TYPE string,
        matnr TYPE string,
        menge TYPE string,
        meins TYPE string,
        xnul  TYPE abap_bool,
      END OF ty_payload_item,

      ty_t_payload_item TYPE TABLE OF ty_payload_item WITH DEFAULT KEY,

      "! Estrutura CPI - Payload principal
      BEGIN OF ty_payload,
        header TYPE ty_payload_header,
        item   TYPE ty_t_payload_item,
      END OF ty_payload,

      "! Estrutura de erro retornados do CPI
      BEGIN OF ty_ref_error,
        InventoryDocument TYPE zi_mm_wms_doc_inventario-PhysicalInventoryDocument,
        type              TYPE bapiret2-type,
        error             TYPE ztmm_doc_mat-zmessage,
      END OF ty_ref_error,

      "! Tipo de tabela para payload da integração c/ CPI
      ty_t_payload        TYPE TABLE OF ty_payload WITH DEFAULT KEY,
      "! Tipo de tabela para cds view de documentos de inventário
      ty_t_doc_inventario TYPE TABLE OF zi_mm_wms_doc_inventario WITH DEFAULT KEY.

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

      "! Tipo de documento de inventário
      gc_documento_inventario TYPE ze_tipo_doc VALUE 'I' ##NO_TEXT,
      "! Tipo de mensagem de inventário
      gc_message_id           TYPE t100-arbgb  VALUE 'ZMM' ##NO_TEXT,
      "! Origem dos dados
      gc_origem_sap           TYPE ze_origem   VALUE 'S' ##NO_TEXT,

      "! Constantes para integração CPI
      BEGIN OF gc_cpi,
        processo    TYPE ze_processo   VALUE 'ZMM_ENVIO_DOCINVENTARIO_WMS' ##NO_TEXT,
        method_post TYPE ze_method_api VALUE 'POST' ##NO_TEXT,
      END OF gc_cpi.

    "! Executar integração
    "! it_documentos | Lista de documentos
    "! rt_return | Retorno com sucesso/erro da integração com CPI
    METHODS exec_integracao
      IMPORTING
        it_documentos    TYPE ty_t_doc_inventario
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t.

    "! Obter retorno da integração CPI
    "! p_task | Nome da task RFC
    METHODS finish_integracao
      IMPORTING
        p_task TYPE any.

  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Mapear cds entity para tabela de banco
    "! is_entity | Valores da entity (zi_mm_wms_doc_inventario)
    METHODS map_entity_to_table
      IMPORTING
        is_entity TYPE zi_mm_wms_doc_inventario.

    "! Persistencia com banco de dados
    METHODS persist.

    DATA gt_return_task  TYPE bapiret2_t.
    DATA gt_ref_error    TYPE TABLE OF ty_ref_error.
    DATA gt_ztmm_doc_mat TYPE TABLE OF ztmm_doc_mat.

ENDCLASS.



CLASS zclmm_wms_integrar_docinv IMPLEMENTATION.
  METHOD exec_integracao.
    DATA ls_payload TYPE ty_payload.
    DATA lt_return  TYPE bapiret2_t.

    DATA(lo_cpi) = NEW zclca_cpi( ).

    LOOP AT it_documentos ASSIGNING FIELD-SYMBOL(<fs_result>) GROUP BY (
      PhysicalInventoryDocument = <fs_result>-PhysicalInventoryDocument
    ) ASSIGNING FIELD-SYMBOL(<fs_documents_group>).

      LOOP AT GROUP <fs_documents_group> ASSIGNING FIELD-SYMBOL(<fs_document_group>).
        me->map_entity_to_table( <fs_document_group> ).

        ls_payload-header-ztipo_doc = zclmm_wms_integrar_docinv=>gc_documento_inventario.
        ls_payload-header-iblnr     = <fs_document_group>-PhysicalInventoryDocument.
        ls_payload-header-gjahr     = <fs_document_group>-FiscalYear.
        ls_payload-header-werks     = <fs_document_group>-Plant.
        ls_payload-header-lgort     = <fs_document_group>-StorageLocation.
        ls_payload-header-bldat     = <fs_document_group>-DocumentDate.

        APPEND VALUE #(
          zeile = <fs_document_group>-PhysicalInventoryDocumentItem
          matnr = <fs_document_group>-Material
          menge = 0
          meins = <fs_document_group>-MaterialUnitMeasure
          xnul  = <fs_document_group>-PhysicalInventoryItemIsZero
        ) TO ls_payload-item.

      ENDLOOP.

      DATA(lv_payload_str) = /ui2/cl_json=>serialize( ls_payload ).

      CALL FUNCTION 'ZFMMM_WMS_INTEGRAR_DOCINV'
        STARTING NEW TASK <fs_document_group>-PhysicalInventoryDocument
        CALLING me->finish_integracao ON END OF TASK
        EXPORTING
          iv_payload = lv_payload_str.

      WAIT UNTIL lines( me->gt_return_task ) > 0.

      APPEND LINES OF me->gt_return_task TO rt_return.
      CLEAR: ls_payload, me->gt_return_task.
    ENDLOOP.

    me->persist( ).

  ENDMETHOD.

  METHOD map_entity_to_table.

    APPEND VALUE #(
      mblnr               = is_entity-PhysicalInventoryDocument
      mjahr               = is_entity-FiscalYear
      zeile               = is_entity-PhysicalInventoryDocumentItem
      ztipo_doc           = gc_documento_inventario
      zstatus_integracao  = gc_status_integracao-enviado
      zorigem             = gc_origem_sap
      matnr               = is_entity-Material
      bwart               = is_entity-GoodsMovementType
      shkzg               = is_entity-DebitCreditCode
      werks               = is_entity-Plant
      lgort               = is_entity-StorageLocation
      erfmg               = is_entity-Quantity
      erfme               = is_entity-MaterialUnitMeasure
      zqtenv              = is_entity-Quantity
      meins               = is_entity-MaterialUnitMeasure
      xnull               = is_entity-PhysicalInventoryItemIsZero
      ernam               = sy-uname
      erdat               = sy-datum
      erzet               = sy-uzeit
    ) TO me->gt_ztmm_doc_mat.

  ENDMETHOD.

  METHOD finish_integracao.
    DATA lv_error_cpi TYPE ty_ref_error-error.

    RECEIVE RESULTS FROM FUNCTION 'ZFMMM_WMS_INTEGRAR_DOCINV'
      IMPORTING
        et_return = me->gt_return_task
        ev_return = lv_error_cpi.

    IF line_exists( me->gt_return_task[ type = if_xo_const_message~error ] ). "#EC CI_STDSEQ
      APPEND VALUE #( inventorydocument = p_task type = if_xo_const_message~error error = lv_error_cpi ) TO me->gt_ref_error.
    ENDIF.
  ENDMETHOD.

  METHOD persist.
    SORT me->gt_ref_error    BY inventorydocument.
    SORT me->gt_ztmm_doc_mat BY mblnr.

    LOOP AT me->gt_ztmm_doc_mat ASSIGNING FIELD-SYMBOL(<Fs_docmat>).
      READ TABLE me->gt_ref_error ASSIGNING FIELD-SYMBOL(<fs_ref_error>)
        WITH KEY inventorydocument = <fs_docmat>-mblnr
        BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        <fs_docmat>-zstatus_integracao = gc_status_integracao-erro_integracao.
        <fs_docmat>-zmessage           = <fs_ref_error>-error.
        <fs_docmat>-type               = <fs_ref_error>-type.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'ZFMMM_WMS_ATUALIZA_TABXX1'
      STARTING NEW TASK 'UPDATE_DOCMAT'
      EXPORTING
        it_ztbmmxxx2 = me->gt_ztmm_doc_mat.

  ENDMETHOD.

ENDCLASS.
