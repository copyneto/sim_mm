class ZCLMM_RETO_CON_INV definition
  public
  final
  create public .

PUBLIC SECTION.

  "! Estrutura dados entrada
  DATA gs_reto_cont_inv TYPE zsmm_reto_cont_inv .
  CONSTANTS gc_error TYPE char1 VALUE 'E' ##NO_TEXT.
  CONSTANTS gc_barramento TYPE char1 VALUE 'B' ##NO_TEXT.
  CONSTANTS gc_type_s TYPE bapi_mtype VALUE 'S' ##NO_TEXT.
  CONSTANTS gc_id_zmm TYPE symsgid VALUE 'ZMM' ##NO_TEXT.
  CONSTANTS gc_znumber_suc TYPE symsgno VALUE '001' ##NO_TEXT.
  CONSTANTS:
    "! Status de integração
    BEGIN OF gc_status_integracao,
      nao_enviado      TYPE ze_status_int  VALUE '00', "Enviar documento
      enviado          TYPE ze_status_int  VALUE '01', "Documento enviado
      retorno          TYPE ze_status_int  VALUE '02', "Retorno integração
      finalizado       TYPE ze_status_int  VALUE '03', "Processo Finalizado
      erro_processo    TYPE ze_status_int  VALUE '04', "Erro na execução do processo
      erro_integracao  TYPE ze_status_int  VALUE '05', "Erro integração
      retorno_eliminar TYPE ze_status_int  VALUE '06', "Retorno eliminar
      nao_enviar       TYPE ze_status_int  VALUE '07', "Não Enviar
    END OF gc_status_integracao .

  "! Carregar dados de execução
  "! is_reto_cont_inv | Dados entrada JSON
  METHODS constructor
    IMPORTING
      !is_reto_cont_inv TYPE zsmm_reto_cont_inv .

  "! Processar dados de integracão
  "! ct_return        | Dados de retorno da execução
  METHODS process
    RETURNING
      VALUE(rt_return) TYPE tab_bapiret1 .

PROTECTED SECTION.
PRIVATE SECTION.

  "! Converter unidade de medida
  "! iv_meins | Unidade entrada
  "! rv_meins | Unidade saída
  METHODS conv
    IMPORTING
      iv_meins        TYPE zsmm_doc_rem_rec_item-meins
    RETURNING
      VALUE(rv_meins) TYPE ztmm_doc_rem_rec-meins .

  "! Preencher dados BAPI
  "! is_reto_cont_inv | Dados entrada JSON
  "! ct_itens         | Itens
  METHODS preenche_dados_inv_count
    IMPORTING
      is_reto_cont_inv TYPE zsmm_reto_cont_inv
    CHANGING
      ct_itens         TYPE bapi_physinv_count_items_ty.

  "! Processar BAPI
  "! it_itens         | Itens
  "! is_reto_cont_inv | Dados entrada JSON
  "! ct_return        | Dados de retorno BAPI
  "! ct_mm_doc_mat    | Tabela de log de processos
  METHODS processa_dados_inv_count
    IMPORTING
      it_itens         TYPE bapi_physinv_count_items_ty
      is_reto_cont_inv TYPE zsmm_reto_cont_inv
    CHANGING
      ct_return        TYPE bapiret2_tab
      ct_mm_doc_mat    TYPE zctgmm_doc_mat.

ENDCLASS.



CLASS ZCLMM_RETO_CON_INV IMPLEMENTATION.


  METHOD constructor.

    gs_reto_cont_inv = is_reto_cont_inv.
    gs_reto_cont_inv-header-iblnr = |{ gs_reto_cont_inv-header-iblnr WIDTH = 10 ALIGN = RIGHT PAD = '0' }|.

  ENDMETHOD.


  METHOD CONV.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = iv_meins
        language       = sy-langu
      IMPORTING
        output         = rv_meins
      EXCEPTIONS
        unit_not_found = 01.

    IF sy-subrc NE 0.
      rv_meins = iv_meins.
    ENDIF.

  ENDMETHOD.


  METHOD process.
    DATA: lt_return     TYPE TABLE OF bapiret2,
          lt_itens      TYPE bapi_physinv_count_items_ty,
          lt_mm_doc_mat TYPE zctgmm_doc_mat.

    CLEAR:  lt_return[].

    preenche_dados_inv_count( EXPORTING is_reto_cont_inv = gs_reto_cont_inv
                              CHANGING  ct_itens         = lt_itens[] ).

    processa_dados_inv_count( EXPORTING it_itens         = lt_itens[]
                                        is_reto_cont_inv = gs_reto_cont_inv
                              CHANGING  ct_return        = lt_return
                                        ct_mm_doc_mat    = lt_mm_doc_mat ).

    IF  lt_mm_doc_mat[] IS NOT INITIAL.

      CALL FUNCTION 'ZFMMM_WMS_ATUALIZA_TABXX1'
        EXPORTING
          it_ztbmmxxx2 = lt_mm_doc_mat.

    ENDIF.

    rt_return[] = lt_return[].

  ENDMETHOD.

  METHOD preenche_dados_inv_count.

    ct_itens = VALUE #( FOR <fs_itens> IN is_reto_cont_inv-header-item
                           (  item            = <fs_itens>-zeile
                              entry_qnt       = <fs_itens>-menge
                              entry_uom       = conv( <fs_itens>-meins )
                              entry_uom_iso   = conv( <fs_itens>-meins )
                              zero_count      = <fs_itens>-xnull
                              material_long   = |{ <fs_itens>-matnr WIDTH = 18 ALIGN = RIGHT PAD = '0' }| ) ).

  ENDMETHOD.

  METHOD processa_dados_inv_count.
    DATA: lt_itens      TYPE bapi_physinv_count_items_ty.

    DATA: lv_status           TYPE ze_status_int.

    DATA: lv_message TYPE bapi_msg,
          lv_type    TYPE bapi_mtype,
          lv_id      TYPE symsgid,
          lv_znumber TYPE symsgno.

    lt_itens[] = it_itens[].

    CALL FUNCTION 'BAPI_MATPHYSINV_COUNT'
      EXPORTING
        physinventory = is_reto_cont_inv-header-iblnr
        fiscalyear    = is_reto_cont_inv-header-gjahr
        count_date    = is_reto_cont_inv-header-bldat
      TABLES
        items         = lt_itens[]
        return        = ct_return[].

    SORT ct_return BY type.

    READ TABLE ct_return
    ASSIGNING FIELD-SYMBOL(<fs_return>)
    WITH KEY type = gc_error
     BINARY SEARCH.
    IF sy-subrc EQ 0.
      lv_status  = gc_status_integracao-erro_processo.
      lv_type    = <fs_return>-type.
      lv_id      = <fs_return>-id.
      lv_znumber = <fs_return>-number.

      MESSAGE ID lv_id
            TYPE lv_type
          NUMBER lv_znumber
            INTO lv_message
            WITH <fs_return>-message_v1
                 <fs_return>-message_v2
                 <fs_return>-message_v3
                 <fs_return>-message_v4.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ELSE.
      lv_status = gc_status_integracao-finalizado.

      lv_type    = gc_type_s.
      lv_id      = gc_id_zmm.
      lv_znumber = gc_znumber_suc.

      MESSAGE ID lv_id
            TYPE lv_type
          NUMBER lv_znumber
            INTO lv_message.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.

    DATA(lt_itens_json) = is_reto_cont_inv-header-item[].

    SORT lt_itens_json BY zeile.


    SELECT *
    FROM ztmm_doc_mat
    INTO TABLE ct_mm_doc_mat
    WHERE  mblnr  = is_reto_cont_inv-header-iblnr
      AND  mjahr  = is_reto_cont_inv-header-gjahr.
    IF sy-subrc = 0.
      LOOP AT ct_mm_doc_mat ASSIGNING FIELD-SYMBOL(<fs_mm_doc_mat>).
        <fs_mm_doc_mat>-zstatus_integracao  = lv_status.
        <fs_mm_doc_mat>-aenam               = sy-uname.
        <fs_mm_doc_mat>-aedat               = sy-datum.
        <fs_mm_doc_mat>-aezet               = sy-uzeit.
        <fs_mm_doc_mat>-type                = lv_type.
        <fs_mm_doc_mat>-zid                 = lv_id.
        <fs_mm_doc_mat>-znumber             = lv_znumber.
        <fs_mm_doc_mat>-zmessage            = lv_message.
        READ TABLE lt_itens_json
           ASSIGNING FIELD-SYMBOL(<fs_itens_json>)
          WITH KEY zeile = <fs_mm_doc_mat>-zeile
          BINARY SEARCH.
        if sy-subrc = 0.
            <fs_mm_doc_mat>-erfmg = <fs_itens_json>-menge.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
