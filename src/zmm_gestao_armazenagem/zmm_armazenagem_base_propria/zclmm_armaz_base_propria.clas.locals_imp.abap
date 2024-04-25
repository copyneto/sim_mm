CLASS lcl_Propria DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PUBLIC SECTION.
    METHODS setup_messages
      IMPORTING
        VALUE(p_task) TYPE clike OPTIONAL .

  PRIVATE SECTION.
    TYPES: ty_reported_propria TYPE TABLE FOR REPORTED EARLY zi_mm_armaz_base_propria\\propria.

    DATA: go_parametros TYPE REF TO zclca_tabela_parametros.
    DATA: gt_return     TYPE bapiret2_t.
    DATA: gs_goodsmvt_headret TYPE bapi2017_gm_head_ret.
    DATA: gv_wait_async TYPE bool.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Propria RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Propria RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ Propria RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK Propria.

    METHODS ajuste_estoque FOR MODIFY
      IMPORTING keys FOR ACTION Propria~ajuste_estoque.

    METHODS descarga FOR MODIFY
      IMPORTING keys FOR ACTION Propria~descarga.

    METHODS gerar_retorno FOR MODIFY
      IMPORTING keys FOR ACTION Propria~gerar_retorno.

    METHODS motivo FOR MODIFY
      IMPORTING keys FOR ACTION Propria~motivo.
    METHODS set_qtd_receb FOR MODIFY
      IMPORTING keys FOR ACTION Propria~set_qtd_receb.

    METHODS get_parameters
      CHANGING
        !cv_TyMove  TYPE bwart
        !cv_EstEspe TYPE sobkz.

    METHODS format_Info_Danfe
      IMPORTING
        iv_InfoDanfe        TYPE ztmm_base_propr-info_danfe
      RETURNING
        VALUE(rv_InfoDanfe) TYPE ztmm_base_propr-info_danfe.

    METHODS create_msg
      CHANGING ct_reported_propria TYPE ty_reported_propria.

ENDCLASS.

CLASS lcl_Propria IMPLEMENTATION.

  METHOD get_instance_features.
    RETURN.
  ENDMETHOD.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD read.

    IF keys IS NOT INITIAL.
      SELECT MaterialDocYear,
             MaterialDoc,
             MaterialDocItem,
             notafiscal,
             notafiscalitem,
             NfeRemessa,
             Data,
             Centro,
             NomeCentro,
             Material,
             MaterialName,
             QtdDescarga,
             QtsFaturada,
             PerdaGanho,
             UniMedidaBasica,
             valorPerdaGanho,
             MotivoMov,
             BusinessPartner,
             NomeBP,
             Doc_Material_Retorno,
             Ano_Material_Retorno,
             Status,
             TyMove,
             Estoque_Ajustado,
             Doc_Material_Ajuste,
             Ano_Material_Ajuste,
             BR_NotaFiscal,
             QtdReceb,
             Bp_Cliente,
             Modalidade_Frete,
             Agente_Frete,
             Placa,
             "Info_Danfe,
             MaterialType,
             GoodsMovementType,
             Deposito,
             Empresa
          FROM zi_mm_armaz_base_propria
          INTO TABLE @DATA(lt_base_propr)
          FOR ALL ENTRIES IN @keys
          WHERE materialdocyear  = @keys-MaterialDocYear
            AND materialdoc      = @keys-MaterialDoc
            AND materialdocitem  = @keys-MaterialDocItem.

      MOVE-CORRESPONDING lt_base_propr TO result.

    ENDIF.

  ENDMETHOD.

  METHOD lock.
    RETURN.
  ENDMETHOD.

  METHOD ajuste_estoque.
    CONSTANTS: lc_gm_code TYPE bapi2017_gm_code VALUE '03',
               lc_id TYPE symsgid VALUE 'ZMM',
               lc_no TYPE symsgno VALUE '005',
               lc_type TYPE bapi_mtype VALUE 'W'.

    DATA: lt_goodsmvt_item    TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          lt_reported_propria TYPE ty_reported_propria.
    DATA: ls_goodsmvt_header TYPE bapi2017_gm_head_01,
          ls_goodsmvt_item   LIKE LINE OF lt_goodsmvt_item.
    DATA: lv_goodsmvt_headret TYPE bapi2017_gm_head_ret,
          lv_materialdocument TYPE mblnr,
          lv_matdocumentyear  TYPE mjahr.
    DATA: lv_TyMove           TYPE bwart,
          lv_EstoqueEspecial  TYPE sobkz.

    go_parametros = zclca_tabela_parametros=>get_instance( ).

    me->get_parameters(
      CHANGING
        cv_tymove  = lv_TyMove
        cv_estespe = lv_EstoqueEspecial
    ).

    READ TABLE keys ASSIGNING FIELD-SYMBOL(<fs_key>) INDEX 1.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    READ ENTITIES OF zi_mm_armaz_base_propria IN LOCAL MODE
      ENTITY Propria
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_base_propr).

    IF lt_base_propr IS INITIAL.
      RETURN.
    ENDIF.

    ls_goodsmvt_header = VALUE bapi2017_gm_head_01( pstng_date = sy-datum
                                                    doc_date   = sy-datum ).
    LOOP AT lt_base_propr ASSIGNING FIELD-SYMBOL(<fs_base_propr>).
      IF <fs_base_propr>-Estoque_Ajustado EQ abap_on.
        APPEND VALUE bapiret2( id = lc_id number = lc_no type = lc_type message_v1 =  <fs_base_propr>-NfeRemessa ) TO gt_return.
      ELSE.
        ls_goodsmvt_item-material  = <fs_base_propr>-Material.
        ls_goodsmvt_item-plant     = <fs_base_propr>-Centro.
        ls_goodsmvt_item-stge_loc  = <fs_base_propr>-Deposito.
        ls_goodsmvt_item-move_type = <fs_base_propr>-TyMove.
        ls_goodsmvt_item-entry_qnt = ABS( <fs_base_propr>-PerdaGanho ).
        ls_goodsmvt_item-spec_stock = lv_EstoqueEspecial.
        ls_goodsmvt_item-vendor     = <fs_base_propr>-BusinessPartner.
        ls_goodsmvt_item-customer   = <fs_base_propr>-BusinessPartner.

        ls_goodsmvt_item-move_reas = <fs_base_propr>-MotivoMov.
        APPEND ls_goodsmvt_item TO lt_goodsmvt_item.
      ENDIF.
    ENDLOOP.

    me->create_msg(
      CHANGING
        ct_reported_propria = lt_reported_propria
    ).

    gv_wait_async = abap_false.
    IF lt_goodsmvt_item IS NOT INITIAL.
      CALL FUNCTION 'ZFMMM_GOODSMVT_CREATE'
        STARTING NEW TASK 'TASK'
        CALLING setup_messages ON END OF TASK
        EXPORTING
          it_goodsmvt_item   = lt_goodsmvt_item
          is_goodsmvt_header = ls_goodsmvt_header
          iv_goodsmvt_code   = lc_gm_code.
    ENDIF.
    WAIT UNTIL gv_wait_async = abap_true.

    IF gs_goodsmvt_headret IS NOT INITIAL.
      DATA: lt_mm_base_propr TYPE STANDARD TABLE OF ztmm_base_propr.

      LOOP AT lt_base_propr ASSIGNING FIELD-SYMBOL(<fs_base_prop>).
        <fs_base_prop>-Doc_Material_Ajuste = gs_goodsmvt_headret-mat_doc.
        <fs_base_prop>-Ano_Material_Ajuste = gs_goodsmvt_headret-doc_year.
        <fs_base_prop>-Estoque_Ajustado = abap_on.
      ENDLOOP.

      MOVE-CORRESPONDING lt_base_propr TO lt_mm_base_propr.

      MODIFY ztmm_base_propr FROM TABLE lt_mm_base_propr.

    ENDIF.

    me->create_msg(
      CHANGING
        ct_reported_propria = lt_reported_propria
    ).

    reported-propria = lt_reported_propria.

  ENDMETHOD.

  METHOD gerar_retorno.
    CONSTANTS: lc_gm_code TYPE bapi2017_gm_code VALUE '05'.

    DATA: lt_mm_base_propr    TYPE STANDARD TABLE OF ztmm_base_propr,
          lt_reported_propria TYPE ty_reported_propria.
    DATA: ls_goodsmvt_header  TYPE bapi2017_gm_head_01.
    DATA: lt_goodsmvt_item    TYPE STANDARD TABLE OF bapi2017_gm_item_create.
    DATA: lv_goodsmvt_headret TYPE bapi2017_gm_head_ret,
          lv_materialdocument TYPE mblnr,
          lv_matdocumentyear  TYPE mjahr,
          lv_TyMove           TYPE bwart,
          lv_EstoqueEspecial  TYPE sobkz.

    go_parametros = zclca_tabela_parametros=>get_instance( ).

    me->get_parameters(
      CHANGING
        cv_tymove  = lv_TyMove
        cv_estespe = lv_EstoqueEspecial
    ).

    READ TABLE keys ASSIGNING FIELD-SYMBOL(<fs_key>) INDEX 1.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    READ ENTITIES OF zi_mm_armaz_base_propria IN LOCAL MODE
      ENTITY Propria
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_base_propr).
    IF lt_base_propr IS INITIAL.
      RETURN.
    ENDIF.

    SORT lt_base_propr by materialDocYear  materialDoc  materialDocItem.
    LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_k>). "#EC CI_LOOP_INTO_WA
        READ TABLE lt_base_propr  ASSIGNING FIELD-SYMBOL(<fs_base_propr_aux>) WITH KEY  materialDocYear   = <fs_k>-materialdocyear
                                                                                        materialDoc       = <fs_k>-materialdoc
                                                                                        materialDocItem   = <fs_k>-materialdocitem BINARY SEARCH.

        IF sy-subrc is INITIAL.
           <fs_base_propr_aux>-bp_cliente        = <fs_k>-%param-BpCliente.
           <fs_base_propr_aux>-modalidade_frete  = <fs_k>-%param-ModalidadeFrete.
           <fs_base_propr_aux>-agente_frete      = <fs_k>-%param-agentefrete.
           <fs_base_propr_aux>-placa             = <fs_k>-%param-Placa.
           <fs_base_propr_aux>-info_danfe        = me->format_info_danfe( iv_infodanfe = <fs_key>-%param-InfoDanfe ).
           <fs_base_propr_aux>-qtdreceb          = <fs_k>-%param-QtdRes.
        ENDIF.
    ENDLOOP.


    MOVE-CORRESPONDING lt_base_propr TO lt_mm_base_propr.

    ls_goodsmvt_header = VALUE bapi2017_gm_head_01( pstng_date = sy-datum
                                                    doc_date   = sy-datum
                                                    ref_doc_no = lt_base_propr[ 1 ]-NfeRemessa ).

    lt_goodsmvt_item = VALUE #( FOR ls_base_propr IN lt_base_propr (
                                    material   = ls_base_propr-Material
                                    plant      = ls_base_propr-Centro
                                    stge_loc   = ls_base_propr-Deposito
                                    move_type  = lv_TyMove
                                    spec_stock = lv_EstoqueEspecial
                                    vendor     = <fs_key>-%param-BpCliente
                                    customer   = <fs_key>-%param-BpCliente
                                    entry_qnt  = COND #( WHEN ls_base_propr-QtdReceb IS INITIAL THEN ls_base_propr-QtsFaturada
                                                         ELSE ls_base_propr-QtdReceb )
                                    ref_doc    = ls_base_propr-NotaFiscal
                                    ref_doc_it = ls_base_propr-NotaFiscalItem
                               ) ).

    gv_wait_async = abap_false.

    CALL FUNCTION 'ZFMMM_GOODSMVT_CREATE'
      STARTING NEW TASK 'TASK'
      CALLING setup_messages ON END OF TASK
      EXPORTING
        it_base_propr      = lt_mm_base_propr
        it_goodsmvt_item   = lt_goodsmvt_item
        is_goodsmvt_header = ls_goodsmvt_header
        iv_goodsmvt_code   = lc_gm_code.
    WAIT UNTIL gv_wait_async = abap_true.

    IF gs_goodsmvt_headret IS NOT INITIAL.

      MOVE-CORRESPONDING lt_base_propr TO lt_mm_base_propr.

      LOOP AT lt_mm_base_propr ASSIGNING FIELD-SYMBOL(<fs_base_prop>).
        <fs_base_prop>-doc_material_retorno = gs_goodsmvt_headret-mat_doc.
        <fs_base_prop>-ano_material_retorno = gs_goodsmvt_headret-doc_year.
        <fs_base_prop>-bp_cliente           = <fs_key>-%param-BpCliente.
        <fs_base_prop>-modalidade_frete     = <fs_key>-%param-ModalidadeFrete.
        <fs_base_prop>-agente_frete         = <fs_key>-%param-agentefrete.
        <fs_base_prop>-info_danfe           = <fs_key>-%param-InfoDanfe.
      ENDLOOP.

      MODIFY ztmm_base_propr FROM TABLE lt_mm_base_propr.

    ENDIF.

    reported-propria = VALUE #(
        FOR ls_messages IN gt_return (
            %msg = new_message(
                     id       = ls_messages-id
                     number   = ls_messages-number
                     severity = CONV #( ls_messages-type )
                     v1       = ls_messages-message_v1
                     v2       = ls_messages-message_v2
                     v3       = ls_messages-message_v3
                     v4       = ls_messages-message_v3
            )
        )
    ).

  ENDMETHOD.

  METHOD descarga.
    DATA ls_base_propr TYPE ztmm_base_propr.

    READ TABLE keys ASSIGNING FIELD-SYMBOL(<fs_key>) INDEX 1.

    SELECT SINGLE materialdocyear, materialdoc, materialdocitem, notafiscal, notafiscalitem, docnum,
           qtddescarga, materialbaseunit, Doc_Material_Retorno, Ano_Material_Retorno, status,
           estoque_ajustado, doc_material_ajuste, ano_material_ajuste, motivomov, bp_cliente,
           modalidade_frete, agente_frete, placa, info_danfe
        FROM ztmm_base_propr
        INTO CORRESPONDING FIELDS OF @ls_base_propr
        WHERE materialdocyear  = @<fs_key>-MaterialDocYear
          AND materialdoc      = @<fs_key>-MaterialDoc
          AND materialdocitem  = @<fs_key>-MaterialDocItem.
    IF sy-subrc IS NOT INITIAL.
      MOVE-CORRESPONDING <fs_key> TO ls_base_propr.
    ENDIF.

    ls_base_propr-qtddescarga = <fs_key>-%param-descarga.

    MODIFY ztmm_base_propr FROM ls_base_propr.

  ENDMETHOD.

  METHOD motivo.
    DATA ls_base_propr TYPE ztmm_base_propr.

    READ TABLE keys ASSIGNING FIELD-SYMBOL(<fs_key>) INDEX 1.

    SELECT SINGLE materialdocyear, materialdoc, materialdocitem, notafiscal, notafiscalitem, docnum,
           qtddescarga, materialbaseunit, Doc_Material_Retorno, Ano_Material_Retorno, status,
           estoque_ajustado, doc_material_ajuste, ano_material_ajuste, motivomov, bp_cliente,
           modalidade_frete, agente_frete, placa, info_danfe
        FROM ztmm_base_propr
        INTO CORRESPONDING FIELDS OF @ls_base_propr
        WHERE materialdocyear  = @<fs_key>-MaterialDocYear
          AND materialdoc      = @<fs_key>-MaterialDoc
          AND materialdocitem  = @<fs_key>-MaterialDocItem.
    IF sy-subrc IS NOT INITIAL.
      MOVE-CORRESPONDING <fs_key> TO ls_base_propr.
    ENDIF.

    ls_base_propr-motivomov = <fs_key>-%param-motivoparam.

    MODIFY ztmm_base_propr FROM ls_base_propr.
  ENDMETHOD.

  METHOD set_qtd_receb.
    RETURN.
*    DATA: lt_base_propr TYPE STANDARD TABLE OF ztmm_base_propr,
*          lt_CDS_base_propria TYPE TABLE OF zi_mm_armaz_base_propria.
*
*
*    SELECT materialdocyear, materialdoc, materialdocitem, notafiscal, notafiscalitem, docnum,
*           qtddescarga, materialbaseunit, Doc_Material_Retorno, Ano_Material_Retorno, status,
*           estoque_ajustado, doc_material_ajuste, ano_material_ajuste, motivomov, bp_cliente,
*           modalidade_frete, agente_frete, placa, info_danfe
*        FROM ztmm_base_propr
*        INTO CORRESPONDING FIELDS OF TABLE @lt_base_propr
*        FOR ALL ENTRIES IN @keys
*        WHERE materialdocyear  = @keys-MaterialDocYear
*          AND materialdoc      = @keys-MaterialDoc
*          AND materialdocitem  = @keys-MaterialDocItem. "#EC CI_FAE_NO_LINES_OK
*    IF sy-subrc IS NOT INITIAL.
*      MOVE-CORRESPONDING Keys TO lt_base_propr.
*    ENDIF.
*
*   SORT lt_base_propr by materialDocYear  materialDoc  materialDocItem.
*
*    LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_k>). "#EC CI_LOOP_INTO_WA
*
*        READ TABLE lt_base_propr  ASSIGNING FIELD-SYMBOL(<fs_base_propr>) WITH KEY  materialDocYear   = <fs_k>-materialdocyear
*                                                                                    materialDoc       = <fs_k>-materialdoc
*                                                                                    materialDocItem   = <fs_k>-materialdocitem BINARY SEARCH.
*
*        IF sy-subrc is INITIAL.
*            <fs_base_propr>-qtdreceb = <fs_k>-%param-descarga.
*        ENDIF.
*
*
*    ENDLOOP. "#EC CI_LOOP_INTO_WA
*
*    MODIFY ztmm_base_propr FROM TABLE lt_base_propr.

  ENDMETHOD.

  METHOD setup_messages.

    RECEIVE RESULTS FROM FUNCTION 'ZFMMM_GOODSMVT_CREATE'
          IMPORTING
            et_return        = gt_return
            es_goodsmvt_headret = gs_goodsmvt_headret.

    gv_wait_async = abap_true.

  ENDMETHOD.

  METHOD get_parameters.

    CONSTANTS: gc_modulo TYPE ze_param_modulo VALUE 'MM',
               gc_chave1 TYPE ze_param_chave1 VALUE 'ZMM_GESTAO_ARMAZENAGEM',
               gc_bwart  TYPE ze_param_chave2 VALUE 'BWART',
               gc_retorn TYPE ze_param_chave3 VALUE 'RETORNO',
               gc_sobkz  TYPE ze_param_chave2 VALUE 'SOBKZ',
               gc_k      TYPE ze_param_chave3 VALUE 'K'.

    TRY.
        go_parametros->m_get_single( EXPORTING iv_modulo = gc_modulo
                                               iv_chave1 = gc_chave1
                                               iv_chave2 = gc_bwart
                                               iv_chave3 = gc_retorn
                                     IMPORTING ev_param  = cv_TyMove ).
        IF cv_TyMove IS INITIAL.
          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_modulo
                                                       iv_chave1 = gc_chave1
                                                       iv_chave2 = gc_bwart
                                                       iv_chave3 = gc_retorn ).
        ENDIF.

      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros
        RETURN.
    ENDTRY.

    TRY.
        go_parametros->m_get_single( EXPORTING iv_modulo = gc_modulo
                                               iv_chave1 = gc_chave1
                                               iv_chave2 = gc_sobkz
                                               iv_chave3 = gc_k
                                     IMPORTING ev_param  = cv_EstEspe ).
        IF cv_EstEspe IS INITIAL.
          RAISE EXCEPTION NEW zcxca_tabela_parametros( iv_modulo = gc_modulo
                                                       iv_chave1 = gc_chave1
                                                       iv_chave2 = gc_sobkz
                                                       iv_chave3 = gc_k ).
        ENDIF.
      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD format_info_danfe.
    DATA lv_infodanfe LIKE iv_InfoDanfe.

    "SPLIT iv_InfoDanfe AT |\n| INTO TABLE DATA(lt_table_n).
    SPLIT iv_InfoDanfe AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_table_newline).

    LOOP AT lt_table_newline ASSIGNING FIELD-SYMBOL(<fs_linha>).
      IF lv_infodanfe IS INITIAL.
        lv_infodanfe = <fs_linha>.
      ELSE.
        CONCATENATE lv_infodanfe <fs_linha> INTO lv_infodanfe SEPARATED BY space. "cl_abap_char_utilities=>cr_lf.
      ENDIF.
    ENDLOOP.

    rv_InfoDanfe = lv_infodanfe.

  ENDMETHOD.

  METHOD create_msg.

    LOOP AT gt_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      APPEND VALUE #(
              %msg = new_message(
                       id       = <fs_return>-id
                       number   = <fs_return>-number
                       severity = CONV #( <fs_return>-type )
                       v1       = <fs_return>-message_v1
                       v2       = <fs_return>-message_v2
                       v3       = <fs_return>-message_v3
                       v4       = <fs_return>-message_v3
              )
          ) TO ct_reported_propria.
    ENDLOOP.

    CLEAR gt_return.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_ZI_MM_ARMAZ_BASE_PROPRIA DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lcl_ZI_MM_ARMAZ_BASE_PROPRIA IMPLEMENTATION.

  METHOD finalize.
    RETURN.
  ENDMETHOD.

  METHOD check_before_save.
    RETURN.
  ENDMETHOD.

  METHOD save.
    RETURN.
  ENDMETHOD.

  METHOD cleanup.
    RETURN.
  ENDMETHOD.

  METHOD cleanup_finalize.
    RETURN.
  ENDMETHOD.

ENDCLASS.
