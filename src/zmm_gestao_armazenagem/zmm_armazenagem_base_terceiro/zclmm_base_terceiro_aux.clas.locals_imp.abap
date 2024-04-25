CLASS lcl_lhc_zi_mm_base_terceiro DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PUBLIC SECTION.
    METHODS setup_messages
      IMPORTING
        VALUE(p_task) TYPE clike OPTIONAL .

  PRIVATE SECTION.
    DATA: go_parametros TYPE REF TO zclca_tabela_parametros.
    DATA: gt_return     TYPE bapiret2_t.
    DATA: gs_goodsmvt_headret TYPE bapi2017_gm_head_ret.
    DATA: gv_wait_async TYPE bool.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_mm_base_terceiro_aux RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_mm_base_terceiro_aux RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zi_mm_base_terceiro_aux.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zi_mm_base_terceiro_aux RESULT result.

    METHODS descarga FOR MODIFY
      IMPORTING keys FOR ACTION zi_mm_base_terceiro_aux~descarga.
    METHODS motivo FOR MODIFY
      IMPORTING keys FOR ACTION zi_mm_base_terceiro_aux~motivo.

    METHODS tipomov FOR MODIFY
      IMPORTING keys FOR ACTION zi_mm_base_terceiro_aux~tipomov.
    METHODS ajuste_estoque FOR MODIFY
      IMPORTING keys FOR ACTION zi_mm_base_terceiro_aux~ajuste_estoque.

    METHODS gerar_retorno FOR MODIFY
      IMPORTING keys FOR ACTION zi_mm_base_terceiro_aux~gerar_retorno.

    METHODS get_parameters
      CHANGING
        !cv_tymove  TYPE bwart
        !cv_estespe TYPE sobkz.

    METHODS format_info_danfe
      IMPORTING
        iv_infodanfe        TYPE ztmm_base_terc-info_danfe
      RETURNING
        VALUE(rv_infodanfe) TYPE ztmm_base_terc-info_danfe.

ENDCLASS.

CLASS lcl_lhc_zi_mm_base_terceiro IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD read.

    IF keys IS NOT INITIAL.

      SELECT deliverydocument,
             deliverydocumentitem,
             notafiscal,
             notafiscalitem,
             semaforo,
             deliverydocumentbysupplier,
             documentdate,
             plant,
             werkscodename,
             material,
             materialname,
             descarga,
             actualdeliveredqtyinbaseunit,
             perdaganho,
             baseunit,
             valorperdaganho,
             br_nfpriceamountwithtaxes,
             supplier,
             docnumremessa,
             status,
             estoqueajustado,
             docmatajest,
             docnumretorno,
             tipomov,
             motivomov,
             storagelocation,
             bp_cliente,
             agente_frete,
             "info_danfe,
             placa,
             modalidade_frete
        FROM zi_mm_base_terceiro_aux
         FOR ALL ENTRIES IN @keys
       WHERE deliverydocument      = @keys-deliverydocument
         AND deliverydocumentitem  = @keys-deliverydocumentitem "#EC CI_ALL_FIELDS_NEEDED
        INTO TABLE @DATA(lt_base_propr).

      MOVE-CORRESPONDING lt_base_propr TO result.

    ENDIF.

  ENDMETHOD.

  METHOD lock.
    RETURN.
  ENDMETHOD.

  METHOD get_instance_features.
    RETURN.
  ENDMETHOD.

  METHOD descarga.
    DATA ls_base_terc TYPE ztmm_base_terc.

    READ TABLE keys ASSIGNING FIELD-SYMBOL(<fs_key>) INDEX 1.

    SELECT SINGLE mandt,
                  deliverydocument,
                  deliverydocumentitem,
                  notafiscal,
                  notafiscalitem,
                  descarga,
                  docnumremessa,
                  docnumretorno,
                  docmatajest,
                  tipomov,
                  motivomov,
                  status,
                  estoqueajustado,
                  bp_cliente,
                  modalidade_frete,
                  agente_frete,
                  placa,
                  info_danfe
        FROM ztmm_base_terc
        INTO @ls_base_terc
        WHERE deliverydocument      = @<fs_key>-deliverydocument
          AND deliverydocumentitem  = @<fs_key>-deliverydocumentitem.

    ls_base_terc-deliverydocument = <fs_key>-deliverydocument.
    ls_base_terc-deliverydocumentitem  = <fs_key>-deliverydocumentitem.
    IF ls_base_terc-docnumremessa IS INITIAL.
      ls_base_terc-descarga          = <fs_key>-%param-descarga.
    ELSE.

      CONSTANTS: lc_id  TYPE symsgid VALUE 'ZMM_BASE_MESSAGE',
                 lc_002 TYPE symsgno VALUE '004'.

      reported-zi_mm_base_terceiro_aux = VALUE #( ( %msg = new_message(
                                                       id       = lc_id
                                                       number   = lc_002
                                                       severity = if_abap_behv_message=>severity-error  ) ) ).
    ENDIF.

    MODIFY ztmm_base_terc FROM ls_base_terc.
  ENDMETHOD.

  METHOD motivo.
    DATA ls_base_terc TYPE ztmm_base_terc.

    READ TABLE keys ASSIGNING FIELD-SYMBOL(<fs_key>) INDEX 1.

    SELECT SINGLE mandt,
                  deliverydocument,
                  deliverydocumentitem,
                  notafiscal,
                  notafiscalitem,
                  descarga,
                  docnumremessa,
                  docnumretorno,
                  docmatajest,
                  tipomov,
                  motivomov,
                  status,
                  estoqueajustado,
                  bp_cliente,
                  modalidade_frete,
                  agente_frete,
                  placa,
                  info_danfe
        FROM ztmm_base_terc
        INTO @ls_base_terc
        WHERE deliverydocument      = @<fs_key>-deliverydocument
          AND deliverydocumentitem  = @<fs_key>-deliverydocumentitem.

    ls_base_terc-deliverydocument = <fs_key>-deliverydocument.
    ls_base_terc-deliverydocumentitem  = <fs_key>-deliverydocumentitem.
    IF ls_base_terc-docnumremessa IS INITIAL.
      ls_base_terc-motivomov           = <fs_key>-%param-motivoparam.
    ELSE.

      CONSTANTS: lc_id  TYPE symsgid VALUE 'ZMM_BASE_MESSAGE',
                 lc_002 TYPE symsgno VALUE '004'.

      reported-zi_mm_base_terceiro_aux = VALUE #( ( %msg = new_message(
                                                       id       = lc_id
                                                       number   = lc_002
                                                       severity = if_abap_behv_message=>severity-error  ) ) ).
    ENDIF.

    MODIFY ztmm_base_terc FROM ls_base_terc.
  ENDMETHOD.

  METHOD tipomov.
    DATA ls_base_terc TYPE ztmm_base_terc.

    READ TABLE keys ASSIGNING FIELD-SYMBOL(<fs_key>) INDEX 1.

    SELECT SINGLE mandt,
                  deliverydocument,
                  deliverydocumentitem,
                  notafiscal,
                  notafiscalitem,
                  descarga,
                  docnumremessa,
                  docnumretorno,
                  docmatajest,
                  tipomov,
                  motivomov,
                  status,
                  estoqueajustado,
                  bp_cliente,
                  modalidade_frete,
                  agente_frete,
                  placa,
                  info_danfe
        FROM ztmm_base_terc
        INTO @ls_base_terc
        WHERE deliverydocument      = @<fs_key>-deliverydocument
          AND deliverydocumentitem  = @<fs_key>-deliverydocumentitem.

    ls_base_terc-deliverydocument = <fs_key>-deliverydocument.
    ls_base_terc-deliverydocumentitem  = <fs_key>-deliverydocumentitem.
    IF ls_base_terc-docnumremessa IS INITIAL.
      ls_base_terc-tipomov          = <fs_key>-%param-tipomov.
    ELSE.

      CONSTANTS: lc_id  TYPE symsgid VALUE 'ZMM_BASE_MESSAGE',
                 lc_002 TYPE symsgno VALUE '004'.

      reported-zi_mm_base_terceiro_aux = VALUE #( ( %msg = new_message(
                                                       id       = lc_id
                                                       number   = lc_002
                                                       severity = if_abap_behv_message=>severity-error  ) ) ).
    ENDIF.

    MODIFY ztmm_base_terc FROM ls_base_terc.
  ENDMETHOD.

  METHOD ajuste_estoque.

    CONSTANTS: lc_sim TYPE string VALUE 'Sim',
               lc_03  TYPE bapi2017_gm_code  VALUE '03',
               lc_id  TYPE symsgid VALUE 'ZMM_BASE_MESSAGE',
               lc_001 TYPE symsgno VALUE '001',
               lc_002 TYPE symsgno VALUE '002'.

    DATA: ls_goodsmvt_header TYPE bapi2017_gm_head_01.
    DATA: lt_goodsmvt_item TYPE STANDARD TABLE OF bapi2017_gm_item_create.
    DATA: lv_goodsmvt_headret TYPE bapi2017_gm_head_ret,
          lv_materialdocument TYPE mblnr,
          lv_matdocumentyear  TYPE mjahr.

    READ TABLE keys ASSIGNING FIELD-SYMBOL(<fs_key>) INDEX 1.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    READ ENTITIES OF zi_mm_base_terceiro_aux IN LOCAL MODE
      ENTITY zi_mm_base_terceiro_aux
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_base_aux).

    IF lt_base_aux IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_base_aux ASSIGNING FIELD-SYMBOL(<fs_base>).
      IF <fs_base>-estoqueajustado = lc_sim.

        reported-zi_mm_base_terceiro_aux = VALUE #( ( %msg = new_message(
                                                         id       = lc_id
                                                         number   = lc_001
                                                         v1       = <fs_base>-deliverydocument
                                                         severity = if_abap_behv_message=>severity-error  ) ) ).
        RETURN.

      ENDIF.

      IF <fs_base>-motivomov IS INITIAL.

        reported-zi_mm_base_terceiro_aux = VALUE #( ( %msg = new_message(
                                                         id       = lc_id
                                                         number   = lc_002
                                                         severity = if_abap_behv_message=>severity-error  ) ) ).
        RETURN.

      ENDIF.
    ENDLOOP.

    ls_goodsmvt_header = VALUE bapi2017_gm_head_01( pstng_date = sy-datum
                                                    doc_date   = sy-datum ).

    lt_goodsmvt_item = VALUE #( FOR ls_base_aux IN lt_base_aux (
                                    material  = ls_base_aux-material
                                    plant     = ls_base_aux-plant
                                    stge_loc  = ls_base_aux-storagelocation
                                    move_type = ls_base_aux-tipomov
                                    entry_qnt = abs( ls_base_aux-perdaganho )
                                    move_reas = ls_base_aux-motivomov
                               ) ).

    gv_wait_async = abap_false.

    CALL FUNCTION 'ZFMMM_GOODSMVT_CREATE'
      STARTING NEW TASK 'TASK'
      CALLING setup_messages ON END OF TASK
      EXPORTING
        it_goodsmvt_item   = lt_goodsmvt_item
        is_goodsmvt_header = ls_goodsmvt_header
        iv_goodsmvt_code   = lc_03.
    WAIT UNTIL gv_wait_async = abap_true.

    IF gs_goodsmvt_headret IS NOT INITIAL.
      DATA: lt_mm_base_aux TYPE STANDARD TABLE OF ztmm_base_terc.

      LOOP AT lt_base_aux ASSIGNING FIELD-SYMBOL(<fs_base_aux>).
        <fs_base_aux>-docmatajest = gs_goodsmvt_headret-mat_doc.
*            <fs_base_aux>-DeliveryDocumentItem = gs_goodsmvt_headret-doc_year.
        <fs_base_aux>-estoqueajustado = abap_on.
        <fs_base_aux>-status = abap_false.
      ENDLOOP.

      MOVE-CORRESPONDING lt_base_aux TO lt_mm_base_aux.

      MODIFY ztmm_base_terc FROM TABLE lt_mm_base_aux.

    ENDIF.

    reported-zi_mm_base_terceiro_aux = VALUE #(
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

  METHOD gerar_retorno.

    TYPES: BEGIN OF ty_nfdocnum,
             docnum TYPE j_1bnfdoc-docnum,
           END OF ty_nfdocnum.

    DATA: lt_mm_base_terc TYPE STANDARD TABLE OF ztmm_base_terc.  "#######
    DATA: ls_goodsmvt_header TYPE bapi2017_gm_head_01.
    DATA: lt_goodsmvt_item TYPE STANDARD TABLE OF bapi2017_gm_item_create.
    DATA: lv_goodsmvt_headret TYPE bapi2017_gm_head_ret,
          lv_materialdocument TYPE mblnr,
          lv_matdocumentyear  TYPE mjahr,
          lv_tymove           TYPE bwart,
          lv_estoqueespecial  TYPE sobkz,
          lt_nfdocnum         TYPE STANDARD TABLE OF ty_nfdocnum.

    DATA: lv_nfe        TYPE string,
          lv_nfenumber  TYPE string,
          lv_nfseries   TYPE string,
          lv_docnum     TYPE j_1bnfdoc-docnum,
          lv_refkey     TYPE j_1bnflin-refkey,
          lv_plant_dplc TYPE werks_d.

    CONSTANTS: lc_03  TYPE bapi2017_gm_code VALUE '03',
               lc_05  TYPE bapi2017_gm_code VALUE '05',
               lc_e   TYPE c LENGTH 1 VALUE 'E',
               lc_w   TYPE c LENGTH 1 VALUE 'A',
               lc_s   TYPE c LENGTH 1 VALUE 'S',
               lc_id  TYPE symsgid VALUE 'ZMM_BASE_MESSAGE',
               lc_001 TYPE symsgno VALUE '003',
               lc_005 TYPE symsgno VALUE '005',
               lc_md  TYPE j_1bnflin-reftyp VALUE 'MD'.

    go_parametros = zclca_tabela_parametros=>get_instance( ).

    me->get_parameters(
      CHANGING
        cv_tymove  = lv_tymove
        cv_estespe = lv_estoqueespecial
    ).

    READ TABLE keys ASSIGNING FIELD-SYMBOL(<fs_key>) INDEX 1.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    READ ENTITIES OF zi_mm_base_terceiro_aux IN LOCAL MODE
      ENTITY zi_mm_base_terceiro_aux
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_base_aux).

    IF lt_base_aux IS INITIAL.

      RETURN.

    ELSE.

      LOOP AT lt_base_aux ASSIGNING FIELD-SYMBOL(<fs_base_aux>).

        IF <fs_base_aux>-docnumremessa IS NOT INITIAL.

          APPEND VALUE #( docnum = <fs_base_aux>-docnumremessa ) TO lt_nfdocnum.

        ENDIF.

      ENDLOOP.

      IF lt_nfdocnum[] IS NOT INITIAL.

        SELECT br_notafiscal,
               br_nfiscanceled
          FROM i_br_nfdocument
           FOR ALL ENTRIES IN @lt_nfdocnum
         WHERE br_notafiscal = @lt_nfdocnum-docnum
         INTO TABLE @DATA(lt_bnfdoc).

        IF sy-subrc IS NOT INITIAL.
          SORT lt_bnfdoc BY br_notafiscal.
        ENDIF.

      ENDIF.
    ENDIF.

    SORT lt_base_aux BY deliverydocument deliverydocumentitem.

    MOVE-CORRESPONDING lt_base_aux TO lt_mm_base_terc.

    SELECT _c~deliverydocument,
           _a~deliverydocumentitem,
           _a~referencesddocument,
           _a~referencesddocumentitem,
           _a~br_notafiscal,
           _a~br_notafiscalitem,
           _a~br_nfpriceamountwithtaxes,
           br_nfenumber,
           br_nfseries,
           _c~deliverydocumentbysupplier
      FROM zi_mm_base_nf_aux AS _a
      INNER JOIN @lt_base_aux AS _d
            ON  _d~deliverydocument     = _a~deliverydocument
            AND _d~deliverydocumentitem = _a~deliverydocumentitem
      INNER JOIN i_deliverydocument AS _c
            ON _c~deliverydocument      = _a~deliverydocument
      INNER JOIN i_br_nfdocument AS _b
            ON _b~br_notafiscal         = _a~br_notafiscal
      INTO TABLE @DATA(lt_cds).

    LOOP AT lt_cds ASSIGNING FIELD-SYMBOL(<fs_cds>).
      SHIFT <fs_cds>-br_nfenumber LEFT DELETING LEADING '0'.
      SHIFT <fs_cds>-br_nfseries LEFT DELETING LEADING '0'.
    ENDLOOP.

    SORT lt_cds BY deliverydocument deliverydocumentitem br_nfenumber br_nfseries.

    "Verificar se é um Recebimento de Transferência entre Centros
    SELECT a~purchaseorder,
           a~purchaseorderitem,
           a~deliverydocument,
           a~deliverydocumentitem
      FROM i_deliverydocumentitem AS a
      INNER JOIN i_purchaseorderitem AS b
         ON b~purchaseorder     EQ a~purchaseorder     "Número do pedido de compras
        AND b~purchaseorderitem EQ a~purchaseorderitem "Item do documento de Compras
      INNER JOIN @lt_base_aux AS c
         ON c~deliverydocument     = a~deliverydocument
        AND c~deliverydocumentitem = a~deliverydocumentitem
      WHERE a~referencesddocumentcategory EQ 'V' "Pedido
        AND b~purchaseorderitemcategory   EQ '7' "Transferência Estoque
        ORDER BY a~deliverydocument, a~deliverydocumentitem
      INTO TABLE @DATA(lt_remessa).
    IF sy-subrc EQ 0.
      SELECT _c~deliverydocument,
             _a~deliverydocumentitem,
             _a~referencesddocument,
             _a~referencesddocumentitem,
             _a~br_notafiscal,
             _a~br_notafiscalitem,
             _a~br_nfpriceamountwithtaxes,
             _c~deliverydocumentbysupplier
        FROM zi_mm_base_nf_aux AS _a
        INNER JOIN @lt_base_aux AS _d
              ON  _d~deliverydocument     = _a~deliverydocument
              AND _d~deliverydocumentitem = _a~deliverydocumentitem
        INNER JOIN i_deliverydocument AS _c
              ON  _c~deliverydocument           = _a~deliverydocument
              AND _c~deliverydocumentbysupplier = _d~deliverydocumentbysupplier
        INTO TABLE @DATA(lt_cds_remessa).

      SORT lt_cds_remessa BY deliverydocument deliverydocumentitem deliverydocumentbysupplier.
    ENDIF.


    LOOP AT lt_base_aux ASSIGNING <fs_base_aux>.

      IF lv_plant_dplc IS INITIAL.
        lv_plant_dplc = <fs_base_aux>-plant.
      ELSE.
        IF lv_plant_dplc NE <fs_base_aux>-plant.
          reported-zi_mm_base_terceiro_aux = VALUE #( ( %msg = new_message(
                                                    id       = lc_id
                                                    number   = lc_005
                                                    severity = if_abap_behv_message=>severity-error  ) ) ).
          RETURN.
        ENDIF.
      ENDIF.

      IF <fs_base_aux>-docnumremessa IS NOT INITIAL.

        lv_docnum = <fs_base_aux>-docnumremessa.

        READ TABLE lt_bnfdoc ASSIGNING FIELD-SYMBOL(<fs_bnfdoc>)
                                           WITH KEY br_notafiscal = lv_docnum
                                           BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          IF <fs_bnfdoc>-br_nfiscanceled IS INITIAL.

            reported-zi_mm_base_terceiro_aux = VALUE #( ( %msg = new_message(
                                                      id       = lc_id
                                                      number   = lc_001
                                                      severity = if_abap_behv_message=>severity-error  ) ) ).
            RETURN.

          ENDIF.

        ELSE.

          reported-zi_mm_base_terceiro_aux = VALUE #( ( %msg = new_message(
                                                    id       = lc_id
                                                    number   = lc_001
                                                    severity = if_abap_behv_message=>severity-error  ) ) ).
          RETURN.

        ENDIF.
      ENDIF.

      READ TABLE lt_remessa TRANSPORTING NO FIELDS
        WITH KEY deliverydocument     = <fs_base_aux>-deliverydocument
                 deliverydocumentitem = <fs_base_aux>-deliverydocumentitem BINARY SEARCH.
      IF sy-subrc EQ 0.
        "Se for uma transferência entre centros será realizada a leitura
        "da tabela lt_cds_remessa pelo documento de Remessa
        READ TABLE lt_cds_remessa INTO DATA(ls_cds_remessa)
          WITH KEY deliverydocument           = <fs_base_aux>-deliverydocument
                   deliverydocumentitem       = <fs_base_aux>-deliverydocumentitem
                   deliverydocumentbysupplier = <fs_base_aux>-deliverydocumentbysupplier BINARY SEARCH.
        CHECK sy-subrc EQ 0.

        <fs_base_aux>-notafiscal     = ls_cds_remessa-br_notafiscal.
        <fs_base_aux>-notafiscalitem = ls_cds_remessa-br_notafiscalitem.
      ELSE.

        lv_nfe = <fs_base_aux>-deliverydocumentbysupplier.
        SPLIT lv_nfe AT '-' INTO lv_nfenumber lv_nfseries.

        SHIFT lv_nfenumber LEFT DELETING LEADING '0'.
        SHIFT lv_nfseries LEFT DELETING LEADING '0'.

        READ TABLE lt_cds INTO DATA(ls_cds)
          WITH KEY deliverydocument     = <fs_base_aux>-deliverydocument
                   deliverydocumentitem = <fs_base_aux>-deliverydocumentitem
                   br_nfenumber         = lv_nfenumber
                   br_nfseries          = lv_nfseries BINARY SEARCH.
        CHECK sy-subrc EQ 0.

        <fs_base_aux>-notafiscal     = ls_cds-br_notafiscal.
        <fs_base_aux>-notafiscalitem = ls_cds-br_notafiscalitem.
      ENDIF.
    ENDLOOP.


    LOOP AT keys ASSIGNING <fs_key>.
      READ TABLE lt_mm_base_terc ASSIGNING FIELD-SYMBOL(<fs_base_terc>)
        WITH KEY deliverydocument     = <fs_key>-deliverydocument
                 deliverydocumentitem = <fs_key>-deliverydocumentitem BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      <fs_base_terc>-bp_cliente           = <fs_key>-%param-bpcliente.
      <fs_base_terc>-modalidade_frete     = <fs_key>-%param-modalidadefrete.
      <fs_base_terc>-agente_frete         = <fs_key>-%param-agente.
      <fs_base_terc>-placa                = <fs_key>-%param-placa.
      <fs_base_terc>-info_danfe           = me->format_info_danfe( iv_infodanfe = <fs_key>-%param-infodanfe ).
    ENDLOOP.

    ls_goodsmvt_header = VALUE bapi2017_gm_head_01( pstng_date = sy-datum
                                                    doc_date   = sy-datum ).

    lt_goodsmvt_item = VALUE #( FOR ls_base_aux IN lt_base_aux (
                                    material   = ls_base_aux-material
                                    plant      = ls_base_aux-plant
                                    stge_loc   = ls_base_aux-storagelocation
                                    move_type  = lv_tymove
                                   "spec_stock = lv_estoqueespecial
                                    vendor     = <fs_key>-%param-bpcliente
                                    customer   = <fs_key>-%param-bpcliente
*                                    entry_qnt  = ls_base_aux-actualdeliveredqtyinbaseunit
                                    entry_qnt  = ls_base_aux-descarga
                                    ref_doc    = ls_base_aux-notafiscal
                                    ref_doc_it = ls_base_aux-notafiscalitem
                               ) ).

    gv_wait_async = abap_false.

    CLEAR: gs_goodsmvt_headret,
           gt_return.

    CALL FUNCTION 'ZFMMM_GOODSMVT_CREATE'
      STARTING NEW TASK 'TASK'
      CALLING setup_messages ON END OF TASK
      EXPORTING
        it_base_terc       = lt_mm_base_terc
        it_goodsmvt_item   = lt_goodsmvt_item
        is_goodsmvt_header = ls_goodsmvt_header
        iv_goodsmvt_code   = lc_03.
    WAIT UNTIL gv_wait_async = abap_true.

*    SORT gt_return by type.
*
*    READ TABLE gt_return TRANSPORTING NO FIELDS WITH KEY type = 'E' BINARY SEARCH.

    IF gs_goodsmvt_headret IS NOT INITIAL.
      DATA: lt_mm_base_aux TYPE STANDARD TABLE OF ztmm_base_terc.

      lv_refkey = gs_goodsmvt_headret-mat_doc && gs_goodsmvt_headret-doc_year.

      DO 30 TIMES.
*        SELECT SINGLE xblnr,
*                      mblnr
*          FROM matdoc
*          INTO @DATA(ls_matdoc)
*         WHERE mblnr = @gs_goodsmvt_headret-mat_doc. "#EC CI_SROFC_NESTED
*
*        IF ls_matdoc-xblnr IS NOT INITIAL.
*          EXIT.
*        ENDIF.

        SELECT br_notafiscal,
               br_nfsourcedocumentnumber
          FROM i_br_nfitem
         WHERE br_nfsourcedocumentnumber = @lv_refkey
           AND br_nfsourcedocumenttype   = @lc_md
          INTO @DATA(ls_lin)
          UP TO 1 ROWS.
        ENDSELECT.

        IF sy-subrc IS NOT INITIAL.
          WAIT UP TO 1 SECONDS.
        ELSE.
          EXIT.
        ENDIF.

      ENDDO.

      DATA ls_nfdoc TYPE i_br_nfdocument.

*      SPLIT ls_matdoc-xblnr AT '-' INTO ls_nfdoc-br_nfenumber ls_nfdoc-br_nfseries.
*      SHIFT ls_nfdoc-br_nfenumber LEFT DELETING LEADING '0'.
*
*
*      SELECT SINGLE br_notafiscal,
*             br_nfenumber,
*             br_nfseries
*        FROM i_br_nfdocument
*        INTO @DATA(ls_docnumremessa)
*        WHERE br_nfenumber = @ls_nfdoc-br_nfenumber
*          AND br_nfseries = @ls_nfdoc-br_nfseries.

      LOOP AT keys ASSIGNING <fs_key>.
        READ TABLE lt_base_aux ASSIGNING FIELD-SYMBOL(<fs_base_prop>)
          WITH KEY deliverydocument     = <fs_key>-deliverydocument
                   deliverydocumentitem = <fs_key>-deliverydocumentitem BINARY SEARCH.
        CHECK sy-subrc EQ 0.

*        <fs_base_prop>-deliverydocument = <fs_key>-deliverydocument.
        <fs_base_prop>-bp_cliente          = <fs_key>-%param-bpcliente.
        <fs_base_prop>-modalidade_frete    = <fs_key>-%param-modalidadefrete.
        <fs_base_prop>-agente_frete        = <fs_key>-%param-agente.
        <fs_base_prop>-info_danfe          = <fs_key>-%param-infodanfe.
*        <fs_base_prop>-docnumremessa = ls_docnumremessa-br_notafiscal.
        <fs_base_prop>-docnumremessa       = ls_lin-br_notafiscal.
        CLEAR <fs_base_prop>-docnumretorno.
      ENDLOOP.

      MOVE-CORRESPONDING lt_base_aux TO lt_mm_base_aux.

      MODIFY ztmm_base_terc FROM TABLE lt_mm_base_aux.

    ENDIF.

    reported-zi_mm_base_terceiro_aux = VALUE #(
        FOR ls_messages IN gt_return (
            %msg = new_message(
                     id       = ls_messages-id
                     number   = ls_messages-number
                     severity = COND #(
                                     WHEN ls_messages-type EQ lc_e THEN if_abap_behv_message=>severity-error
                                     WHEN ls_messages-type EQ lc_s THEN if_abap_behv_message=>severity-success
                                     WHEN ls_messages-type EQ lc_w THEN if_abap_behv_message=>severity-warning
                                     ELSE if_abap_behv_message=>severity-none
                                  )
                     v1       = ls_messages-message_v1
                     v2       = ls_messages-message_v2
                     v3       = ls_messages-message_v3
                     v4       = ls_messages-message_v3
            )
        )
    ).
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
               gc_retorn TYPE ze_param_chave3 VALUE 'REMESSA',
               gc_sobkz  TYPE ze_param_chave2 VALUE 'SOBKZ',
               gc_k      TYPE ze_param_chave3 VALUE 'K'.

    TRY.
        go_parametros->m_get_single( EXPORTING iv_modulo = gc_modulo
                                               iv_chave1 = gc_chave1
                                               iv_chave2 = gc_bwart
                                               iv_chave3 = gc_retorn
                                     IMPORTING ev_param  = cv_tymove ).
        IF cv_tymove IS INITIAL.
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
                                     IMPORTING ev_param  = cv_estespe ).
        IF cv_estespe IS INITIAL.
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
    DATA lv_infodanfe LIKE iv_infodanfe.

    "SPLIT iv_infodanfe AT |\n| INTO TABLE DATA(lt_table_n).
    SPLIT iv_infodanfe AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_table_newline).

    LOOP AT lt_table_newline ASSIGNING FIELD-SYMBOL(<fs_linha>).
      IF lv_infodanfe IS INITIAL.
        lv_infodanfe = <fs_linha>.
      ELSE.
        CONCATENATE lv_infodanfe <fs_linha> INTO lv_infodanfe SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    rv_infodanfe = lv_infodanfe.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_lsc_zi_mm_base_terceiro DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lcl_lsc_zi_mm_base_terceiro IMPLEMENTATION.

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
