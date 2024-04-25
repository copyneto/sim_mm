class ZCLMM_SALD_EST_MOV definition
  public
  final
  create public .

PUBLIC SECTION.

  TYPES:
    ty_return TYPE STANDARD TABLE OF bapireturn1 WITH DEFAULT KEY .

  DATA gt_sald_est_mov TYPE zsmm_sald_est_mov .

  DATA gs_sald_est_mov TYPE zsmm_sald_est_mov .

  CONSTANTS: gc_error         TYPE char1                 VALUE 'E',
             gc_move_type_mov TYPE bwart                 VALUE '311',
             gc_move_type_bai TYPE bwart                 VALUE '201',
             gc_gm_code_mov   TYPE gm_code               VALUE '04',
             gc_gm_code_bai   TYPE gm_code               VALUE '03',
             gc_barramento    TYPE char1                 VALUE 'B',
             gc_type_s        TYPE BAPI_MTYPE            VALUE 'S',
             gc_id_zmm        TYPE SYMSGID               VALUE 'ZMM',
             gc_znumber_suc   TYPE SYMSGNO               VALUE '001'.

    CONSTANTS:
      BEGIN OF gc_status_integracao,
        nao_enviado      TYPE ze_status_int  VALUE '00', "Enviar documento
        enviado          TYPE ze_status_int  VALUE '01', "Documento enviado
        retorno          TYPE ze_status_int  VALUE '02', "Retorno integração
        finalizado       TYPE ze_status_int  VALUE '03', "Processo Finalizado
        erro_processo    TYPE ze_status_int  VALUE '04', "Erro na execução do processo
        erro_integracao  TYPE ze_status_int  VALUE '05', "Erro integração
        retorno_eliminar TYPE ze_status_int  VALUE '06', "Retorno eliminar
        nao_enviar       TYPE ze_status_int  VALUE '07', "Não Enviar
      END OF gc_status_integracao.

  METHODS constructor
    IMPORTING
      !is_sald_est_mov TYPE zsmm_sald_est_mov .
  METHODS process
    RETURNING
      VALUE(rt_return) TYPE ty_return .
PROTECTED SECTION.
PRIVATE SECTION.
  METHODS processa_dados_goodsmvt_create
    IMPORTING
      is_goodsmvt_header TYPE bapi2017_gm_head_01
      is_goodsmvt_code   TYPE bapi2017_gm_code
      it_goodsmvt_item   TYPE tab_bapi_goodsmvt_item
      is_sald_est_mov    TYPE zsmm_sald_est_mov
    CHANGING
      ct_return          TYPE bapiret2_tab
      ct_mm_doc_mat      TYPE zctgmm_doc_mat.
  METHODS preenche_dados_goodsmvt_create
    IMPORTING
      !is_sald_est_mov TYPE zsmm_sald_est_mov
    CHANGING
      ct_goodsmvt_item TYPE tab_bapi_goodsmvt_item.
    "! Converter unidade de medida
    "! iv_meins | Unidade entrada
    "! rv_meins | Unidade saída
    METHODS conv
      IMPORTING
        iv_meins        TYPE zsmm_doc_rem_rec_item-meins
      RETURNING
        VALUE(rv_meins) TYPE ztmm_doc_rem_rec-meins.

ENDCLASS.



CLASS ZCLMM_SALD_EST_MOV IMPLEMENTATION.


  METHOD constructor.

    gs_sald_est_mov = is_sald_est_mov.

  ENDMETHOD.


  METHOD process.
    DATA: lt_return        TYPE TABLE OF bapiret2,
          lt_goodsmvt_item TYPE TABLE OF bapi2017_gm_item_create,
          lt_mm_doc_mat    TYPE zctgmm_doc_mat.

    DATA: ls_goodsmvt_header TYPE bapi2017_gm_head_01,
          ls_goodsmvt_code   TYPE bapi2017_gm_code.

    CLEAR:  lt_return[],
            lt_goodsmvt_item[].

    CLEAR:  ls_goodsmvt_header,
            ls_goodsmvt_code.

    ls_goodsmvt_header-pstng_date = gs_sald_est_mov-header-budat.
    ls_goodsmvt_header-doc_date   = gs_sald_est_mov-header-bldat.
    ls_goodsmvt_header-ref_doc_no = gs_sald_est_mov-header-bktxt.

    READ TABLE gs_sald_est_mov-header-item ASSIGNING FIELD-SYMBOL(<fs_itens>) INDEX 1.

    ls_goodsmvt_code-gm_code  = SWITCH #( <fs_itens>-bwart WHEN gc_move_type_mov
                                                           THEN gc_gm_code_mov
                                                           ELSE gc_gm_code_bai ).

    preenche_dados_goodsmvt_create( EXPORTING is_sald_est_mov     = gs_sald_est_mov
                                    CHANGING  ct_goodsmvt_item    = lt_goodsmvt_item[] ).


    processa_dados_goodsmvt_create( EXPORTING is_goodsmvt_header  = ls_goodsmvt_header
                                              is_goodsmvt_code    = ls_goodsmvt_code
                                              it_goodsmvt_item    = lt_goodsmvt_item[]
                                              is_sald_est_mov     = gs_sald_est_mov
                                    CHANGING  ct_return           = lt_return[]
                                              ct_mm_doc_mat       = lt_mm_doc_mat ).

    IF  lt_mm_doc_mat[] IS NOT INITIAL.

      CALL FUNCTION 'ZFMMM_WMS_ATUALIZA_TABXX1'
         EXPORTING
           it_ztbmmxxx2 = lt_mm_doc_mat.

    ENDIF.

    rt_return[] = lt_return[].

  ENDMETHOD.


  METHOD preenche_dados_goodsmvt_create.

    ct_goodsmvt_item = VALUE #( FOR <fs_itens> IN is_sald_est_mov-header-item
                           (  material        = |{ <fs_itens>-matnr WIDTH = 18 ALIGN = RIGHT PAD = '0' }|
                              plant           = <fs_itens>-werks
                              stge_loc        = <fs_itens>-lgort
                              move_type       = <fs_itens>-bwart
                              entry_qnt       = <fs_itens>-erfmg
                              entry_uom       = conv( <fs_itens>-meins )
                              entry_uom_iso   = conv( <fs_itens>-meins )
                              move_plant      = <fs_itens>-umwrk
                              move_stloc      = <fs_itens>-umlgo
                              costcenter      = SWITCH #( <fs_itens>-bwart WHEN gc_move_type_bai
                                                                           THEN <fs_itens>-kostl
                                                                           ELSE space ) ) ).

  ENDMETHOD.


  METHOD processa_dados_goodsmvt_create.
    DATA: lt_goodsmvt_item   TYPE TABLE OF bapi2017_gm_item_create.

    DATA: lv_materialdocument TYPE bapi2017_gm_head_ret-mat_doc,
          lv_matdocumentyear  TYPE bapi2017_gm_head_ret-doc_year.

    DATA: lv_message          TYPE bapi_msg.

    lt_goodsmvt_item[] = it_goodsmvt_item[].

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = is_goodsmvt_header
        goodsmvt_code    = is_goodsmvt_code
      IMPORTING
        materialdocument = lv_materialdocument
        matdocumentyear  = lv_matdocumentyear
      TABLES
        goodsmvt_item    = lt_goodsmvt_item
        return           = ct_return.

    IF lv_materialdocument IS INITIAL.
      SORT ct_return BY type.

      READ TABLE ct_return
      ASSIGNING FIELD-SYMBOL(<fs_return>)
      WITH KEY type = gc_error
       BINARY SEARCH.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
    ELSE.

      MESSAGE ID gc_id_zmm
            TYPE gc_type_s
          NUMBER gc_znumber_suc
            INTO lv_message.

      ct_mm_doc_mat = VALUE #( FOR <fs_itens> IN is_sald_est_mov-header-item
                             (  mblnr               = lv_materialdocument
                                mjahr               = lv_matdocumentyear
                                zeile               = sy-tabix
                                ztipo_doc           = is_sald_est_mov-header-ztipo_doc
                                zstatus_integracao  = gc_status_integracao-finalizado
                                zorigem             = gc_barramento
                                matnr               = |{ <fs_itens>-matnr WIDTH = 18 ALIGN = RIGHT PAD = '0' }|
                                bwart               = <fs_itens>-bwart
                                werks               = <fs_itens>-werks
                                lgort               = <fs_itens>-lgort
                                umwrk               = <fs_itens>-umwrk
                                umlgo               = <fs_itens>-umlgo
                                erfmg               = <fs_itens>-erfmg
                                meins               = conv( <fs_itens>-meins )
                                ernam               = sy-uname
                                erdat               = sy-datum
                                erzet               = sy-uzeit
                                type                = gc_type_s
                                zid                 = gc_id_zmm
                                znumber             = gc_znumber_suc
                                zmessage            = lv_message ) ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD conv.

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

ENDCLASS.
