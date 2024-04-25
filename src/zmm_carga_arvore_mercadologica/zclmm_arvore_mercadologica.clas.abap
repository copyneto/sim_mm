"! <p><strong>Classe para criação da árvore mercadológica</strong></p>
"! <p><strong>Autor:</strong>Enio Rafael de Jesus</p>
"! <p><strong>Data:</strong>14/12/2023</p>
CLASS zclmm_arvore_mercadologica DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_xo_const_message.

    CONSTANTS:
      "! Classe de mensagem
      gc_message_id    TYPE symsgid   VALUE 'ZMM_ARVORE_MERC' ##NO_TEXT,
      "! Objeto de log
      gc_log_object    TYPE balobj_d  VALUE 'ZMM_ARVORE_MERC' ##NO_TEXT,
      "! Subobjeto de log
      gc_log_subobject TYPE balsubobj VALUE 'CARGA' ##NO_TEXT.

    TYPES:
      "! Type para estrutura pai da hierarquia
      BEGIN OF ty_dados,
        hi               TYPE bapi_wrf_hier_change_head-hier_id,
        node             TYPE bapi_wrf_hier_change_struc-node,
        parent           TYPE bapi_wrf_hier_change_struc-parent,
        date_from        TYPE bapi_wrf_hier_change_struc-date_from,
        date_to          TYPE bapi_wrf_hier_change_struc-date_to,
        node2            TYPE bapi_wrf_hier_change_struc-node,
        node_description TYPE bapi_wrf_desc_change_struc-node_description,
      END OF ty_dados,

      "! Type para estrutura filha da hierarquia
      BEGIN OF ty_dados_c,
        hier_id          TYPE bapi_wrf_hier_create_head-hier_id,
        salesorg         TYPE bapi_wrf_hier_create_head-salesorg,
        distr_chan       TYPE bapi_wrf_hier_create_head-distr_chan,
        node             TYPE bapi_wrf_hier_create_struc_sty-node,
        date_from        TYPE bapi_wrf_hier_create_struc_sty-date_from,
        date_to          TYPE bapi_wrf_hier_create_struc_sty-date_to,
        hier_description TYPE bapi_wrf_desc_create_hier_sty-hier_description,
        node_description TYPE bapi_wrf_desc_change_struc-node_description,
      END OF ty_dados_c.

    "! Construtor da classe
    "! iv_slug | Nome do arquivo importado
    METHODS constructor
      IMPORTING
        iv_slug TYPE string.

    "! Carregar arquivo e formatar dados
    "! iv_content | Conteúdo no formato string recebido do serviço
    METHODS upload_arquivo
      IMPORTING
        iv_content TYPE string.

    "! Método para executar a criação da árvore hierárquica
    "! iv_savelog | Indicador p/ saber se log deve ser armazenado
    "! rt_return | Mensagens de retorno
    METHODS criar_arvore
      IMPORTING
        iv_savelog       TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_Return) TYPE bapiret2_t.

  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Salvar log
    "! it_return | Mensagens retornadas do processamento
    METHODS save_log_history
      IMPORTING
        it_return TYPE bapiret2_t.

    DATA gt_dados   TYPE STANDARD TABLE OF ty_dados.
    DATA gt_dados_c TYPE STANDARD TABLE OF ty_dados_c.
    DATA gv_slug    TYPE string.
ENDCLASS.



CLASS zclmm_arvore_mercadologica IMPLEMENTATION.

  METHOD constructor.

    me->gv_slug = iv_slug.

  ENDMETHOD.

  METHOD save_log_history.

    TRY.
        DATA(lv_guid) = CONV balnrext( cl_system_uuid=>create_uuid_x16_static( ) ).
      CATCH cx_uuid_error.
    ENDTRY.

    CALL FUNCTION 'ZFMCA_ADD_LOG'
      STARTING NEW TASK lv_guid
      EXPORTING
        iv_ext_number = lv_guid
        iv_object     = gc_log_object
        iv_subobject  = gc_log_subobject
        it_return     = it_return.

    DATA(ls_carga_arvore) = VALUE ztmm_arvore_merc(
      uuid         = lv_guid
      filename     = me->gv_slug
      created_by   = sy-uname
      created_date = sy-datum
      created_at   = sy-uzeit
    ).

    INSERT ztmm_arvore_merc FROM ls_carga_arvore.
    COMMIT WORK.

  ENDMETHOD.

  METHOD criar_arvore.

    DATA lt_hierarchy_structure_c   TYPE STANDARD TABLE OF bapi_wrf_hier_create_struc_sty.
    DATA lt_description_hierarchy_c TYPE STANDARD TABLE OF bapi_wrf_desc_create_hier_sty.
    DATA lt_description_structure_c TYPE STANDARD TABLE OF bapi_wrf_desc_create_struc_sty.
    DATA lt_extension_in_c          TYPE STANDARD TABLE OF bapiparex.
    DATA lt_hierarchy_structure     TYPE STANDARD TABLE OF bapi_wrf_hier_change_struc.
    DATA lt_description_hierarchy   TYPE STANDARD TABLE OF bapi_wrf_desc_change_hier.
    DATA lt_description_structure   TYPE STANDARD TABLE OF bapi_wrf_desc_change_struc.
    DATA lt_hierarchy_items         TYPE STANDARD TABLE OF bapi_wrf_hier_change_items.
    DATA lt_extension_in            TYPE STANDARD TABLE OF bapiparex.
    DATA lt_return                  TYPE STANDARD TABLE OF bapiret2.
    DATA ls_hierarchy_data_c        TYPE bapi_wrf_hier_create_head.
    DATA ls_hierarchy_data          TYPE bapi_wrf_hier_change_head.
    DATA ls_testrun                 TYPE bapi_wrf_testrun_sty.

    CHECK NOT gt_dados_c IS INITIAL
      AND NOT gt_dados   IS INITIAL.

    "@ Create header/father hierarchy
    READ TABLE gt_dados_c ASSIGNING FIELD-SYMBOL(<fs_dados_c>) INDEX 1.

    IF sy-subrc IS INITIAL.

      ls_hierarchy_data_c-hier_id    = <fs_dados_c>-hier_id.
      ls_hierarchy_data_c-salesorg   = <fs_dados_c>-salesorg .
      ls_hierarchy_data_c-distr_chan = <fs_dados_c>-distr_chan.
      ls_hierarchy_data_c-bw_flag    = abap_true.
      ls_hierarchy_data_c-date_flag  = abap_true.

      APPEND INITIAL LINE TO lt_hierarchy_structure_c ASSIGNING FIELD-SYMBOL(<fs_hierarchy_structure_c>).

      <fs_hierarchy_structure_c>-node      = <fs_dados_c>-node.
      <fs_hierarchy_structure_c>-date_from = <fs_dados_c>-date_from.
      <fs_hierarchy_structure_c>-date_to   = <fs_dados_c>-date_to.

      APPEND INITIAL LINE TO lt_description_hierarchy_c ASSIGNING FIELD-SYMBOL(<fs_description_hierarchy_c>).

      <fs_description_hierarchy_c>-langu_iso        = sy-langu.
      <fs_description_hierarchy_c>-langu            = sy-langu.
      <fs_description_hierarchy_c>-hier_description = <fs_dados_c>-hier_description.

      APPEND INITIAL LINE TO lt_description_structure_c ASSIGNING FIELD-SYMBOL(<fs_description_structure_c>).

      <fs_description_structure_c>-node             = <fs_dados_c>-node.
      <fs_description_structure_c>-langu_iso        = sy-langu.
      <fs_description_structure_c>-langu            = sy-langu.
      <fs_description_structure_c>-node_description = <fs_dados_c>-node_description.

    ENDIF.

    CALL FUNCTION 'BAPI_WRF_MATGRP_CREATE'
      EXPORTING
        hierarchy_data        = ls_hierarchy_data_c
        testrun               = VALUE bapi_wrf_testrun_sty( testrun = abap_false )
        hierarchy_structure   = lt_hierarchy_structure_c
        description_hierarchy = lt_description_hierarchy_c
        description_structure = lt_description_structure_c
        extension_in          = lt_extension_in_c
      IMPORTING
        return                = lt_return.

    DELETE lt_return WHERE type <> if_xo_const_message~error."#EC CI_STDSEQ
    IF NOT line_Exists( lt_return[ type = if_xo_const_message~error ] )."#EC CI_STDSEQ
      APPEND VALUE #(
        id         = gc_message_id
        number     = 001
        type       = if_xo_const_message~success
        message_v1 = <fs_dados_c>-node
      ) TO rt_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ELSE.
      APPEND VALUE #(
        id         = gc_message_id
        number     = 002
        type       = if_xo_const_message~error
        message_v1 = <fs_dados_c>-node
      ) TO rt_return.

      APPEND LINES OF lt_return TO rt_return.
    ENDIF.

    CLEAR lt_return.

    "@ Create children hierarchy
    READ TABLE gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados_children>) INDEX 1.

    IF <fs_dados_children> IS ASSIGNED.
      ls_hierarchy_data-hier_id = <fs_dados_children>-hi.
    ENDIF.

    ls_hierarchy_data-mulitple_flag = abap_true.

    LOOP AT gt_dados ASSIGNING <fs_dados_children>.
      APPEND INITIAL LINE TO lt_hierarchy_structure ASSIGNING FIELD-SYMBOL(<fs_hierarchy_structure>).

      <fs_hierarchy_structure>-node      = <fs_dados_children>-node.
      <fs_hierarchy_structure>-parent    = <fs_dados_children>-parent.
      <fs_hierarchy_structure>-date_from = <fs_dados_children>-date_from.
      <fs_hierarchy_structure>-date_to   = <fs_dados_children>-date_to.
      <fs_hierarchy_structure>-change    = 'I'.

      APPEND INITIAL LINE TO lt_description_structure ASSIGNING FIELD-SYMBOL(<fs_description_structure>).

      <fs_description_structure>-node             = <fs_dados_children>-node2.
      <fs_description_structure>-langu_iso        = sy-langu.
      <fs_description_structure>-langu            = sy-langu.
      <fs_description_structure>-change           = 'I'.
      <fs_description_structure>-node_description = <fs_dados_children>-node_description.

      CALL FUNCTION 'BAPI_WRF_MATGRP_CHANGE'
        EXPORTING
          hierarchy_data        = ls_hierarchy_data
          testrun               = VALUE bapi_wrf_testrun_sty( testrun = abap_false )
          hierarchy_structure   = lt_hierarchy_structure
          description_hierarchy = lt_description_hierarchy
          description_structure = lt_description_structure
          hierarchy_items       = lt_hierarchy_items
          extension_in          = lt_extension_in
        IMPORTING
          return                = lt_return.

      DELETE lt_return WHERE type <> if_xo_const_message~error."#EC CI_STDSEQ
      IF NOT line_Exists( lt_return[ type = if_xo_const_message~error ] )."#EC CI_STDSEQ
        APPEND VALUE #(
          id         = gc_message_id
          number     = 001
          type       = if_xo_const_message~success
          message_v1 = <fs_dados_children>-node2
        ) TO rt_return.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ELSE.
        APPEND VALUE #(
          id         = gc_message_id
          number     = 002
          type       = if_xo_const_message~error
          message_v1 = <fs_dados_children>-node2
        ) TO rt_return.

        APPEND LINES OF lt_return TO rt_return.
      ENDIF.

      CLEAR:
        lt_hierarchy_structure,
        lt_description_hierarchy,
        lt_description_structure,
        lt_hierarchy_items,
        lt_extension_in,
        lt_return.
    ENDLOOP.

    IF iv_savelog = abap_true.
      me->save_log_history( rt_return ).
    ENDIF.
  ENDMETHOD.

  METHOD upload_arquivo.
    SPLIT iv_content AT cl_abap_char_utilities=>cr_lf INTO TABLE DATA(lt_content).

    "//delete header
    DELETE lt_content INDEX 1.

    READ TABLE lt_content ASSIGNING FIELD-SYMBOL(<fs_content>) INDEX 1.
    SPLIT <fs_content> AT ';' INTO TABLE DATA(lt_content_val).

    APPEND INITIAL LINE TO gt_dados_c ASSIGNING FIELD-SYMBOL(<fs_dados_c>).
    LOOP AT lt_content_val ASSIGNING FIELD-SYMBOL(<Fs_content_header>).

      CASE sy-tabix.

        WHEN 1.
          <fs_dados_c>-hier_id          = <Fs_content_header>.

        WHEN 2.
          <fs_dados_c>-salesorg         = <Fs_content_header>.

        WHEN 3.
          <fs_dados_c>-distr_chan       = <Fs_content_header>.

        WHEN 4.
          <fs_dados_c>-node             = <Fs_content_header>.

        WHEN 5.
          <fs_dados_c>-date_from        = <Fs_content_header>.

        WHEN 6.
          <fs_dados_c>-date_to          = <Fs_content_header>.

        WHEN 7.
          <fs_dados_c>-hier_description = <Fs_content_header>.

        WHEN 8.
          <fs_dados_c>-node_description = <Fs_content_header>.

        WHEN OTHERS.
          EXIT.

      ENDCASE.
    ENDLOOP.

    DELETE lt_content INDEX 1.

    LOOP AT lt_content ASSIGNING FIELD-SYMBOL(<fs_content_items>) FROM 2.

      SPLIT <fs_content_items> AT ';' INTO TABLE lt_content_val.
      APPEND INITIAL LINE TO me->gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).

      LOOP AT lt_content_val ASSIGNING FIELD-SYMBOL(<fs_content_item>)."#EC CI_NESTED

        CASE sy-tabix.

          WHEN 1.
            <fs_dados>-hi               = <fs_content_item>.

          WHEN 2.
            <fs_dados>-node             = <fs_content_item>.

          WHEN 3.
            <fs_dados>-parent           = <fs_content_item>.

          WHEN 4.
            <fs_dados>-date_from        = <fs_content_item>.

          WHEN 5.
            <fs_dados>-date_to          = <fs_content_item>.

          WHEN 6.
            <fs_dados>-node2            = <fs_content_item>.

          WHEN 7.
            <fs_dados>-node_description = <fs_content_item>.

          WHEN OTHERS.
            EXIT.

        ENDCASE.

      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
