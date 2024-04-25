"! <p><strong>Rotina para leitura de dados históricos CST60</strong></p>
"! <p><strong>Autor:</strong>Enio Rafael de Jesus</p>
"! <p><strong>Classe para dos dados históricos do ICMS CST60</strong></p>
"! <p><strong>Autor:</strong>Enio Rafael de Jesus</p>
"! <p><strong>Data:</strong>13/02/2024</p>
CLASS zclmm_read_hist_cst60 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_xo_const_message.

    CONSTANTS:
      "! Classe de mensagem
      gc_message_id     TYPE t100-arbgb VALUE 'ZMM_HIST_CST60' ##NO_TEXT,

      "! Parâmetro - Módulo
      gc_module         TYPE ztca_param_mod-modulo VALUE 'MM' ##NO_TEXT,

      "! BP Isento
      gc_bp_isento_icms TYPE j_1bnfdoc-stains VALUE 'ISENTO' ##NO_TEXT,

      "! Parâmetro - Chave 1
      BEGIN OF gc_param_chave1,
        st_retido_cst_60 TYPE ztca_param_par-chave1 VALUE 'ST_RETIDO_CST_60' ##NO_TEXT,
      END OF gc_param_chave1,

      "! Parâmetro - Chave 2
      BEGIN OF gc_param_chave2,
        cfops_entrada        TYPE ztca_param_par-chave1 VALUE 'CFOPS_ENTRADA' ##NO_TEXT,
        cst_entrada          TYPE ztca_param_par-chave2 VALUE 'LISTA_CST_ENTRADA' ##NO_TEXT,
        cst_ja_retido        TYPE ztca_param_par-chave2 VALUE 'LISTA_CST_JA_RETIDO' ##NO_TEXT,
        taxtype_st_entrada   TYPE ztca_param_par-chave2 VALUE 'LISTA_TAXTYPE_ST_ENTRADA' ##NO_TEXT,
        taxtype_icms_entrada TYPE ztca_param_par-chave2 VALUE 'LISTA_TAXTYPE_ICMS_ENTRADA' ##NO_TEXT,
        taxtype_fcp_entrada  TYPE ztca_param_par-chave2 VALUE 'LISTA_TAXTYPE_FCP_ST_ENTRADA' ##NO_TEXT,
        cst_saida            TYPE ztca_param_par-chave2 VALUE 'LISTA_CST_SAIDA  ' ##NO_TEXT,
        cfops_saida          TYPE ztca_param_par-chave2 VALUE 'CFOPS_SAIDA' ##NO_TEXT,
      END OF gc_param_chave2.

    TYPES:
      "! Type para estrutura
      BEGIN OF ty_othertax,
        docnum          TYPE j_1bnfdoc-docnum,
        itmnum          TYPE j_1bnflin-itmnum,
        matnr           TYPE j_1bnflin-matnr,
        menge           TYPE j_1bnflin-menge,
        vbcstret        TYPE j_1bnflin-vbcstret,
        vicmsstret      TYPE j_1bnflin-vicmsstret,
        pst             TYPE j_1bnflin-pst,
        vicmssubstituto TYPE j_1bnflin-vicmssubstituto,
        vbcfcpstret     TYPE j_1bnflin-vbcfcpstret,
        vfcpstret       TYPE j_1bnflin-vfcpstret,
        pfcpstret       TYPE j_1bnflin-pfcpstret,
      END OF ty_othertax,

      "! Dados BP
      BEGIN OF ty_bp_data,
        businesspartner TYPE bu_partner,
        icmstaxpay      TYPE j_1bicmstaxpay,
        indsector       TYPE j_1bindtyp,
      END OF ty_bp_data.

    "! Construtor da classe
    METHODS constructor
      RAISING
        zclca_msg_erro.

    "! Método para a rotina de leitura dos dados historicos
    "! it_nflin | Tabela de itens NF-e
    "! is_nfheader | Cabeçalho NF-e
    "! rv_result | Indicador se cenário está ativo
    METHODS check_is_activated
      IMPORTING
        it_nflin         TYPE j_1bnflin_tab
        is_nfheader      TYPE j_1bnfdoc
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    "! Preenchimento dos dados históricos
    "! is_nfitem | Itens da nota fiscal
    "! is_nfheader | Cabeçalho da NF-e
    "! cs_item_data | Dados adicionais item da NF-e
    METHODS fill_data
      IMPORTING
        is_nfitem    TYPE logbr_s_nf_item
        is_nfheader  TYPE j_1bnfdoc
      CHANGING
        cs_item_data TYPE j_1bnf_badi_item.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gs_partner_data               TYPE zi_mm_bp_j1b_taxdata.
    DATA gt_st_retido_cst_entrada_in   TYPE rsis_t_Range.
    DATA gt_st_retido_cst_entrada      TYPE rsis_t_Range.
    DATA gt_st_retido_cst_ja_retido    TYPE rsis_t_Range.
    DATA gt_st_retido_cst_ja_retido_in TYPE rsis_t_Range.
    DATA gt_st_retido_cst_saida        TYPE rsis_t_Range.
    DATA gt_st_retido_cfops_entrada    TYPE rsis_t_Range.
    DATA gt_st_retido_cfops_saida      TYPE rsis_t_Range.
    DATA gt_st_retido_taxtype_st       TYPE rsis_t_Range.
    DATA gt_st_retido_taxtype_icms     TYPE rsis_t_Range.
    DATA gt_st_retido_taxtype_fcp_st   TYPE rsis_t_Range.

    "! Método para a rotina de leitura dos dados hostoricos
    "! iv_docnum   | Conteudo contendo o numero de documento
    "! iv_plant    | Conteudo contendo o centro
    "! iv_material | Conteudo contendo o numero do material
    "! rs_data     | Conteudo contendo os dados da leitura de dados historicos
    METHODS get_tax_contribuinte
      IMPORTING
        iv_docnum      TYPE j_1bnfdoc-docnum
        iv_plant       TYPE  j_1bnflin-werks
        iv_material    TYPE j_1bnflin-matnr
      RETURNING
        VALUE(rs_data) TYPE zsmm_hist_cst60.

    "! Método para informar ao usuário o status dos parametros
    METHODS get_parameters
      RAISING
        zclca_msg_erro.
ENDCLASS.



CLASS zclmm_read_hist_cst60 IMPLEMENTATION.


  METHOD get_tax_contribuinte.
    DATA ls_othertax TYPE ty_othertax.

    SELECT                                              "#EC CI_NOFIELD
      docnum,
      itmnum,
      matnr,
      menge,
      werks,
      taxsit,
      vbcstret,
      vicmsstret,
      pst,
      vicmssubstituto,
      vbcfcpstret,
      vfcpstret,
      pfcpstret
    FROM j_1bnflin
   WHERE matnr  EQ @iv_material
     AND taxsit IN @gt_st_retido_cst_entrada_in
     AND cfop   IN @gt_st_retido_cfops_entrada
     AND werks  EQ @iv_plant
  ORDER BY j_1bnflin~docnum DESCENDING
    INTO @DATA(ls_result)
   UP TO 1 ROWS.
    ENDSELECT.

    IF sy-subrc IS INITIAL.

      IF ls_result-taxsit IN gt_st_retido_cst_ja_retido
      OR ls_result-taxsit IN gt_st_retido_cst_ja_retido_in.

        rs_data-docnum      = ls_result-docnum.
        rs_data-itmnum      = ls_result-itmnum.
        rs_data-matnr       = ls_result-matnr.
        rs_data-werks       = ls_result-werks.
        rs_data-taxsit      = ls_result-taxsit.
        rs_data-vbcstret    = ls_result-vbcstret    / ls_result-menge.
        rs_data-vicmsstret  = ls_result-vicmsstret  / ls_result-menge.
        rs_data-pst         = ls_result-pst.
        rs_data-vicmssubsti = ls_result-vicmssubstituto / ls_result-menge.
        rs_data-vbcfcpstret = ls_result-vbcfcpstret / ls_result-menge.
        rs_data-vfcpstret   = ls_result-vfcpstret   / ls_result-menge.
        rs_data-pfcpstret   = ls_result-pfcpstret.

      ELSE.

        SELECT SINGLE
          docnum,
          itmnum,
          CASE
            WHEN base IS INITIAL THEN othbas
            ELSE base
          END    AS vbcstret,
          taxval AS vicmsstret
        FROM j_1bnfstx
       WHERE docnum EQ @ls_result-docnum
         AND itmnum EQ @ls_result-itmnum
         AND taxtyp IN @gt_st_retido_taxtype_icms
        INTO CORRESPONDING FIELDS OF @ls_othertax.

        SELECT SINGLE
          docnum,
          itmnum,
          rate   AS pst,
          taxval AS vicmssubstituto
        FROM j_1bnfstx
       WHERE docnum EQ @ls_result-docnum
         AND itmnum EQ @ls_result-itmnum
         AND taxtyp IN @gt_st_retido_taxtype_st
        INTO CORRESPONDING FIELDS OF @ls_othertax.

        SELECT SINGLE
          docnum,
          itmnum,
          CASE
            WHEN base IS INITIAL THEN othbas
            ELSE base
          END    AS vbcfcpstret,
          taxval AS vfcpstret,
          rate   AS pfcpstret
        FROM j_1bnfstx
       WHERE docnum EQ @ls_result-docnum
         AND itmnum EQ @ls_result-itmnum
         AND taxtyp IN @gt_st_retido_taxtype_fcp_st
        INTO CORRESPONDING FIELDS OF @ls_othertax.

        IF ls_othertax IS NOT INITIAL.
          rs_data-docnum      = ls_result-docnum.
          rs_data-itmnum      = ls_result-itmnum.
          rs_data-matnr       = ls_result-matnr.
          rs_data-werks       = ls_result-werks.
          rs_data-taxsit      = ls_result-taxsit.
          rs_data-vbcstret    = ls_othertax-vbcstret   / ls_result-menge.
          rs_data-vicmsstret  = ls_othertax-vicmsstret / ls_result-menge.
          rs_data-pst         = ls_othertax-pst.
          rs_data-vicmssubsti = ls_othertax-vicmssubstituto / ls_result-menge.
          rs_data-vbcfcpstret = ls_othertax-vbcfcpstret / ls_result-menge.
          rs_data-vfcpstret   = ls_othertax-vfcpstret   / ls_result-menge.
          rs_data-pfcpstret   = ls_othertax-pfcpstret.
        ENDIF.

      ENDIF.

    ELSE.

      SELECT
        nfenum,
        series,
        matnr,
        werks,
        cst_icms,
        menge,
        bc_icms,
        v_icms_pr,
        bc_icms_st,
        v_icms_st,
        bc_icms_st_fcp,
        v_icms_st_fcp,
        v_icms_rate,
        v_icms_rate_st,
        v_icms_rate_fcp
      FROM ztmm_hist_cst60
     WHERE matnr    EQ @iv_material
       AND cst_icms IN @gt_st_retido_cst_entrada
       AND cfop     IN @gt_st_retido_cfops_entrada
       AND werks    EQ @iv_plant
  ORDER BY credat DESCENDING
      INTO @DATA(ls_hist_cst60)
     UP TO 1 ROWS.
      ENDSELECT.

      IF sy-subrc IS INITIAL.

        DATA(lv_cst_icms) = ls_hist_cst60-cst_icms.
        CALL FUNCTION 'CONVERSION_EXIT_TXSIT_INPUT'
          EXPORTING
            input  = lv_cst_icms
          IMPORTING
            output = lv_cst_icms.

        rs_data-docnum      = ls_hist_cst60-nfenum.
        rs_data-itmnum      = ls_hist_cst60-series.
        rs_data-matnr       = ls_hist_cst60-matnr.
        rs_data-werks       = ls_hist_cst60-werks.
        rs_data-taxsit      = COND #( WHEN lv_cst_icms IS NOT INITIAL THEN lv_cst_icms ELSE ls_hist_cst60-cst_icms ).
        rs_data-vbcstret    = ls_hist_cst60-bc_icms_st     / ls_hist_cst60-menge.
        rs_data-vicmsstret  = ls_hist_cst60-v_icms_st      / ls_hist_cst60-menge.
        rs_data-pst         = ls_hist_cst60-v_icms_rate_st.
        rs_data-vicmssubsti = ls_hist_cst60-v_icms_pr      / ls_hist_cst60-menge.
        rs_data-vbcfcpstret = ls_hist_cst60-bc_icms_st_fcp / ls_hist_cst60-menge.
        rs_data-vfcpstret   = ls_hist_cst60-v_icms_st_fcp  / ls_hist_cst60-menge.
        rs_data-pfcpstret   = ls_hist_cst60-v_icms_rate_fcp.
      ENDIF.

    ENDIF.

    CHECK rs_data IS INITIAL.

    rs_data-matnr       = iv_material.
    rs_data-werks       = iv_plant.
    rs_data-taxsit      = ''.
    rs_data-vbcstret    = 0.
    rs_data-vicmsstret  = 0.
    rs_data-pst         = 0.
    rs_data-vicmssubsti = 0.
    rs_data-vbcfcpstret = 0.
    rs_data-vfcpstret   = 0.
    rs_data-pfcpstret   = 0.

  ENDMETHOD.


  METHOD get_parameters.
    TRY.
        zclca_tabela_parametros=>get_instance( )->m_get_range(
          EXPORTING
            iv_modulo = me->gc_module
            iv_chave1 = me->gc_param_chave1-st_retido_cst_60
            iv_chave2 = me->gc_param_chave2-cst_entrada
          IMPORTING
            et_range  = gt_st_retido_cst_entrada
        ).

        LOOP AT me->gt_st_retido_cst_entrada ASSIGNING FIELD-SYMBOL(<fs_cst>).
          DATA(ls_cst_in) = <fs_cst>.

          CALL FUNCTION 'CONVERSION_EXIT_TXSIT_INPUT'
            EXPORTING
              input  = ls_cst_in-low
            IMPORTING
              output = ls_cst_in-low.

          IF ls_cst_in-low IS NOT INITIAL.
            APPEND ls_cst_in TO me->gt_st_retido_cst_entrada_in.
            APPEND ls_cst_in TO me->gt_St_retido_cst_entrada.
          ENDIF.
        ENDLOOP.

      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = CONV #( if_xo_const_message~error )
            textid   = VALUE #(
              msgid = me->gc_message_id
              msgno = 003
              attr1 = |{ me->gc_module }/{ me->gc_param_chave1-st_retido_cst_60 }/{ me->gc_param_chave2-cst_entrada }|
            ).
    ENDTRY.

    TRY.
        zclca_tabela_parametros=>get_instance( )->m_get_range(
          EXPORTING
            iv_modulo = me->gc_module
            iv_chave1 = me->gc_param_chave1-st_retido_cst_60
            iv_chave2 = me->gc_param_chave2-cst_ja_retido
          IMPORTING
            et_range  = gt_st_retido_cst_ja_retido
        ).

        LOOP AT me->gt_st_retido_cst_ja_retido INTO DATA(ls_cst).
          CALL FUNCTION 'CONVERSION_EXIT_TXSIT_INPUT'
            EXPORTING
              input  = ls_cst-low
            IMPORTING
              output = ls_cst-low.

          APPEND ls_cst TO me->gt_st_retido_cst_ja_retido_in.
        ENDLOOP.

      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = CONV #( if_xo_const_message~error )
            textid   = VALUE #(
              msgid = me->gc_message_id
              msgno = 003
              attr1 = |{ me->gc_module }/{ me->gc_param_chave1-st_retido_cst_60 }/{ me->gc_param_chave2-cst_ja_retido }|
            ).
    ENDTRY.

    TRY.
        zclca_tabela_parametros=>get_instance( )->m_get_range(
          EXPORTING
            iv_modulo = me->gc_module
            iv_chave1 = me->gc_param_chave1-st_retido_cst_60
            iv_chave2 = me->gc_param_chave2-cfops_entrada
          IMPORTING
            et_range  = gt_st_retido_cfops_entrada
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = CONV #( if_xo_const_message~error )
            textid   = VALUE #(
              msgid = me->gc_message_id
              msgno = 003
              attr1 = |{ me->gc_module }/{ me->gc_param_chave1-st_retido_cst_60 }/{ me->gc_param_chave2-cfops_entrada }|
            ).

    ENDTRY.

    TRY.
        zclca_tabela_parametros=>get_instance( )->m_get_range(
          EXPORTING
            iv_modulo = me->gc_module
            iv_chave1 = me->gc_param_chave1-st_retido_cst_60
            iv_chave2 = me->gc_param_chave2-taxtype_st_entrada
          IMPORTING
            et_range  = gt_st_retido_taxtype_st
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = CONV #( if_xo_const_message~error )
            textid   = VALUE #(
              msgid = me->gc_message_id
              msgno = 003
              attr1 = |{ me->gc_module }/{ me->gc_param_chave1-st_retido_cst_60 }/{ me->gc_param_chave2-taxtype_st_entrada }|
            ).

    ENDTRY.

    TRY.
        zclca_tabela_parametros=>get_instance( )->m_get_range(
          EXPORTING
            iv_modulo = me->gc_module
            iv_chave1 = me->gc_param_chave1-st_retido_cst_60
            iv_chave2 = me->gc_param_chave2-taxtype_icms_entrada
          IMPORTING
            et_range  = gt_st_retido_taxtype_icms
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = CONV #( if_xo_const_message~error )
            textid   = VALUE #(
              msgid = me->gc_message_id
              msgno = 003
              attr1 = |{ me->gc_module }/{ me->gc_param_chave1-st_retido_cst_60 }/{ me->gc_param_chave2-taxtype_icms_entrada }|
            ).

    ENDTRY.

    TRY.
        zclca_tabela_parametros=>get_instance( )->m_get_range(
          EXPORTING
            iv_modulo = me->gc_module
            iv_chave1 = me->gc_param_chave1-st_retido_cst_60
            iv_chave2 = me->gc_param_chave2-taxtype_fcp_entrada
          IMPORTING
            et_range  = gt_st_retido_taxtype_fcp_st
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = CONV #( if_xo_const_message~error )
            textid   = VALUE #(
              msgid = me->gc_message_id
              msgno = 003
              attr1 = |{ me->gc_module }/{ me->gc_param_chave1-st_retido_cst_60 }/{ me->gc_param_chave2-taxtype_fcp_entrada }|
            ).

    ENDTRY.

    TRY.
        zclca_tabela_parametros=>get_instance( )->m_get_range(
          EXPORTING
            iv_modulo = me->gc_module
            iv_chave1 = me->gc_param_chave1-st_retido_cst_60
            iv_chave2 = me->gc_param_chave2-cst_saida
          IMPORTING
            et_range  = gt_st_retido_cst_saida
        ).

        LOOP AT me->gt_st_retido_cst_saida ASSIGNING <fs_cst>.
          CALL FUNCTION 'CONVERSION_EXIT_TXSIT_INPUT'
            EXPORTING
              input  = <fs_cst>-low
            IMPORTING
              output = <fs_cst>-low.
        ENDLOOP.

      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = CONV #( if_xo_const_message~error )
            textid   = VALUE #(
              msgid = me->gc_message_id
              msgno = 003
              attr1 = |{ me->gc_module }/{ me->gc_param_chave1-st_retido_cst_60 }/{ me->gc_param_chave2-cst_saida }|
            ).
    ENDTRY.

    TRY.
        zclca_tabela_parametros=>get_instance( )->m_get_range(
          EXPORTING
            iv_modulo = me->gc_module
            iv_chave1 = me->gc_param_chave1-st_retido_cst_60
            iv_chave2 = me->gc_param_chave2-cfops_saida
          IMPORTING
            et_range  = gt_st_retido_cfops_saida
        ).
      CATCH zcxca_tabela_parametros.
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            severity = CONV #( if_xo_const_message~error )
            textid   = VALUE #(
              msgid = me->gc_message_id
              msgno = 003
              attr1 = |{ me->gc_module }/{ me->gc_param_chave1-st_retido_cst_60 }/{ me->gc_param_chave2-cfops_saida }|
            ).
    ENDTRY.

  ENDMETHOD.


  METHOD check_is_activated.
    DATA(lv_cfop_found) = abap_false.
    DATA(lv_cst_found)  = abap_false.

    LOOP AT it_nflin ASSIGNING FIELD-SYMBOL(<fs_nfitem>).
      IF <fs_nfitem>-cfop   IN me->gt_st_retido_cfops_saida.
        lv_cfop_found = abap_true.
      ENDIF.

      IF <fs_nfitem>-taxsit IN me->gt_st_retido_cst_saida.
        lv_cst_found = abap_true.
      ENDIF.
    ENDLOOP.

    IF lv_cfop_found = abap_true
   AND lv_cst_found  = abap_true.
      rv_result = abap_true.

      SELECT SINGLE *
        FROM zi_mm_bp_j1b_taxdata
        INTO @gs_partner_data
       WHERE BusinessPartner EQ @is_nfheader-parid
         AND BPType          EQ 'BP_CUST'.

      IF sy-subrc <> 0.
        SELECT SINGLE *
          FROM zi_mm_bp_j1b_taxdata
          INTO @gs_partner_data
         WHERE BusinessPartner EQ @is_nfheader-parid
           AND BPType          EQ 'BP_VEND'.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    TRY.
        me->get_parameters( ).
      CATCH zclca_msg_erro INTO DATA(lo_cx).
        RAISE EXCEPTION TYPE zclca_msg_erro
          EXPORTING
            previous = lo_cx.
    ENDTRY.
  ENDMETHOD.


  METHOD fill_data.
    DATA ls_branch_addr TYPE sadr.
    DATA lv_icms_rate   TYPE j_1btxrate.
    DATA lv_icms_base   TYPE j_1btxbase.
    DATA lv_date_in     TYPE j_1btxdatf.
    DATA lv_icms_exempt TYPE j_1btxexic.

    IF is_nfheader-stains IS INITIAL OR is_nfheader-stains = gc_bp_isento_icms.
      DATA(lv_caller) = COND j_1bcaller( WHEN is_nfitem-reftyp = 'BI' THEN 'SD' ELSE 'MM' ).

      CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
        EXPORTING
          branch            = is_nfheader-branch
          bukrs             = is_nfheader-bukrs
        IMPORTING
          address           = ls_branch_addr
        EXCEPTIONS
          branch_not_found  = 1
          address_not_found = 2
          company_not_found = 3
          OTHERS            = 4.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      DATA(lv_date_out) = sy-datum+6(2) && sy-datum+4(2) && sy-datum(4).
      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          input  = lv_date_out
        IMPORTING
          output = lv_date_in.

      CALL FUNCTION 'J_1B_READ_DYNAMIC_TABLE'
        EXPORTING
          caller         = lv_caller
          country        = 'BR'
          state_from     = ls_branch_addr-regio
          state_to       = is_nfheader-regio
          material       = is_nfitem-matnr
          material_group = is_nfitem-matkl
          material_usage = is_nfitem-matuse
          material_orig  = is_nfitem-matorg
          mwskz          = is_nfitem-mwskz
          vendor         = is_nfheader-parid
          customer       = is_nfheader-parid
          ncm            = is_nfitem-nbm
          date           = lv_date_in
          icms           = abap_true
          i_bukrs        = is_nfheader-bukrs
          i_werks        = is_nfitem-werks
          icmstaxpay     = me->gs_partner_data-IcmsTaxPay
          indtyp         = me->gs_partner_data-IndustrySector
        IMPORTING
          rate           = lv_icms_rate
          base           = lv_icms_base
          exempt         = lv_icms_exempt.

      IF lv_icms_exempt = abap_true AND lv_icms_base IS INITIAL.
        lv_icms_base = 100.
      ENDIF.

      cs_item_data-predbcefet = lv_icms_base.
      cs_item_data-vbcefet    = ( is_nfitem-nfpri * is_nfitem-menge ) * lv_icms_base / 100.
      cs_item_data-picmsefet  = lv_icms_rate.
      cs_item_data-vicmsefet  = cs_item_data-vbcefet * lv_icms_rate / 100.

    ELSE.

      CALL METHOD me->get_tax_contribuinte
        EXPORTING
          iv_docnum   = is_nfheader-docnum
          iv_plant    = is_nfitem-werks
          iv_material = is_nfitem-matnr
        RECEIVING
          rs_data     = DATA(ls_tax_data).

      cs_item_data-vbcstret        = ls_tax_data-vbcstret    * is_nfitem-menge.
      cs_item_data-vicmsstret      = ls_tax_data-vicmsstret  * is_nfitem-menge.
      cs_item_data-pst             = ls_tax_data-pst.
      cs_item_data-vicmssubstituto = ls_tax_data-vicmssubsti * is_nfitem-menge.
      cs_item_data-vbcfcpstret     = ls_tax_data-vbcfcpstret * is_nfitem-menge.
      cs_item_data-vfcpstret       = ls_tax_data-vfcpstret   * is_nfitem-menge.
      cs_item_data-pfcpstret       = ls_tax_data-pfcpstret.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
