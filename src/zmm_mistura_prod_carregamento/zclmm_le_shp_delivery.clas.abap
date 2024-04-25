CLASS zclmm_le_shp_delivery DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_le_shp_delivery_proc .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLMM_LE_SHP_DELIVERY IMPLEMENTATION.


  METHOD if_ex_le_shp_delivery_proc~change_delivery_header.

    CONSTANTS:
      BEGIN OF gc_parametros,
        modulo TYPE ztca_param_mod-modulo VALUE 'TM',
        chave1 TYPE ztca_param_par-chave1 VALUE 'TIPO_EXPEDICAO',
        chave2 TYPE ztca_param_par-chave2 VALUE 'EMPRESAS',
        chave3 TYPE ztca_param_par-chave3 VALUE '',
      END OF gc_parametros .
    DATA:
      lr_empresas TYPE RANGE OF t001k-bukrs.
    FIELD-SYMBOLS:
      <fs_bukrs> TYPE ekko-bukrs.
    TRY.
        DATA(lr_param) = zclca_tabela_parametros=>get_instance( ).
        CLEAR lr_empresas.
        lr_param->m_get_range( EXPORTING iv_modulo = gc_parametros-modulo
                                         iv_chave1 = gc_parametros-chave1
                                         iv_chave2 = gc_parametros-chave2
                               IMPORTING et_range  = lr_empresas ).
      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.

    "TM - Alterar tipo de expedição
    CONSTANTS:
      lc_likp_lfart   TYPE likp-lfart VALUE 'TFDT',
      lc_likp_vbtyp_7 TYPE likp-vbtyp VALUE '7',
      lc_cond_exp_zf  TYPE vsbed      VALUE 'ZF',
      lc_cond_exp_01  TYPE vsbed      VALUE '01'.

    IF cs_likp-vbtyp = lc_likp_vbtyp_7 OR cs_likp-lfart = lc_likp_lfart.
      IF cs_likp-werks IS NOT INITIAL.
        SELECT SINGLE bukrs
          FROM t001k
          WHERE bwkey = @cs_likp-werks
          INTO @DATA(lv_empresa).
        IF sy-subrc = 0.
          lv_empresa = lv_empresa.
        ENDIF.
      ELSE.
        ASSIGN ('(SAPFV50K)EKKO-BUKRS') TO <fs_bukrs>.
        IF sy-subrc = 0 AND <fs_bukrs> IS NOT INITIAL.
          lv_empresa = <fs_bukrs>.
        ELSE.
          DATA(lv_werks) = VALUE #( it_xlips[ 1 ]-werks OPTIONAL ).
          IF lv_werks IS NOT INITIAL.
            SELECT SINGLE bukrs
              FROM t001k
              WHERE bwkey = @lv_werks
              INTO @lv_empresa.
          ENDIF.
        ENDIF.
      ENDIF.
      IF lv_empresa IN lr_empresas.
        DATA: lv_modfrete TYPE j_1bmodfrete_det-modfrete.

        SELECT SINGLE modfrete
          FROM j_1bmodfrete_det
          INTO lv_modfrete
          WHERE inco1 = cs_likp-inco1.
        IF sy-subrc = 0.
          DATA(lv_encontrou) = abap_true.
        ENDIF.

        cs_likp-vsbed = SWITCH #( lv_modfrete
          WHEN '0' THEN lc_cond_exp_01
          WHEN '1' THEN lc_cond_exp_zf
          WHEN '9' THEN ''
          ELSE cs_likp-vsbed
        ).
      ENDIF.
    ENDIF.
    IF cs_likp-inco1 IS NOT INITIAL.
      CONSTANTS:
        BEGIN OF gc_parametros_cond_exp,
          modulo TYPE ztca_param_mod-modulo VALUE 'TM',
          chave1 TYPE ztca_param_par-chave1 VALUE 'DETERMINA_CONDICAO_EXPEDICAO',
          chave2 TYPE ztca_param_par-chave2 VALUE 'REMESSA_E_ORDEM_VENDAS',
          chave3 TYPE ztca_param_par-chave3 VALUE '',
        END OF gc_parametros_cond_exp .
      DATA:
        lr_vsbed TYPE RANGE OF char3.
      TRY.
          DATA(lr_param_cond_exp) = zclca_tabela_parametros=>get_instance( ).
          CLEAR lr_vsbed.
          lr_param_cond_exp->m_get_range( EXPORTING iv_modulo = gc_parametros_cond_exp-modulo
                                           iv_chave1 = gc_parametros_cond_exp-chave1
                                           iv_chave2 = gc_parametros_cond_exp-chave2
                                 IMPORTING et_range  = lr_vsbed ).
        CATCH zcxca_tabela_parametros.
      ENDTRY.
      READ TABLE lr_vsbed ASSIGNING FIELD-SYMBOL(<fs_vsbed>) WITH KEY low = cs_likp-inco1. "#EC CI_STDSEQ
      IF sy-subrc = 0.
        cs_likp-vsbed = <fs_vsbed>-high.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~change_delivery_item.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~change_fcode_attributes.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~change_field_attributes.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~check_item_deletion.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~delivery_deletion.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~delivery_final_check.

    DATA: lv_error TYPE boolean.

    DATA: lt_log TYPE HASHED TABLE OF finchdel WITH UNIQUE KEY vbeln,
          ls_log LIKE LINE OF lt_log.

    DATA lo_proc_mistura TYPE REF TO zclmm_proc_mistura.
    CREATE OBJECT lo_proc_mistura.

    FIELD-SYMBOLS <fs_t185f> TYPE any.
    ASSIGN ('(SAPLV00F)T185F-FCODE') TO <fs_t185f>.

    IF <fs_t185f> IS ASSIGNED.
      IF ( <fs_t185f> = 'LOES_T' OR
           <fs_t185f> = 'SICH_T' ).
*      CALL METHOD lo_proc_mistura->proc_estorno
*        EXPORTING
*          it_xlips = it_xlips.
      ELSEIF if_trtyp = 'V' AND
             <fs_t185f> = 'WABU_T'.

        CALL METHOD lo_proc_mistura->process_mistura
          EXPORTING
            it_xlips = it_xlips
          IMPORTING
            ev_error = lv_error.

        IF lv_error = abap_true.

          READ TABLE it_xlips ASSIGNING FIELD-SYMBOL(<fs_xlips>) INDEX 1.
          IF sy-subrc = 0.
            ls_log-vbeln = <fs_xlips>-vbeln.
            ls_log-msgno = 002.
            ls_log-msgid = 'ZMM_MISTURA_COMB'.
            ls_log-msgty = 'E'.
            INSERT ls_log INTO TABLE ct_finchdel.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~document_number_publish.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~fill_delivery_header.
    CONSTANTS:
      BEGIN OF gc_parametros,
        modulo TYPE ztca_param_mod-modulo VALUE 'TM',
        chave1 TYPE ztca_param_par-chave1 VALUE 'TIPO_EXPEDICAO',
        chave2 TYPE ztca_param_par-chave2 VALUE 'EMPRESAS',
        chave3 TYPE ztca_param_par-chave3 VALUE '',
      END OF gc_parametros .
    DATA:
      lr_empresas TYPE RANGE OF t001k-bukrs.
    FIELD-SYMBOLS:
      <fs_bukrs> TYPE ekko-bukrs.
    TRY.
        DATA(lr_param) = zclca_tabela_parametros=>get_instance( ).
        CLEAR lr_empresas.
        lr_param->m_get_range( EXPORTING iv_modulo = gc_parametros-modulo
                                         iv_chave1 = gc_parametros-chave1
                                         iv_chave2 = gc_parametros-chave2
                               IMPORTING et_range  = lr_empresas ).
      CATCH zcxca_tabela_parametros.
    ENDTRY.

    "TM - Alterar tipo de expedição
    CONSTANTS:
      lc_likp_lfart   TYPE likp-lfart VALUE 'TFDT',
      lc_likp_vbtyp_7 TYPE likp-vbtyp VALUE '7',
      lc_cond_exp_zf  TYPE vsbed      VALUE 'ZF',
      lc_cond_exp_01  TYPE vsbed      VALUE '01'.

    IF cs_likp-vbtyp = lc_likp_vbtyp_7 OR cs_likp-lfart = lc_likp_lfart.
      IF cs_likp-werks IS NOT INITIAL.
        SELECT SINGLE bukrs
          FROM t001k
          WHERE bwkey = @cs_likp-werks
          INTO @DATA(lv_empresa).
        IF sy-subrc = 0.
          lv_empresa = lv_empresa.
        ENDIF.
      ELSE.
        ASSIGN ('(SAPFV50K)EKKO-BUKRS') TO <fs_bukrs>.
        IF sy-subrc = 0 AND <fs_bukrs> IS NOT INITIAL.
          lv_empresa = <fs_bukrs>.
        ELSE.
          DATA(lv_werks) = VALUE #( it_xlips[ 1 ]-werks OPTIONAL ).
          IF lv_werks IS NOT INITIAL.
            SELECT SINGLE bukrs
              FROM t001k
              WHERE bwkey = @lv_werks
              INTO @lv_empresa.
          ENDIF.
        ENDIF.
      ENDIF.
      IF lv_empresa IN lr_empresas.
        DATA: lv_modfrete TYPE j_1bmodfrete_det-modfrete.

        SELECT SINGLE modfrete
          FROM j_1bmodfrete_det
          INTO lv_modfrete
          WHERE inco1 = cs_likp-inco1.
        IF sy-subrc = 0.
          DATA(lv_encontrou) = abap_true.
        ENDIF.

        cs_likp-vsbed = SWITCH #( lv_modfrete
          WHEN '0' THEN lc_cond_exp_01
          WHEN '1' THEN lc_cond_exp_zf
          WHEN '9' THEN ''
          ELSE cs_likp-vsbed
        ).
      ENDIF.
    ENDIF.


    IF cs_likp-inco1 IS NOT INITIAL.
      CONSTANTS:
        BEGIN OF gc_parametros_cond_exp,
          modulo TYPE ztca_param_mod-modulo VALUE 'TM',
          chave1 TYPE ztca_param_par-chave1 VALUE 'DETERMINA_CONDICAO_EXPEDICAO',
          chave2 TYPE ztca_param_par-chave2 VALUE 'REMESSA_E_ORDEM_VENDAS',
          chave3 TYPE ztca_param_par-chave3 VALUE '',
        END OF gc_parametros_cond_exp .
      DATA:
        lr_vsbed TYPE RANGE OF char3.
      TRY.
          DATA(lr_param_cond_exp) = zclca_tabela_parametros=>get_instance( ).
          CLEAR lr_vsbed.
          lr_param_cond_exp->m_get_range( EXPORTING iv_modulo = gc_parametros_cond_exp-modulo
                                           iv_chave1 = gc_parametros_cond_exp-chave1
                                           iv_chave2 = gc_parametros_cond_exp-chave2
                                 IMPORTING et_range  = lr_vsbed ).
        CATCH zcxca_tabela_parametros.
      ENDTRY.
      READ TABLE lr_vsbed ASSIGNING FIELD-SYMBOL(<fs_vsbed>) WITH KEY low = cs_likp-inco1. "#EC CI_STDSEQ
      IF sy-subrc = 0.
        cs_likp-vsbed = <fs_vsbed>-high.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~fill_delivery_item.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~initialize_delivery.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~item_deletion.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~publish_delivery_item.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~read_delivery.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~save_and_publish_before_output.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~save_and_publish_document.
**    DATA: lv_error TYPE boolean.
    DATA lo_proc_mistura TYPE REF TO zclmm_proc_mistura.
    CREATE OBJECT lo_proc_mistura.

    FIELD-SYMBOLS <fs_t185f> TYPE any.
    ASSIGN ('(SAPLV00F)T185F-FCODE') TO <fs_t185f>.

    IF sy-tcode = 'VL09'      AND
       <fs_t185f> IS ASSIGNED AND
       <fs_t185f> IS INITIAL.
      CALL METHOD lo_proc_mistura->proc_estorno
        EXPORTING
          it_xlips = it_xlips.
    ENDIF.
**    ELSE.
**
**      CALL METHOD lo_proc_mistura->process_mistura
**        EXPORTING
**          it_xlips = it_xlips
**        IMPORTING
**          ev_error = lv_error.
**
**      IF lv_error = abap_true.
**        MESSAGE e002(zmm_mistura_comb).
**      ENDIF.
**
**    ENDIF.

  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~save_document_prepare.
    RETURN.
  ENDMETHOD.
ENDCLASS.
