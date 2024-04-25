class ZCLMM_ENHANCEMENTS definition
  public
  final
  create public .

public section.

  types TY_ACSDEF type OIH_J1B_ACSDEF .
  types:
    ty_t_acsdef TYPE SORTED TABLE OF ty_acsdef WITH NON-UNIQUE KEY acsid acsseq .
  types:
    BEGIN OF ty_acsval,
        acsid     TYPE oih_j1b_acsval-acsid,
        acsseq    TYPE oih_j1b_acsval-acsseq,
        varkey    TYPE oih_j1b_acsval-varkey,
        validfrom TYPE oih_j1b_acsval-validfrom,
        validto   TYPE oih_j1b_acsval-validto,
        value     TYPE oih_j1b_acsval-value,
        value01   TYPE oih_j1b_acsval-value01,
        found     TYPE flag,
        number    TYPE f,
        number1   TYPE f,
      END OF ty_acsval .
  types:
    ty_t_acsval  TYPE SORTED TABLE OF ty_acsval WITH NON-UNIQUE KEY acsid acsseq varkey validfrom .
  types:
    ty_big_p(12) TYPE p DECIMALS 6 .

  constants GC_COF type J_1BAJ-TAXGRP value 'COFI' ##NO_TEXT.
  constants GC_PIS type J_1BAJ-TAXGRP value 'PIS' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCLMM_ENHANCEMENTS .
  methods M_DRC_ICMS_MONOFASICO
    importing
      !IV_VALUE type J_1BNFE_RFC_TAXVAL
      !IV_ORIG type STRING
    changing
      !CV_NETPR type J_1BNETPRI
      !CV_ORIG type J_1BMATORG .
  methods M_DRC_DEL_DUPLIC_FUEL
    changing
      !CT_FUEL type J_1BNFE_T_FUEL .
  class-methods M_DRC_PIS_COFINS_PAUTA_CALC
    importing
      !IS_NF_ITEM type J_1BNFLIN
      !IT_NF_TAX type J_1BNFSTX_TAB
    changing
      !CS_EXIT type XFELD .
  class-methods M_DRC_PIS_COFINS_PAUTA_NFE
    importing
      !IS_NF_HEADER type J_1BNFDOC
      !IT_NF_ITEM type TY_J_1BNFLIN
    changing
      !CT_NF_ITEM_TAX type TY_J_1BNFSTX .
  class-methods M_ICMS_ST_STATISTICAL
    importing
      !IT_NF_ITEM_TAX type TY_J_1BNFSTX
    changing
      !CS_EXT_HEADER type J_1BINDOC .
  class-methods M_OIH_CALC_ICMS_MONOFASICO
    importing
      !IS_KOMK type KOMK
      !IS_KOMP type KOMP
      !IS_OIH_J1B_TXCD_CALC type OIH_J1B_TXCD_CALC
      !IS_OIH_J1B_ACSFLD type OIH_J1B_ACSFLD
      !IV_MIX_AMOUNT type TY_BIG_P
      !IV_MIX_QUANTITY type TY_BIG_P
    exporting
      !ES_RETURN type BAPIRET2
    changing
      !CV_AMOUNT type TY_BIG_P
      !CV_QBCMONORET type J_1BNF_QBCMONORET
      !CV_VICMSMONORET type J_1BNF_VICMSMONORET .
  methods M_OIL_CTE_EXC_DINAMICA_ICMS
    importing
      !IV_FREIGHT type CHAR1
      !IV_CALLER type CHAR2
      !IS_GLOBAL_KOMP type KOMP
      !IS_GLOBAL_KOMK type KOMK
      !IS_GLOBAL_007A type T007A
      !IT_J_1BTXIC1 type J_1BTXIC1
      !IT_J_1BTXIC3 type ZCTGTM_J_1BTXIC3
    changing
      !CV_RATE type J_1BTXIC3-RATE
      !CS_NF_LAWS type J_1BTXLAWS .
  methods M_DRC_DEL_PIS_FUTURA
    importing
      !IS_NFE_DET type EDOC_BR_NFE400NFE_DET
    changing
      !CT_MAT_MAX type J_1BNFE_T_RFC_IN_GOOD_TAX .
  methods M_DRC_DEL_COFINS_FUTURA
    importing
      !IS_NFE_DET type EDOC_BR_NFE400NFE_DET
    changing
      !CT_MAT_MAX type J_1BNFE_T_RFC_IN_GOOD_TAX .
  methods M_DRC_DEL_MSG_ICMS_FUTURA
    importing
      !IV_PROCESS type J_1BNFE_PROCESS
      !IV_TAXSIT type J_1BTAXSIT_ALL
    changing
      !CT_RETURN type BAPIRETTAB .
  class-methods M_PIS_COFINS_BASEOUTRAS
    importing
      !IS_NF_HEADER type J_1BNFDOC
      !IT_NF_ITEM type TY_J_1BNFLIN
    changing
      !CT_NF_ITEM_TAX type TY_J_1BNFSTX .
  PROTECTED SECTION.
private section.

  class-data GO_INSTANCE type ref to ZCLMM_ENHANCEMENTS .
  data GT_ACSDEF type TY_T_ACSDEF .
  data GT_ACSVAL type TY_T_ACSVAL .
  constants LC_MODULO_MM type ZTCA_PARAM_MOD-MODULO value 'MM' ##NO_TEXT.
  constants LC_CHAVE1_1 type ZTCA_PARAM_PAR-CHAVE1 value 'DRC_ICMS_MONOF' ##NO_TEXT.
  constants LC_CHAVE2_1 type ZTCA_PARAM_PAR-CHAVE2 value 'ATIVA' ##NO_TEXT.
  constants LC_CHAVE3_1 type ZTCA_PARAM_PAR-CHAVE3 value '' ##NO_TEXT.
  constants GC_CHAVE2 type ZTCA_PARAM_PAR-CHAVE2 value 'CST_ICMS' ##NO_TEXT.
  constants GC_ACSID_034 type OIH_J1B_ACSDEF-ACSID value '034' ##NO_TEXT.
  constants:
    BEGIN OF gc_param_icms_st_negativo,
        modulo TYPE ztca_param_mod-modulo VALUE 'MM' ##NO_TEXT,
        chave1 TYPE ztca_param_par-chave1 VALUE 'ICMS_ST' ##NO_TEXT,
        chave2 TYPE ztca_param_par-chave2 VALUE 'NEGATIVO' ##NO_TEXT,
      END OF gc_param_icms_st_negativo .
  constants:
    BEGIN OF gc_param_cfop_entrega_futura,
        modulo TYPE ztca_param_mod-modulo VALUE 'MM' ##NO_TEXT,
        chave2 TYPE ztca_param_par-chave2 VALUE 'ENTREGA_FUTURA' ##NO_TEXT,
        chave3 TYPE ztca_param_par-chave3 VALUE 'CFOP_EM' ##NO_TEXT,
      END OF gc_param_cfop_entrega_futura .
  constants:
    BEGIN OF gc_oih_calc_icms_monofasico,
        modulo TYPE ztca_param_mod-modulo VALUE 'MM' ##NO_TEXT,
        chave1 TYPE ztca_param_par-chave1 VALUE 'ICMS_MONOF' ##NO_TEXT,
        chave2 TYPE ztca_param_par-chave2 VALUE 'CST61' ##NO_TEXT,
        chave3 TYPE ztca_param_par-chave3 VALUE 'ATIVO' ##NO_TEXT,
      END OF gc_oih_calc_icms_monofasico .
  constants GC_CHAVE1_2 type ZTCA_PARAM_PAR-CHAVE1 value 'PIS_COFINS_BASEOUTRAS' ##NO_TEXT.

  methods GET_OIH_J1B_ACS
    importing
      !IV_ACSID type OIH_J1B_ACSDEF-ACSID
    exporting
      !ET_ACSDEF type TY_T_ACSDEF
      !ET_ACSVAL type TY_T_ACSVAL .
ENDCLASS.



CLASS ZCLMM_ENHANCEMENTS IMPLEMENTATION.


  METHOD m_drc_del_duplic_fuel.

    DATA(lo_param) = zclca_tabela_parametros=>get_instance( ).
    DATA lv_ativo TYPE c LENGTH 1.

    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_1
            iv_chave2 = lc_chave2_1
            iv_chave3 = lc_chave3_1
          IMPORTING
            ev_param  = lv_ativo
        ).

        IF lv_ativo EQ 'X' AND ct_fuel[] IS NOT INITIAL.
          DELETE ct_fuel WHERE cprodanp IS INITIAL.      "#EC CI_STDSEQ
        ENDIF.

      CATCH zcxca_tabela_parametros.

    ENDTRY.

  ENDMETHOD.


  METHOD m_drc_icms_monofasico.

    DATA(lo_param) = zclca_tabela_parametros=>get_instance( ).
    DATA lv_ativo TYPE c LENGTH 1.

    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_1
            iv_chave2 = lc_chave2_1
            iv_chave3 = lc_chave3_1
          IMPORTING
            ev_param  = lv_ativo
        ).

        IF lv_ativo EQ 'X' AND iv_value IS NOT INITIAL.
          cv_netpr = ( cv_netpr - iv_value ).
          IF iv_orig IS NOT INITIAL.
            cv_orig = iv_orig.
          ENDIF.

        ENDIF.

      CATCH zcxca_tabela_parametros.

    ENDTRY.

  ENDMETHOD.


  METHOD m_drc_pis_cofins_pauta_calc.

    DATA lr_range TYPE RANGE OF j_1btaxsit.
    DATA(lo_param) = zclca_tabela_parametros=>get_instance( ).
    DATA lv_ativo TYPE c LENGTH 1.

    "Verificar se o ICMS Monofásico está ativo.
    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_1
            iv_chave2 = lc_chave2_1
            iv_chave3 = lc_chave3_1
          IMPORTING
            ev_param  = lv_ativo
        ).

        TRY.
            lo_param->m_get_range(
              EXPORTING
                iv_modulo = lc_modulo_mm
                iv_chave1 = lc_chave1_1
                iv_chave2 = gc_chave2
              IMPORTING
                et_range  = lr_range
            ).

            "Verificar se o item da nota fiscal possui CST de ICMS Monofásico
            IF lv_ativo EQ 'X' AND
               it_nf_tax IS NOT INITIAL AND is_nf_item-taxsit IN lr_range.

              IF  sy-subrc EQ 0.

                cs_exit = 'X'.

              ENDIF.

            ENDIF.

          CATCH zcxca_tabela_parametros.

        ENDTRY.

      CATCH zcxca_tabela_parametros.

    ENDTRY.


  ENDMETHOD.


  METHOD m_drc_pis_cofins_pauta_nfe.

    DATA(lo_param) = zclca_tabela_parametros=>get_instance( ).
    DATA lv_ativo TYPE c LENGTH 1.
    DATA lr_range TYPE RANGE OF j_1btaxsit.

    IF is_nf_header-docnum IS INITIAL AND
       is_nf_header-doctyp EQ '1' AND      "Nota Fiscal
       is_nf_header-direct EQ '1' AND      "Entrada
       is_nf_header-autom_incoming EQ 'X'. "Entrada pelo DRC


      "Verificar se o ICMS Monofásico está ativo.
      TRY.
          lo_param->m_get_single(
            EXPORTING
              iv_modulo = lc_modulo_mm
              iv_chave1 = lc_chave1_1
              iv_chave2 = lc_chave2_1
              iv_chave3 = lc_chave3_1
            IMPORTING
              ev_param  = lv_ativo
          ).

          IF lv_ativo EQ 'X'.

            SORT ct_nf_item_tax BY itmnum ASCENDING
                                   taxgrp ASCENDING.

            TRY.
                lo_param->m_get_range(
                  EXPORTING
                    iv_modulo = lc_modulo_mm
                    iv_chave1 = lc_chave1_1
                    iv_chave2 = gc_chave2
                  IMPORTING
                    et_range  = lr_range
                ).

                LOOP AT it_nf_item ASSIGNING FIELD-SYMBOL(<fs_item>).
                  IF lr_range IS NOT INITIAL AND <fs_item>-taxsit IN lr_range.

                    "Leitura do COFINS
                    READ TABLE ct_nf_item_tax ASSIGNING FIELD-SYMBOL(<fs_nf_item_tax_cofi>) WITH KEY itmnum = <fs_item>-itmnum
                                                                                                     taxgrp = gc_cof BINARY SEARCH. "COFINS
                    IF sy-subrc EQ 0.

                      "Se for um COFINS pautado calcula a base.
                      IF <fs_nf_item_tax_cofi>-factor GT 0.
                        <fs_nf_item_tax_cofi>-base = <fs_item>-menge.
                      ENDIF.

                    ENDIF.

                    "Leitura do PIS
                    READ TABLE ct_nf_item_tax ASSIGNING FIELD-SYMBOL(<fs_nf_item_tax_pis>) WITH KEY itmnum = <fs_item>-itmnum
                                                                                                    taxgrp = gc_pis BINARY SEARCH. "PIS
                    IF sy-subrc EQ 0.

                      "Se for um PIS pautado calcula a base.
                      IF <fs_nf_item_tax_pis>-factor GT 0.
                        <fs_nf_item_tax_pis>-base = <fs_item>-menge.
                      ENDIF.

                    ENDIF.
                  ELSE.

                    CONTINUE.

                  ENDIF.

                ENDLOOP.

              CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros
            ENDTRY.

          ENDIF.

        CATCH zcxca_tabela_parametros.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD m_icms_st_statistical.

    DATA: lr_taxtyp TYPE RANGE OF j_1bnfstx-taxtyp.

* ---------------------------------------------------------------------------
* Recupera parâmetro - Tipo de Imposto (ICMS, não dedutível, substit.)
* ---------------------------------------------------------------------------
    DATA(lo_param) = zclca_tabela_parametros=>get_instance( ).

    TRY.
        lo_param->m_get_range( EXPORTING iv_modulo = gc_param_icms_st_negativo-modulo
                                         iv_chave1 = gc_param_icms_st_negativo-chave1
                                         iv_chave2 = gc_param_icms_st_negativo-chave2
                               IMPORTING et_range  = lr_taxtyp ).
      CATCH zcxca_tabela_parametros.
        FREE lr_taxtyp.
    ENDTRY.

* ---------------------------------------------------------------------------
* Somente prosseguir se o parâmetro estiver cadastrado
* ---------------------------------------------------------------------------
    IF lr_taxtyp IS INITIAL.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Recalcula o valor total da Nota Fiscal
* ---------------------------------------------------------------------------
    DATA(lv_nftot_original) = cs_ext_header-nftot.

    LOOP AT it_nf_item_tax REFERENCE INTO DATA(ls_nf_item_tax).

      " Verifica se existe alguma linha com o imposto cadastrado
      CHECK ls_nf_item_tax->taxtyp IN lr_taxtyp AND lr_taxtyp IS NOT INITIAL.
      CHECK ls_nf_item_tax->stattx EQ abap_true.

      " Desconsidera o valor do imposto do total
      cs_ext_header-nftot = cs_ext_header-nftot - ls_nf_item_tax->taxval.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_instance.

* ---------------------------------------------------------------------------
* Caso a instância não exista, criar uma nova
* ---------------------------------------------------------------------------
    IF NOT go_instance IS BOUND.
      go_instance = NEW zclmm_enhancements( ).
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.


  METHOD m_oih_calc_icms_monofasico.

    DATA: lv_field     TYPE string,
          lv_field_val TYPE oih_j1b_acsval-varkey,
          lv_fdpos     TYPE sy-fdpos.

    FREE: es_return.

* ---------------------------------------------------------------------------
* Recupera parâmetro - Ativa/Desativa solução
* ---------------------------------------------------------------------------
    DATA(lo_param) = zclca_tabela_parametros=>get_instance( ).

    TRY.
        DATA(lv_ativo) = abap_false.

        lo_param->m_get_single( EXPORTING iv_modulo = gc_oih_calc_icms_monofasico-modulo
                                          iv_chave1 = gc_oih_calc_icms_monofasico-chave1
                                          iv_chave2 = gc_oih_calc_icms_monofasico-chave2
                                          iv_chave3 = gc_oih_calc_icms_monofasico-chave3
                                IMPORTING ev_param  = lv_ativo ).
      CATCH zcxca_tabela_parametros.
        FREE lv_ativo.
    ENDTRY.

* ---------------------------------------------------------------------------
* Somente prosseguir se o parâmetro estiver cadastrado
* ---------------------------------------------------------------------------
    IF lv_ativo IS INITIAL.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Recupera em memória as informações de acesso Oil
* ---------------------------------------------------------------------------
    DATA(lo_memory) = zclmm_enhancements=>get_instance( ).

    lo_memory->get_oih_j1b_acs( EXPORTING iv_acsid  = gc_acsid_034
                                IMPORTING et_acsdef = DATA(lt_acsdef)
                                          et_acsval = DATA(lt_acsval) ).

* ---------------------------------------------------------------------------
* Lógica para determinar a regra de acesso.
*
* Referência: Inlcude SAPLOIH_J1BR_01 FORM oih_j1b_acs_get_value
* ---------------------------------------------------------------------------

    LOOP AT lt_acsdef INTO DATA(ls_acsdef).

      DATA(lv_varkey) = CONV oih_j1b_acsval-varkey( '' ).
      lv_fdpos        = 0.

      " Separa os campos de acordo com o delimitador
      SPLIT ls_acsdef-varkeyf AT ls_acsdef-delimiter INTO TABLE DATA(lt_field_def).

      DESCRIBE TABLE lt_field_def LINES DATA(lv_tfill).

      LOOP AT lt_field_def INTO DATA(lv_field_def).

        DATA(lv_tabix)  = sy-tabix.

        " Recupera o campo de acordo com a estrutura
        DATA(lv_tabname) = SWITCH tabname( lv_field_def(1)
                                           WHEN '1' THEN 'IS_KOMK'
                                           WHEN '2' THEN 'IS_KOMP'
                                           WHEN '3' THEN 'IS_OIH_J1B_TXCD_CALC'
                                           WHEN '4' THEN 'IS_OIH_J1B_ACSFLD'
                                           ELSE '' ).

        IF lv_tabname IS INITIAL.
          " ID tabelas: 1=KOMK, 2=KOMP, 3=OIH_J1B_TXCD_CALC, 4=OIH_J1B_ACSFLD
          es_return = VALUE #( type = 'E' id = 'OIH_J1B' number = '024' ).
          RETURN.
        ENDIF.

        " Acessa o valor do campo
        CONCATENATE lv_tabname lv_field_def+2 INTO lv_field SEPARATED BY '-'.

        ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_value>).
        CHECK <fs_value> IS ASSIGNED.
        WRITE <fs_value> TO lv_field_val.

        sy-fdpos = strlen( lv_field_val ).

        " Monta chave com a combinação dos valores
        IF sy-fdpos IS NOT INITIAL.
          lv_varkey+lv_fdpos = lv_field_val(sy-fdpos).
          lv_fdpos  = lv_fdpos + sy-fdpos.
        ENDIF.

        IF lv_tabix < lv_tfill.
          lv_varkey+lv_fdpos = ls_acsdef-delimiter.
          lv_fdpos  = lv_fdpos + 1.
        ENDIF.

      ENDLOOP.

      " Verifica se o valor de acesso existe
      READ TABLE lt_acsval INTO DATA(ls_acsval) WITH KEY acsid  = ls_acsdef-acsid
                                                         acsseq = ls_acsdef-acsseq
                                                         varkey = lv_varkey.

      IF sy-subrc EQ 0.
        "Registro encontrado
        EXIT.
      ENDIF.

    ENDLOOP.

* ---------------------------------------------------------------------------
* Se a regra estiver de acordo, temos que remover o valor e quantidade da mistura
* ---------------------------------------------------------------------------
    IF ls_acsval IS NOT INITIAL.
      cv_amount        = cv_amount - iv_mix_amount.
      cv_qbcmonoret    = cv_qbcmonoret - iv_mix_quantity.
      cv_vicmsmonoret  = cv_amount.
    ENDIF.

  ENDMETHOD.


  METHOD get_oih_j1b_acs.

    FREE: et_acsdef, et_acsval.

* ---------------------------------------------------------------------------
* Recupera e guarda em memória as informações de acesso Oil
* ---------------------------------------------------------------------------
    IF NOT line_exists( gt_acsdef[ acsid = iv_acsid ] ).

      " Brasil: definição de acesso OIL
      SELECT *
          FROM oih_j1b_acsdef
          APPENDING TABLE gt_acsdef
          WHERE acsid = iv_acsid.

      DATA(lv_date_inv) = CONV j_1btxdatf( '99999999' - CONV j_1btxdatf( sy-datum ) ).

      " Brasil: valores de acesso Oil
      SELECT *
          FROM oih_j1b_acsval
          APPENDING CORRESPONDING FIELDS OF TABLE gt_acsval
          WHERE acsid     EQ iv_acsid
            AND validfrom GE lv_date_inv
            AND validto   LE lv_date_inv.

    ENDIF.

* ---------------------------------------------------------------------------
* Devolve as informações de acesso Oil
* ---------------------------------------------------------------------------
    et_acsdef = VALUE #( FOR ls_acsdef_ IN gt_acsdef WHERE ( acsid = iv_acsid ) ( ls_acsdef_ ) ).
    et_acsval = VALUE #( FOR ls_acsval_ IN gt_acsval WHERE ( acsid = iv_acsid ) ( ls_acsval_ ) ).

  ENDMETHOD.


  METHOD m_oil_cte_exc_dinamica_icms.

    IF iv_freight   EQ 'X' AND "Processo de frete
       it_j_1btxic1 IS NOT INITIAL AND "Tabela origem e destino de ICMS
       iv_caller    EQ 'IV'. "Entrada da Fatura

      "Verificar a CST de ICMS está preenchida
      IF cs_nf_laws-icmslaw IS INITIAL.
        cs_nf_laws-icmslaw = is_global_007a-j_1btaxlw1.
      ENDIF.

      "Preencher alíquota de ICMS
      READ TABLE it_j_1btxic3 ASSIGNING FIELD-SYMBOL(<fs_btxic3>) WITH KEY not_found = space. "Pegar apenas o primeiro registro encontrado.

      IF <fs_btxic3> IS ASSIGNED.
        cv_rate = <fs_btxic3>-rate. "Preencher com a alíquota de ICMS
        IF <fs_btxic3>-taxlaw IS NOT INITIAL.
          cs_nf_laws-icmslaw = <fs_btxic3>-taxlaw.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD m_drc_del_pis_futura.

    DATA(lo_param) = zclca_tabela_parametros=>get_instance( ).
    DATA lr_cfop_param TYPE RANGE OF j_1bnflin-cfop.
    DATA lv_is_enabled TYPE abap_bool.

    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_1
            iv_chave2 = lc_chave2_1
          IMPORTING
            ev_param  = lv_is_enabled
        ).

        IF lv_is_enabled IS INITIAL .
          RETURN.
        ENDIF.

      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.

    TRY.
        lo_param->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_1
            iv_chave2 = gc_param_cfop_entrega_futura-chave2
            iv_chave3 = gc_param_cfop_entrega_futura-chave3
          IMPORTING
            et_range  = lr_cfop_param
        ).

        IF NOT is_nfe_det-prod-cfop IN lr_cfop_param .
          RETURN.
        ENDIF.

      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.

    "Lógica para eliminar linha de COFINS
    IF is_nfe_det-imposto-icms-icms02 IS NOT INITIAL  OR "Tributação monofásica própria sobre combustíveis.
       is_nfe_det-imposto-icms-icms15 IS NOT INITIAL  OR "Tributação monofásica própria e com responsabilidade pela
       is_nfe_det-imposto-icms-icms53 IS NOT INITIAL  OR "Tributação monofásica sobre combustíveis com recolhimento
       is_nfe_det-imposto-icms-icms61 IS NOT INITIAL.    "Tributação monofásica sobre combustíveis cobrada anteriorm

      IF ct_mat_max IS NOT INITIAL.
        DELETE ct_mat_max WHERE taxgrp EQ gc_pis.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD m_drc_del_cofins_futura.
    DATA lv_is_enabled TYPE abap_bool.
    DATA lr_cfop_param TYPE RANGE OF  j_1bnflin-cfop.

    DATA(lo_param) = zclca_tabela_parametros=>get_instance( ).

    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_1
            iv_chave2 = lc_chave2_1
          IMPORTING
            ev_param  = lv_is_enabled
        ).

        IF lv_is_enabled IS INITIAL.
          RETURN.
        ENDIF.

      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.

    TRY.
        lo_param->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_1
            iv_chave2 = gc_param_cfop_entrega_futura-chave2
            iv_chave3 = gc_param_cfop_entrega_futura-chave3
          IMPORTING
            et_range  = lr_cfop_param
        ).

        IF NOT is_nfe_det-prod-cfop IN lr_cfop_param .
          RETURN.
        ENDIF.

      CATCH zcxca_tabela_parametros.
        RETURN.
    ENDTRY.

    "Lógica para eliminar linha de COFINS
    IF is_nfe_det-imposto-icms-icms02 IS NOT INITIAL  OR "Tributação monofásica própria sobre combustíveis.
       is_nfe_det-imposto-icms-icms15 IS NOT INITIAL  OR "Tributação monofásica própria e com responsabilidade pela
       is_nfe_det-imposto-icms-icms53 IS NOT INITIAL  OR "Tributação monofásica sobre combustíveis com recolhimento
       is_nfe_det-imposto-icms-icms61 IS NOT INITIAL.    "Tributação monofásica sobre combustíveis cobrada anteriorm

      IF ct_mat_max IS NOT INITIAL.
        DELETE ct_mat_max WHERE taxgrp EQ gc_cof.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD m_drc_del_msg_icms_futura.
    CONSTANTS lc_future_delivery_gr TYPE j_1bnfe_process_erp_grc  VALUE '12'.
    CONSTANTS lc_taxsit_02          TYPE j_1btaxsit_all           VALUE '02'.
    CONSTANTS lc_taxsit_15          TYPE j_1btaxsit_all           VALUE '15'.
    CONSTANTS lc_taxsit_53          TYPE j_1btaxsit_all           VALUE '53'.
    CONSTANTS lc_taxsit_61          TYPE j_1btaxsit_all           VALUE '61'.
    CONSTANTS lc_return_id          TYPE char20                   VALUE 'J1B_NFE'.
    CONSTANTS lc_return_number      TYPE numc3                    VALUE '346'.
    CONSTANTS lc_low                TYPE char255                  VALUE 'X'.

    DATA lv_is_enabled TYPE abap_bool.
    DATA(lo_param) = zclca_tabela_parametros=>get_instance( ).

    IF iv_process EQ lc_future_delivery_gr. "Entrega Futura

      TRY.
          lo_param->m_get_single(
            EXPORTING
              iv_modulo = lc_modulo_mm
              iv_chave1 = lc_chave1_1
              iv_chave2 = lc_chave2_1
            IMPORTING
              ev_param  = lv_is_enabled
          ).

          IF lv_is_enabled IS INITIAL.
            RETURN.
          ENDIF.

        CATCH zcxca_tabela_parametros.
          RETURN.
      ENDTRY.

      "Lógica para eliminar mensagem J1B_NFE346 que impede o lançamento do ICMS Monofásico.
      IF iv_taxsit EQ lc_taxsit_02 OR "Tributação monofásica própria sobre combustíveis.
         iv_taxsit EQ lc_taxsit_15 OR "Tributação monofásica própria e com responsabilidade pela
         iv_taxsit EQ lc_taxsit_53 OR "Tributação monofásica sobre combustíveis com recolhimento
         iv_taxsit EQ lc_taxsit_61.   "Tributação monofásica sobre combustíveis cobrada anteriorm

        IF ct_return IS NOT INITIAL.
          DELETE ct_return WHERE id EQ lc_return_id AND number EQ lc_return_number.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD m_pis_cofins_baseoutras.

    DATA: ls_nf_item_tax TYPE j_1bnfstx.

    DATA: lv_ativo TYPE char1.

    DATA(lo_param) = zclca_tabela_parametros=>get_instance( ).

    " Verificar se o PISCONFINSBASEOUTRAS está ativo no APP Paramentros.
    TRY.
        lo_param->m_get_single( EXPORTING iv_modulo = lc_modulo_mm
                                          iv_chave1 = gc_chave1_2
                                          iv_chave2 = lc_chave2_1
                                IMPORTING ev_param  = lv_ativo ).
      CATCH zcxca_tabela_parametros.
    ENDTRY.

    IF lv_ativo IS NOT INITIAL.

      SORT ct_nf_item_tax BY itmnum
                             taxgrp.

      LOOP AT it_nf_item ASSIGNING FIELD-SYMBOL(<fs_item>).

        " Alteração do COFINS
        READ TABLE ct_nf_item_tax ASSIGNING FIELD-SYMBOL(<fs_tax>)
                                                WITH KEY itmnum = <fs_item>-itmnum
                                                         taxgrp = gc_cof
                                                         BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          " Transferindo o valor da coluna "Base Excluídas" para "Outras Bases"
          IF <fs_tax>-rate   EQ 0
         AND <fs_tax>-excbas IS NOT INITIAL.
            <fs_tax>-othbas = <fs_tax>-excbas.
            <fs_tax>-excbas = 0.
          ENDIF.
        ENDIF.

        " Alteração do PIS
        READ TABLE ct_nf_item_tax ASSIGNING <fs_tax>
                                   WITH KEY itmnum = <fs_item>-itmnum
                                            taxgrp = gc_pis
                                            BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          " Transferindo o valor da coluna "Base Excluídas" para "Outras Bases"
          IF <fs_tax>-rate   EQ 0
         AND <fs_tax>-excbas IS NOT INITIAL.
            <fs_tax>-othbas = <fs_tax>-excbas.
            <fs_tax>-excbas = 0.
          ENDIF.

        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
