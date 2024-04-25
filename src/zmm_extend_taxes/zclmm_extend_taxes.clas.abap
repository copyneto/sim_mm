class ZCLMM_EXTEND_TAXES definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_BADI_J1B_EXTEND_TAXES .
protected section.
private section.
ENDCLASS.



CLASS ZCLMM_EXTEND_TAXES IMPLEMENTATION.


  method IF_EX_BADI_J1B_EXTEND_TAXES~DETERMINE_DISCOUNT_IN_IPI_BASE.
    RETURN.
  endmethod.


  METHOD if_ex_badi_j1b_extend_taxes~exclude_icms_from_pis_cof_base.

    CONSTANTS: lc_mm                  TYPE kappl VALUE 'TX', "MM
               lc_sd                  TYPE kappl VALUE 'V', "SD
               lc_dev                 TYPE j_1bdoctyp VALUE '6', "Devolução
               lc_mod_mm              TYPE ze_param_modulo VALUE 'MM',
               lc_mod_sd              TYPE ze_param_modulo VALUE 'SD',
               lc_exc_icms_pis_cofins TYPE ze_param_chave1 VALUE 'EXC_ICMS_PIS_COFINS',
               lc_valid               TYPE ze_param_chave2 VALUE 'DATA_INICIO_VALIDADE',
               lc_icms_piscof         TYPE ze_param_chave1 VALUE 'EXC_ICMS_PISCOF',
               lc_icms                TYPE ze_param_chave2 VALUE 'SD_ICMS',
               lc_icms_fcp            TYPE ze_param_chave2 VALUE 'SD_ICMS_FCP',
               lc_icms_par            TYPE ze_param_chave2 VALUE 'SD_ICMS_PARTILHA',
               lc_icms_par_fcp        TYPE ze_param_chave2 VALUE 'SD_ICMS_PART_FCP',
               lc_ativo               TYPE ze_param_chave3 VALUE 'ATIVO'.

    DATA(lo_param) = NEW zclca_tabela_parametros( ).

    DATA: lv_docdat            TYPE j_1bdocdat,
          lv_icms_actv         TYPE abap_bool,
          lv_icms_fcp_actv     TYPE abap_bool,
          lv_icms_par_actv     TYPE abap_bool,
          lv_icms_par_fcp_actv TYPE abap_bool.

    "Se for um processo de MM (definido por TX) ou Se for um processo de SD (definido por V)
    IF is_icms_excl_info-komk-kappl = lc_mm OR is_icms_excl_info-komk-kappl = lc_sd.

      "Verificar se é um processo de devolução a fornecedor.
      IF is_icms_excl_info-komk-doctyp = lc_dev.
        FIELD-SYMBOLS <fs_j_1bnfdoc> TYPE j_1bnfdoc.
        "Pegar os dados da NF-e de referência para devolução.
        ASSIGN ('(SAPLJ1BA)J_1BNFDOC') TO <fs_j_1bnfdoc>.

        "Pegar data a partir da qual a nova regra entrou em vigor
        IF is_icms_excl_info-komk-kappl = lc_mm.

          TRY.
              lo_param->m_get_single(
                EXPORTING
                  iv_modulo = lc_mod_mm
                  iv_chave1 = lc_exc_icms_pis_cofins
                  iv_chave2 = lc_valid
                IMPORTING
                  ev_param  = lv_docdat
              ).
            CATCH zcxca_tabela_parametros.
          ENDTRY.

        ELSEIF is_icms_excl_info-komk-kappl = lc_sd.

          TRY.
              lo_param->m_get_single(
                EXPORTING
                  iv_modulo = lc_mod_sd
                  iv_chave1 = lc_exc_icms_pis_cofins
                  iv_chave2 = lc_valid
                IMPORTING
                  ev_param  = lv_docdat
              ).
            CATCH zcxca_tabela_parametros.
          ENDTRY.

        ENDIF.

        IF lv_docdat IS INITIAL.
          RETURN.
        ENDIF.

        "Comparar a data da NF-e de referência com a data de início da nova regra,
        "se a data da NF-e for menor, ou seja, anterior a data de início não será necessário
        "preencher as variáveis que excluem o ICMS da base de cálculo do PIS e COFINS.
        IF <fs_j_1bnfdoc> IS NOT ASSIGNED.
          RETURN.
        ENDIF.
        IF <fs_j_1bnfdoc>-docdat LT lv_docdat.
          RETURN.
        ENDIF.

      ENDIF.

      "Preenchimento da variáveis para excluir o ICMS da base de cálculo do PIS e COFINS.
      "MM
      IF is_icms_excl_info-komk-kappl = lc_mm.
        "Verificar se é um processo com ICMS Monofásico
        SELECT COUNT(*)
          FROM a003   AS a
          JOIN j_1baj AS b
            ON b~taxtyp EQ a~kschl
         BYPASSING BUFFER
         WHERE a~kappl  EQ is_icms_excl_info-komk-kappl "Aplicação
           AND a~aland  EQ is_icms_excl_info-komk-aland "País
           AND a~mwskz  EQ is_icms_excl_info-komp-mwskz "Código de Imposto
           AND ( b~subdivision EQ '005'    "ICMS AD REM
            OR   b~subdivision EQ '006' ).  "ICMS AD REM RETEN.
        "Se for um processo com ICMS Monofásico não excluir o ICMS.
        IF sy-subrc IS INITIAL.
          RETURN.
        ENDIF.


        cs_icms_excl_params-icms_partilha      = abap_true.
        cs_icms_excl_params-icms_fcp           = abap_true.
        cs_icms_excl_params-icms               = abap_true.
        cs_icms_excl_params-icms_partilha_fcp  = abap_true.

        "SD
      ELSEIF is_icms_excl_info-komk-kappl = lc_sd.

        TRY.
            lo_param->m_get_single(
              EXPORTING
                iv_modulo = lc_mod_sd
                iv_chave1 = lc_icms_piscof
                iv_chave2 = lc_icms
                iv_chave3 = lc_ativo
              IMPORTING
                ev_param  = lv_icms_actv
            ).
          CATCH zcxca_tabela_parametros.
        ENDTRY.

        TRY.
            lo_param->m_get_single(
              EXPORTING
                iv_modulo = lc_mod_sd
                iv_chave1 = lc_icms_piscof
                iv_chave2 = lc_icms_fcp
                iv_chave3 = lc_ativo
              IMPORTING
                ev_param  = lv_icms_fcp_actv
            ).
          CATCH zcxca_tabela_parametros.
        ENDTRY.

        TRY.
            lo_param->m_get_single(
              EXPORTING
                iv_modulo = lc_mod_sd
                iv_chave1 = lc_icms_piscof
                iv_chave2 = lc_icms_par
                iv_chave3 = lc_ativo
              IMPORTING
                ev_param  = lv_icms_par_actv
            ).
          CATCH zcxca_tabela_parametros.
        ENDTRY.

        TRY.
            lo_param->m_get_single(
              EXPORTING
                iv_modulo = lc_mod_sd
                iv_chave1 = lc_icms_piscof
                iv_chave2 = lc_icms_par_fcp
                iv_chave3 = lc_ativo
              IMPORTING
                ev_param  = lv_icms_par_fcp_actv
            ).
          CATCH zcxca_tabela_parametros.
        ENDTRY.

        IF lv_icms_actv EQ abap_true.
          cs_icms_excl_params-icms = abap_true.
        ENDIF.

        IF lv_icms_fcp_actv EQ abap_true.
          cs_icms_excl_params-icms_fcp = abap_true.
        ENDIF.

        IF lv_icms_par_actv EQ abap_true.
          cs_icms_excl_params-icms_partilha = abap_true.
        ENDIF.

        IF lv_icms_par_fcp_actv EQ abap_true.
          cs_icms_excl_params-icms_partilha_fcp = abap_true.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD if_ex_badi_j1b_extend_taxes~exclude_ipi_from_pis_cof_base.

    CONSTANTS: lc_mm                    TYPE kappl VALUE 'TX', "MM
               lc_sd                    TYPE kappl VALUE 'V',  "SD
               lc_dev                   TYPE j_1bdoctyp VALUE '6', "Devolução
               lc_mod_sd                TYPE ze_param_modulo VALUE 'SD',
               lc_exc_icms_pis_cofins   TYPE ze_param_chave1 VALUE 'EXC_ICMS_PIS_COFINS',
               lc_valid                 TYPE ze_param_chave2 VALUE 'DATA_INICIO_VALIDADE',

               lc_exc_ipi_pis_cofins    TYPE ze_param_chave1 VALUE 'EXC_IPI_PISCOF',
               lc_exc_ipi_pis_cofins_sd TYPE ze_param_chave2 VALUE 'SD',
               lc_ativo                 TYPE ze_param_chave3 VALUE 'ATIVO'.

    DATA(lo_param) = NEW zclca_tabela_parametros( ).
    DATA: lv_docdat   TYPE j_1bdocdat,
          lv_ipi_actv TYPE abap_bool.

    "Se for um processo de MM (definido por TX) ou Se for um processo de SD (definido por V)
    IF is_ipi_excl_info-komk-kappl = lc_mm OR is_ipi_excl_info-komk-kappl = lc_sd.

      "Preenchimento da variável para excluir o IPI da base de cálculo do PIS e COFINS.
      IF is_ipi_excl_info-komk-kappl = lc_mm.
        "Verificar se é um processo com ICMS Monofásico
        SELECT COUNT(*)
          FROM a003   AS a
          JOIN j_1baj AS b
            ON b~taxtyp EQ a~kschl
         BYPASSING BUFFER
         WHERE a~kappl  EQ is_ipi_excl_info-komk-kappl "Aplicação
           AND a~aland  EQ is_ipi_excl_info-komk-aland "País
           AND a~mwskz  EQ is_ipi_excl_info-komp-mwskz "Código de Imposto
           AND ( b~subdivision EQ '005'    "ICMS AD REM
            OR   b~subdivision EQ '006' ).  "ICMS AD REM RETEN.

        "Se for um processo com ICMS Monofásico não excluir o IPI.
        IF sy-subrc IS INITIAL.
          RETURN.
        ENDIF.

        cs_ipi_excl_params = abap_true.
      ELSEIF is_ipi_excl_info-komk-kappl = lc_sd.

        "Verificar se é um processo de devolução a fornecedor.
        IF is_ipi_excl_info-komk-doctyp = lc_dev.

          FIELD-SYMBOLS <fs_j_1bnfdoc> TYPE j_1bnfdoc.
          "Pegar os dados da NF-e de referência para devolução.
          ASSIGN ('(SAPLJ1BA)J_1BNFDOC') TO <fs_j_1bnfdoc>.

          TRY.
              lo_param->m_get_single(
                EXPORTING
                  iv_modulo = lc_mod_sd
                  iv_chave1 = lc_exc_icms_pis_cofins
                  iv_chave2 = lc_valid
                IMPORTING
                  ev_param  = lv_docdat
              ).
            CATCH zcxca_tabela_parametros.
          ENDTRY.

          IF lv_docdat IS INITIAL.
            RETURN.
          ENDIF.

          IF <fs_j_1bnfdoc>-docdat LT lv_docdat.
            RETURN.
          ENDIF.
        ENDIF.

        TRY.
            lo_param->m_get_single(
              EXPORTING
                iv_modulo = lc_mod_sd
                iv_chave1 = lc_exc_ipi_pis_cofins
                iv_chave2 = lc_exc_ipi_pis_cofins_sd
                iv_chave3 = lc_ativo
              IMPORTING
                ev_param  = lv_ipi_actv
            ).
          CATCH zcxca_tabela_parametros.
        ENDTRY.

        IF lv_ipi_actv EQ abap_true.
          cs_ipi_excl_params = abap_true.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD if_ex_badi_j1b_extend_taxes~icms_st_recalculate.
    DATA: ls_j_1btxst3     TYPE j_1btxst3.

    DATA: lv_chave2_modulo TYPE ztca_param_par-chave2,
          lv_ativo         TYPE ztca_param_val-low.

    FIELD-SYMBOLS: <fs_j_1btxst3> TYPE any.

    CONSTANTS: lc_modulo_mm TYPE ztca_param_mod-modulo VALUE 'MM',
               lc_modulo_sd TYPE ztca_param_mod-modulo VALUE 'SD',
               lc_pmpf      TYPE ztca_param_par-chave1 VALUE 'PMPF',
               lc_ativo     TYPE ztca_param_par-chave3 VALUE 'ATIVO',
               lc_kappl_sd  TYPE komk-kappl            VALUE 'V'.

    IF  is_icms_st_recalculation-komk-kappl = lc_kappl_sd.
      lv_chave2_modulo = lc_modulo_sd.
    ELSE.
      lv_chave2_modulo = lc_modulo_mm.
    ENDIF.

    CLEAR: lv_ativo.
    DATA(lo_param) = NEW zclca_tabela_parametros( ).
    TRY.
        lo_param->m_get_single(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_pmpf
            iv_chave2 = lv_chave2_modulo
            iv_chave3 = lc_ativo
          IMPORTING
            ev_param  = lv_ativo
       ).
      CATCH zcxca_tabela_parametros INTO DATA(lo_cx).
    ENDTRY.

    IF lv_ativo IS NOT INITIAL.

      SELECT SINGLE land1,
                    txreg,
                    matnr,
                    validfrom,
                    validto,
                    price,
                    factor,
                    unit
        FROM ztmm_pmpf
        WHERE land1     =  @is_icms_st_recalculation-komk-aland
          AND txreg     =  @is_icms_st_recalculation-komp-txreg_st
          AND matnr     =  @is_icms_st_recalculation-komp-matnr
          AND validfrom <= @sy-datum
          AND validto   >= @sy-datum
      INTO @DATA(ls_ztmm_pmpf).
      "Calculo PMPF
      IF sy-subrc           =  0 AND
         ls_ztmm_pmpf-price <> 0.
        IF is_icms_st_recalculation-icms_destination_rate <> 0.
          DATA(lv_aliquota) = is_icms_st_recalculation-icms_destination_rate.
        ELSE.
          lv_aliquota = is_icms_st_recalculation-icms_rate.
        ENDIF.
        DATA(lv_base)     = CONV if_ex_badi_j1b_extend_taxes~mty_tax( is_icms_st_recalculation-komp-mglme * ls_ztmm_pmpf-price ).
        DATA(lv_valor)    = CONV if_ex_badi_j1b_extend_taxes~mty_tax( ( lv_base * lv_aliquota ) - is_icms_st_recalculation-icms_amount ).
        "Comparação dos valores
        IF  is_icms_st_recalculation-komk-kappl = lc_kappl_sd.
          IF cv_icms_st_amount = 0.
            ASSIGN ('(SAPLJ1BR)J_1BTXST3') TO <fs_j_1btxst3>.
            IF <fs_j_1btxst3> IS ASSIGNED.
              ls_j_1btxst3 = <fs_j_1btxst3>.
            ENDIF.
            UNASSIGN <fs_j_1btxst3>.
            IF ls_j_1btxst3 IS NOT INITIAL.
              cv_icms_st_base         = CONV if_ex_badi_j1b_extend_taxes~mty_tax( is_icms_st_recalculation-icms_base * ( ls_j_1btxst3-rate / 100  ) ).
              DATA(lv_icms_st_bruto)  = CONV if_ex_badi_j1b_extend_taxes~mty_tax( ( cv_icms_st_base + is_icms_st_recalculation-icms_base ) * lv_aliquota ).
              cv_icms_st_amount       = CONV if_ex_badi_j1b_extend_taxes~mty_tax( lv_icms_st_bruto - is_icms_st_recalculation-icms_amount ).
              IF lv_valor <> 0 AND
                 lv_valor > cv_icms_st_amount.
                cv_icms_st_base   = lv_base.
                cv_icms_st_amount = lv_valor.
              ENDIF.
            ENDIF.
          ELSE.
            IF lv_valor <> 0 AND
               lv_valor > cv_icms_st_amount.
              cv_icms_st_base   = lv_base.
              cv_icms_st_amount = lv_valor.
            ENDIF.
          ENDIF.
        ELSE.
          IF lv_valor <> 0 AND
             lv_valor > cv_icms_st_amount.
            cv_icms_st_base   = lv_base.
            cv_icms_st_amount = lv_valor.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method IF_EX_BADI_J1B_EXTEND_TAXES~PARTILHA_ICMS_RECALCULATE.
    RETURN.
  endmethod.


  method IF_EX_BADI_J1B_EXTEND_TAXES~SUBTRACT_DISC_FROM_IPI_BASE.
    RETURN.
  endmethod.
ENDCLASS.
