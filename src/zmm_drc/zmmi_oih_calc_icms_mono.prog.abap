*&---------------------------------------------------------------------*
*& Include          ZMMI_OIH_CALC_ICMS_MONO
*&---------------------------------------------------------------------*

* ----------------------------------------------------------------------
* Ajuste base e valor do ICMS Monofásico
* ----------------------------------------------------------------------
TRY.
    IF oih_icms_adrem-tax_situation = oih_adrem_cst-cst61.

      zclmm_enhancements=>m_oih_calc_icms_monofasico(
        EXPORTING
          is_komk              = global_komk                     " Regra 1
          is_komp              = global_komp                     " Regra 2
          is_oih_j1b_txcd_calc = oih_j1b_txcd_calc               " Regra 3
          is_oih_j1b_acsfld    = gs_oih_j1b_acsfld               " Regra 4
          iv_mix_amount        = oih_icms_adrem-mix_amount       " Valor da mistura
          iv_mix_quantity      = oih_icms_adrem-mix_quantity     " Quantidade da mistura
        CHANGING
          cv_amount            = oih_icms_adrem-amount           " Valor
          cv_qbcmonoret        = oih_item_params-qbcmonoret      " Quantidade
          cv_vicmsmonoret      = oih_item_params-vicmsmonoret ). " Valor ICMS
    ENDIF.

  CATCH cx_root.
ENDTRY.
