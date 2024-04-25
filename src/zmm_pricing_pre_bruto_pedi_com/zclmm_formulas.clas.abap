class ZCLMM_FORMULAS definition
  public
  final
  create public .

public section.

  "! RET-Fórmula valor 922 - Copiar da Memória (a abreviação RET indica que é do Retail)
  "! @parameter IS_KOMK | Determinação de preço - cabeçalho comunicação
  "! @parameter IS_KOMP | Determinação de preço item de comunicação
  "! @parameter CS_RETTXKOMV | Registro de condição de comunicação p/determinação de preço
  "! @parameter CS_XKOMV | Estrutura-KOMV plus índice
  "! @parameter CV_XKWERT | Valor condição
  class-methods M_RET_CALC_RV64A922
    importing
      !IS_KOMK type KOMK
      !IS_KOMP type KOMP
    changing
      !CS_RETTXKOMV type KOMV
      !CS_XKOMV type KOMV_INDEX
      !CV_XKWERT type KONV-KWERT .
  "! RET-Fórmula valor 923 - Cálculo de Preço Líquido (a abreviação RET indica que é do Retail)
  "! @parameter IS_KOMK | Determinação de preço - cabeçalho comunicação
  "! @parameter IV_XWORKD | Valor condição
  "! @parameter IV_XWORKE | Valor condição
  "! @parameter IV_XWORKF | Valor condição
  "! @parameter IV_XWORKG | Valor condição
  "! @parameter IV_XWORKH | Valor condição
  "! @parameter IV_XWORKI | Valor condição
  "! @parameter IV_XWORKJ | Valor condição
  "! @parameter IV_XWORKK | Montante ou porcentagem da condição
  "! @parameter IV_XWORKL | Montante ou porcentagem da condição
  "! @parameter IV_XWORKM | Montante ou porcentagem da condição
  "! @parameter CS_KOMP | Determinação de preço item de comunicação
  "! @parameter CS_XKOMV | Estrutura-KOMV plus índice
  "! @parameter CV_XKWERT | Valor condição
  class-methods M_RET_CALC_RV64A923
    importing
      !IS_KOMK type KOMK
      !IV_XWORKD type KONV-KWERT
      !IV_XWORKE type KONV-KWERT
      !IV_XWORKF type KONV-KWERT
      !IV_XWORKG type KONV-KWERT
      !IV_XWORKH type KONV-KWERT
      !IV_XWORKI type KONV-KWERT
      !IV_XWORKJ type KONV-KWERT
      !IV_XWORKK type KOMV-KBETR
      !IV_XWORKL type KOMV-KBETR
      !IV_XWORKM type KOMV-KBETR
    changing
      !CS_KOMP type KOMP
      !CS_XKOMV type KOMV_INDEX
      !CV_XKWERT type KONV-KWERT .
  "! RET-Excluir condição zerada (a abreviação RET indica que é do Retail)
  "! @parameter IS_KOMK | Determinação de preço - cabeçalho comunicação
  "! @parameter IS_KOMP | Determinação de preço item de comunicação
  "! @parameter CS_XKOMV | Estrutura-KOMV plus índice
  "! @parameter CV_XKWERT | Valor condição
  class-methods M_RET_EXCL_COND_ZERADA
    importing
      !IS_KOMK type KOMK
      !IS_KOMP type KOMP
    changing
      !CS_XKOMV type KOMV_INDEX
      !CV_XKWERT type KONV-KWERT .
  class-methods M_OIL_CALC_RV64A924
    importing
      !IS_KOMK type KOMK
      !IS_KOMP type KOMP
      !IS_KOMV type KOMV
    changing
      !CV_KSCHL type KOMV-KSCHL .
  class-methods M_OIL_CALC_RV64A925
    importing
      !IS_KOMK type KOMK
      !IS_KOMP type KOMP
    changing
      !CS_RETTXKOMV type KOMV
      !CS_XKOMV type KOMV_INDEX
      !CV_XKWERT type KONV-KWERT .
  class-methods M_OIL_CALC_RV64A926
    importing
      !IS_KOMK type KOMK
      !IV_XWORKD type KONV-KWERT
      !IV_XWORKE type KONV-KWERT
      !IV_XWORKF type KONV-KWERT
      !IV_XWORKG type KONV-KWERT
      !IV_XWORKH type KONV-KWERT
      !IV_XWORKI type KONV-KWERT
      !IV_XWORKJ type KONV-KWERT
      !IV_XWORKK type KOMV-KBETR
      !IV_XWORKL type KOMV-KBETR
      !IV_XWORKM type KOMV-KBETR
    changing
      !CS_KOMP type KOMP
      !CS_XKOMV type KOMV_INDEX
      !CV_XKWERT type KONV-KWERT .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF gc_modulo,
        mm  TYPE ze_param_modulo VALUE 'MM',
      END OF gc_modulo,
      BEGIN OF gc_trtyp,
        create  TYPE trtyp VALUE 'H', "Inserir
        modify  TYPE trtyp VALUE 'V', "Modificar
        display TYPE trtyp VALUE 'A', "Exibir
      END OF gc_trtyp,
      BEGIN OF gc_kappl,
        impostos TYPE kappl VALUE 'TX', "Imposto
        sd       TYPE kappl VALUE 'V',  "SD
        mm       TYPE kappl VALUE 'M',  "MM
      END OF gc_kappl,
      BEGIN OF gc_kvewe,
        a TYPE kvewe   VALUE 'A', "Utilização da tabela de condições
      END OF gc_kvewe,
      BEGIN OF gc_kalsm,
        taxbra TYPE kalsm_d VALUE 'TAXBRA', "Esquema de cálculo localização Brasil
        zrmm01 TYPE kalsm_d VALUE 'ZRMM01', "Esquema de cálculo pedido
        zomm01 TYPE kalsm_d VALUE 'ZOMM01', "Esquema de cálculo pedido Oil&Gás
        taxbrj TYPE kalsm_d VALUE 'TAXBRJ', "Esquema de cálculo localização Brasil Oil&gás
      END OF gc_kalsm,
      BEGIN OF gc_kofra,
        excl_014 TYPE kofra VALUE '0000014', "Excluir condições de valores
        excl_910 TYPE kofra VALUE '0000910', "Ativar condição CONH
        excl_911 TYPE kofra VALUE '0000911', "Desativar ICMS normal se condição CONH estiver ativa
      END OF gc_kofra .
ENDCLASS.



CLASS ZCLMM_FORMULAS IMPLEMENTATION.


  METHOD m_ret_calc_rv64a922.

    CONSTANTS: lc_serv TYPE mtart VALUE 'SERV'.

*-- Processamento ------------------------------------------------------*
    IF is_komk-trtyp EQ gc_trtyp-display.
      RETURN.
    ENDIF.

*     Verificar impostos do código IVA
    SELECT DISTINCT kappl,
                    kschl,
                    aland,
                    mwskz
              FROM a003
              INTO TABLE @DATA(lt_a003)
              WHERE kappl EQ @gc_kappl-impostos
                AND aland EQ @is_komk-aland
                AND mwskz EQ @is_komp-mwskz.

    IF sy-subrc EQ 0.

      SELECT DISTINCT kschl
                 FROM t683s
                 INTO TABLE @DATA(lt_kschl)
                 FOR ALL ENTRIES IN @lt_a003
                WHERE kvewe EQ @gc_kvewe-a
                  AND kappl EQ @lt_a003-kappl
                  AND kalsm EQ @gc_kalsm-taxbra
                  AND kschl EQ @lt_a003-kschl
                  AND kstat EQ @abap_false.

      IF sy-subrc EQ 0.

        SELECT DISTINCT taxgrp
                   FROM j_1baj
                   INTO TABLE @DATA(lt_taxgrp)
                   FOR ALL ENTRIES IN @lt_kschl
                  WHERE taxtyp EQ @lt_kschl-kschl
                    AND taxoff EQ @abap_false.

        IF sy-subrc NE 0.
          CLEAR: lt_taxgrp.
        ENDIF.

      ENDIF.

    ENDIF.

    IF cs_rettxkomv-koaid EQ 'A' AND "Classe de condição suplementos e deduções
       cs_rettxkomv-krech EQ 'A'.    "Percentual

      SELECT code
          UP TO 1 ROWS
        FROM j_1btxcond
        INTO @DATA(lv_code)
       WHERE kappl EQ @gc_kappl-impostos
         AND kschl EQ @cs_rettxkomv-kschl.
      ENDSELECT.

      IF sy-subrc EQ 0.

        SELECT SINGLE taxgrp
                 FROM j_1bcondmap
                WHERE code EQ @lv_code
                 INTO @DATA(lv_taxgrp).

        IF sy-subrc NE 0.
          CLEAR: lv_taxgrp.
        ENDIF.

      ENDIF.

      IF line_exists( lt_taxgrp[ taxgrp = lv_taxgrp ] ).

        cs_rettxkomv-kwert = cs_rettxkomv-kbetr.

      ELSE.

        cs_rettxkomv-kbetr = 0.
        cs_rettxkomv-kawrt = 0.
        cs_rettxkomv-kwert = 0.

      ENDIF.

    ENDIF.

    IF cs_rettxkomv-koaid EQ 'D' AND "Classe de condição impostos
       cs_rettxkomv-krech EQ 'B'.    "Montante fixo

      SELECT SINGLE taxgrp
               FROM j_1baj
              WHERE taxtyp EQ @cs_rettxkomv-kschl
               INTO @lv_taxgrp.

      IF sy-subrc NE 0.
        CLEAR: lv_taxgrp.
      ENDIF.

      IF line_exists( lt_taxgrp[ taxgrp = lv_taxgrp ] ).

        cs_rettxkomv-kbetr = 0.

      ELSE.

        cs_rettxkomv-kbetr = 0.
        cs_rettxkomv-kawrt = 0.
        cs_rettxkomv-kwert = 0.

      ENDIF.

    ENDIF.

    cs_xkomv-kbetr = cs_rettxkomv-kbetr.
    cs_xkomv-kawrt = cs_rettxkomv-kawrt.
    cv_xkwert      = cs_rettxkomv-kwert.

    IF is_komp-mtart EQ lc_serv.
      cs_rettxkomv-kbetr = '0'.
      cs_rettxkomv-kawrt = '0'.
      cs_rettxkomv-kwert = '0'.
    ENDIF.

  ENDMETHOD.


  METHOD m_ret_calc_rv64a923.
*-- Legenda ------------------------------------------------------------*
*     XWORKD - $ Valor Produto
*     XWORKE - $ Seguro
*     XWORKF - $ Frete
*     XWORKG -
*     XWORKH - % Alíquota PIS + COFINS
*     XWORKI - $ Desconto
*     XWORKJ - $ Despesa
*     XWORKK - % Alíquota ICMS
*     XWORKL - % Redução Base ICMS
*     XWORKM - % Alíquota IPI
*-- Processamento ------------------------------------------------------*

    IF is_komk-trtyp EQ gc_trtyp-display.
      cs_komp-netwr  = cv_xkwert.
      RETURN.
    ENDIF.

    "Carregar variáveis para cálculo
    DATA(lv_valor_produto)  = CONV decfloat16( iv_xworkd ). "Valor bruto produto
    DATA(lv_valor_desconto) = CONV decfloat16( iv_xworki ). "Valor bruto desconto
    DATA(lv_valor_despesa)  = CONV decfloat16( iv_xworkj ). "Valor bruto despesa
    DATA(lv_valor_frete)    = CONV decfloat16( iv_xworkf ). "Valor bruto frete
    DATA(lv_valor_seguro)   = CONV decfloat16( iv_xworke ). "Valor bruto seguro

    "Determinar Base
    DATA(lv_valor_base) = CONV decfloat16( 0 ).

    CASE cs_xkomv-kschl.
      WHEN 'ZODC'. "Desconto Bruto
        lv_valor_base = lv_valor_desconto.
      WHEN 'ZODP'. "Despesa Bruto
        lv_valor_base = lv_valor_despesa.
      WHEN 'ZOFR'. "Frete Bruto
        lv_valor_base = lv_valor_frete.
      WHEN 'ZOSG'. "Seguro Bruto
        lv_valor_base = lv_valor_seguro.
      WHEN 'WOTB'. "Preço Bruto
        lv_valor_base = ( lv_valor_produto + lv_valor_desconto + lv_valor_despesa + lv_valor_frete + lv_valor_seguro ).
    ENDCASE.

    "Converter alíquotas dos impostos
    DATA(lv_aliq_ipi)           = CONV decfloat16( iv_xworkm / 1000 ). "Alíquota IPI
    DATA(lv_aliq_icms)          = CONV decfloat16( iv_xworkk / 1000 ). "Alíquota ICMS
    DATA(lv_aliq_red_base_icms) = CONV decfloat16( iv_xworkl / 1000 ). "Alíquota Redução de Base ICMS
    DATA(lv_aliq_pis_cofins)    = CONV decfloat16( iv_xworkh / 1000 ). "Alíquota PIS e COFINS

    "Cálcular valor do IPI
    DATA(lv_valor_ipi) = ( lv_valor_base * lv_aliq_ipi ).

    "Cálcular valores dos impostos
    DATA(lv_base_icms)            = CONV decfloat16( 0 ).
    DATA(lv_valor_icms)           = CONV decfloat16( 0 ).
    DATA(lv_base_pis_cofins_ipi)  = CONV decfloat16( 0 ).
    DATA(lv_base_pis_cofins)      = CONV decfloat16( 0 ).
    DATA(lv_valor_pis_cofins_ipi) = CONV decfloat16( 0 ).
    DATA(lv_valor_pis_cofins)     = CONV decfloat16( 0 ).
    DATA(lv_valor_icor)           = CONV decfloat16( 0 ).

    CASE cs_komp-mtuse.

      WHEN 0. "Material de revenda

        lv_base_icms  = ( lv_valor_base * lv_aliq_red_base_icms ). "Valor base do ICMS
        lv_valor_icms = ( lv_base_icms  * lv_aliq_icms ).          "Valor ICMS

        lv_base_pis_cofins_ipi  = ( lv_valor_base - lv_valor_icms ).                  "Valor base do PIS e COFINS sem IPI
        lv_valor_pis_cofins_ipi = ( lv_base_pis_cofins_ipi * lv_aliq_pis_cofins  ). "Valor PIS e COFINS com IPI na base

        lv_base_pis_cofins  = ( lv_valor_base - lv_valor_icms ).             "Valor base do PIS e COFINS sem ICMS
        lv_valor_pis_cofins = ( lv_base_pis_cofins * lv_aliq_pis_cofins  ). "Valor PIS e COFINS sem IPI na base

        lv_valor_icor = ( lv_valor_pis_cofins_ipi - lv_valor_pis_cofins ). "Diferença valor dos impostos PIS e COFINS com e sem o IPI no valor da base

        lv_valor_pis_cofins = 0.

      WHEN 1. "Material industrializado (contribuinte IPI)

        lv_base_icms  = lv_valor_base * lv_aliq_red_base_icms. "Valor base do ICMS
        lv_valor_icms = lv_base_icms  * lv_aliq_icms.          "Valor ICMS

        lv_base_pis_cofins  = ( lv_valor_base - lv_valor_icms ).       "Valor base do PIS e COFINS sem ICMS
        lv_valor_pis_cofins = lv_base_pis_cofins * lv_aliq_pis_cofins. "Valor PIS e COFINS sem valor do IPI na base

      WHEN 2 OR 3. "Material de consumo ou ativo

        lv_base_icms  = ( lv_valor_base * lv_aliq_red_base_icms ) + lv_valor_ipi. "Valor base do ICMS
        lv_valor_icms = ( lv_base_icms  * lv_aliq_icms ).                         "Valor ICMS

        lv_base_pis_cofins_ipi  = ( lv_valor_base - lv_valor_icms ). "Valor base do PIS e COFINS com IPI
        lv_valor_pis_cofins_ipi = ( lv_base_pis_cofins_ipi * lv_aliq_pis_cofins ). "Valor PIS e COFINS com IPI na base

      WHEN 4. "Material de consumo para atividade principal

        lv_base_icms  = ( lv_valor_base * lv_aliq_red_base_icms ). "Valor base do ICMS
        lv_valor_icms = ( lv_base_icms  * lv_aliq_icms ).          "Valor ICMS

        lv_base_pis_cofins_ipi  = ( lv_valor_base - lv_valor_icms ).                  "Valor base do PIS e COFINS sem IPI
        lv_valor_pis_cofins_ipi = ( lv_base_pis_cofins_ipi * lv_aliq_pis_cofins  ). "Valor PIS e COFINS com IPI na base

        lv_base_pis_cofins  = ( lv_valor_base - lv_valor_icms ).             "Valor base do PIS e COFINS sem ICMS
        lv_valor_pis_cofins = ( lv_base_pis_cofins * lv_aliq_pis_cofins  ). "Valor PIS e COFINS sem IPI na base

        lv_valor_icor = ( lv_valor_pis_cofins_ipi - lv_valor_pis_cofins ). "Diferença valor dos impostos PIS e COFINS com e sem o IPI no valor da base

        lv_valor_pis_cofins = 0.

    ENDCASE.

    "Calcular valores das condições
    DATA(lv_valor_liquido) = CONV decfloat16( 0 ).

    CASE cs_xkomv-kschl.
      WHEN 'ZODC'. "Desconto Líquido
        lv_valor_liquido = ( lv_valor_desconto - lv_valor_icms - lv_valor_pis_cofins_ipi - lv_valor_pis_cofins ).
      WHEN 'ZODP'. "Despesa Líquido
        lv_valor_liquido = ( lv_valor_despesa  - lv_valor_icms - lv_valor_pis_cofins_ipi - lv_valor_pis_cofins ).
      WHEN 'ZOFR'. "Frete Líquido
        lv_valor_liquido = ( lv_valor_frete    - lv_valor_icms - lv_valor_pis_cofins_ipi - lv_valor_pis_cofins ).
      WHEN 'ZOSG'. "Seguro Líquido
        lv_valor_liquido = ( lv_valor_seguro   - lv_valor_icms - lv_valor_pis_cofins_ipi - lv_valor_pis_cofins ).
      WHEN 'WOTB'. "Preço Líquido
        lv_valor_liquido = ( lv_valor_produto  + lv_valor_desconto + lv_valor_despesa + lv_valor_frete + lv_valor_seguro - lv_valor_icms - lv_valor_pis_cofins_ipi - lv_valor_pis_cofins + lv_valor_icor ).
    ENDCASE.

    "Preencher retorno das condições
    cs_xkomv-kbetr = lv_valor_liquido.
    cs_xkomv-kawrt = lv_valor_liquido.
    cv_xkwert      = lv_valor_liquido.
    cs_komp-netpr  = lv_valor_liquido.
    cs_komp-netwr  = lv_valor_liquido.

  ENDMETHOD.


  METHOD m_ret_excl_cond_zerada.

    CONSTANTS: lc_mod    TYPE ztca_param_mod-modulo VALUE 'MM',
               lc_chave2 TYPE ztca_param_par-chave2 VALUE 'CONDICOES',
               lc_chave3 TYPE ztca_param_par-chave3 VALUE 'ELIMINAR',
               lc_serv   TYPE komp-mtart VALUE 'SERV'.

*-- Processamento ------------------------------------------------------*

    IF is_komk-trtyp  EQ gc_trtyp-display OR
       is_komp-kposn  IS INITIAL OR
       is_komk-kappl  NE gc_kappl-mm OR
       ( is_komk-kalsm  NE gc_kalsm-zrmm01 AND
         is_komk-kalsm  NE gc_kalsm-zomm01 ).
      RETURN.
    ENDIF.

    CASE cs_xkomv-kofra.

        "Eliminar condições de valores que não estão vinculadas ao IVA
      WHEN gc_kofra-excl_014.

        IF cs_xkomv-krech EQ 'B'.

          IF sy-subrc EQ 0.

            SELECT SINGLE kschl_tax
              FROM j_1b_po_tax_map
             WHERE kschl_po EQ @cs_xkomv-kschl
             INTO @DATA(lv_kschl).

            IF sy-subrc EQ 0.

              "Verificar impostos do código IVA
              SELECT kappl,
                     kschl,
                     aland,
                     mwskz
                FROM a003
               WHERE kappl EQ @gc_kappl-impostos
                 AND kschl EQ @lv_kschl
                 AND aland EQ @is_komk-aland
                 AND mwskz EQ @is_komp-mwskz
                INTO TABLE @DATA(lt_a003).

              IF sy-subrc NE 0.
                cs_xkomv-kschl = ' '.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        "Verificar se o tipo de material é serviço
        IF is_komp-mtart EQ lc_serv.

          "Selecionar as condições que devem ser eliminadas
          SELECT COUNT(*)
            FROM zi_ca_get_parameter
           WHERE modulo EQ @lc_mod
             AND chave1 EQ @is_komk-kalsm  "Pricing executada no momento
             AND chave2 EQ @lc_chave2
             AND chave3 EQ @lc_chave3
             AND low    EQ @cs_xkomv-kschl. "Condição da pricing

          "Se encontrar limpar a condição.
          IF sy-subrc EQ 0.
            cs_xkomv-kschl = ' '. "Limpar a condição
          ENDIF.

        ENDIF.

        "Selecionar Selecionar IVA de Frete
        SELECT COUNT(*)
          FROM zi_ca_get_parameter
         WHERE modulo EQ @lc_mod
           AND chave1 EQ @is_komk-kalsm  "Pricing executada no momento
           AND chave2 EQ 'IVA_FRETE'
           AND low    EQ @is_komp-mwskz. "IVA da operação
        IF sy-subrc EQ 0.
          SELECT COUNT(*)
            FROM zi_ca_get_parameter
           WHERE modulo EQ @lc_mod
             AND chave1 EQ @is_komk-kalsm  "Pricing executada no momento
             AND chave2 EQ @lc_chave2
             AND chave3 EQ @lc_chave3
             AND low    EQ @cs_xkomv-kschl. "Condição da pricing
          "Se encontrar limpar a condição.
          IF sy-subrc EQ 0.
            cs_xkomv-kschl = ' '. "Limpar a condição
          ENDIF.
        ENDIF.

        "Ativar condição CONH
      WHEN gc_kofra-excl_910.

        "Verificar impostos do código IVA
        SELECT kappl,
               kschl,
               aland,
               mwskz
          FROM a003
         WHERE kappl EQ @gc_kappl-impostos
           AND ( kschl EQ 'CIC0' OR
                 kschl EQ 'CIC1' OR
                 kschl EQ 'CIC2' )
           AND aland EQ @is_komk-aland
           AND mwskz EQ @is_komp-mwskz
          INTO TABLE @lt_a003.

        IF sy-subrc NE 0.
          cs_xkomv-kschl = ' '.
        ENDIF.

        "Desativar ICMS se condição CONH estiver ativa
      WHEN gc_kofra-excl_911.

        "Verificar impostos do código IVA
        SELECT kappl,
               kschl,
               aland,
               mwskz
          FROM a003
         WHERE kappl EQ @gc_kappl-impostos
           AND ( kschl EQ 'CIC0' OR
                 kschl EQ 'CIC1' OR
                 kschl EQ 'CIC2' )
           AND aland EQ @is_komk-aland
           AND mwskz EQ @is_komp-mwskz
          INTO TABLE @lt_a003.

        IF sy-subrc EQ 0.
          cs_xkomv-kschl = ' '.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD m_oil_calc_rv64a924.

    CONSTANTS: lc_func_cond TYPE ze_param_chave2 VALUE 'DETERMINAR_FUNCAO_CONDICAO'.
    DATA: lr_funcao_cond TYPE RANGE OF string.
    DATA(lo_param) = NEW zclca_tabela_parametros( ).

*"- Processamento ------------------------------------------------------*
    IF is_komk-trtyp EQ gc_trtyp-display.
      RETURN.
    ENDIF.

*- Verificar impostos do código IVA
    TRY.

        lo_param->m_get_range(
          EXPORTING
            iv_modulo = gc_modulo-mm
            iv_chave1 = CONV #( gc_kalsm-zomm01 )
            iv_chave2 = lc_func_cond
          IMPORTING
            et_range  = lr_funcao_cond ).

        IF line_exists( lr_funcao_cond[ low = is_komv-kschl ] ).

          DATA(lv_kschl) = lr_funcao_cond[ low = is_komv-kschl ]-high.

          SELECT DISTINCT
            kappl, kschl, aland, mwskz
            FROM a003
            WHERE kappl EQ @gc_kappl-impostos
              AND kschl EQ @lv_kschl
              AND aland EQ @is_komk-aland
              AND mwskz EQ @is_komp-mwskz
            INTO TABLE @DATA(lt_a003).

          IF sy-subrc EQ 0.
            cv_kschl = lv_kschl.
          ENDIF.

        ENDIF.

      CATCH zcxca_tabela_parametros INTO DATA(lo_cx).
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD m_oil_calc_rv64a925.

*- Variáveis ----------------------------------------------------------*
    CONSTANTS: lc_condicoes TYPE ze_param_chave2 VALUE 'CONDICOES',
               lc_func_cond TYPE ze_param_chave2 VALUE 'DETERMINAR_FUNCAO_CONDICAO'.

    DATA:
      lv_rate      TYPE j_1btxic2-rate,
      lv_base      TYPE j_1btxic2-base,
      lv_amount    TYPE j_1btxip3-amount,
      lv_factor    TYPE j_1btxip3-factor,
      lv_unit      TYPE j_1btxip3-unit,
      lv_tax_group TYPE j_1btxgruop-gruop.

*- Processamento ------------------------------------------------------*
    IF is_komk-trtyp EQ gc_trtyp-display.
      RETURN.
    ENDIF.

*- Carregar variáveis para seleção dos dados
    DATA(lv_pais)        = is_komk-aland . "Valor bruto produto
    DATA(lv_uf_origem)   = is_komk-wkreg . "Estado Origem
    DATA(lv_uf_destino)  = is_komp-wkreg . "Estado destino
    DATA(lv_material)    = is_komp-matnr . "Material
    DATA(lv_grupo)       = is_komp-matkl .  "Grupo de Mercadorias
    DATA(lv_utilizacao)  = is_komp-mtuse . "Utilização do Material
    DATA(lv_origem)      = is_komp-mtorg . "Origem do Material
    DATA(lv_iva)         = is_komp-mwskz . "Código de Imposto
    DATA(lv_fornecedor)  = is_komk-lifnr . "Fornecedor
    DATA(lv_ncm)         = is_komp-steuc . "NCM do material

    DATA(lv_empresa)     = is_komk-bukrs . "Empresa
    DATA(lv_centro)      = is_komp-werks . "Centro

    DATA: lr_condicoes TYPE RANGE OF string.
    DATA: lr_funcao_cond TYPE RANGE OF string.
    DATA: lv_c_data TYPE char8.
    DATA: lv_data TYPE j_1btxdatf.
    DATA(lo_param) = NEW zclca_tabela_parametros( ).

    "Data do dia
    lv_c_data = sy-datum+6(2) && sy-datum+4(2) && sy-datum(4).

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lv_c_data
      IMPORTING
        output = lv_data.

*- Veificar impostos do código IVA
    SELECT DISTINCT kappl,
                    kschl,
                    aland,
                    mwskz
               FROM a003
              WHERE kappl EQ @gc_kappl-impostos
                AND aland EQ @is_komk-aland
                AND mwskz EQ @is_komp-mwskz
               INTO TABLE @DATA(lt_a003).

    IF sy-subrc EQ 0.

      SELECT DISTINCT kschl
                 FROM t683s
                  FOR ALL ENTRIES IN @lt_a003
                WHERE kvewe EQ @gc_kvewe-a
                  AND kappl EQ @lt_a003-kappl
                  AND kalsm EQ @gc_kalsm-taxbrj
                  AND kschl EQ @lt_a003-kschl
                  AND kstat EQ @abap_false
                 INTO TABLE @DATA(lt_kschl).

      IF sy-subrc EQ 0.

        SELECT DISTINCT taxtyp,
                        taxgrp
                   FROM j_1baj
                    FOR ALL ENTRIES IN @lt_kschl
                  WHERE taxtyp EQ @lt_kschl-kschl
                    AND taxoff EQ @abap_false
                    AND subdivision EQ @abap_false
                   INTO TABLE @DATA(lt_taxgrp).


        IF sy-subrc NE 0.
          CLEAR: lt_taxgrp.
        ENDIF.

      ENDIF.

    ENDIF.


*- Início da Seleção das alíquotas e bases dos impostos e preenchimento das variáveis de impostos.

    IF lt_taxgrp IS NOT INITIAL.

      TRY.

          lo_param->m_get_range(
            EXPORTING
              iv_modulo = gc_modulo-mm
              iv_chave1 = CONV #( gc_kalsm-zomm01 )
              iv_chave2 = lc_func_cond
            IMPORTING
              et_range  = lr_funcao_cond ).

          lo_param->m_get_range(
            EXPORTING
              iv_modulo = gc_modulo-mm
              iv_chave1 = CONV #( gc_kalsm-zomm01 )
              iv_chave2 = lc_condicoes
            IMPORTING
              et_range  = lr_condicoes ).


        CATCH zcxca_tabela_parametros INTO DATA(lo_cx).
          RETURN.
      ENDTRY.


      READ TABLE lr_funcao_cond ASSIGNING FIELD-SYMBOL(<fs_fun_con>)
                                  WITH KEY low = cs_xkomv-kschl.

      IF sy-subrc EQ 0.

        READ TABLE lr_condicoes ASSIGNING FIELD-SYMBOL(<fs_cond>)
                                  WITH KEY low = <fs_fun_con>-low.

        READ TABLE lt_taxgrp ASSIGNING FIELD-SYMBOL(<fs_taxgrp>)
                             WITH KEY taxgrp = <fs_cond>-high.

        IF <fs_taxgrp> IS ASSIGNED.

          CASE <fs_cond>-high.

              "Seleção da alíquota e base de ICMS
            WHEN 'ICMS'.

              "Primeira seleção da alíquota e base de ICMS
              SELECT SINGLE matnr,
                            rate,
                            base
                       FROM j_1btxic2
                      WHERE land1 EQ @lv_pais
                        AND shipfrom  EQ @lv_uf_origem
                        AND shipto    EQ @lv_uf_destino
                        AND matnr     EQ @lv_material
                        AND validfrom GE @lv_data
                        AND validto   LE @lv_data
                       INTO @DATA(ls_1btxic2).


              IF ls_1btxic2-matnr IS NOT INITIAL.
                lv_rate = ls_1btxic2-rate.
                lv_base = ls_1btxic2-base.

              ELSE.

                "Segunda seleção da alíquota e base de ICMS
                CALL FUNCTION 'J_1B_READ_DYNAMIC_TABLE'
                  EXPORTING
                    caller         = 'MM'
                    country        = lv_pais
                    state_from     = lv_uf_origem
                    state_to       = lv_uf_destino
                    material       = lv_material
                    material_group = lv_grupo
                    material_usage = lv_utilizacao
                    material_orig  = lv_origem
                    mwskz          = lv_iva
                    vendor         = lv_fornecedor
                    ncm            = lv_ncm
                    date           = lv_data
                    icms           = 'X'
                    i_bukrs        = lv_empresa
                    i_werks        = lv_centro
                  IMPORTING
                    rate           = lv_rate
                    base           = lv_base
                    amount         = lv_amount
                    factor         = lv_factor
                    unit           = lv_unit
                    tax_group      = lv_tax_group.

                IF lv_rate IS NOT INITIAL OR
                   lv_base IS NOT INITIAL OR
                   lv_tax_group IS NOT INITIAL.

                ELSE.

                  "Terceira seleção ICMS Origem e Destino
                  CALL FUNCTION 'J_1B_READ_TXIC1'
                    EXPORTING
                      country    = lv_pais
                      state_from = lv_uf_origem
                      state_to   = lv_uf_destino
                      date       = lv_data
                    IMPORTING
                      rate       = lv_rate.

                  IF lv_rate IS NOT INITIAL.
                    lv_base = '100'.
                  ENDIF.

                ENDIF.
              ENDIF.


              "Seleção da alíquota e base de IPI
            WHEN 'IPI'.

              "Primeira seleção da alíquota e base de IPI
              SELECT SINGLE matnr,
                            rate,
                            base
                       FROM j_1btxip2
                      WHERE matnr     EQ @lv_material
                        AND validfrom GE @lv_data
                       INTO @DATA(ls_1btxip2).


              IF ls_1btxip2-matnr IS NOT INITIAL.
                lv_rate = ls_1btxip2-rate.
                lv_base = ls_1btxip2-base.

              ELSE.

                "Segunda seleção da alíquota e base de ICMS
                CALL FUNCTION 'J_1B_READ_DYNAMIC_TABLE'
                  EXPORTING
                    caller         = 'MM'
                    country        = lv_pais
                    state_from     = lv_uf_origem
                    state_to       = lv_uf_destino
                    material       = lv_material
                    material_group = lv_grupo
                    material_usage = lv_utilizacao
                    material_orig  = lv_origem
                    mwskz          = lv_iva
                    vendor         = lv_fornecedor
                    ncm            = lv_ncm
                    date           = lv_data
                    ipi            = 'X'
                    i_bukrs        = lv_empresa
                    i_werks        = lv_centro
                  IMPORTING
                    rate           = lv_rate
                    base           = lv_base
                    amount         = lv_amount
                    factor         = lv_factor
                    unit           = lv_unit
                    tax_group      = lv_tax_group.

                IF lv_rate IS NOT INITIAL OR
                   lv_base IS NOT INITIAL OR
                   lv_tax_group IS NOT INITIAL.

                ELSE.

                  "Terceira seleção ICMS Origem e Destino
                  CALL FUNCTION 'J_1B_READ_TXIP1'
                    EXPORTING
                      ncm_code = lv_ncm
                      date     = lv_data
                    IMPORTING
                      rate     = lv_rate.

                  IF lv_rate IS NOT INITIAL.
                    lv_base = '100'.
                  ENDIF.
                ENDIF.
              ENDIF.

              "Seleção da alíquota e base de PIS
            WHEN 'PIS'.

              "Seleção única alíquota e base PIS.
              CALL FUNCTION 'J_1B_READ_DYNAMIC_TABLE'
                EXPORTING
                  caller         = 'MM'
                  country        = lv_pais
                  state_from     = lv_uf_origem
                  state_to       = lv_uf_destino
                  material       = lv_material
                  material_group = lv_grupo
                  material_usage = lv_utilizacao
                  material_orig  = lv_origem
                  mwskz          = lv_iva
                  vendor         = lv_fornecedor
                  ncm            = lv_ncm
                  date           = lv_data
                  pis            = 'X'
                  i_bukrs        = lv_empresa
                  i_werks        = lv_centro
                IMPORTING
                  rate           = lv_rate
                  base           = lv_base
                  amount         = lv_amount
                  factor         = lv_factor
                  unit           = lv_unit
                  tax_group      = lv_tax_group.

              IF lv_rate IS INITIAL.
                CLEAR: lv_rate.
                CLEAR: lv_base.
              ENDIF.


              "Seleção da alíquota e base de COFINS
            WHEN 'COFI'.

              "Seleção única alíquota e base COFINS.
              CALL FUNCTION 'J_1B_READ_DYNAMIC_TABLE'
                EXPORTING
                  caller         = 'MM'
                  country        = lv_pais
                  state_from     = lv_uf_origem
                  state_to       = lv_uf_destino
                  material       = lv_material
                  material_group = lv_grupo
                  material_usage = lv_utilizacao
                  material_orig  = lv_origem
                  mwskz          = lv_iva
                  vendor         = lv_fornecedor
                  ncm            = lv_ncm
                  date           = lv_data
                  cofins         = 'X'
                  i_bukrs        = lv_empresa
                  i_werks        = lv_centro
                IMPORTING
                  rate           = lv_rate
                  base           = lv_base
                  amount         = lv_amount
                  factor         = lv_factor
                  unit           = lv_unit
                  tax_group      = lv_tax_group.

              IF lv_rate IS INITIAL.
                CLEAR: lv_rate.
                CLEAR: lv_base.
              ENDIF.

          ENDCASE.
        ENDIF.
      ENDIF.

    ENDIF.

*- Preenchimento valores
    IF lv_rate IS NOT INITIAL OR
       lv_base IS NOT INITIAL.

      IF <fs_fun_con>-high EQ 'A'.

        lv_rate        = ( lv_rate * 10 ).
        cs_xkomv-kbetr = lv_rate.
        cs_xkomv-kawrt = lv_rate.
        cv_xkwert      = lv_rate.

      ENDIF.

      IF <fs_fun_con>-high EQ 'B'.

        lv_base        = ( lv_base * 10 ).
        cs_xkomv-kbetr = lv_base.
        cs_xkomv-kawrt = lv_base.
        cv_xkwert      = lv_base.

      ENDIF.

    ELSE.

      cs_xkomv-kbetr = 0.
      cs_xkomv-kawrt = 0.
      cv_xkwert      = 0.

    ENDIF.

  ENDMETHOD.


  METHOD m_oil_calc_rv64a926.

*- Legenda ------------------------------------------------------------*
*     XWORKD - $ Valor Produto
*     XWORKE - $ Seguro
*     XWORKF - $ Frete
*     XWORKG - $ ICMS Monofásico
*     XWORKH - % Alíquota PIS + COFINS
*     XWORKI - $ Desconto
*     XWORKJ - $ Despesa
*     XWORKK - % Alíquota ICMS
*     XWORKL - % Redução Base ICMS
*     XWORKM - % Alíquota IPI

    CONSTANTS: lc_zodc TYPE kscha VALUE 'ZODC',
               lc_zodp TYPE kscha VALUE 'ZODP',
               lc_zofr TYPE kscha VALUE 'ZOFR',
               lc_zosg TYPE kscha VALUE 'ZOSG',
               lc_wotb TYPE kscha VALUE 'WOTB'.

*- Processamento ------------------------------------------------------*
    IF is_komk-trtyp EQ gc_trtyp-display.
      cs_komp-netwr  = cv_xkwert.
      RETURN.
    ENDIF.

    "Carregar variáveis para cálculo
    DATA(lv_valor_produto)   = CONV decfloat16( iv_xworkd ). "Valor bruto produto
    DATA(lv_valor_desconto)  = CONV decfloat16( iv_xworki ). "Valor bruto desconto
    DATA(lv_valor_despesa)   = CONV decfloat16( iv_xworkj ). "Valor bruto despesa
    DATA(lv_valor_frete)     = CONV decfloat16( iv_xworkf ). "Valor bruto frete
    DATA(lv_valor_seguro)    = CONV decfloat16( iv_xworke ). "Valor bruto seguro
    DATA(lv_valor_icms_mono) = CONV decfloat16( iv_xworkg ). "Valor ICMS Monofásico


    "Determinar Base
    DATA(lv_valor_base) = CONV decfloat16( 0 ).

    CASE cs_xkomv-kschl.
      WHEN lc_zodc. "Desconto Bruto
        lv_valor_base = lv_valor_desconto.
      WHEN lc_zodp. "Despesa Bruto
        lv_valor_base = lv_valor_despesa.
      WHEN lc_zofr. "Frete Bruto
        lv_valor_base = lv_valor_frete.
      WHEN lc_zosg. "Seguro Bruto
        lv_valor_base = lv_valor_seguro.
      WHEN lc_wotb. "Preço Bruto
        lv_valor_base = ( lv_valor_produto + lv_valor_desconto + lv_valor_despesa +  lv_valor_seguro ).
    ENDCASE.

    "Converter alíquotas dos impostos
    DATA(lv_aliq_ipi)           = CONV decfloat16( iv_xworkm / 1000 ). "Alíquota IPI
    DATA(lv_aliq_icms)          = CONV decfloat16( iv_xworkk / 1000 ). "Alíquota ICMS
    DATA(lv_aliq_red_base_icms) = CONV decfloat16( iv_xworkl / 1000 ). "Alíquota Redução de Base ICMS
    DATA(lv_aliq_pis_cofins)    = CONV decfloat16( iv_xworkh / 1000 ). "Alíquota PIS e COFINS

    "Cálcular valor do IPI
    DATA(lv_valor_ipi) = ( lv_valor_base * lv_aliq_ipi ).

    "Cálcular valores dos impostos
    DATA(lv_base_icms)            = CONV decfloat16( 0 ).
    DATA(lv_valor_icms)           = CONV decfloat16( 0 ).
    DATA(lv_base_pis_cofins_ipi)  = CONV decfloat16( 0 ).
    DATA(lv_base_pis_cofins)      = CONV decfloat16( 0 ).
    DATA(lv_valor_pis_cofins_ipi) = CONV decfloat16( 0 ).
    DATA(lv_valor_pis_cofins)     = CONV decfloat16( 0 ).
    DATA(lv_valor_icor)           = CONV decfloat16( 0 ).

    CASE cs_komp-mtuse.

      WHEN 0. "Material de revenda

        lv_base_icms  = ( lv_valor_base * lv_aliq_red_base_icms ). "Valor base do ICMS
        lv_valor_icms = ( lv_base_icms  * lv_aliq_icms ).          "Valor ICMS

        lv_base_pis_cofins_ipi  = ( lv_valor_base - lv_valor_icms ).                  "Valor base do PIS e COFINS sem IPI
        lv_valor_pis_cofins_ipi = ( lv_base_pis_cofins_ipi * lv_aliq_pis_cofins  ). "Valor PIS e COFINS com IPI na base

        lv_base_pis_cofins  = ( lv_valor_base - lv_valor_icms ).             "Valor base do PIS e COFINS sem ICMS
        lv_valor_pis_cofins = ( lv_base_pis_cofins * lv_aliq_pis_cofins  ). "Valor PIS e COFINS sem IPI na base

        lv_valor_icor = ( lv_valor_pis_cofins_ipi - lv_valor_pis_cofins ). "Diferença valor dos impostos PIS e COFINS com e sem o IPI no valor da base

        lv_valor_pis_cofins = 0.

      WHEN 1. "Material industrializado (contribuinte IPI)

        lv_base_icms  = lv_valor_base * lv_aliq_red_base_icms. "Valor base do ICMS
        lv_valor_icms = lv_base_icms  * lv_aliq_icms.          "Valor ICMS

        lv_base_pis_cofins  = ( lv_valor_base - lv_valor_icms ).       "Valor base do PIS e COFINS sem ICMS
        lv_valor_pis_cofins = lv_base_pis_cofins * lv_aliq_pis_cofins. "Valor PIS e COFINS sem valor do IPI na base

      WHEN 2 OR 3. "Material de consumo ou ativo

        lv_base_icms  = ( lv_valor_base * lv_aliq_red_base_icms ) + lv_valor_ipi. "Valor base do ICMS
        lv_valor_icms = ( lv_base_icms  * lv_aliq_icms ).                         "Valor ICMS

        lv_base_pis_cofins_ipi  = ( lv_valor_base - lv_valor_icms ). "Valor base do PIS e COFINS com IPI
        lv_valor_pis_cofins_ipi = ( lv_base_pis_cofins_ipi * lv_aliq_pis_cofins ). "Valor PIS e COFINS com IPI na base

      WHEN 4. "Material de consumo para atividade principal

        lv_base_icms  = ( lv_valor_base * lv_aliq_red_base_icms ). "Valor base do ICMS
        lv_valor_icms = ( lv_base_icms  * lv_aliq_icms ).          "Valor ICMS

        lv_base_pis_cofins_ipi  = ( lv_valor_base - lv_valor_icms ).                  "Valor base do PIS e COFINS sem IPI
        lv_valor_pis_cofins_ipi = ( lv_base_pis_cofins_ipi * lv_aliq_pis_cofins  ). "Valor PIS e COFINS com IPI na base

        lv_base_pis_cofins  = ( lv_valor_base - lv_valor_icms ).             "Valor base do PIS e COFINS sem ICMS
        lv_valor_pis_cofins = ( lv_base_pis_cofins * lv_aliq_pis_cofins  ). "Valor PIS e COFINS sem IPI na base

        lv_valor_icor = ( lv_valor_pis_cofins_ipi - lv_valor_pis_cofins ). "Diferença valor dos impostos PIS e COFINS com e sem o IPI no valor da base

        lv_valor_pis_cofins = 0.

    ENDCASE.

    "Calcular valores das condições
    DATA(lv_valor_liquido) = CONV decfloat16( 0 ).

    CASE cs_xkomv-kschl.
      WHEN lc_zodc. "Desconto Líquido
        lv_valor_liquido = ( lv_valor_desconto - lv_valor_icms - lv_valor_pis_cofins_ipi - lv_valor_pis_cofins ).
      WHEN lc_zodp. "Despesa Líquido
        lv_valor_liquido = ( lv_valor_despesa  - lv_valor_icms - lv_valor_pis_cofins_ipi - lv_valor_pis_cofins ).
      WHEN lc_zofr. "Frete Líquido
        lv_valor_liquido = ( lv_valor_frete    - lv_valor_icms - lv_valor_pis_cofins_ipi - lv_valor_pis_cofins ).
      WHEN lc_zosg. "Seguro Líquido
        lv_valor_liquido = ( lv_valor_seguro   - lv_valor_icms - lv_valor_pis_cofins_ipi - lv_valor_pis_cofins ).
      WHEN lc_wotb. "Preço Líquido
        lv_valor_liquido = ( lv_valor_produto + lv_valor_desconto - lv_valor_icms - lv_valor_pis_cofins_ipi - lv_valor_pis_cofins + lv_valor_icor - lv_valor_icms_mono ).
    ENDCASE.

    "Preencher retorno das condições
    cs_xkomv-kbetr = lv_valor_liquido.
    cs_xkomv-kawrt = lv_valor_liquido.
    cv_xkwert      = lv_valor_liquido.
    cs_komp-netpr  = lv_valor_liquido.
    cs_komp-netwr  = lv_valor_liquido.

  ENDMETHOD.
ENDCLASS.
