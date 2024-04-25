CLASS zclmm_define_calctype DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_ex_me_define_calctype .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLMM_DEFINE_CALCTYPE IMPLEMENTATION.


  METHOD if_ex_me_define_calctype~define_calctype.
    CONSTANTS: lc_pb00 TYPE j_1baj-taxtyp VALUE 'PB00', "Condição PB00
               lc_pbxx TYPE j_1baj-taxtyp VALUE 'PBXX'. "Condição PBXX

    DATA: lv_brtwr      TYPE komv-kbetr,
          lv_komv_value TYPE komv-kbetr.

    FIELD-SYMBOLS: <fs_tkomv_tab> TYPE komv_tab.

    "Quando for consignação não deverá executar o processo.
    IF ch_x_lf_calctype EQ 'A' AND IM_X_NBEKPO-PSTYP EQ '2'. "JSILVA - 05/02/2024 - DS4K903203 - GAP-MM-10 - Pricing Preço Bruto Pedido de Compras-TAXBR
        RETURN.
    ENDIF.

*   Verificar se atualização de preço é diferente de B ou A (Redetermina preço)
    CHECK ch_x_lf_calctype NE 'B'.

*   Reprecificar quando os parâmetros abaixo forem alterados
    IF ( im_x_obekpo-mwskz      NE  im_x_nbekpo-mwskz )      "IVA
    OR ( im_x_obekpo-menge      NE  im_x_nbekpo-menge )      "Quantidade
    OR ( im_x_obekpo-j_1bnbm    NE  im_x_nbekpo-j_1bnbm )    "NCM
    OR ( im_x_obekpo-peinh      NE  im_x_nbekpo-peinh )      "Dividido por
    OR ( im_x_obekpo-j_1bmatorg NE  im_x_nbekpo-j_1bmatorg ) "Origem
    OR ( im_x_obekpo-netpr      NE  im_x_nbekpo-netpr )
    OR ( im_x_obekpo-netwr      NE  im_x_nbekpo-netwr ).
      ch_x_lf_calctype = 'C'.
      RETURN.
    ENDIF.

    ASSIGN ('(SAPLMEPO)TKOMV[]') TO <fs_tkomv_tab>.
    IF <fs_tkomv_tab> IS ASSIGNED.

*   Leitura do preço bruto informado na tela do pedido
      CLEAR: lv_brtwr.

      LOOP AT <fs_tkomv_tab> ASSIGNING FIELD-SYMBOL(<fs_tkomv>).
        CHECK <fs_tkomv>-kposn EQ im_x_nbekpo-ebelp.
        CHECK <fs_tkomv>-kschl EQ lc_pb00
           OR <fs_tkomv>-kschl EQ lc_pbxx.

        IF im_x_nbekpo-menge = 0.
          lv_brtwr = 0.
        ELSE.
          lv_brtwr = im_x_obekpo-brtwr / im_x_nbekpo-menge.
        ENDIF.

        IF <fs_tkomv>-kpein IS INITIAL.
          lv_komv_value = 0.
        ELSE.
          lv_komv_value = <fs_tkomv>-kbetr / <fs_tkomv>-kpein.
        ENDIF.

        IF lv_komv_value NE lv_brtwr.       "Preço bruto total
          ch_x_lf_calctype = 'C'.
          RETURN.
        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
