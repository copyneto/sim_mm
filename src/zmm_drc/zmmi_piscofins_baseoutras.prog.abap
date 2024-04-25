*&---------------------------------------------------------------------*
*& Include          ZMMI_PISCOFINS_BASEOUTRAS
*&---------------------------------------------------------------------*
  DATA: lt_item       TYPE STANDARD TABLE OF j_1bnflin,
        lt_tax        TYPE STANDARD TABLE OF j_1bnfstx,
        lt_nf_tax_aux	TYPE j_1bnfstx_tab.

  DATA: lv_exit TYPE xfeld.

  lt_item[] = nf_item[].
  lt_tax[]  = nf_item_tax[].

  lt_nf_tax_aux = CORRESPONDING #( lt_tax ).

  zclmm_enhancements=>m_pis_cofins_baseoutras(
    EXPORTING
      is_nf_header   = nf_header
      it_nf_item     = lt_item
    CHANGING
      ct_nf_item_tax = lt_tax[] ).

  nf_item_tax[] = lt_tax[].
