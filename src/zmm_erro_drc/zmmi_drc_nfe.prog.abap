*&---------------------------------------------------------------------*
*& Include ZMMI_DRC_NFE
*&---------------------------------------------------------------------*

  DATA: lt_nf_item     TYPE ty_j_1bnflin,
        lt_nf_item_tax TYPE ty_j_1bnfstx.

  lt_nf_item_tax[] = nf_item_tax[].
  lt_nf_item[]     = nf_item[].

* ---------------------------------------------------------------------------
* Recalcula o valor do imposto PIS/COFINS pautado.
* ---------------------------------------------------------------------------
  TRY.
      zclmm_enhancements=>m_drc_pis_cofins_pauta_nfe( EXPORTING is_nf_header   = nf_header
                                                                it_nf_item     = lt_nf_item[]
                                                      CHANGING  ct_nf_item_tax = lt_nf_item_tax[] ).
    CATCH cx_root.
  ENDTRY.

* ---------------------------------------------------------------------------
* Recalcula o valor total da Nota Fiscal para compras Interestaduais
* ---------------------------------------------------------------------------
  TRY.
      zclmm_enhancements=>m_icms_st_statistical( EXPORTING it_nf_item_tax = lt_nf_item_tax[]
                                                 CHANGING  cs_ext_header  = ext_header ).
    CATCH cx_root.
  ENDTRY.

  nf_item_tax[] = lt_nf_item_tax[].
