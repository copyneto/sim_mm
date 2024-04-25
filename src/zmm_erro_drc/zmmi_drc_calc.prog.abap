*&---------------------------------------------------------------------*
*& Include ZMMI_DRC_CALC
*&---------------------------------------------------------------------*

DATA lv_exit TYPE xfeld.

 zclmm_enhancements=>m_drc_pis_cofins_pauta_calc(
  EXPORTING
    is_nf_item    = nf_item
    it_nf_tax     = nf_item_tax
  CHANGING
    cs_exit =  lv_exit
).

IF lv_exit IS NOT INITIAL.
  EXIT.
ENDIF.
