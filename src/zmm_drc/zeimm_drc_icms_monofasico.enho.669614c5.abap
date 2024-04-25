"Name: \TY:CL_EDOC_BR_FM_ENTITY_MAPPER\ME:MAP_MAT_ORDERED\SE:END\EI
ENHANCEMENT 0 ZEIMM_DRC_ICMS_MONOFASICO.

DATA(lo_enhancements) = NEW zclmm_enhancements( ).

lo_enhancements->m_drc_icms_monofasico(
  EXPORTING
    iv_value = CONV #( is_goods_detail-imposto-icms-icms15-vicmsmono )
    iv_orig  = is_goods_detail-imposto-icms-icms15-orig
  CHANGING
    cv_netpr    = rs_mat_ordered-netpr
    cv_orig     = rs_mat_ordered-orig
).

lo_enhancements->m_drc_icms_monofasico(
  EXPORTING
    iv_value = CONV #( is_goods_detail-imposto-icms-icms53-vicmsmonoop )
    iv_orig  = is_goods_detail-imposto-icms-icms53-orig
  CHANGING
    cv_netpr    = rs_mat_ordered-netpr
    cv_orig     = rs_mat_ordered-orig
).

lo_enhancements->m_drc_icms_monofasico(
  EXPORTING
    iv_value = CONV #( is_goods_detail-imposto-icms-icms61-vicmsmonoret )
    iv_orig  = is_goods_detail-imposto-icms-icms61-orig
  CHANGING
    cv_netpr    = rs_mat_ordered-netpr
    cv_orig     = rs_mat_ordered-orig
).

lo_enhancements->m_drc_icms_monofasico(
  EXPORTING
    iv_value = CONV #( is_goods_detail-imposto-icms-icms02-vicmsmono )
    iv_orig  = is_goods_detail-imposto-icms-icms02-orig
  CHANGING
    cv_netpr    = rs_mat_ordered-netpr
    cv_orig     = rs_mat_ordered-orig
).

ENDENHANCEMENT.
