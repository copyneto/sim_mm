"Name: \PR:SAPLJ_1B_NFE_IN\FO:COMPARE_TAX_RATES\SE:END\EI
ENHANCEMENT 0 ZEIMM_DEL_DRC_MSG_ICMS_FUTURA.
  NEW zclmm_enhancements( )->m_drc_del_msg_icms_futura(
    EXPORTING
      iv_process = i_process
      iv_taxsit  = ls_nfe_tax-taxsit
    CHANGING
      ct_return  = et_bapiret2
  ).
ENDENHANCEMENT.
