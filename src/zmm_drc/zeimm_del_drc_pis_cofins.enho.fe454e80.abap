"Name: \TY:CL_EDOC_BR_MAPPING_NP_TAXES\ME:MAP_TAXES_PIS\SE:END\EI
ENHANCEMENT 0 ZEIMM_DEL_DRC_PIS_COFINS.
 new zclmm_enhancements( )->m_drc_del_pis_futura(
   EXPORTING
     is_nfe_det = is_nfe_det
   CHANGING
     ct_mat_max = rt_mat_tax
 ).
ENDENHANCEMENT.
