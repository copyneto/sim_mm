FUNCTION zfmmm_interface_00001250.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_MARA_NEW) LIKE  MARA STRUCTURE  MARA OPTIONAL
*"     VALUE(I_MARA_OLD) LIKE  MARA STRUCTURE  MARA OPTIONAL
*"     VALUE(UPD_MARA) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MARC_NEW) LIKE  MARC STRUCTURE  MARC OPTIONAL
*"     VALUE(I_MARC_OLD) LIKE  MARC STRUCTURE  MARC OPTIONAL
*"     VALUE(UPD_MARC) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MARD_NEW) LIKE  MARD STRUCTURE  MARD OPTIONAL
*"     VALUE(I_MARD_OLD) LIKE  MARD STRUCTURE  MARD OPTIONAL
*"     VALUE(UPD_MARD) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MBEW_NEW) LIKE  MBEW STRUCTURE  MBEW OPTIONAL
*"     VALUE(I_MBEW_OLD) LIKE  MBEW STRUCTURE  MBEW OPTIONAL
*"     VALUE(UPD_MBEW) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MFHM_NEW) LIKE  MFHM STRUCTURE  MFHM OPTIONAL
*"     VALUE(I_MFHM_OLD) LIKE  MFHM STRUCTURE  MFHM OPTIONAL
*"     VALUE(UPD_MFHM) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MLGN_NEW) LIKE  MLGN STRUCTURE  MLGN OPTIONAL
*"     VALUE(I_MLGN_OLD) LIKE  MLGN STRUCTURE  MLGN OPTIONAL
*"     VALUE(UPD_MLGN) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MLGT_NEW) LIKE  MLGT STRUCTURE  MLGT OPTIONAL
*"     VALUE(I_MLGT_OLD) LIKE  MLGT STRUCTURE  MLGT OPTIONAL
*"     VALUE(UPD_MLGT) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MPGD_NEW) LIKE  MPGD STRUCTURE  MPGD OPTIONAL
*"     VALUE(I_MPGD_OLD) LIKE  MPGD STRUCTURE  MPGD OPTIONAL
*"     VALUE(UPD_MPGD) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MPOP_NEW) LIKE  MPOP STRUCTURE  MPOP OPTIONAL
*"     VALUE(I_MPOP_OLD) LIKE  MPOP STRUCTURE  MPOP OPTIONAL
*"     VALUE(UPD_MPOP) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(I_MVKE_NEW) LIKE  MVKE STRUCTURE  MVKE OPTIONAL
*"     VALUE(I_MVKE_OLD) LIKE  MVKE STRUCTURE  MVKE OPTIONAL
*"     VALUE(UPD_MVKE) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MAKT) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MARM) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MEAN) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MLAN) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_ICDTXT_MATERIAL) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_PROW) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_GESV) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_UNGV) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MAEX) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MAPE) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_MKAL) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(UPD_QMAT) LIKE  CDPOS-CHNGIND OPTIONAL
*"     VALUE(FL_UPD_TASK) TYPE  XFELD OPTIONAL
*"  TABLES
*"      T_ICDTXT_MATERIAL STRUCTURE  CDTXT OPTIONAL
*"      T_MAKT_NEW STRUCTURE  XDMAKT OPTIONAL
*"      T_MAKT_OLD STRUCTURE  XDMAKT OPTIONAL
*"      T_MARM_NEW STRUCTURE  XDMARM OPTIONAL
*"      T_MARM_OLD STRUCTURE  XDMARM OPTIONAL
*"      T_MEAN_NEW STRUCTURE  XDMEAN OPTIONAL
*"      T_MEAN_OLD STRUCTURE  XDMEAN OPTIONAL
*"      T_MLAN_NEW STRUCTURE  XDMLAN OPTIONAL
*"      T_MLAN_OLD STRUCTURE  XDMLAN OPTIONAL
*"      T_MAEX_NEW STRUCTURE  XDMAEX OPTIONAL
*"      T_MAEX_OLD STRUCTURE  XDMAEX OPTIONAL
*"      T_MAPE_NEW STRUCTURE  XDMAPE OPTIONAL
*"      T_MAPE_OLD STRUCTURE  XDMAPE OPTIONAL
*"      T_MKAL_NEW STRUCTURE  XDMKAL OPTIONAL
*"      T_MKAL_OLD STRUCTURE  XDMKAL OPTIONAL
*"      T_PROW_NEW STRUCTURE  XDPROW OPTIONAL
*"      T_PROW_OLD STRUCTURE  XDPROW OPTIONAL
*"      T_QMAT_NEW STRUCTURE  XDQMAT OPTIONAL
*"      T_QMAT_OLD STRUCTURE  XDQMAT OPTIONAL
*"      T_GESV_NEW STRUCTURE  XDGESV OPTIONAL
*"      T_GESV_OLD STRUCTURE  XDGESV OPTIONAL
*"      T_UNGV_NEW STRUCTURE  XDUNGV OPTIONAL
*"      T_UNGV_OLD STRUCTURE  XDUNGV OPTIONAL
*"----------------------------------------------------------------------
  IF upd_mara IS NOT INITIAL OR
     upd_marc IS NOT INITIAL OR
     upd_mard IS NOT INITIAL OR
     upd_mbew IS NOT INITIAL OR
     upd_mfhm IS NOT INITIAL OR
     upd_mlgn IS NOT INITIAL OR
     upd_mlgt IS NOT INITIAL OR
     upd_mpgd IS NOT INITIAL OR
     upd_mpop IS NOT INITIAL OR
     upd_mvke IS NOT INITIAL OR
     upd_makt IS NOT INITIAL OR
     upd_marm IS NOT INITIAL OR
     upd_mean IS NOT INITIAL OR
     upd_mlan IS NOT INITIAL OR
     upd_icdtxt_material IS NOT INITIAL OR
     upd_prow IS NOT INITIAL OR
     upd_gesv IS NOT INITIAL OR
     upd_ungv IS NOT INITIAL OR
     upd_maex IS NOT INITIAL OR
     upd_mape IS NOT INITIAL OR
     upd_mkal IS NOT INITIAL OR
     upd_qmat IS NOT INITIAL OR
     fl_upd_task IS NOT INITIAL
    .

    NEW zclsd_valida_mensageria(  )->valida_produto_mm( iv_matnr = i_mara_new-matnr ).

  ENDIF.

ENDFUNCTION.
