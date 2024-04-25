FUNCTION zfmmm_settlement_group.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_STATUS_SUC) TYPE  ZE_STATUS_MISTUR
*"     VALUE(IV_STATUS_ERR) TYPE  ZE_STATUS_MISTUR
*"  CHANGING
*"     VALUE(CS_PROC_MISTURA) TYPE  ZTMM_PROC_MISTUR
*"----------------------------------------------------------------------
  DATA: lt_rem_obj   TYPE kabrt_objtab_table,
        lt_trace_obj TYPE kabrt_objtab_table,
        lt_setyptab  TYPE kabrt_setyp_table,
        lt_objtab    TYPE TABLE OF jsto_pre,
        lt_set       TYPE kabrt_aufsel_table,
        lt_late_obj  TYPE kabrt_objtab_table,
        lt_add_sdr   TYPE kabrt_objtab_table,
        lt_bel_all   TYPE kabrt_bel_all_table,
        lt_matnr     TYPE kabrt_cose_sel_table,
        lt_wsum      TYPE kabrt_wsum_table,
        lt_gsum      TYPE kabrt_gsum_table,
        ls_lst       TYPE kabr_lst,
        ls_auak      TYPE auak,
        ls_control   TYPE kabr_control,
        lv_objnr     TYPE j_objnr.

  CONSTANTS: lc_kokrs    TYPE auak-kokrs  VALUE '1000',
             lc_wrttp    TYPE auak-wrttp VALUE '04',
             lc_co_vaart TYPE auak-co_vaart VALUE '1',
             lc_wrttp_r  TYPE kabr_control-wrttp VALUE '04',
             lc_selart   TYPE kabr_control-selart VALUE 'OR1',
             lc_ok       TYPE kabr_control-ok VALUE 'AUSF',
             lc_prefixo  TYPE string VALUE 'OR'.

  CLEAR: ls_auak,
         ls_control,
         ls_lst,
         lt_rem_obj[],
         lt_trace_obj[],
         lt_setyptab[],
         lt_objtab[],
         lt_set[],
         lt_late_obj[],
         lt_add_sdr[],
         lt_bel_all[],
         lt_matnr[],
         lt_wsum[],
         lt_gsum[].

  lv_objnr = |{ lc_prefixo }{ cs_proc_mistura-aufnr }|.

  ls_auak-kokrs    = lc_kokrs.
  ls_auak-cpudt    = sy-datum.
  ls_auak-budat    = sy-datum.
  ls_auak-gjahr    = sy-datum(4).
  ls_auak-bugjahr  = sy-datum(4).
  ls_auak-perio    = sy-datum+4(2).
  ls_auak-buperio  = sy-datum+4(2).
  ls_auak-wrttp    = lc_wrttp.
  ls_auak-saprl    = sy-saprl.
  ls_auak-objnr    = lv_objnr.
  ls_auak-co_vaart = '1'.
  CONDENSE ls_auak-co_vaart NO-GAPS.

  ls_control-wrttp             = lc_wrttp_r.
  ls_control-detaillist        = abap_true.
  ls_control-tdcheck           = abap_true.
  ls_control-selart            = lc_selart.
  ls_control-ok                = lc_ok.
  ls_control-afpo_pre_read     = abap_true.
  ls_control-coslv_read        = abap_true.
  ls_control-single_processing = abap_true.
  ls_control-mess_identif      = sy-uzeit.

  APPEND VALUE #( setyp = 'OR') TO lt_setyptab.
  APPEND VALUE #( objnr = lv_objnr ) TO lt_objtab.

  CALL FUNCTION 'MESSAGES_INITIALIZE'.

  CALL FUNCTION 'K_SETTLEMENT_GROUP_PROCESS'
    EXPORTING
      i_auak       = ls_auak
      i_control    = ls_control
      it_rem_obj   = lt_rem_obj
      it_trace_obj = lt_trace_obj
      it_setyptab  = lt_setyptab
    IMPORTING
      e_lst        = ls_lst
    TABLES
      it_objtab    = lt_objtab
    CHANGING
      ct_set       = lt_set
      ct_late_obj  = lt_late_obj
      ct_add_sdr   = lt_add_sdr
      ct_bel_all   = lt_bel_all
      ct_matnr     = lt_matnr
      ct_wsum      = lt_wsum
      ct_gsum      = lt_gsum.

  IF ls_lst-subrc <> 0.

    cs_proc_mistura-status = iv_status_err.
    cs_proc_mistura-msg = TEXT-t02.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ELSE.
    cs_proc_mistura-status = iv_status_suc.

    IF iv_status_suc = '1'.
      cs_proc_mistura-uname  = sy-uname.
      cs_proc_mistura-data  = sy-datum.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDIF.

ENDFUNCTION.
