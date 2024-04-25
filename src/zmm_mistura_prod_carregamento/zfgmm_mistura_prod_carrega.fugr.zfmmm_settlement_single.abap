FUNCTION zfmmm_settlement_single.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     VALUE(CS_PROC_MISTURA) TYPE  ZTMM_PROC_MISTUR
*"----------------------------------------------------------------------
  DATA: ls_auak         TYPE auak,
        ls_control      TYPE kabr_control,
        lt_bel_all      TYPE kabrt_bel_all_table,
        lt_proc_mistura TYPE TABLE OF ztmm_proc_mistur,
        lv_aufnr        TYPE j_objnr,
        ls_e_lst        TYPE kabr_lst.

  CLEAR:  lv_aufnr,
          ls_auak,
          ls_control,
          ls_e_lst,
          lt_bel_all[],
          lt_proc_mistura[].

  lv_aufnr   = |OR{ cs_proc_mistura-aufnr }|.

  ls_auak-kokrs   = '1000'.
  ls_auak-gjahr   = cs_proc_mistura-data(4).
  ls_auak-perio   = cs_proc_mistura-data+4(2).
  ls_auak-objnr   = lv_aufnr.
  ls_auak-co_vaart = '1'.
  ls_auak-saprl    = sy-saprl.

  ls_control-wrttp            = '04'.
  ls_control-detaillist       = 'X'.
  ls_control-tdcheck          = 'X'.
  ls_control-selart           = 'OR1'.
  ls_control-ok               = 'AUSF'.
  ls_control-afpo_pre_read    = 'X'.
  ls_control-coslv_read       = 'X'.
  ls_control-single_processing = 'X'.
  ls_control-mess_identif     = sy-uzeit.

  CALL FUNCTION 'MESSAGES_INITIALIZE'.

  CALL FUNCTION 'K_SETTLEMENT_SINGLE_PROCESS'
    EXPORTING
      i_objnr    = lv_aufnr
      i_auak     = ls_auak
      i_control  = ls_control
      it_bel_all = lt_bel_all
    IMPORTING
      e_lst      = ls_e_lst.

  IF ls_e_lst-subrc <> 0.

    cs_proc_mistura-status = '4'.
    cs_proc_mistura-msg = TEXT-t03."'Não foi possível estornar a liquidação da Ordem'.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ELSE.
    cs_proc_mistura-status = '2'.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDIF.

ENDFUNCTION.
