FUNCTION zfmmm_mistura_prod_carregament.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     VALUE(CS_PROC_MISTURA) TYPE  ZTMM_PROC_MISTUR
*"----------------------------------------------------------------------
  DATA: ls_bflushflags   TYPE  bapi_rm_flg,
        ls_bflushdatagen TYPE  bapi_rm_datgen,
        lv_confirmation  TYPE  bapi_rm_datkey-confirmation,
        ls_return        TYPE  bapiret2.

  ls_bflushflags-bckfltype = '01'.
  ls_bflushdatagen-materialnr = cs_proc_mistura-matnr.
  ls_bflushdatagen-prodplant = cs_proc_mistura-werks.
  ls_bflushdatagen-storageloc = cs_proc_mistura-lgort.
  ls_bflushdatagen-prodversion = cs_proc_mistura-verid.
  ls_bflushdatagen-postdate = sy-datum.
  ls_bflushdatagen-docdate = sy-datum.
  ls_bflushdatagen-docheadertxt = |{ cs_proc_mistura-vbeln }-{ cs_proc_mistura-posnr }|.
  ls_bflushdatagen-backflquant = cs_proc_mistura-lfimg.


  CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS'
    EXPORTING
      bflushflags   = ls_bflushflags
      bflushdatagen = ls_bflushdatagen
    IMPORTING
      confirmation  = lv_confirmation
      return        = ls_return.

  IF lv_confirmation IS INITIAL.

    cs_proc_mistura-status = '3'.

    IF ls_return-message IS NOT INITIAL.

      cs_proc_mistura-msg = ls_return-message.

    ELSEIF ls_return-number <> 0.

      MESSAGE ID ls_return-id
             TYPE ls_return-type
           NUMBER ls_return-number
             INTO cs_proc_mistura-msg
             WITH ls_return-message_v1
                  ls_return-message_v2
                  ls_return-message_v3
                  ls_return-message_v4.

    ELSE.

      cs_proc_mistura-msg = TEXT-t01.

    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ELSE.

    cs_proc_mistura-prtnr = lv_confirmation.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

ENDFUNCTION.
