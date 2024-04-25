FUNCTION zfmmm_wms_integrar_docinv.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_PAYLOAD) TYPE  STRING
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"     VALUE(EV_RETURN) TYPE  STRING
*"----------------------------------------------------------------------
  DATA ls_payload TYPE zclmm_wms_integrar_docinv=>ty_payload.

  DATA(lo_cpi)         = NEW zclca_cpi( ).
  DATA(lo_cpi_monitor) = NEW zclca_monitor_cpi( ).

  /ui2/cl_json=>deserialize(
    EXPORTING
      json             = iv_payload
    CHANGING
      data             = ls_payload
  ).

  CALL METHOD lo_cpi->send
    EXPORTING
      iv_processo  = zclmm_wms_integrar_docinv=>gc_cpi-processo
      iv_metodo    = zclmm_wms_integrar_docinv=>gc_cpi-method_post
      is_structure = ls_payload
    IMPORTING
      ev_result    = ev_return
      et_return    = DATA(lt_return).

  IF ( lt_return IS NOT INITIAL ).
    APPEND VALUE #(
      id         = zclmm_wms_integrar_docinv=>gc_message_id
      type       = 'E'
      number     = 003
      message_v1 = ls_payload-header-iblnr
    ) TO et_return.
  ELSE.
    APPEND VALUE #(
      id         = zclmm_wms_integrar_docinv=>gc_message_id
      type       = 'S'
      number     = 002
      message_v1 = ls_payload-header-iblnr
    ) TO et_return.
  ENDIF.

  lo_cpi_monitor->started_process(
    EXPORTING
      iv_processo  = zclmm_wms_integrar_docinv=>gc_cpi-processo
      iv_metodo    = zclmm_wms_integrar_docinv=>gc_cpi-method_post
      iv_chave_ref = conv #( ls_payload-header-iblnr && ls_payload-header-gjahr )
      iv_json      = iv_payload
  ).

  lo_cpi_monitor->save_log(
    EXPORTING
      iv_processo     = zclmm_wms_integrar_docinv=>gc_cpi-processo
      iv_metodo       = zclmm_wms_integrar_docinv=>gc_cpi-method_post
      iv_json_retorno = ev_return
      iv_json         = iv_payload
      it_return       = et_return
  ).

ENDFUNCTION.
