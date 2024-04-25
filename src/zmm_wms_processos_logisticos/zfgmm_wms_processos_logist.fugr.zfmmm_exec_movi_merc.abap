***********************************************************************
*** © REDE SIM ***
***********************************************************************
***                                                                   *
*** DESCRIÇÃO: RFC Execução Movimentação de Merccadorias - JOB        *
*** AUTOR : Jean Silva – Meta                                         *
*** FUNCIONAL: Leandro Martins Silva                                  *
*** DATA : 16/11/2023                                                 *
***********************************************************************
*** HISTÓRICO DAS MODIFICAÇÕES *
***-------------------------------------------------------------------*
*** DATA | AUTOR | DESCRIÇÃO *
***-------------------------------------------------------------------*
*** 16/11/2023| Jean Silva | Versão Inicial                           *
***********************************************************************

*&--------------------------------------------------------------------*
*& Função ZFMMM_EXEC_MOVI_MERC
*&--------------------------------------------------------------------*
*&
*&--------------------------------------------------------------------*
FUNCTION zfmmm_exec_movi_merc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_R_VBELN) TYPE  LMD_VL_T_VBELN_VL_RANGE OPTIONAL
*"  EXPORTING
*"     VALUE(ET_MESSAGES) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
  CONSTANTS lc_jobname TYPE btcjob VALUE 'ZMMR_EXE_MOVI_MERC_DEV'.

  DATA lv_number TYPE btcjobcnt.
  DATA ls_message TYPE bapiret2.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = lc_jobname
    IMPORTING
      jobcount         = lv_number
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.
  IF sy-subrc IS NOT INITIAL.
    APPEND VALUE bapiret2(
                     type         =  sy-msgty
                     id           =  sy-msgid
                     number       =  sy-msgno
                     message_v1   =  sy-msgv1
                     message_v2   =  sy-msgv2
                     message_v3   =  sy-msgv3
                     message_v4   =  sy-msgv4
                   ) TO et_messages.
    EXIT.
  ENDIF.

  SUBMIT (lc_jobname)
    USING SELECTION-SCREEN '1000'
    "WITH SELECTION-TABLE 'ZTMM_DOC_REM_REC'
    WITH s_vbeln IN it_r_vbeln
    "USING SELECTION-SET iv_variant " g_pname = nome programa and g_varnt nome da variante
    TO SAP-SPOOL
    DESTINATION '01'
    IMMEDIATELY abap_on
    KEEP IN SPOOL abap_off
    WITHOUT SPOOL DYNPRO
    VIA JOB lc_jobname NUMBER lv_number
    AND RETURN.

  IF sy-subrc IS NOT INITIAL.
    APPEND VALUE bapiret2(
                     type         =  sy-msgty
                     id           =  sy-msgid
                     number       =  sy-msgno
                     message_v1   =  sy-msgv1
                     message_v2   =  sy-msgv2
                     message_v3   =  sy-msgv3
                     message_v4   =  sy-msgv4
                   ) TO et_messages.
  ELSE.
    APPEND VALUE bapiret2(
                     type         =  'S'
                     message      =  TEXT-t01
                   ) TO et_messages.
  ENDIF.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = lv_number
      jobname              = lc_jobname
      strtimmed            = 'X'
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      OTHERS               = 8.
  IF sy-subrc IS NOT INITIAL.
    APPEND VALUE bapiret2(
                     type         =  sy-msgty
                     id           =  sy-msgid
                     number       =  sy-msgno
                     message_v1   =  sy-msgv1
                     message_v2   =  sy-msgv2
                     message_v3   =  sy-msgv3
                     message_v4   =  sy-msgv4
                   ) TO et_messages.
    EXIT.
  ENDIF.

ENDFUNCTION.
