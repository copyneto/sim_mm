***********************************************************************
*** © REDE SIM ***
***********************************************************************
***                                                                   *
*** DESCRIÇÃO: Execução de Processos WMS                              *
*** AUTOR : Marcos Rubik – Meta                                       *
*** FUNCIONAL: Leandro Martins Silva                                  *
*** DATA : 13/11/2023                                                 *
***********************************************************************
*** HISTÓRICO DAS MODIFICAÇÕES *
***-------------------------------------------------------------------*
*** DATA | AUTOR | DESCRIÇÃO *
***-------------------------------------------------------------------*
*** 13/11/2023| Marcos Rubik | Versão Inicial                         *
***********************************************************************

*&---------------------------------------------------------------------*
*& Report ZMMR_REPLICA_MATERIAL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr_exe_proc_wms.

TABLES: ztmm_doc_rem_rec.

CONSTANTS:
  BEGIN OF gc_status_integracao,
    retorno       TYPE ze_status_int  VALUE '02', "Retorno integração
    erro_processo TYPE ze_status_int  VALUE '04', "Erro na execução do processo
  END OF gc_status_integracao.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_vbeln FOR ztmm_doc_rem_rec-vbeln,
                  s_ztipo FOR ztmm_doc_rem_rec-ztipo_doc OBLIGATORY.
  PARAMETERS:     p_zstat TYPE ztmm_doc_rem_rec-zstatus_integracao OBLIGATORY DEFAULT '02'.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  IF p_zstat <> gc_status_integracao-retorno AND
     p_zstat <> gc_status_integracao-erro_processo.
    MESSAGE TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  DATA(go_proc_wms) = NEW zclmm_exe_proc_wms( ).

  CALL METHOD go_proc_wms->processar
    EXPORTING
      ir_vbeln = s_vbeln[]
      ir_ztipo = s_ztipo[]
      iv_zstat = p_zstat.

  MESSAGE TEXT-003 TYPE 'S'.
