***********************************************************************
*** © REDE SIM ***
***********************************************************************
*** *
*** DESCRIÇÃO: Replicação de dados de material                        *
*** AUTOR : Flavia Nunes – Meta                                       *
*** FUNCIONAL: Fabio Luis Valenga – Meta                              *
*** DATA : 06/10/2023                                                 *
***********************************************************************
*** HISTÓRICO DAS MODIFICAÇÕES *
***-------------------------------------------------------------------*
*** DATA | AUTOR | DESCRIÇÃO *
***-------------------------------------------------------------------*
*** 06/10/2023| Flavia Nunes | Versão Inicial                         *
***********************************************************************

*&---------------------------------------------------------------------*
*& Report ZMMR_REPLICA_MATERIAL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr_replica_material.

TABLES mara.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_manual AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS:
  s_matnr FOR mara-matnr NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.

  DATA lt_return TYPE TABLE OF ztmm_log_mat.
  DATA(go_replica) = NEW zclmm_replica_material( ).

  CALL METHOD go_replica->criar_dados
    EXPORTING
      ir_material = s_matnr[]
      iv_manual   = p_manual
    IMPORTING
      et_return   = lt_return.

  IF sy-subrc <> 0.
    MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL METHOD go_replica->exibir_log
    CHANGING
      ct_return = lt_return.
