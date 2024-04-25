***********************************************************************
*** © REDE SIM ***
***********************************************************************
***                                                                   *
*** DESCRIÇÃO: Movimentar Mercadorias Devolução                       *
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
REPORT zmmr_exe_movi_merc_dev.

TABLES: ztmm_doc_rem_rec.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_vbeln FOR ztmm_doc_rem_rec-vbeln.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  DATA(go_movi_merc_dev) = NEW zclmm_exe_movi_merc_dev( ).

  CALL METHOD go_movi_merc_dev->processar
    EXPORTING
      ir_vbeln = s_vbeln[].

  MESSAGE TEXT-003 TYPE 'S'.
