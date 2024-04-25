*&---------------------------------------------------------------------*
*& Report zmmr_wms_envio_doc_recebimento
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr_wms_envio_doc_material.
TABLES: ztmm_doc_mat, t001.

DATA go_integracao TYPE REF TO zclmm_wms_integrar_docmaterial.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  SELECT-OPTIONS:
  s_mblnr  FOR ztmm_doc_mat-mblnr,
  s_erdat  FOR ztmm_doc_mat-erdat,
  s_ztipo  FOR ztmm_doc_mat-ztipo_doc,
  s_status FOR ztmm_doc_mat-zstatus_integracao,
  s_werks  FOR ztmm_doc_mat-werks,
  s_bukrs  FOR t001-bukrs.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
  PARAMETERS:
  p_test TYPE char1 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  go_integracao = NEW #( ).

  go_integracao->get_parameters(
    IMPORTING
      er_empresa = DATA(lt_empresa)
      er_centro  = DATA(lt_centro)
  ).

  s_werks[]  = CORRESPONDING #( lt_centro ).
  s_bukrs[]  = CORRESPONDING #( lt_empresa ).
  s_Status[] = go_integracao->get_default_status( ).

  LOOP AT SCREEN.
    IF screen-name CS 'S_WERKS-'
    OR screen-name CS 'S_BUKRS-'
    OR screen-name CS 'S_STATUS-'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.

  go_integracao->exec_integracao(
    EXPORTING
      ir_documento     = CORRESPONDING #( s_mblnr[]  )
      ir_datadocumento = CORRESPONDING #( s_erdat[]  )
      ir_tipodocumento = CORRESPONDING #( s_ztipo[]  )
      ir_statusintegr  = CORRESPONDING #( s_status[] )
      ir_centro        = CORRESPONDING #( s_werks[]  )
      ir_empresa       = CORRESPONDING #( s_bukrs[]  )
      iv_test          = p_test
    RECEIVING
      rt_return        = DATA(lt_return)
  ).

  TRY.
      CALL METHOD cl_salv_table=>factory(
        IMPORTING
          r_salv_table = DATA(go_Alv)
        CHANGING
          t_table      = lt_return
      ).

      DATA(go_columns) = go_alv->get_columns( ).
      go_columns->get_column( TEXT-c04 )->set_visible( abap_false ).

      go_alv->display( ).
    CATCH cx_salv_msg cx_salv_not_found.
  ENDTRY.
