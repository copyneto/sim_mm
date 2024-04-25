class ZCLMM_NFE_PRINT definition
  public
  final
  create public .

public section.

  interfaces IF_EX_CL_NFE_PRINT .
protected section.
private section.
ENDCLASS.



CLASS ZCLMM_NFE_PRINT IMPLEMENTATION.


  METHOD if_ex_cl_nfe_print~call_rsnast00.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~check_subsequent_documents.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~determine_matdoc_cancel_date.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~exclude_nfes_from_batch.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_add_inflin.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_autxml.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_cte.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_cte_200.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_cte_300.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_cte_400.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_export.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_fuel.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_header.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_import.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_item.
    CONSTANTS: lc_reftyp TYPE j_1bnflin-reftyp VALUE 'LI',
               lc_doctyp TYPE j_1baa-doctyp    VALUE '6',
               lc_direct TYPE j_1baa-direct    VALUE '2',
               lc_taxgrp TYPE j_1btaxgrp       VALUE 'IPI'.

    IF in_lin-reftyp EQ lc_reftyp.

      SELECT SINGLE nftype
      INTO @DATA(lv_nftype)
      FROM j_1baa
      WHERE nftype = @in_doc-nftype
        AND doctyp = @lc_doctyp
        AND direct = @lc_direct.

      IF sy-subrc IS INITIAL.
        IF out_item-pipidevol IS INITIAL.
          out_item-pipidevol = in_lin-pipidevol.

*          SORT in_tax BY docnum itmnum taxgrp.
          LOOP AT in_tax ASSIGNING FIELD-SYMBOL(<fs_in_tax>) WHERE docnum = in_lin-docnum
                                                               AND itmnum = in_lin-itmnum
                                                               AND taxgrp = lc_taxgrp.
            out_item-pipidevol = <fs_in_tax>-rate.
            out_item-vipidevol = <fs_in_tax>-taxval.
            out_item-docnum    = in_lin-docnum.
            out_item-itmnum    = in_lin-itmnum.
          ENDLOOP.

*          READ TABLE <fs_in_tax> ASSIGNING FIELD-SYMBOL(<fs_in_tax>) WITH KEY docnum = in_lin-docnum
*                                                                              itmnum = in_lin-itmnum
*                                                                              taxgrp = lc_taxgrp BINARY SEARCH.
*          IF sy-subrc IS INITIAL.
**            out_item-pipidevol = <fs_in_tax>-rate.
**            out_item-vipidevol = <fs_in_tax>-taxval.
*            out_item-docnum    = in_lin-docnum.
*            out_item-itmnum    = in_lin-itmnum.
*          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_nve.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_refnfe.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~fill_trace.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~get_server.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~get_server_dfe.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~is_icms_part_in_exception_list.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~reset_subrc.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~set_commit.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_cl_nfe_print~set_order_for_batch.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
