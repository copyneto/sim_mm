class ZCLMM_PROCESS_PO_CUST definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_PROCESS_PO_CUST .
protected section.
private section.
ENDCLASS.



CLASS ZCLMM_PROCESS_PO_CUST IMPLEMENTATION.


  METHOD if_ex_me_process_po_cust~check.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~close.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~fieldselection_header.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~fieldselection_header_refkeys.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~fieldselection_item.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~fieldselection_item_refkeys.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~initialize.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~open.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~post.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~process_account.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~process_header.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~process_item.
    DATA(ls_item) = im_item->get_data( ).

    IF ls_item-pstyp EQ '7' AND
       ls_item-bstae IS INITIAL AND
       ls_item-werks IS NOT INITIAL.

      SELECT SINGLE b~bstae
        FROM t001w AS a
        JOIN lfm1 AS b "#EC CI_BUFFJOIN
          ON ( b~lifnr EQ a~lifnr
         AND b~ekorg EQ a~ekorg )
       WHERE a~werks EQ @ls_item-werks
        INTO @DATA(lv_bstae).

      IF sy-subrc EQ 0.
        ls_item-bstae = lv_bstae.
        im_item->set_data( im_data = ls_item ).
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~process_schedule.
    RETURN.
  ENDMETHOD.
ENDCLASS.
