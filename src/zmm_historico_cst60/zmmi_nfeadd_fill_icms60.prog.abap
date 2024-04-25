*&---------------------------------------------------------------------*
*& Include zmmi_nfeadd_fill_icms60
*&---------------------------------------------------------------------*
    TRY.
        DATA(lo_read_cst60) = NEW zclmm_read_hist_cst60( ).
        IF lo_read_cst60->check_is_activated(
          EXPORTING
            it_nflin    = it_nflin
            is_nfheader = is_header
          ) = abap_true.

          LOOP AT it_nflin ASSIGNING FIELD-SYMBOL(<fs_nflin>).
            READ TABLE ct_item ASSIGNING FIELD-SYMBOL(<fs_item_add>) WITH KEY itmnum = <fs_nflin>-itmnum.

            IF sy-subrc IS NOT INITIAL.
              APPEND INITIAL LINE TO ct_item ASSIGNING <fs_item_add>.
              <fs_item_add>-itmnum = <fs_nflin>-itmnum.
            ENDIF.

            CALL METHOD lo_read_cst60->fill_data
              EXPORTING
                is_nfitem    = <fs_nflin>
                is_nfheader  = is_header
              CHANGING
                cs_item_data = <fs_item_add>.
          ENDLOOP.

        ENDIF.
      CATCH zclca_msg_erro.
    ENDTRY.
