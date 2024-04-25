*&---------------------------------------------------------------------*
*& Include ZMMI_TYPES_COMPATIBLE
*&---------------------------------------------------------------------*
    CONSTANTS: gc_modulo TYPE ztca_param_mod-modulo VALUE 'MM',
               gc_chave1 TYPE ztca_param_par-chave1 VALUE 'API_PEDIDO_COMPRAS',
               gc_chave2 TYPE ztca_param_par-chave2 VALUE 'PEDIDOS_TRANSFERÊNCIA',
               gc_chave3 TYPE ztca_param_par-chave3 VALUE 'TIPOS_DE_PEDIDO'.

    DATA:      lr_po_types     TYPE RANGE OF bsart.

    DATA(lo_param) = NEW zclca_tabela_parametros( ).

    TRY.
        lo_param->m_get_range(
          EXPORTING
            iv_modulo = gc_modulo
            iv_chave1 = gc_chave1
            iv_chave2 = gc_chave2
            iv_chave3 = gc_chave3
          IMPORTING
            et_range  = lr_po_types
        ).
      CATCH zcxca_tabela_parametros.
    ENDTRY.

    IF lr_po_types[] IS NOT INITIAL.
      LOOP AT lr_po_types ASSIGNING FIELD-SYMBOL(<fs_po_types>).
        APPEND VALUE #( po_type           = <fs_po_types>-low  )
                   TO lst_po_types_comp_nb.

        APPEND VALUE #( po_type           = <fs_po_types>-low  )
                   TO rt_po_types.
      ENDLOOP.
    ENDIF.
