"Name: \TY:CL_API_PRODUCT_DPC_EXT\IN:/IWBEP/IF_MGW_APPL_SRV_RUNTIME\ME:GET_EXPANDED_ENTITYSET\SE:END\EI
ENHANCEMENT 0 ZENH_EXPAND_MATCHAR_QUERY.
DATA: lt_characteristic TYPE TABLE OF bapimatcha.

DATA: lt_charac     TYPE TABLE OF zi_material_charmer.

IF lines( et_expanded_tech_clauses ) > 0.

  IF et_expanded_tech_clauses[ 1 ] = 'TO_MATCHARACTERISCEXT'.

    LOOP AT er_entityset->* ASSIGNING FIELD-SYMBOL(<fs_entity_set>).
      ASSIGN COMPONENT 'PRODUCT' OF STRUCTURE <fs_entity_set> TO FIELD-SYMBOL(<fs_product>).
      DATA(lv_product) = CONV matnr( <fs_product> ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_product
        IMPORTING
          output = lv_product.

      SHIFT lv_product LEFT DELETING LEADING space.

      DATA(lv_material) = CONV matnr18( lv_product ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_material
        IMPORTING
          output = lv_material.

      CALL FUNCTION 'BAPI_MATERIAL_GETCHARMERCHHIER'
        EXPORTING
          material        = lv_material
        TABLES
          characteristics = lt_characteristic.


      ASSIGN COMPONENT 'TO_MATCHARACTERISCEXT' OF STRUCTURE <fs_entity_set> TO FIELD-SYMBOL(<fs_charac1>).

      IF lt_characteristic IS NOT INITIAL.
        lt_charac = VALUE #(
          FOR ls_characteristic IN lt_characteristic (
            matnr           = lv_material
            name_char       = ls_characteristic-name_char
            descr_char      = ls_characteristic-descr_char
            relevancy       = ls_characteristic-relevancy
            char_value      = ls_characteristic-char_value
            descr_cval      = ls_characteristic-descr_cval
            char_value_long = ls_characteristic-char_value_long
            descr_cval_long = ls_characteristic-descr_cval_long
         ) ).
        <fs_charac1> = CORRESPONDING #( lt_charac ).
      ELSE.
        FREE <fs_charac1>.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDIF.
ENDENHANCEMENT.
