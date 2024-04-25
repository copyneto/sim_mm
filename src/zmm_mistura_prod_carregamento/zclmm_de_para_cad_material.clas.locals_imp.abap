CLASS lcl_Manutencao DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Manutencao RESULT result.
    METHODS validarcampos FOR VALIDATE ON SAVE
      IMPORTING keys FOR manutencao~validarcampos.
    METHODS validardeposito FOR VALIDATE ON SAVE
      IMPORTING keys FOR manutencao~validardeposito.
ENDCLASS.

CLASS lcl_Manutencao IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD ValidarCampos.

    READ ENTITIES OF zi_mm_de_para_cad_material IN LOCAL MODE
  ENTITY Manutencao
    FIELDS ( Centro Material Versao ) WITH CORRESPONDING #( keys )
  RESULT DATA(lt_manutencao).

    SELECT matnr, werks, verid
    FROM mkal
    INTO TABLE @DATA(lt_mkal)
    WHERE werks <> ''.

    SORT: lt_mkal BY matnr werks verid,
          lt_manutencao BY Material Centro Versao.

    LOOP AT lt_manutencao ASSIGNING FIELD-SYMBOL(<fs_manutencao>).

      READ TABLE lt_mkal ASSIGNING FIELD-SYMBOL(<fs_mkal>) WITH KEY matnr = <fs_manutencao>-material
                                                                    werks = <fs_manutencao>-centro
                                                                    verid = <fs_manutencao>-versao BINARY SEARCH.

      IF sy-subrc IS NOT INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_manutencao>-material
          IMPORTING
            output = <fs_manutencao>-material.

        APPEND VALUE #( %tky = <fs_manutencao>-%tky ) TO reported-manutencao.
        reported-manutencao = VALUE #(  ( %msg = new_message( id = 'ZMM_MISTURA_COMB'
                                                             number = '000'
                                                             v1 = <fs_manutencao>-material
                                                             v2 = <fs_manutencao>-centro
                                                             severity = if_abap_behv_message=>severity-error )
                                                             ) ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD ValidarDeposito.

    READ ENTITIES OF zi_mm_de_para_cad_material IN LOCAL MODE
     ENTITY Manutencao
     FIELDS ( Centro Material Versao deposito_rev ) WITH CORRESPONDING #( keys )
     RESULT DATA(lt_manutencao).

    SELECT werks, lgort
    FROM t001l "#EC CI_SGLSELECT
    INTO TABLE @DATA(lt_t001l)
    WHERE lgort <> ''.

    SORT: lt_t001l BY lgort,
          lt_manutencao BY Deposito_rev.

    LOOP AT lt_manutencao ASSIGNING FIELD-SYMBOL(<fs_manutencao>).

      READ TABLE lt_t001l ASSIGNING FIELD-SYMBOL(<fs_t001l>) WITH KEY lgort = <fs_manutencao>-Deposito_rev BINARY SEARCH.

      IF sy-subrc IS NOT INITIAL.

        APPEND VALUE #( %tky = <fs_manutencao>-%tky ) TO reported-manutencao.
        reported-manutencao = VALUE #(  ( %msg = new_message( id = 'ZMM_MISTURA_COMB'
                                                             number = '001'
                                                             v1 = <fs_manutencao>-Deposito_rev
                                                             severity = if_abap_behv_message=>severity-error )
                                                             ) ).

      ENDIF.

    ENDLOOP.


  ENDMETHOD.

ENDCLASS.
