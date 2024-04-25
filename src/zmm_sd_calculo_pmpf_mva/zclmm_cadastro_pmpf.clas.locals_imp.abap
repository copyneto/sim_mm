CLASS lcl_Manutencao DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Manutencao RESULT result.

    METHODS valida_campos FOR VALIDATE ON SAVE
      IMPORTING keys FOR manutencao~valida_campos.

    METHODS build_reported
      IMPORTING it_return   TYPE bapiret2_t
      EXPORTING es_reported TYPE ty_reported.

ENDCLASS.

CLASS lcl_Manutencao IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD valida_campos.

    DATA: lt_return     TYPE bapiret2_t.

* ---------------------------------------------------------------------------
* Recupera dados das linhas selecionadas
* ---------------------------------------------------------------------------
    READ ENTITIES OF zi_mm_cadastro_pmpf IN LOCAL MODE ENTITY Manutencao
      FIELDS ( Chave_pais Regiao_fiscal Material Uni_preco Valido_des )
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_Manutencao)
      FAILED DATA(lt_failed).

    IF sy-subrc NE 0.
      " Registro não encontrado para a chave informada.
      lt_return = VALUE #( BASE lt_return ( type = 'E' id = 'ZMM_PMPF' number = '001' ) ).
    ENDIF.

* ---------------------------------------------------------------------------
* Verificar se o valor do campo existe
* ---------------------------------------------------------------------------

    SELECT COUNT(*)
    FROM t005
    FOR ALL ENTRIES IN @lt_manutencao
    WHERE land1 EQ @lt_manutencao-Chave_pais.

    IF sy-subrc IS NOT INITIAL.

      lt_return = VALUE #( BASE lt_return ( type = 'E' id = 'ZMM_PMPF' number = '003' ) ).

    ENDIF.

    SELECT COUNT(*)
    FROM j_1btregx
    FOR ALL ENTRIES IN @lt_manutencao
    WHERE txreg EQ @lt_manutencao-Regiao_fiscal.

    IF sy-subrc IS NOT INITIAL.

      lt_return = VALUE #( BASE lt_return ( type = 'E' id = 'ZMM_PMPF' number = '004' ) ).

    ENDIF.

    SELECT COUNT(*)
    FROM mara
    FOR ALL ENTRIES IN @lt_manutencao
    WHERE matnr EQ @lt_manutencao-material.

    IF sy-subrc IS NOT INITIAL.

      lt_return = VALUE #( BASE lt_return ( type = 'E' id = 'ZMM_PMPF' number = '005' ) ).

    ENDIF.

* ---------------------------------------------------------------------------
* Verifica se existe outra chave com mesmo módulo
* ---------------------------------------------------------------------------
    IF lt_Manutencao[] IS NOT INITIAL.

      SELECT land1, txreg, matnr, unit
          FROM ztmm_pmpf
          FOR ALL ENTRIES IN @lt_Manutencao
            WHERE land1 EQ @lt_Manutencao-Chave_pais
            AND txreg EQ @lt_Manutencao-Regiao_fiscal
            AND matnr EQ @lt_Manutencao-Material
            AND validfrom  EQ @lt_Manutencao-Valido_des
          INTO TABLE @DATA(lt_Manutencao_r).

      IF sy-subrc EQ 0.
        " O processo informado já existe.
        lt_return = VALUE #( BASE lt_return ( type = 'E' id = 'ZMM_PMPF' number = '002' field = 'CHAVE_PAIS' ) ).
      ENDIF.
    ENDIF.

* ---------------------------------------------------------------------------
* Retornar mensagens
* ---------------------------------------------------------------------------
    me->build_reported( EXPORTING it_return   = lt_return
                        IMPORTING es_reported = DATA(lt_reported) ).

    reported = CORRESPONDING #( DEEP lt_reported ).

  ENDMETHOD.

  METHOD build_reported.
    DATA: ls_Manutencao TYPE LINE OF ty_reported-Manutencao.

    FREE: es_reported.

    LOOP AT it_return INTO DATA(ls_return).

* ---------------------------------------------------------------------------
* Converte mensagem
* ---------------------------------------------------------------------------
      ls_Manutencao-%msg = new_message( id       = ls_return-id
                                        number   = ls_return-number
                                       v1       = ls_return-message_v1
                                        v2       = ls_return-message_v2
                                        v3       = ls_return-message_v3
                                        v4       = ls_return-message_v4
                                        severity = CONV #( ls_return-type ) ).

* ---------------------------------------------------------------------------
* Destaca o campo com erro
* ---------------------------------------------------------------------------
      IF ls_return-field IS NOT INITIAL.
        ASSIGN COMPONENT |%element-{ ls_return-field }| OF STRUCTURE ls_Manutencao TO FIELD-SYMBOL(<fs_field>).

        IF sy-subrc EQ 0.
          TRY.
              <fs_field> = if_abap_behv=>mk-on.
            CATCH cx_root.
          ENDTRY.
        ENDIF.
      ENDIF.

* ---------------------------------------------------------------------------
* Retorna as mensagens de erro no aplicativo
* ---------------------------------------------------------------------------
      es_reported-Manutencao[] = VALUE #( BASE es_reported-Manutencao[] ( CORRESPONDING #( ls_Manutencao ) ) ).

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
