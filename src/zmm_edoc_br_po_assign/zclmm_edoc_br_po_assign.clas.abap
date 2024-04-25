CLASS zclmm_edoc_br_po_assign DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_edoc_br_po_assign .

    CONSTANTS: BEGIN OF gc_param_ativo,
                 modulo TYPE ztca_param_mod-modulo VALUE 'MM',
                 chave1 TYPE ztca_param_par-chave1 VALUE 'RETAIL',
                 chave2 TYPE ztca_param_par-chave2 VALUE 'PEDIDO_XML',
                 chave3 TYPE ztca_param_par-chave3 VALUE 'ATIVO',
               END OF gc_param_ativo,

               BEGIN OF gc_param_prefixo_po,
                 modulo TYPE ztca_param_mod-modulo VALUE 'MM',
                 chave1 TYPE ztca_param_par-chave1 VALUE 'RETAIL',
                 chave2 TYPE ztca_param_par-chave2 VALUE 'PEDIDO_XML',
                 chave3 TYPE ztca_param_par-chave3 VALUE 'PREFIXO_PO',
               END OF gc_param_prefixo_po,

               BEGIN OF gc_param_busca_sem_po,
                 modulo TYPE ztca_param_mod-modulo VALUE 'MM',
                 chave1 TYPE ztca_param_par-chave1 VALUE 'RETAIL',
                 chave2 TYPE ztca_param_par-chave2 VALUE 'PEDIDO_XML',
                 chave3 TYPE ztca_param_par-chave3 VALUE 'BUSCA_SEM_PO',
               END OF gc_param_busca_sem_po.

    METHODS find_purchase_order_in_info
      IMPORTING iv_text       TYPE string
                iv_prefixo_po TYPE char02 DEFAULT '45'
      EXPORTING ev_ebeln      TYPE ekpo-ebeln
                ev_ebelp      TYPE ekpo-ebelp.

    METHODS update_nfe_with_purchase_order
      IMPORTING is_nfe         TYPE edoc_br_nfe400nfe
                iv_ponumber    TYPE ekpo-ebeln
                iv_poitem      TYPE ekpo-ebelp
      CHANGING  cs_po_assigned TYPE edoc_po_assigned_cs.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zclmm_edoc_br_po_assign IMPLEMENTATION.


  METHOD if_edoc_br_po_assign~po_assign.

    " Tabela Interna
    DATA: lt_orgdata TYPE j_1bnfe_orgdata_tab,
          lt_vendor  TYPE j_1bnfe_vendor_tab.

    " Ranges
    DATA: lr_j_1bnbm TYPE RANGE OF j_1bnbmco1,
          lr_idnlf   TYPE RANGE OF idnlf,
          lr_ebeln   TYPE RANGE OF ebeln.

    " Variáveis
    DATA: lv_ativo        TYPE flag,
          lv_prefixo_po   TYPE char2,
          lv_busca_sem_po TYPE flag,
          lv_pedido       TYPE ebeln,
          lv_itempedido   TYPE ebelp.

    DATA(lo_param) = zclca_tabela_parametros=>get_instance( ).

    " Ponto A
    TRY.
        lo_param->m_get_single( EXPORTING iv_modulo = gc_param_ativo-modulo
                                          iv_chave1 = gc_param_ativo-chave1
                                          iv_chave2 = gc_param_ativo-chave2
                                          iv_chave3 = gc_param_ativo-chave3
                                IMPORTING ev_param  = lv_ativo ).

        lo_param->m_get_single( EXPORTING iv_modulo = gc_param_prefixo_po-modulo
                                          iv_chave1 = gc_param_prefixo_po-chave1
                                          iv_chave2 = gc_param_prefixo_po-chave2
                                          iv_chave3 = gc_param_prefixo_po-chave3
                                IMPORTING ev_param  = lv_prefixo_po ).

        lo_param->m_get_single( EXPORTING iv_modulo = gc_param_busca_sem_po-modulo
                                          iv_chave1 = gc_param_busca_sem_po-chave1
                                          iv_chave2 = gc_param_busca_sem_po-chave2
                                          iv_chave3 = gc_param_busca_sem_po-chave3
                                IMPORTING ev_param  = lv_busca_sem_po ).
      CATCH zcxca_tabela_parametros. " Classe de exceção Tabela de Parâmetros
        RETURN.
    ENDTRY.

    CHECK lv_ativo IS NOT INITIAL.

    " Ponto B
    DATA(ls_nfe) = io_nfe->get_nfe_structure( ).

    IF NOT line_exists( cs_po_assigned-po_assigned_tab[ ponumber = space ] ). "#EC CI_STDSEQ

      lr_ebeln = VALUE #( FOR <fs_ebeln> IN cs_po_assigned-po_assigned_tab
                  WHERE ( ponumber IS NOT INITIAL )
                        ( sign   = /bobf/if_conf_c=>sc_sign_option_including
                          option = /bobf/if_conf_c=>sc_sign_equal
                          low    = <fs_ebeln>-ponumber ) ). "#EC CI_STDSEQ

      DELETE ADJACENT DUPLICATES FROM lr_ebeln COMPARING low.

* BEGIN OF CHANGE - JWSILVA - 08.04.2024
      " Verificamos se o pedido existe. Se não existir, limpamos pois se trata de um pedido inválido.
      SELECT COUNT(*) FROM ekko WHERE ebeln IN @lr_ebeln.

      IF sy-subrc NE 0.
        FREE lr_ebeln.
      ENDIF.

    ENDIF.

    " Seguimos verificando se o pedido existe nas Informações Adicionais
    IF lr_ebeln[] IS INITIAL.
* END OF CHANGE - JWSILVA - 08.04.2024

      IF ls_nfe-infnfe-compra-xped IS NOT INITIAL.

* BEGIN OF CHANGE - JWSILVA - 18.04.2024
        me->find_purchase_order_in_info( EXPORTING iv_text       = ls_nfe-infnfe-compra-xped
                                                   iv_prefixo_po = lv_prefixo_po
                                         IMPORTING ev_ebeln      = lv_pedido
                                                   ev_ebelp      = lv_itempedido ).

        IF lv_pedido IS NOT INITIAL AND lv_itempedido IS NOT INITIAL.

          me->update_nfe_with_purchase_order( EXPORTING is_nfe         = ls_nfe
                                                        iv_ponumber    = lv_pedido
                                                        iv_poitem      = lv_itempedido
                                              CHANGING  cs_po_assigned = cs_po_assigned ).
          RETURN.

        ENDIF.
* END OF CHANGE - JWSILVA - 18.04.2024

      ELSE.

* BEGIN OF CHANGE - JWSILVA - 11.04.2024
        me->find_purchase_order_in_info( EXPORTING iv_text       = ls_nfe-infnfe-infadic-infcpl
                                                   iv_prefixo_po = lv_prefixo_po
                                         IMPORTING ev_ebeln      = lv_pedido
                                                   ev_ebelp      = lv_itempedido ).

        IF lv_pedido IS NOT INITIAL AND lv_itempedido IS NOT INITIAL.

          me->update_nfe_with_purchase_order( EXPORTING is_nfe         = ls_nfe
                                                        iv_ponumber    = lv_pedido
                                                        iv_poitem      = lv_itempedido
                                              CHANGING  cs_po_assigned = cs_po_assigned ).
          RETURN.

        ENDIF.
* END OF CHANGE - JWSILVA - 11.04.2024

      ENDIF.
    ENDIF.

    IF lv_pedido IS NOT INITIAL.
      APPEND INITIAL LINE TO lr_ebeln ASSIGNING FIELD-SYMBOL(<fs_pedido>).
      <fs_pedido>-sign   = /bobf/if_conf_c=>sc_sign_option_including.
      <fs_pedido>-option = /bobf/if_conf_c=>sc_sign_equal.
      <fs_pedido>-low    = lv_pedido.
    ENDIF.

    " Verifica se busca pode seguir sem Pedido
    IF lr_ebeln IS INITIAL AND
       lv_busca_sem_po EQ abap_false.
      EXIT.
    ENDIF.

    " Ponto C
    CALL FUNCTION 'J_1BNFE_GET_ORGDATA_FROM_CNPJ'
      EXPORTING
        i_cnpj     = io_nfe->retrieve_dest_cnpj( )
      IMPORTING
        et_orgdata = lt_orgdata
      EXCEPTIONS
        not_found  = 1.

    CHECK sy-subrc IS INITIAL.

    READ TABLE lt_orgdata INTO DATA(ls_orgdata) INDEX 1.

    IF ls_orgdata-werks IS INITIAL.
      EXIT.
    ENDIF.

    " Ponto D
    CALL FUNCTION 'J_1BNFE_GET_VENDOR_FROM_CNPJ'
      EXPORTING
        i_cnpj             = io_nfe->retrieve_emit_cnpj( )
      IMPORTING
        et_vendor          = lt_vendor
      EXCEPTIONS
        not_found          = 1
        missing_parameters = 2
        partner_blocked    = 3
        OTHERS             = 4.

    CHECK sy-subrc IS INITIAL.

    READ TABLE lt_vendor INTO DATA(ls_vendor) INDEX 1.

    IF ls_vendor-stcd1 IS INITIAL.
      EXIT.
    ENDIF.

    " Ponto E
    lr_j_1bnbm = VALUE #( FOR <fs_ncm> IN ls_nfe-infnfe-det
                        ( sign   = /bobf/if_conf_c=>sc_sign_option_including
                          option = /bobf/if_conf_c=>sc_sign_equal
                          low    = <fs_ncm>-prod-ncm(4) && '.' && <fs_ncm>-prod-ncm+4(2) && '.' && <fs_ncm>-prod-ncm+6(2) ) ).

    lr_idnlf   = VALUE #( FOR <fs_cprod> IN ls_nfe-infnfe-det
                        ( sign   = /bobf/if_conf_c=>sc_sign_option_including
                          option = /bobf/if_conf_c=>sc_sign_equal
                          low    = |{ <fs_cprod>-prod-cprod ALPHA = OUT }| ) ).

    " Ponto F
* BEGIN OF CHANGE - JWSILVA - 11.04.2024
    SELECT ebeln,
           ebelp,
           matnr,
           idnlf,
           j_1bnbm
      INTO TABLE @DATA(lt_ekpo)
      FROM zi_mm_edoc_br_purchase_order
     WHERE ebeln    IN @lr_ebeln
       AND lifnr    EQ @ls_vendor-lifnr
       AND idnlf    IN @lr_idnlf
       AND werks    EQ @ls_orgdata-werks
       AND loekz    EQ @abap_false
       AND elikz    EQ @abap_false.
* END OF CHANGE - JWSILVA - 11.04.2024

    IF sy-subrc EQ 0.
      SORT lt_ekpo BY ebeln ebelp.
    ENDIF.

* BEGIN OF CHANGE - JWSILVA - 11.04.2024
    SELECT ebeln,
           ebelp,
           matnr,
           idnlf,
           j_1bnbm
      INTO TABLE @DATA(lt_ekpo_ncm)
      FROM zi_mm_edoc_br_purchase_order
     WHERE ebeln    IN @lr_ebeln
       AND lifnr    EQ @ls_vendor-lifnr
       AND j_1bnbm  IN @lr_j_1bnbm
       AND werks    EQ @ls_orgdata-werks
       AND loekz    EQ @abap_false
       AND elikz    EQ @abap_false.
* END OF CHANGE - JWSILVA - 11.04.2024

    IF sy-subrc EQ 0.
      SORT lt_ekpo_ncm BY ebeln ebelp.
    ENDIF.

    " Ponto G
    LOOP AT ls_nfe-infnfe-det INTO DATA(ls_prod).

      DATA(lv_idnlf) = CONV idnlf( |{ ls_prod-prod-cprod ALPHA = OUT }| ).

      " Busca item NFe
      READ TABLE cs_po_assigned-po_assigned_tab ASSIGNING FIELD-SYMBOL(<fs_line_po>)
                                                WITH KEY nitem = ls_prod-nitem.

      IF sy-subrc IS NOT INITIAL.
        CONTINUE .
      ENDIF.

      " Procura PO para o Item NFe
      LOOP AT lt_ekpo INTO DATA(ls_ekpo) WHERE idnlf   EQ lv_idnlf
                                            OR j_1bnbm EQ ls_prod-prod-ncm.

        " Atribuir Pedido ao item NFe
        <fs_line_po>-ponumber = ls_ekpo-ebeln.
        <fs_line_po>-poitem   = ls_ekpo-ebelp.
        EXIT.

      ENDLOOP.

      LOOP AT lt_ekpo_ncm INTO DATA(ls_ekpo_ncm) WHERE idnlf   EQ lv_idnlf
                                                    OR j_1bnbm EQ ls_prod-prod-ncm.

        " NCM
        IF ls_ekpo_ncm-j_1bnbm EQ ls_prod-prod-ncm AND <fs_line_po>-poitem IS INITIAL.

          " Atribuir Pedido ao item NFe
          <fs_line_po>-ponumber = ls_ekpo_ncm-ebeln.
          <fs_line_po>-poitem   = ls_ekpo_ncm-ebelp.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_purchase_order_in_info.

    DATA: lt_ekpo_key TYPE STANDARD TABLE OF ekpo,
          lv_ebeln    TYPE c LENGTH 10,
          lv_ebelp    TYPE c LENGTH 6.

    FREE: ev_ebeln, ev_ebelp.

    " Verifica todos os pontos onde foi encontrado o prefixo do número do pedido
    FIND ALL OCCURRENCES OF iv_prefixo_po IN iv_text RESULTS DATA(lt_results).

* ---------------------------------------------------------------------------
* Prepara para encontrar o número e item do pedido
* ---------------------------------------------------------------------------
    DATA(lv_max_size) = strlen( iv_text ).

    LOOP AT lt_results INTO DATA(ls_results).

      DATA(lv_search)   = CONV char20( iv_text+ls_results-offset ).

      lv_ebeln      = lv_search+0(10).
      lv_ebelp      = lv_search+10(7).

      " Tratamento para item do pedido
      SHIFT lv_ebelp LEFT DELETING LEADING space.
      SPLIT lv_ebelp AT space INTO lv_ebelp DATA(lv_dummy).
      REPLACE ALL OCCURRENCES OF PCRE '[^0-9]' IN lv_ebelp WITH space.
      CONDENSE lv_ebelp NO-GAPS.

      " Adiciona chave para futura validação
      lt_ekpo_key = VALUE #( BASE lt_ekpo_key ( ebeln = lv_ebeln
                                                ebelp = lv_ebelp ) ).

    ENDLOOP.

* ---------------------------------------------------------------------------
* Recupera a lista de pedidos
* ---------------------------------------------------------------------------
    IF lt_ekpo_key[] IS NOT INITIAL.

      SELECT DISTINCT ebeln, ebelp
          FROM ekpo
          INTO TABLE @DATA(lt_ekpo)
          FOR ALL ENTRIES IN @lt_ekpo_key
          WHERE ebeln = @lt_ekpo_key-ebeln.

      IF sy-subrc EQ 0.
        SORT lt_ekpo BY ebeln ebelp.
      ENDIF.
    ENDIF.

* ---------------------------------------------------------------------------
* Recupera o primeiro pedido válido, priorizando os que tem item
* ---------------------------------------------------------------------------
    SORT lt_ekpo_key BY ebelp DESCENDING
                        ebeln ASCENDING.

    LOOP AT lt_ekpo_key REFERENCE INTO DATA(ls_ekpo_key).

* ---------------------------------------------------------------------------
* Cenário onde o número e item do pedido foram encontrados
* ---------------------------------------------------------------------------
      IF ls_ekpo_key->ebelp IS NOT INITIAL.

        READ TABLE lt_ekpo REFERENCE INTO DATA(ls_ekpo) WITH KEY ebeln = ls_ekpo_key->ebeln
                                                                 ebelp = ls_ekpo_key->ebelp
                                                                 BINARY SEARCH.
        IF sy-subrc EQ 0.
          ev_ebeln = ls_ekpo->ebeln.
          ev_ebelp = ls_ekpo->ebelp.
          EXIT.
        ENDIF.

        " Se o número do item não for encontrado, passamos apenas o número do pedido
        READ TABLE lt_ekpo REFERENCE INTO ls_ekpo WITH KEY ebeln = ls_ekpo_key->ebeln
                                                           BINARY SEARCH.

        IF sy-subrc EQ 0.
          ev_ebeln = ls_ekpo->ebeln.
          ev_ebelp = space.
          EXIT.
        ENDIF.

* ---------------------------------------------------------------------------
* Cenário onde apenas o número do pedido foi encontrado
* ---------------------------------------------------------------------------
      ELSE.

        READ TABLE lt_ekpo REFERENCE INTO ls_ekpo WITH KEY ebeln = ls_ekpo_key->ebeln
                                                           BINARY SEARCH.

        IF sy-subrc EQ 0.
          ev_ebeln = ls_ekpo->ebeln.
          ev_ebelp = space.
          EXIT.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD update_nfe_with_purchase_order.

    CHECK iv_ponumber IS NOT INITIAL.
    CHECK iv_poitem   IS NOT INITIAL.

    LOOP AT is_nfe-infnfe-det INTO DATA(ls_prod).

      " Busca item NFe
      READ TABLE cs_po_assigned-po_assigned_tab REFERENCE INTO DATA(ls_assigned)
                                                WITH KEY nitem = ls_prod-nitem.

      CHECK sy-subrc EQ 0.

      ls_assigned->ponumber = iv_ponumber.
      ls_assigned->poitem   = iv_poitem.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
