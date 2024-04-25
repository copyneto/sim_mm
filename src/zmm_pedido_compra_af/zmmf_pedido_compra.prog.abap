*&---------------------------------------------------------------------*
*& Report ZMMF_PEDIDO_COMPRA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmf_pedido_compra.

TABLES: nast.

FORM f_adobe_entry_neu USING uv_ent_retco
                             uv_ent_screen.

  CONSTANTS lc_5 TYPE c VALUE '5'.  "==> HMC - META - 04.03.2024 - 3.4.52. GAP-MM-03 (Adobe Pedido) (ajuste).

  DATA: lv_druvo       LIKE t166k-druvo,
        lv_from_memory.

  DATA: lt_doc     TYPE meein_purchase_doc_print,
        lt_bin_tab TYPE solix_tab.  "==> HMC - META - 04.03.2024 - 3.4.52. GAP-MM-03 (Adobe Pedido) (ajuste).

  DATA: ls_output     TYPE zsmm_ped_compra,
        ls_formoutput TYPE fpformoutput.  "==> HMC - META - 04.03.2024 - 3.4.52. GAP-MM-03 (Adobe Pedido) (ajuste).

  DATA: ls_address     TYPE sadr,
        ls_branch_data TYPE j_1bbranch,
        lv_cgc_number  TYPE j_1bwfield-cgc_number,
        ls_address1    TYPE addr1_val,
        ls_address_f   TYPE addr1_val,  "==> HMC - META - 04.03.2024 - 3.4.52. GAP-MM-03 (Adobe Pedido) (ajuste).
        ls_ekko        TYPE ekko.       "==> HMC - META - 04.03.2024 - 3.4.52. GAP-MM-03 (Adobe Pedido) (ajuste).

  DATA: ls_tel  TYPE  wiso_pladdr.

  DATA: lv_name     TYPE thead-tdname,
        lt_lines    TYPE TABLE OF tline,
        lv_text_aux TYPE string.

  DATA: lv_erro TYPE string.

  DATA:
    lv_formname     TYPE funcname,
    ls_outputparams TYPE sfpoutputparams,
    ls_params       TYPE sfpdocparams,
    ls_joboutput    TYPE sfpjoboutput.

  CLEAR uv_ent_retco.
  IF nast-aende EQ space.
    lv_druvo = '1'. "Nova impressão
  ELSE.
    lv_druvo = '2'. "Alterar impressão
  ENDIF.

  CALL FUNCTION 'ME_READ_PO_FOR_PRINTING'
    EXPORTING
      ix_nast        = nast
      ix_screen      = uv_ent_retco
    IMPORTING
      ex_retco       = uv_ent_retco
      doc            = lt_doc
    CHANGING
      cx_druvo       = lv_druvo
      cx_from_memory = lv_from_memory.

  CHECK uv_ent_retco IS INITIAL.

  READ TABLE lt_doc-xekpo ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX 1.

  CHECK sy-subrc IS INITIAL.

  "------------>> Dados Filial <<------------
  "------------>> Dados Faturamento <<------------

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
    EXPORTING
      branch            = <fs_item>-werks
      bukrs             = <fs_item>-bukrs
    IMPORTING
      address           = ls_address
      branch_data       = ls_branch_data
      cgc_number        = lv_cgc_number
      address1          = ls_address1
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'PIA_CSO_PLANT_ADDRESS_READ'
    EXPORTING
      pi_plant  = <fs_item>-werks
    IMPORTING
      pe_pladdr = ls_tel.

  "------------>> Dados Fornecedor <<------------
  SELECT SINGLE
    name1 AS nome, stras AS rua,
    ort01 AS cidade, regio AS uf,
    pstlz AS cep,  telf1 AS tel,  stcd1 AS cnpj
    FROM lfa1
    WHERE lifnr = @lt_doc-xekko-lifnr
    INTO @DATA(ls_fornecedor).

  ls_ekko-lifnr = lt_doc-xekko-lifnr. "==> HMC - META - 04.03.2024 - 3.4.52. GAP-MM-03 (Adobe Pedido) (ajuste).

  "------------>> Informações gerais <<------------
  SELECT SINGLE vtext
  FROM tvzbt
  WHERE zterm = @lt_doc-xekko-zterm
    AND spras = @lt_doc-xekko-spras
    INTO @DATA(lv_fpgto).

  SELECT SINGLE bezei
  FROM tinct
  WHERE inco1 = @lt_doc-xekko-inco1
    AND spras = @lt_doc-xekko-spras
    INTO @DATA(lv_incoterms).

  SELECT SINGLE eknam, smtp_addr
  FROM t024
  WHERE ekgrp = @lt_doc-xekko-ekgrp
    INTO @DATA(ls_gru_com).

  "------------>> Dados Entrega <<------------
  IF <fs_item>-emlif IS NOT INITIAL.
    SELECT SINGLE
      name1 AS nome, stras AS rua,
      ort01 AS cidade, ort02 AS bairro, regio AS uf,
      pstlz AS cep, telf1 AS tel, stcd1 AS cnpj, stcd3 AS ie
      FROM lfa1
      WHERE lifnr = @<fs_item>-emlif
      INTO @DATA(ls_entrega).

    IF ls_entrega-tel IS INITIAL.
      SELECT SINGLE addrcomm
      FROM but000
      WHERE partner = @<fs_item>-emlif
      INTO @DATA(lv_addrcomm).

      SELECT SINGLE tel_number
        FROM adrc
        WHERE addrnumber = @lv_addrcomm
        INTO @DATA(lv_tel_number).

      ls_entrega-tel = lv_tel_number.
    ENDIF.


  ELSEIF <fs_item>-emlif IS INITIAL AND <fs_item>-kunnr IS NOT INITIAL.
    SELECT SINGLE
      name1 AS nome, stras AS rua,
      ort01 AS cidade, ort02 AS bairro, regio AS uf,
      pstlz AS cep, telf1 AS tel, stcd1 AS cnpj, stcd3 AS ie
      FROM lfa1
      WHERE lifnr = @<fs_item>-kunnr
      INTO @ls_entrega.
  ENDIF.




  "------------>> DE PARA <<------------
  "------------>> Header <<------------
  ls_output-header-nome = ls_branch_data-name.
  ls_output-header-cnpj = lv_cgc_number.
  ls_output-header-ie = ls_branch_data-state_insc.
  ##NO_TEXT
  ls_output-header-end_completo = |{ ls_address-stras } CEP { ls_address-pstlz } - { ls_address-ort02 } - { ls_address-ort01 } - { ls_address-regio } - { ls_address-land1 } Tel: { ls_tel-tel1_numbr } / Fax: { ls_address1-fax_number } |.

  "------------>> Fornecedor <<------------
  ls_output-fornecedor = CORRESPONDING #( ls_fornecedor ).

  "------------>> Pedido <<------------
  ls_output-pedido-empresa = lt_doc-xekko-bukrs.
  ls_output-pedido-numero = lt_doc-xekko-ebeln.
  ls_output-pedido-data = lt_doc-xekko-bedat.
  ls_output-pedido-num_for = lt_doc-xekko-lifnr.
  ls_output-pedido-moeda = lt_doc-xekko-waers.
  ls_output-pedido-fpgto = lv_fpgto.
  ls_output-pedido-incoterms = lv_incoterms.
  ls_output-pedido-email = ls_gru_com-smtp_addr.
  ls_output-pedido-gru_comp = ls_gru_com-eknam.

  "------------>> Faturamento <<------------
  ls_output-faturamento-nome = ls_branch_data-name.
  ls_output-faturamento-rua = ls_address-stras.
  ls_output-faturamento-bairro = ls_address-ort02.
  ls_output-faturamento-cidade = ls_address-ort01.
  ls_output-faturamento-fax = ''.
  ls_output-faturamento-cep = ls_address-pstlz.
  ls_output-faturamento-tel = ls_tel-tel1_numbr.
  ls_output-faturamento-cnpj = lv_cgc_number.
  ls_output-faturamento-ie = ls_branch_data-state_insc.
  ls_output-faturamento-uf = ls_address-regio.

  CASE <fs_item>-j_1bmatuse.
    WHEN '0' OR '1'.
      ls_output-faturamento-industrializacao = 'X'.
    WHEN '3'.
      ls_output-faturamento-imobilizado = 'X'.
    WHEN '2' OR '4'.
      ls_output-faturamento-uso_consumo = 'X'.
    WHEN OTHERS.
      ls_output-faturamento-industrializacao = 'X'.
  ENDCASE.
  "------------>> Entrega <<------------
  IF ls_entrega IS NOT INITIAL.
    ls_output-entrega = CORRESPONDING #( ls_entrega ).
  ELSE.
    ls_output-entrega = CORRESPONDING #( ls_output-faturamento ).
  ENDIF.

  "------------>> Cabeçalho Pedido <<------------

  REFRESH: lt_lines.
  CLEAR: lv_name, lv_text_aux.
  lv_name = lt_doc-xekko-ebeln.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'F01'
      language                = sy-langu
      name                    = lv_name
      object                  = 'EKKO'
    TABLES
      lines                   = lt_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc IS INITIAL.
    "LOOP AT lt_lines INTO DATA(ls_text).
    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_text>).
      CONCATENATE lv_text_aux <fs_text>-tdline INTO lv_text_aux SEPARATED BY space.
    ENDLOOP.

    ls_output-pedido-observacoes = lv_text_aux.

  ENDIF.


  "------------>> Itens <<------------
  SELECT ebeln, ebelp, eindt
    FROM eket
    WHERE ebeln = @lt_doc-xekko-ebeln AND
    etenr = '1'
    INTO TABLE @DATA(lt_prog_rem).

  IF sy-subrc IS INITIAL.
    SORT lt_prog_rem BY ebelp.
  ENDIF.

  LOOP AT lt_doc-xekpo ASSIGNING FIELD-SYMBOL(<fs_ekpo>).

    APPEND INITIAL LINE TO ls_output-item ASSIGNING FIELD-SYMBOL(<fs_itens>).
    <fs_itens>-item = <fs_ekpo>-ebelp.
    <fs_itens>-id_material = <fs_ekpo>-matnr.
    <fs_itens>-desc_material = <fs_ekpo>-txz01.
    <fs_itens>-qtde = <fs_ekpo>-menge.
    <fs_itens>-unidade = <fs_ekpo>-meins.
    <fs_itens>-val_unitario = ( <fs_ekpo>-brtwr / <fs_ekpo>-menge ).
    <fs_itens>-val_total = <fs_ekpo>-brtwr.
    <fs_itens>-moeda = lt_doc-xekko-waers.

    ADD <fs_ekpo>-brtwr TO ls_output-pedido-vl_total.

    "<fs_itens>-val_unitario = <fs_ekpo>-netpr.
    "<fs_itens>-kwert = <fs_ekpo>-netwr.

    READ TABLE lt_prog_rem ASSIGNING FIELD-SYMBOL(<fs_prog_rem>)
      WITH KEY ebelp = <fs_ekpo>-ebelp BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      <fs_itens>-dt_entrega = <fs_prog_rem>-eindt.
    ENDIF.

    REFRESH: lt_lines.
    CLEAR: lv_name, lv_text_aux.
    CONCATENATE <fs_ekpo>-ebeln <fs_ekpo>-ebelp INTO DATA(lv_pedido_aux).
    lv_name = lv_pedido_aux.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = 'F01'
        language                = sy-langu
        name                    = lv_name
        object                  = 'EKPO'
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc IS INITIAL.
      "LOOP AT lt_lines INTO ls_text.
      LOOP AT lt_lines ASSIGNING <fs_text>.              "#EC CI_NESTED
        CONCATENATE lv_text_aux <fs_text>-tdline INTO lv_text_aux SEPARATED BY space.
      ENDLOOP.

      "<fs_itens>-desc_material = <fs_itens>-desc_material && ' ' && '<p style="color:rgb(0,0,255);">' && lv_text_aux && '</p>'.
      "<fs_itens>-desc_material = <fs_itens>-desc_material && ' ' && '<b>' && lv_text_aux && '</b>'.
      "<fs_itens>-desc_material = <fs_itens>-desc_material && cl_abap_char_utilities=>newline && lv_text_aux .
      <fs_itens>-desc_material = |{ <fs_itens>-desc_material }{ cl_abap_char_utilities=>newline } { lv_text_aux }|.

    ENDIF.

  ENDLOOP.

  TRY.
      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name     = 'ZAFMM_PEDIDO_COMPRA'
        IMPORTING
          e_funcname = lv_formname.

    CATCH cx_fp_api_repository cx_fp_api_usage cx_fp_api_internal.
      RETURN.
  ENDTRY.

  "ls_outputparams-adstrlevel = '01'.

*==> Begin of HMC - META - 04.03.2024 - 3.4.52. GAP-MM-03 (Adobe Pedido) (ajuste).
  IF nast-nacha EQ '5'.

    ls_outputparams-nodialog = abap_true.
    ls_outputparams-preview = abap_false.
    ls_outputparams-dest = nast-ldest.
    ls_outputparams-getpdf = abap_true.

  ENDIF.
*==> End of HMC - META - 04.03.2024 - 3.4.52. GAP-MM-03 (Adobe Pedido) (ajuste).

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = ls_outputparams
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "ls_params-dynamic = 'X'.

  CALL FUNCTION lv_formname
    EXPORTING
      /1bcdwb/docparams  = ls_params
      gs_pedido_compra   = ls_output
    IMPORTING
      /1bcdwb/formoutput = ls_formoutput "==> HMC - META - 04.03.2024 - 3.4.52. GAP-MM-03 (Adobe Pedido) (ajuste).
    EXCEPTIONS
      usage_error        = 1
      system_error       = 2
      internal_error     = 3
      OTHERS             = 4.

  IF sy-subrc <> 0.

    CALL FUNCTION 'FP_GET_LAST_ADS_ERRSTR'
      IMPORTING
        e_adserrstr = lv_erro.

    CALL FUNCTION 'FP_GET_LAST_ADS_TRACE'
      IMPORTING
        e_adstrace = lv_erro.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*==> Begin of HMC - META - 04.03.2024 - 3.4.52. GAP-MM-03 (Adobe Pedido) (ajuste).
  IF nast-nacha EQ lc_5.

    CALL FUNCTION 'MM_ADDRESS_GET'
      EXPORTING
        i_ekko    = ls_ekko
      IMPORTING
        e_address = ls_address_f.

    PERFORM f_send_email USING ls_address_f-addrnumber
                               ls_formoutput
                      CHANGING lt_bin_tab.

  ENDIF.
*==> End of HMC - META - 04.03.2024 - 3.4.52. GAP-MM-03 (Adobe Pedido) (ajuste).

  CALL FUNCTION 'FP_JOB_CLOSE'
    IMPORTING
      e_result       = ls_joboutput
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*==> Begin of HMC - META - 04.03.2024 - 3.4.52. GAP-MM-03 (Adobe Pedido) (ajuste).
FORM f_send_email USING uv_adrnr      TYPE adrnr
                        us_formoutput TYPE fpformoutput
               CHANGING ct_bin_tab TYPE solix_tab.

  CONSTANTS: lc_pdf  TYPE soodk-objtp VALUE 'PDF',
             lc_raw  TYPE so_obj_tp   VALUE 'RAW',
             lc_xml  TYPE so_obj_tp   VALUE 'XML',
             lc_s(1) TYPE c           VALUE 'S',
             lc_e(1) TYPE c           VALUE 'E'.

  DATA: lo_send_request  TYPE REF TO cl_bcs VALUE IS INITIAL,
        lo_sender        TYPE REF TO if_sender_bcs VALUE IS INITIAL,
        lo_recipient     TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
        lo_document      TYPE REF TO cl_document_bcs VALUE IS INITIAL,
        lo_bcs_exception TYPE REF TO cx_bcs.

  DATA lt_message_body TYPE bcsy_text VALUE IS INITIAL.

  DATA: ls_comm_type   TYPE ad_comm,
        ls_comm_values TYPE szadr_comm_values.

  DATA: lv_recipient      TYPE ad_smtpadr,
        lv_sent_to_all(1) TYPE c VALUE IS INITIAL,
        lv_subject        TYPE char50,
        lv_exception_text TYPE string,
        lv_pdf            TYPE xstring.

  TRY.
      " Criar request de envio
      lo_send_request = cl_bcs=>create_persistent( ).
      lv_subject = nast-tdcovtitle.

      " Converter PDF para bin
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = us_formoutput-pdf
        TABLES
          binary_tab = ct_bin_tab.

      DATA(lv_file_size) = xstrlen( us_formoutput-pdf ).

      lo_document = cl_document_bcs=>create_document(
        i_type = lc_raw
        i_text = lt_message_body
        i_subject = lv_subject ).

      " Adicionar anexo
      CALL METHOD lo_document->add_attachment
        EXPORTING
          i_attachment_type    = lc_pdf
          i_attachment_subject = lv_subject
          i_attachment_size    = CONV #( lv_file_size )
          i_att_content_hex    = ct_bin_tab.

      lo_send_request->set_document( lo_document ).

      " Obter e-mail do destino
      CALL FUNCTION 'ADDR_GET_NEXT_COMM_TYPE'
        EXPORTING
          strategy               = nast-tcode
          address_number         = uv_adrnr
        IMPORTING
          comm_type              = ls_comm_type
          comm_values            = ls_comm_values
        EXCEPTIONS
          address_not_exist      = 1
          person_not_exist       = 2
          no_comm_type_found     = 3
          internal_error         = 4
          parameter_error        = 5
          address_blocked        = 6
          person_blocked         = 7
          contact_person_blocked = 8
          OTHERS                 = 9.

      IF sy-subrc IS INITIAL.

        lv_recipient = ls_comm_values-adsmtp-smtp_addr.

      ENDIF.

      " Configurar destino
      lo_sender = cl_cam_address_bcs=>create_internet_address( lv_recipient ).
      lo_sender = cl_sapuser_bcs=>create( sy-uname ).

      lo_send_request->set_sender(
        EXPORTING
          i_sender = lo_sender ).

      lo_recipient = cl_sapuser_bcs=>create( sy-uname ).
      lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_recipient ).

      " Adicionar destinatário
      lo_send_request->add_recipient(
        EXPORTING
          i_recipient = lo_recipient
          i_express = abap_true ).

      " Enviar
      lo_send_request->send(
        EXPORTING
          i_with_error_screen = abap_true
        RECEIVING
          result = lv_sent_to_all ).

      COMMIT WORK.

      MESSAGE TEXT-t01 TYPE lc_s DISPLAY LIKE lc_s.

    CATCH cx_bcs INTO lo_bcs_exception.

      MESSAGE lo_bcs_exception TYPE lc_s DISPLAY LIKE lc_e.

  ENDTRY.

ENDFORM.
*==> End of HMC - META - 04.03.2024 - 3.4.52. GAP-MM-03 (Adobe Pedido) (ajuste).
