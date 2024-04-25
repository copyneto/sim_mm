CLASS zclmm_notas_debito DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA gs_notas_debito TYPE zsmm_notas_debito.

    METHODS m_main
      IMPORTING
                iv_centro         TYPE t001w-werks
                iv_supplier       TYPE i_inbounddelivery-supplier
                iv_agente_frete   TYPE j_1bparid
                iv_dias           TYPE t5a4a-dlydy
                iv_grund          TYPE mb_grbew
                iv_value          TYPE netwr
                iv_tipomov        TYPE bwart
                iv_motivomov      TYPE mb_grbew
                iv_docmatajest    TYPE char20
                iv_DocumentItem   TYPE posnr_vl

      EXPORTING
                et_messages       TYPE bapiret2_t
      RETURNING VALUE(rv_xstring) TYPE fpcontent.

    CLASS-DATA go_instance TYPE REF TO zclmm_notas_debito .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zclmm_notas_debito .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: gc_fpname     TYPE fpname     VALUE 'ZAFMM_NOTAS_DEBITO',
               GC_PDF        TYPE string     VALUE 'PDF',
               GC_LP01       TYPE rspopname  VALUE 'LP01',
               GC_S          TYPE C LENGTH 1 VALUE 'S',
               GC_E          TYPE C LENGTH 1 VALUE 'E',
               GC_Z11        TYPE bwart      VALUE 'Z11'.

    DATA: gt_messages   TYPE bapiret2_t.

    METHODS: m_get_data
      IMPORTING
        iv_centro       TYPE t001w-werks
        iv_supplier     TYPE i_inbounddelivery-supplier
        iv_agente_frete TYPE j_1bparid
        iv_dias         TYPE t5a4a-dlydy
        iv_grund        TYPE mb_grbew
        iv_value        TYPE netwr
        iv_tipomov      TYPE bwart
        iv_motivomov    TYPE mb_grbew
        iv_docmatajest  TYPE char20
        iv_DocumentItem TYPE posnr_vl.

    METHODS m_call_adobe_forms RETURNING VALUE(rv_xstring) TYPE  fpcontent.

ENDCLASS.



CLASS ZCLMM_NOTAS_DEBITO IMPLEMENTATION.


  METHOD m_main.



    me->m_get_data( iv_centro       = iv_centro
                    iv_supplier     = iv_supplier
                    iv_agente_frete = iv_agente_frete
                    iv_dias         = iv_dias
                    iv_grund        = iv_grund
                    iv_value        = iv_value
                    iv_tipomov      = iv_tipomov
                    iv_motivomov    = iv_motivomov
                    iv_docmatajest  = iv_docmatajest
                    iv_DocumentItem = iv_DocumentItem ).

    rv_xstring = me->m_call_adobe_forms(  ).

    et_messages = gt_messages.

    CLEAR gs_notas_debito.

  ENDMETHOD.


  METHOD m_get_data.
    CONSTANTS: lc_range  TYPE nrnr   VALUE '01',
               lc_object TYPE nrobj  VALUE 'ZMM_NOTA_D',
               lc_mes    TYPE num2   VALUE 00,
               lc_ano    TYPE num2   VALUE 00.

    DATA: lv_bpartner    TYPE bapibus1006_head-bpartner,
          lt_taxdetails  TYPE TABLE OF bapibus1006tax,
          lt_taxdetails2 TYPE TABLE OF bapibus1006tax,
          ls_centraldata TYPE bapibus1006_central_organ,
          ls_address     TYPE bapibus1006_address.

    DATA: ls_spell TYPE spell,
          lv_value TYPE string.

    DATA: lv_numero_seq TYPE string,
          lv_vencimento TYPE p0001-begda,
          lv_valor      TYPE string.

    DATA lv_centro TYPE t001w-werks VALUE '2001'.
    DATA lv_supplier TYPE i_inbounddelivery-supplier.
    DATA lv_dias TYPE t5a4a-dlydy.
    DATA lv_grund TYPE mb_grbew VALUE 1234.

    SELECT a~bwkey, b~bukrs, b~butxt
    FROM t001k AS a
    INNER JOIN t001 AS b ON b~bukrs = a~bukrs
    INTO TABLE @DATA(lt_empresa)
      BYPASSING BUFFER
    WHERE bwkey EQ @iv_centro.

    SELECT lifnr, werks, name1
    FROM t001w
    INTO TABLE @DATA(lt_t001w)
    WHERE werks EQ @lv_centro.

    SORT lt_t001w BY werks.

    READ TABLE lt_t001w INTO DATA(ls_t001w) WITH KEY werks = lv_centro BINARY SEARCH.

    IF sy-subrc IS INITIAL.

      lv_bpartner = ls_t001w-lifnr.

    ENDIF.

    CALL FUNCTION 'BAPI_BUPA_TAX_GETDETAILS'
      EXPORTING
        businesspartner = lv_bpartner
      TABLES
        taxdetails      = lt_taxdetails.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = lc_range
        object                  = lc_object
      IMPORTING
        number                  = lv_numero_Seq
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CLEAR lv_bpartner.
    lv_bpartner = |{ iv_agente_frete ALPHA = IN }|.

    CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
      EXPORTING
        businesspartner         = lv_bpartner
      IMPORTING
        centraldataorganization = ls_centraldata.

    CALL FUNCTION 'BAPI_BUPA_TAX_GETDETAILS'
      EXPORTING
        businesspartner = lv_bpartner
      TABLES
        taxdetails      = lt_taxdetails2.

    CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
      EXPORTING
        businesspartner = lv_bpartner
      IMPORTING
        addressdata     = ls_address.

    IF iv_value IS NOT INITIAL.

      CALL FUNCTION 'SPELL_AMOUNT'
        EXPORTING
          amount    = iv_value
          currency  = TEXT-brl
          language  = sy-langu
        IMPORTING
          in_words  = ls_spell
        EXCEPTIONS
          not_found = 1
          too_large = 2
          OTHERS    = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE gc_s NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE gc_e.
      ENDIF.

      DATA(lv_moeda1) = COND char8( WHEN ls_spell-number  EQ 1 THEN TEXT-re0 ELSE TEXT-re1 ).  "TEXT-RE0=REAL     TEXT-RE1=REAIS
      DATA(lv_moeda2) = COND char8( WHEN ls_spell-decimal EQ 10 THEN TEXT-ce0 ELSE TEXT-ce1 ). "TEXT-CE0=CENTAVO  TEXT-CE1=CENTAVOs

      IF  ls_spell-dig01  EQ TEXT-zer "'ZERO'
      AND ls_spell-dig02  EQ TEXT-zer "'ZERO'
      AND ls_spell-dig03  EQ TEXT-zer "'ZERO'
      AND ls_spell-dig04  EQ TEXT-zer "'ZERO'
      AND ls_spell-dig05  EQ TEXT-zer "'ZERO'
      AND ls_spell-dig06  EQ TEXT-zer "'ZERO'
      AND ls_spell-number GT 0.
        lv_moeda1 = |{ TEXT-de0 } { lv_moeda1 }| .
      ENDIF.

      IF ls_spell-decimal IS NOT INITIAL.
        lv_valor = |{ ls_spell-word } { lv_moeda1 } { TEXT-e00 } { ls_spell-decword } { lv_moeda2 }|.
      ELSE.
        lv_valor = |{ ls_spell-word } { lv_moeda1 }|.
      ENDIF.


    ENDIF.

    TRY.
        READ TABLE lt_empresa INTO DATA(ls_empresa) INDEX 1.
      CATCH cx_root INTO DATA(lo_root).
        DATA(lv_exp_msg) = lo_root->get_longtext( ).
    ENDTRY.

    TRY.
        READ TABLE lt_taxdetails INTO DATA(ls_tax1) INDEX 1.
      CATCH cx_root INTO lo_root.
        lv_exp_msg = lo_root->get_longtext( ).
    ENDTRY.


    TRY.
        SORT lt_taxdetails BY taxtype.
        READ TABLE lt_taxdetails INTO DATA(ls_taxdetails) WITH KEY taxtype = TEXT-br1 BINARY SEARCH.
        DATA(lv_cnpj_aux) = ls_taxdetails-taxnumber.
*        DATA(lv_cnpj_aux) = lt_taxdetails[ taxtype = TEXT-br1 ]-taxnumber.
      CATCH cx_root INTO lo_root.
        lv_exp_msg = lo_root->get_longtext( ).
    ENDTRY.

    DATA lv_cnpj TYPE string.

    IF lv_cnpj_aux IS NOT INITIAL.
      DATA(lv_cnpj1) = lv_cnpj_aux(2).
      DATA(lv_cnpj2) = lv_cnpj_aux+2(3).
      DATA(lv_cnpj3) = lv_cnpj_aux+5(3).
      DATA(lv_cnpj4) = lv_cnpj_aux+8(4).
      DATA(lv_cnpj5) = lv_cnpj_aux+12(2).
      lv_cnpj = |{ lv_cnpj1 }.{ lv_cnpj2 }.{ lv_cnpj3 }/{ lv_cnpj4 }-{ lv_cnpj5 }|.
    ENDIF.

    TRY.
        SORT lt_taxdetails2 BY taxtype.
        READ TABLE lt_taxdetails2 INTO DATA(ls_taxdetails2) WITH KEY taxtype = TEXT-br1 BINARY SEARCH.
        DATA(lv_cnpj_aux2) = ls_taxdetails2-taxnumber.
*        DATA(lv_cnpj_aux2) = lt_taxdetails2[ taxtype = TEXT-br1 ]-taxnumber.
      CATCH cx_root INTO lo_root.
        lv_exp_msg = lo_root->get_longtext( ).
    ENDTRY.

    DATA lv_cnpj_2 TYPE string.
    IF lv_cnpj_aux2 IS NOT INITIAL.
      DATA(lv_cnpj1_2) = lv_cnpj_aux2(2).
      DATA(lv_cnpj2_2) = lv_cnpj_aux2+2(3).
      DATA(lv_cnpj3_2) = lv_cnpj_aux2+5(3).
      DATA(lv_cnpj4_2) = lv_cnpj_aux2+8(4).
      DATA(lv_cnpj5_2) = lv_cnpj_aux2+12(2).
      lv_cnpj_2 = |{ lv_cnpj1_2 }.{ lv_cnpj2_2 }.{ lv_cnpj3_2 }/{ lv_cnpj4_2 }-{ lv_cnpj5_2 }|.
    ENDIF.


    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = iv_dias
        months    = lc_mes
        "signum    = '+'
        years     = lc_ano
      IMPORTING
        calc_date = lv_vencimento.

    select grund,
           bwart,
           MAX( mjahr ) as ano
     FROM mseg
     INTO TABLE @DATA(lt_mseg)
      where mblnr EQ @iv_docmatajest
      GROUP BY grund, bwart.

    IF lt_mseg IS NOT INITIAL.
        SELECT grtxt
        FROM t157e
        INTO TABLE @DATA(lt_t157e)
        FOR ALL ENTRIES IN @lt_mseg
        WHERE grund EQ @lt_mseg-grund
          AND spras EQ @sy-langu
          AND bwart EQ @lt_mseg-bwart.
    ENDIF.

    TRY.
        SORT lt_taxdetails BY taxtype.
        CLEAR ls_taxdetails.
        READ TABLE lt_taxdetails INTO ls_taxdetails WITH KEY taxtype = TEXT-br3 BINARY SEARCH.
        DATA(lv_inscr_estadual) = ls_taxdetails-taxnumber.
*        DATA(lv_inscr_estadual) = lt_taxdetails[ taxtype = TEXT-br3 ]-taxnumber.
      CATCH cx_root INTO lo_root.
        lv_exp_msg = lo_root->get_longtext( ).
    ENDTRY.
    TRY.
        SORT lt_taxdetails BY taxtype.
        CLEAR ls_taxdetails.
        READ TABLE lt_taxdetails INTO ls_taxdetails WITH KEY taxtype = TEXT-br4 BINARY SEARCH.
        DATA(lv_inscr_municipal) = ls_taxdetails-taxnumber.
*        DATA(lv_inscr_municipal) = lt_taxdetails[ taxtype = TEXT-br4 ]-taxnumber.
      CATCH cx_root INTO lo_root.
        lv_exp_msg = lo_root->get_longtext( ).
    ENDTRY.

    TRY.
        SORT lt_taxdetails2 BY taxtype.
        CLEAR ls_taxdetails2.
        READ TABLE lt_taxdetails2 INTO ls_taxdetails2 WITH KEY taxtype = TEXT-br3 BINARY SEARCH.
        DATA(lv_inscri_estadual) = ls_taxdetails2-taxnumber.
*        DATA(lv_inscri_estadual) = lt_taxdetails2[ taxtype = TEXT-br3 ]-taxnumber.
      CATCH cx_root INTO lo_root.
        lv_exp_msg = lo_root->get_longtext( ).
    ENDTRY.
    DATA: lv_endereco   TYPE char100,
          lv_cep_cidade TYPE char100.

    lv_endereco = |{ ls_address-street }{ ls_address-district }|.
    lv_cep_cidade = |{ ls_address-postl_cod1 }  { ls_address-city } { ls_address-region }|.

    IF ls_address-street IS NOT INITIAL AND ls_address-district IS NOT INITIAL.
      lv_endereco = |{ ls_address-street } - { ls_address-district }|.
    ENDIF.
    IF ls_address-region IS INITIAL.
      lv_cep_cidade = |{ ls_address-postl_cod1 } - { ls_address-city }|.
    ENDIF.
    IF ls_address-postl_cod1 IS NOT INITIAL AND ls_address-city IS NOT INITIAL  AND ls_address-region IS NOT INITIAL.
      lv_cep_cidade = |{ ls_address-postl_cod1 } - { ls_address-city }/{ ls_address-region }|.
    ENDIF.

    lv_value = iv_value.

    gs_notas_debito = VALUE #( nome_empresa = ls_empresa-butxt
                               bukrs = ls_empresa-bukrs
                               cpf_cnpj = lv_cnpj
                               numero = lv_numero_seq
                               empresa = |{ ls_centraldata-name1 } { ls_centraldata-name2 }|
                               cnpj = lv_cnpj_2
                               endereco = lv_endereco
                               cep_cidade = lv_cep_cidade
                               estabelecimento = |{ ls_t001w-werks }-{ ls_t001w-name1 }|
                               emissao = sy-datum
                               valor_extenso = lv_valor
                               vencimento = lv_vencimento
                               inscr_estadual = lv_inscr_estadual
                               inscr_municipal = lv_inscr_municipal
                               inscri_estadual = lv_inscri_estadual
                               descricao_deb = lt_t157e
                               valor = CONDENSE( lv_value ) "lv_value
                              ).

    CLEAR: lt_empresa, lt_t001w, ls_t001w, lv_bpartner, lt_taxdetails, lv_numero_Seq, ls_centraldata,
    lv_cnpj1, lv_cnpj2, lv_cnpj3, lv_cnpj4, lv_cnpj5, lv_cnpj, lv_cnpj1_2, lv_cnpj2_2,
    lv_cnpj3_2, lv_cnpj4_2, lv_cnpj5_2, lt_t157e.

  ENDMETHOD.


  METHOD m_call_adobe_forms.

    DATA: lv_fm_name            TYPE rs38l_fnam,
          ls_fp_docparams       TYPE sfpdocparams,
          ls_fp_outputparams    TYPE sfpoutputparams,
          ls_fp_formoutput      TYPE fpformoutput,
          ls_control_parameters TYPE  ssfctrlop,
          lv_lines              TYPE i,
          lt_data_tab           TYPE TABLE OF x255,
          lv_filename           TYPE string,
          lv_fullpath           TYPE string,
          lv_path               TYPE string.

    DATA: lo_pdf_merger TYPE REF TO cl_rspo_pdf_merge.

    ls_fp_outputparams-reqnew = abap_true.
    ls_fp_outputparams-dest = GC_LP01.
    ls_fp_outputparams-getpdf = ABAP_ON.

    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_fp_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE GC_S NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    TRY.
*&---- Get the name of the generated function module
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME' ##FM_SUBRC_OK
          EXPORTING
            i_name     = gc_fpname
          IMPORTING
            e_funcname = lv_fm_name.
      CATCH cx_fp_api_internal .
        RETURN.
      CATCH cx_fp_api_repository  .
        RETURN.
      CATCH cx_fp_api_usage .
        RETURN.
    ENDTRY.

    ls_control_parameters-no_open  = abap_true.
    ls_control_parameters-no_close = abap_true.

*&--- Call the generated function module
    CALL FUNCTION lv_fm_name
      EXPORTING
        /1bcdwb/docparams  = ls_fp_docparams
        control_parameters = ls_control_parameters
        zsmm_notas_debito  = gs_notas_debito
      IMPORTING
        /1bcdwb/formoutput = ls_fp_formoutput
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE GC_S NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

*&---- Close the spool job
    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE GC_S NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = ls_fp_formoutput-pdf
      TABLES
        binary_tab = lt_data_tab.



    rv_xstring = ls_fp_formoutput-pdf.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_extension    = GC_PDF
      CHANGING
        filename             = lv_filename
        path                 = lv_fullpath
        fullpath             = lv_path
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = lv_filename
        filetype                = TEXT-BIN
      CHANGING
        data_tab                = lt_data_tab
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.

    IF sy-subrc IS NOT INITIAL.

      gt_messages = VALUE #( (
                        type       = sy-msgty
                        id         = sy-msgid
                        number     = sy-msgno
                        message_v1 = sy-msgv1
                        message_v2 = sy-msgv2
                        message_v3 = sy-msgv3
                        message_v4 = sy-msgv4
        ) ).

    ENDIF.

  ENDMETHOD.


  METHOD get_instance.
* ---------------------------------------------------------------------------
* Caso a instância não exista, criar uma nova
* ---------------------------------------------------------------------------
    IF NOT go_instance IS BOUND.
      go_instance = NEW zclmm_notas_debito( ).
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.
ENDCLASS.
