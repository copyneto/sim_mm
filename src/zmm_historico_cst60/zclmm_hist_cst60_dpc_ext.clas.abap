CLASS zclmm_hist_cst60_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zclmm_hist_cst60_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_file_content,
        nfenum          TYPE string,
        series          TYPE string,
        credat          TYPE string,
        stcd1           TYPE string,
        matnr           TYPE string,
        cfop            TYPE string,
        menge           TYPE string,
        werks           TYPE string,
        waerk           TYPE string,
        cst_icms        TYPE string,
        bc_icms         TYPE string,
        v_icms_pr       TYPE string,
        bc_icms_st      TYPE string,
        v_icms_st       TYPE string,
        bc_icms_st_fcp  TYPE string,
        v_icms_st_fcp   TYPE string,
        v_icms_rate     TYPE string,
        v_icms_rate_fcp TYPE string,
      END OF ty_file_content.

    METHODS /iwbep/if_mgw_appl_srv_runtime~create_stream
        REDEFINITION .

    METHODS upload_file
      IMPORTING
        !iv_file     TYPE xstring
        !iv_filename TYPE string
      EXPORTING
        !et_return   TYPE bapiret2_t .

    METHODS conv_number_ext_to_int
      IMPORTING
        iv_number       TYPE string
      RETURNING
        VALUE(rV_value) TYPE wrbtr.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclmm_hist_cst60_dpc_ext IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_stream.
    TYPES: BEGIN OF ty_entity,
             filename TYPE string,
             message  TYPE string,
           END OF ty_entity.

    DATA: lo_message   TYPE REF TO /iwbep/if_message_container,
          lo_exception TYPE REF TO /iwbep/cx_mgw_busi_exception,
          ls_entity    TYPE ty_entity,
          lv_filename  TYPE string,
          lv_tablename TYPE tablename,
          lt_return    TYPE bapiret2_t,
          lv_mime_type TYPE char100 VALUE 'text/csv'.

*    DATA(lo_timesheet) = NEW zclca_timesheet( ).

    SPLIT iv_slug AT ';' INTO lv_filename lv_tablename.

    IF is_media_resource-mime_type <> lv_mime_type.
      ##NO_TEXT
      lt_return[] = VALUE #( BASE lt_return (
        type       = 'E'
        id         = 'ZCA_EXCEL'
        number     = '000'
        message_v1 = 'Arquivo inválido, salve em formato padrão do .CSV'
      ) ).
    ELSE.
* ----------------------------------------------------------------------
* Gerencia Botão do aplicativo
* ----------------------------------------------------------------------
      me->upload_file( EXPORTING iv_file      = is_media_resource-value
                                           iv_filename  = lv_filename
                                 IMPORTING et_return    = lt_return ).

      TRY.
          ls_entity-filename = lv_filename.
          ls_entity-message  = lt_return[ 1 ]-message.
        CATCH cx_root.
      ENDTRY.

    ENDIF.

* ----------------------------------------------------------------------
* Prepara informações de retorno
* ----------------------------------------------------------------------
    copy_data_to_ref( EXPORTING is_data = ls_entity
                      CHANGING  cr_data = er_entity ).

* ----------------------------------------------------------------------
* Ativa exceção em casos de erro
* ----------------------------------------------------------------------
    IF NOT line_exists( lt_return[ type = 'S' ] ).       "#EC CI_STDSEQ
      lo_message = mo_context->get_message_container( ).
      lo_message->add_messages_from_bapi( it_bapi_messages = lt_return ).
      CREATE OBJECT lo_exception EXPORTING message_container = lo_message.
      RAISE EXCEPTION lo_exception.
    ENDIF.
  ENDMETHOD.


  METHOD upload_file.
*
    DATA: lt_file       TYPE TABLE OF ztmm_hist_cst60.
*    DATA: lt_cnpj     TYPE TABLE OF I_Businesspartnertaxnumber,
*          lt_material TYPE TABLE OF I_MaterialText.
    "DATA: lt_file       TYPE TABLE OF zctgsd_preco_arquivo.
    DATA: lt_hist_cst60  TYPE TABLE OF ztmm_hist_cst60.
    DATA: ls_hist_cst60  TYPE ztmm_hist_cst60.

* ---------------------------------------------------------------------------
* Converte arquivo excel para tabela
** ---------------------------------------------------------------------------
*    DATA(lo_excel) = NEW zclca_excel( iv_filename = iv_filename
*                                      iv_file     = iv_file ).
*    "lo_excel->gv_quant = abap_true.
*    lo_excel->get_sheet( IMPORTING et_return = DATA(lt_return)              " Ignorar validação durante carga
*                         CHANGING  ct_table  = lt_file[] ).
*
*    IF line_exists( lt_return[ type = 'E' ] ).           "#EC CI_STDSEQ
*      et_return = lt_return.
*      RETURN.
*    ENDIF.

    cl_bcs_convert=>xstring_to_string(
      EXPORTING
        iv_xstr   = iv_file
        iv_cp     = 1100
      RECEIVING
        rv_string = DATA(lv_file_content)
    ).

    SPLIT lv_file_content AT cl_abap_char_utilities=>cr_lf INTO TABLE DATA(lt_file_content).

    DELETE lt_file_content INDEX 1.
    LOOP AT lt_file_content ASSIGNING FIELD-SYMBOL(<Fs_file_content>).
      SPLIT <fs_file_content> AT ';' INTO TABLE DATA(lt_file_content_col).
      APPEND INITIAL LINE TO lt_file ASSIGNING FIELD-SYMBOL(<Fs_file>).

      <fs_file>-waerk = 'BRL'.

      LOOP AT lt_file_content_col ASSIGNING FIELD-SYMBOL(<fs_file_content_col>).
        REPLACE ALL OCCURRENCES OF 'R$' IN <fs_file_content_col> WITH space.

        CASE sy-tabix.
          WHEN 1.
            <fs_file>-nfenum = <fs_file_content_col>.
          WHEN 2.
            <fs_file>-series = <fs_file_content_col>.
          WHEN 3. "Data criação
            <fs_file>-credat = <fs_file_content_col>+6(4) && <fs_file_content_col>+3(2) && <fs_file_content_col>(2).
          WHEN 4. "Cnpj
            <fs_file>-stcd1 = <fs_file_content_col>.
          WHEN 5.
            <fs_file>-matnr = CONV char18( |{ <fs_file_content_col> ALPHA = IN }| ).
          WHEN 6.
            <fs_file>-cfop  = <fs_file_content_col>.
          WHEN 7.
            <fs_file>-menge  = <fs_file_content_col>.
          WHEN 8.
            <fs_file>-werks  = <fs_file_content_col>.
          WHEN 9.
*            CALL FUNCTION 'CONVERSION_EXIT_TXSIT_INPUT'
*              EXPORTING
*                input  = <fs_file_content_col>
*              IMPORTING
*                output = <fs_file>-cst_icms.
*
*            IF <fs_file>-cst_icms IS INITIAL.
            <fs_file>-cst_icms = <fs_file_content_col>.
*            ENDIF.
          WHEN 10.
            <fs_file>-bc_icms = me->conv_number_ext_to_int( <fs_file_content_col> ).
          WHEN 11.
            <fs_file>-v_icms_pr = me->conv_number_ext_to_int( <fs_file_content_col> ).
          WHEN 12.
            <fs_file>-bc_icms_st = me->conv_number_ext_to_int( <fs_file_content_col> ).
          WHEN 13.
            <fs_file>-v_icms_st = me->conv_number_ext_to_int( <fs_file_content_col> ).
          WHEN 14.
            <fs_file>-bc_icms_st_fcp = me->conv_number_ext_to_int( <fs_file_content_col> ).
          WHEN 15.
            <fs_file>-v_icms_st_fcp = me->conv_number_ext_to_int( <fs_file_content_col> ).
          WHEN 16.
            <fs_file>-v_icms_rate = me->conv_number_ext_to_int( <fs_file_content_col> ).
          WHEN 17.
            <fs_file>-v_icms_rate_st = me->conv_number_ext_to_int( <fs_file_content_col> ).
          WHEN 18.
            <fs_file>-v_icms_rate_fcp = me->conv_number_ext_to_int( <fs_file_content_col> ).
        ENDCASE.

      ENDLOOP.
    ENDLOOP.

    SELECT stcd1
      FROM kna1
INTO TABLE @DATA(lt_cnpj)
   FOR ALL ENTRIES IN @lt_file
     WHERE stcd1 = @lt_file-stcd1.

    SELECT stcd1
      FROM lfa1
 APPENDING TABLE @lt_cnpj
   FOR ALL ENTRIES IN @lt_file
     WHERE stcd1 = @lt_file-stcd1.

    SELECT Material
    FROM I_Material
    INTO TABLE @DATA(lt_material)
    FOR ALL ENTRIES IN @lt_file
    WHERE Material = @lt_file-matnr.

    SORT lt_cnpj BY stcd1.
    SORT lt_material BY Material.

    LOOP AT lt_file ASSIGNING FIELD-SYMBOL(<fs_file_verify>).
      DATA(lv_file_row) = sy-tabix + 1.

      READ TABLE lt_cnpj ASSIGNING FIELD-SYMBOL(<fs_cnpj>) WITH KEY stcd1 = <fs_file_verify>-stcd1 BINARY SEARCH.
      IF NOT sy-subrc IS INITIAL.
        et_return[] = VALUE #( BASE et_return ( type = zclmm_read_hist_cst60=>if_xo_const_message~error  id = zclmm_read_hist_cst60=>gc_message_id number = '001' message_v1 = lv_file_row  message_v2 = <fs_file_verify>-stcd1 ) ).
      ENDIF.

      READ TABLE lt_material ASSIGNING FIELD-SYMBOL(<fs_material>) WITH KEY Material = <fs_file_verify>-matnr BINARY SEARCH.
      IF NOT sy-subrc IS INITIAL.
        et_return[] = VALUE #( BASE et_return ( type = zclmm_read_hist_cst60=>if_xo_const_message~error  id = zclmm_read_hist_cst60=>gc_message_id number = '002' message_v1 = lv_file_row message_v2 = <fs_file_verify>-matnr ) ).
      ENDIF.

    ENDLOOP.

    IF NOT line_Exists( et_return[ type = 'E' ] ).       "#EC CI_STDSEQ
      MODIFY ztmm_hist_cst60 FROM TABLE lt_file.

      IF sy-subrc IS INITIAL.
        et_return[] = VALUE #( BASE et_return ( type = 'S' id = 'ZCA_EXCEL' number = '000' ) ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD conv_number_ext_to_int.
    DATA:
      lv_compos TYPE i,
      lv_comlen TYPE i,
      lv_dotpos TYPE i.

    CONSTANTS:
      lc_dot_sym TYPE char01 VALUE '.',
      lc_space   TYPE char01 VALUE ' ',
      lc_com_sym TYPE char01 VALUE ','.

    DATA(lv_number) = iv_number.
    CONDENSE lv_number NO-GAPS.

* Find first occurrence of dot character on amount field
    FIND FIRST OCCURRENCE OF lc_dot_sym IN lv_number MATCH OFFSET lv_dotpos.

* Find first occurence of comma character on amount field
    FIND FIRST OCCURRENCE OF lc_com_sym IN lv_number MATCH OFFSET lv_compos MATCH LENGTH lv_comlen.

* If dot comes before the comma, remove all occurrences of dots and replace the comma by dot.
    IF lv_dotpos LT lv_compos.

      REPLACE ALL OCCURRENCES OF lc_dot_sym IN lv_number WITH lc_space.

      REPLACE ALL OCCURRENCES OF lc_com_sym IN lv_number WITH lc_dot_sym.

* If comma comes before the dot, replace the comma by dot.
    ELSE.

      REPLACE ALL OCCURRENCES OF lc_com_sym IN lv_number WITH lc_space.

    ENDIF.

* Remove blank spaces
    CONDENSE lv_number NO-GAPS.
    rv_value = lv_number.
  ENDMETHOD.
ENDCLASS.
