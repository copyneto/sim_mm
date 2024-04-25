CLASS zclmm_modify_wms_tables DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      "! Status de integração
      BEGIN OF gc_Status_integracao,
        nao_enviado      TYPE ze_status_int  VALUE '00', "Enviar documento
        enviado          TYPE ze_status_int  VALUE '01', "Documento enviado
        retorno          TYPE ze_status_int  VALUE '02', "Retorno integração
        finalizado       TYPE ze_status_int  VALUE '03', "Processo Finalizado
        erro_processo    TYPE ze_status_int  VALUE '04', "Erro na execução do processo
        erro_integracao  TYPE ze_status_int  VALUE '05', "Erro integração
        retorno_eliminar TYPE ze_status_int  VALUE '06', "Retorno eliminar
        nao_enviar       TYPE ze_status_int  VALUE '07', "Não Enviar
      END OF gc_status_integracao.

    TYPES:
      "! Tipo de tabela para documentos recebimento
      ty_rem_rec        TYPE STANDARD TABLE OF ztmm_doc_rem_rec WITH DEFAULT KEY,
      "! Tipo de tabela para documentos de materiais
      ty_t_doc_material TYPE STANDARD TABLE OF ztmm_doc_mat WITH DEFAULT KEY.

    DATA: gs_doc_rem_rec TYPE zsmm_doc_rem_rec.

    "! Construtor da classe
    METHODS constructor
      IMPORTING
        is_doc_rem_rec TYPE zsmm_doc_rem_rec OPTIONAL.

    "! Modificar tabela de documentos de recebimento
    "! it_rem_rec | Tabela de documentos de recebimento
    METHODS modify_doc_rec_rem
      IMPORTING
        it_rem_rec TYPE ty_rem_rec.

    "! Modificar tabela de documentos de recebimento
    "! it_rem_rec | Tabela de documentos de recebimento
    METHODS modify_doc_material
      IMPORTING
        it_doc_material TYPE ty_t_doc_material.

    "! Preencher modificações
    METHODS fill_changes_return.

  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Converter unidade de medida
    "! iv_meins | Unidade entrada
    "! rv_meins | Unidade saída
    METHODS conv
      IMPORTING
        iv_meins        TYPE zsmm_doc_rem_rec_item-meins
      RETURNING
        VALUE(rv_meins) TYPE ztmm_doc_rem_rec-meins.

ENDCLASS.



CLASS zclmm_modify_wms_tables IMPLEMENTATION.
  METHOD constructor.

    gs_doc_rem_rec = is_doc_rem_rec.

  ENDMETHOD.

  METHOD fill_changes_return.

    DATA: lt_doc_rem_rec TYPE STANDARD TABLE OF ztmm_doc_rem_rec.

    CHECK gs_doc_rem_rec IS NOT INITIAL.

    gs_doc_rem_rec-header-vbeln =  |{ gs_doc_rem_rec-header-vbeln ALPHA = IN }|.

    SELECT    vbeln,
                    posnr,
                    werks,
                    lgort,
                    ztipodoc,
                    zstatusintegracao,
                    menge,
                    zqtrec,
                    zqtpen,
                    meins,
                    cancel,
                    mblnr,
                    mjahr,
                    movestloc,
                    ernam,
                    erdat,
                    erzet,
                    aenam,
                    aedat,
                    aezet,
                    type,
                    id,
                    znumber,
                    message
      FROM zi_mm_doc_rem_rec
     WHERE vbeln EQ @gs_doc_rem_rec-header-vbeln
    INTO TABLE @DATA(lt_conferencia).

    LOOP AT gs_doc_rem_rec-header-item ASSIGNING FIELD-SYMBOL(<fs_item>).

      <fs_item>-posnr =  |{ <fs_item>-posnr ALPHA = IN }|.

      DATA(ls_item) = VALUE zi_mm_doc_rem_rec( lt_conferencia[ Posnr = <fs_item>-posnr
                                                                                                          zstatusintegracao = '01' ] OPTIONAL ).

      CHECK ls_item IS NOT INITIAL.

      APPEND INITIAL LINE TO lt_doc_rem_rec ASSIGNING FIELD-SYMBOL(<fs_doc>).

      <fs_doc> = CORRESPONDING #( ls_item  ).
      <fs_doc>-werks = <fs_item>-werks.
      <fs_doc>-zqtrec = <fs_item>-zqtrec.
      <fs_doc>-zqtpen = <fs_item>-zqtpen.
      <fs_doc>-lgort = <fs_item>-lgort.
      <fs_doc>-meins = conv( <fs_item>-meins ).
      <fs_doc>-ztipo_doc = gs_doc_rem_rec-header-ztipo_doc.
      <fs_doc>-vbeln = gs_doc_rem_rec-header-vbeln.
      <fs_doc>-aedat = sy-datum.
      <fs_doc>-aezet = sy-uzeit .
      <fs_doc>-zstatus_integracao = '02'.
      <fs_doc>-aenam = sy-uname.

    ENDLOOP.

    CHECK lt_doc_rem_rec IS NOT INITIAL.

    modify_doc_rec_rem( lt_doc_rem_rec ).

  ENDMETHOD.


  METHOD conv.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = iv_meins
        language       = sy-langu
      IMPORTING
        output         = rv_meins
      EXCEPTIONS
        unit_not_found = 01.

    IF sy-subrc NE 0.
      rv_meins = iv_meins.
    ENDIF.

  ENDMETHOD.

  METHOD modify_doc_rec_rem.

    CHECK it_rem_rec IS NOT INITIAL.

    MODIFY ztmm_doc_rem_rec  FROM  TABLE it_rem_rec.

    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.

  METHOD modify_doc_material.

    CHECK it_doc_material IS NOT INITIAL.

    MODIFY ztmm_doc_mat  FROM  TABLE it_doc_material.

    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
