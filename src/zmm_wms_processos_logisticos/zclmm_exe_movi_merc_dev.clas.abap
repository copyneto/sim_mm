CLASS zclmm_exe_movi_merc_dev DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_status_integracao,
        nao_enviado      TYPE ze_status_int  VALUE '00', "Enviar documento
        enviado          TYPE ze_status_int  VALUE '01', "Documento enviado
        retorno          TYPE ze_status_int  VALUE '02', "Retorno integração
        finalizado       TYPE ze_status_int  VALUE '03', "Processo Finalizado
        erro_processo    TYPE ze_status_int  VALUE '04', "Erro na execução do processo
        erro_integracao  TYPE ze_status_int  VALUE '05', "Erro integração
        retorno_eliminar TYPE ze_status_int  VALUE '06', "Retorno eliminar
        nao_enviar       TYPE ze_status_int  VALUE '07', "Não Enviar
        erro_mov_dev     TYPE ze_status_int  VALUE '08', "Erro movimento devolução
      END OF gc_status_integracao.

    CONSTANTS: gc_modulo_mm    TYPE ztca_param_mod-modulo VALUE 'MM',
               gc_chave1       TYPE ztca_param_par-chave1 VALUE 'INTEGRACAO_WMS',
               gc_chave2       TYPE ztca_param_par-chave2 VALUE 'DEPOSITO_DEVOLUCAO',
               gc_ztipodoc_ent TYPE zi_mm_integracao_doc_remessa-ztipodoc VALUE 'E',
               gc_error        TYPE char1                 VALUE 'E',
               gc_move_type    TYPE bwart                 VALUE '311',
               gc_gm_code      TYPE gm_code               VALUE '04'.

    METHODS processar
      IMPORTING
        !ir_vbeln TYPE lmd_vl_t_vbeln_vl_range.

    METHODS selecionar_dados
      IMPORTING
        !ir_vbeln TYPE lmd_vl_t_vbeln_vl_range.

    METHODS processar_dados.


PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_dados,
        vbeln                  TYPE  zi_mm_integracao_doc_remessa-vbeln,
        posnr                  TYPE  zi_mm_integracao_doc_remessa-posnr,
        werks                  TYPE  zi_mm_integracao_doc_remessa-werks,
        lgort                  TYPE  zi_mm_integracao_doc_remessa-lgort,
        ztipodoc               TYPE  zi_mm_integracao_doc_remessa-ztipodoc,
        zstatusintegracao      TYPE  zi_mm_integracao_doc_remessa-zstatusintegracao,
        menge                  TYPE  zi_mm_integracao_doc_remessa-menge,
        zqtrec                 TYPE  zi_mm_integracao_doc_remessa-zqtrec,
        zqtpen                 TYPE  zi_mm_integracao_doc_remessa-zqtpen,
        meins                  TYPE  zi_mm_integracao_doc_remessa-meins,
        cancel                 TYPE  zi_mm_integracao_doc_remessa-cancel,
        mblnr                  TYPE  zi_mm_integracao_doc_remessa-mblnr,
        mjahr                  TYPE  zi_mm_integracao_doc_remessa-mjahr,
        movestloc              TYPE  zi_mm_integracao_doc_remessa-movestloc,
        ernam                  TYPE  zi_mm_integracao_doc_remessa-ernam,
        erdat                  TYPE  zi_mm_integracao_doc_remessa-erdat,
        erzet                  TYPE  zi_mm_integracao_doc_remessa-erzet,
        aenam                  TYPE  zi_mm_integracao_doc_remessa-aenam,
        aedat                  TYPE  zi_mm_integracao_doc_remessa-aedat,
        aezet                  TYPE  zi_mm_integracao_doc_remessa-aezet,
        type                   TYPE  zi_mm_integracao_doc_remessa-type,
        id                     TYPE  zi_mm_integracao_doc_remessa-id,
        znumber                TYPE  zi_mm_integracao_doc_remessa-znumber,
        message                TYPE  zi_mm_integracao_doc_remessa-message,
        overallsdprocessstatus TYPE  zi_mm_integracao_doc_remessa-overallsdprocessstatus,
        arktx                  TYPE  zi_mm_integracao_doc_remessa-deliverydocumentitemtext,
        matnr                  TYPE  zi_mm_integracao_doc_remessa-material,
        matkl                  TYPE  zi_mm_integracao_doc_remessa-materialgroup,
        lfimg                  TYPE  zi_mm_integracao_doc_remessa-lfimg,
        umvkz                  TYPE  zi_mm_integracao_doc_remessa-umvkz,
        umvkn                  TYPE  zi_mm_integracao_doc_remessa-umvkn,
        vrkme                  TYPE  zi_mm_integracao_doc_remessa-vrkme,
        lipsmeins              TYPE  zi_mm_integracao_doc_remessa-lipsmeins,
        vgbel                  TYPE  zi_mm_integracao_doc_remessa-vgbel,
        vgpos                  TYPE  zi_mm_integracao_doc_remessa-vgpos,
        vgtyp                  TYPE  zi_mm_integracao_doc_remessa-vgtyp,
        bukrs                  TYPE  zi_mm_integracao_doc_remessa-bukrs,
        process                TYPE  zi_mm_integracao_doc_remessa-process,
        edoc_type              TYPE  zi_mm_integracao_doc_remessa-edoc_type,
        posting_date           TYPE  zi_mm_integracao_doc_remessa-posting_date,
      END OF ty_dados.

    DATA: gt_dados   TYPE TABLE OF ty_dados.

    DATA gs_return TYPE bapiret2 .

    METHODS processa_dados_goodsmvt_create
      IMPORTING
        is_goodsmvt_header TYPE bapi2017_gm_head_01
        is_goodsmvt_code   TYPE bapi2017_gm_code
        it_goodsmvt_item   TYPE tab_bapi_goodsmvt_item
      CHANGING
        cs_dados           TYPE zclmm_exe_movi_merc_dev=>ty_dados
        ct_return          TYPE bapiret2_tab.
    METHODS preenche_dados_goodsmvt_create
      IMPORTING
        is_dados         TYPE zclmm_exe_movi_merc_dev=>ty_dados
      CHANGING
        ct_goodsmvt_item TYPE tab_bapi_goodsmvt_item.

    METHODS salvar_dados.
ENDCLASS.



CLASS ZCLMM_EXE_MOVI_MERC_DEV IMPLEMENTATION.


  METHOD processar.

    me->selecionar_dados( ir_vbeln = ir_vbeln ).

    me->processar_dados( ).

    me->salvar_dados(  ).

  ENDMETHOD.


  METHOD selecionar_dados.
    DATA: lv_chave3 TYPE zi_ca_get_parameter-chave3.

    SELECT chave3,
           low
      FROM zi_ca_get_parameter
     WHERE modulo EQ @gc_modulo_mm
       AND chave1 EQ @gc_chave1
       AND chave2 EQ @gc_chave2
    INTO TABLE @DATA(lt_lgort).
    IF sy-subrc = 0.
      SORT lt_lgort BY chave3.
    ENDIF.

    SELECT  vbeln, posnr,  werks,  lgort, ztipodoc, zstatusintegracao,
            menge, zqtrec, zqtpen, meins, cancel,   mblnr,  mjahr,
            movestloc,    ernam,  erdat, erzet,    aenam,  aedat,
            aezet,        type,   id,    znumber,  message,
            overallsdprocessstatus,  deliverydocumentitemtext, material, materialgroup,
            lfimg, umvkz,  umvkn, vrkme,  lipsmeins,
            vgbel, vgpos, vgtyp, bukrs, process, edoc_type, posting_date
      FROM zi_mm_integracao_doc_remessa
      INTO TABLE @gt_dados
     WHERE vbeln                  IN @ir_vbeln
       AND ztipodoc               = @gc_ztipodoc_ent
       AND zstatusintegracao      = @gc_status_integracao-finalizado
       AND mercadoria_entrada     = @abap_true
       AND estorno_entrada        = @space
       AND mblnr                  = @space
       AND zqtpen                 <> 0.
    IF sy-subrc = 0.
      LOOP AT gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).
        lv_chave3 = <fs_dados>-werks.
        READ TABLE lt_lgort
             ASSIGNING FIELD-SYMBOL(<fs_lgort>)
          WITH KEY chave3 = lv_chave3
          BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_dados>-movestloc = <fs_lgort>-low.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD processar_dados.
    DATA: lt_return        TYPE TABLE OF bapiret2,
          lt_goodsmvt_item TYPE TABLE OF bapi2017_gm_item_create.

    DATA: ls_goodsmvt_header TYPE bapi2017_gm_head_01,
          ls_goodsmvt_code   TYPE bapi2017_gm_code.

    DATA(lt_dados) = gt_dados[].

    SORT: gt_dados BY vbeln,
          lt_dados BY vbeln.

    DELETE ADJACENT DUPLICATES FROM lt_dados COMPARING vbeln.

    LOOP AT lt_dados ASSIGNING FIELD-SYMBOL(<fs_dados_cab>).
      CLEAR:  lt_return[],
              lt_goodsmvt_item[].

      CLEAR:  ls_goodsmvt_header,
              ls_goodsmvt_code.

      ls_goodsmvt_header-pstng_date = sy-datum.
      ls_goodsmvt_header-doc_date   = sy-datum.
      ls_goodsmvt_header-ref_doc_no = <fs_dados_cab>-vbeln.
      ls_goodsmvt_code-gm_code      = gc_GM_CODE.

      READ TABLE gt_dados
        WITH KEY vbeln = <fs_dados_cab>-vbeln
        TRANSPORTING NO FIELDS
        BINARY SEARCH.
      IF sy-subrc = 0.
        DATA(lv_tabix) = sy-tabix.
        LOOP AT gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>)
           FROM lv_tabix
          WHERE vbeln = <fs_dados_cab>-vbeln.
          me->preenche_dados_goodsmvt_create( EXPORTING is_dados            = <fs_dados>
                                              CHANGING  ct_goodsmvt_item    = lt_goodsmvt_item[] ).

        ENDLOOP.
      ENDIF.

      me->processa_dados_goodsmvt_create( EXPORTING is_goodsmvt_header  = ls_goodsmvt_header
                                                    is_goodsmvt_code    = ls_goodsmvt_code
                                                    it_goodsmvt_item    = lt_goodsmvt_item[]
                                          CHANGING  cs_dados          = <fs_dados_cab>
                                                    ct_return         = lt_return[] ).

      LOOP AT gt_dados ASSIGNING <fs_dados>
         FROM lv_tabix
        WHERE vbeln = <fs_dados_cab>-vbeln.

        <fs_dados>-mblnr   = <fs_dados_cab>-mblnr.
        <fs_dados>-mjahr   = <fs_dados_cab>-mjahr.

      ENDLOOP.

    ENDLOOP.



  ENDMETHOD.


  METHOD salvar_dados.

    IF gt_dados[] IS NOT INITIAL.
      SORT gt_dados BY vbeln posnr.

        SELECT *
          FROM ztmm_doc_rem_rec
        INTO TABLE @DATA(lt_doc_rem_rec)
        FOR ALL ENTRIES IN @gt_dados
       WHERE vbeln = @gt_dados-vbeln
         AND posnr = @gt_dados-posnr.
      IF sy-subrc = 0.
        LOOP AT lt_doc_rem_rec ASSIGNING FIELD-SYMBOL(<fs_doc_rem_rec>).
          READ TABLE gt_dados
                INTO DATA(ls_dados)
            WITH KEY vbeln = <fs_doc_rem_rec>-vbeln
                     posnr = <fs_doc_rem_rec>-posnr
            BINARY SEARCH.
          IF sy-subrc = 0.
            IF ls_dados-mblnr IS INITIAL.
              <fs_doc_rem_rec>-type    = ls_dados-type.
              <fs_doc_rem_rec>-id      = ls_dados-id.
              <fs_doc_rem_rec>-znumber = ls_dados-znumber.
              <fs_doc_rem_rec>-message = ls_dados-message.
              <fs_doc_rem_rec>-zstatus_integracao = ls_dados-zstatusintegracao.

            ELSE.
              <fs_doc_rem_rec>-mblnr   = ls_dados-mblnr.
              <fs_doc_rem_rec>-mjahr   = ls_dados-mjahr.
            ENDIF.
            <fs_doc_rem_rec>-aenam   = sy-uname.
            <fs_doc_rem_rec>-aedat   = sy-datum.
            <fs_doc_rem_rec>-aezet   = sy-uzeit.
          ENDIF.
        ENDLOOP.

        MODIFY ztmm_doc_rem_rec FROM TABLE lt_doc_rem_rec.

        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD preenche_dados_goodsmvt_create.
    APPEND VALUE #( material        = is_dados-matnr
                    plant           = is_dados-werks
                    stge_loc        = is_dados-lgort
                    move_type       = gc_move_type
                    entry_qnt       = is_dados-zqtpen
                    entry_uom       = is_dados-meins
                    entry_uom_iso   = is_dados-meins
                    move_plant      = is_dados-werks
                    MOVE_STLOC      = is_dados-movestloc
                    deliv_numb      = is_dados-vbeln
                    deliv_item      = is_dados-posnr )
              TO ct_goodsmvt_item.
  ENDMETHOD.


  METHOD processa_dados_goodsmvt_create.
    DATA: lt_goodsmvt_item   TYPE TABLE OF bapi2017_gm_item_create.

    DATA: lv_materialdocument TYPE bapi2017_gm_head_ret-mat_doc,
          lv_matdocumentyear  TYPE bapi2017_gm_head_ret-doc_year.

    lt_goodsmvt_item[] = it_goodsmvt_item[].

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = is_goodsmvt_header
        goodsmvt_code    = is_goodsmvt_code
      IMPORTING
        materialdocument = lv_materialdocument
        matdocumentyear  = lv_matdocumentyear
      TABLES
        goodsmvt_item    = lt_goodsmvt_item
        return           = ct_return.

    IF lv_materialdocument IS INITIAL.
      SORT ct_return BY type.

      READ TABLE ct_return
      ASSIGNING FIELD-SYMBOL(<fs_return>)
      WITH KEY type = gc_error
       BINARY SEARCH.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        cs_dados-type = <fs_return>-type.
        cs_dados-id = <fs_return>-id.
        cs_dados-znumber = <fs_return>-number.
        cs_dados-message = <fs_return>-message.
        cs_dados-zstatusintegracao = gc_status_integracao-erro_mov_dev.
      ENDIF.
    ELSE.

      cs_dados-mblnr = lv_materialdocument.
      cs_dados-mjahr = lv_matdocumentyear.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
