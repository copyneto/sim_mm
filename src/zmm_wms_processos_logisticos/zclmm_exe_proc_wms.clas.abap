CLASS zclmm_exe_proc_wms DEFINITION
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
      END OF gc_status_integracao.

    CONSTANTS lc_modulo_mm TYPE ztca_param_mod-modulo VALUE 'MM' ##NO_TEXT.

    METHODS processar
      IMPORTING
        !ir_vbeln TYPE lmd_vl_t_vbeln_vl_range
        !ir_ztipo TYPE zctgmm_range_ztipo_doc
        !iv_zstat TYPE ztmm_doc_rem_rec-zstatus_integracao.

    METHODS selecionar_dados
      IMPORTING
        !ir_vbeln TYPE lmd_vl_t_vbeln_vl_range
        !ir_ztipo TYPE zctgmm_range_ztipo_doc
        !iv_zstat TYPE ztmm_doc_rem_rec-zstatus_integracao.

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

    METHODS processa_dados_po_change
      IMPORTING
        is_poheader  TYPE bapimepoheader
        is_poheaderx TYPE bapimepoheaderx
        it_poitem    TYPE bapimepoitem_tp
        it_poitemx   TYPE bapimepoitemx_tp
      CHANGING
        cs_dados     TYPE zclmm_exe_proc_wms=>ty_dados
        ct_return    TYPE bapiret2_t
        cv_erro      TYPE flag.
    METHODS processa_dados_delivery_update
      IMPORTING
        is_vbkok  TYPE vbkok
        it_vbpok  TYPE tab_vbpok
      CHANGING
        cs_dados  TYPE zclmm_exe_proc_wms=>ty_dados
        ct_return TYPE bapiret2_t
        cv_erro   TYPE flag.
    METHODS processa_dados_delivery_change
      IMPORTING
        is_header_data    TYPE bapiobdlvhdrchg
        is_header_control TYPE bapiobdlvhdrctrlchg
        it_item_data      TYPE bapiobdlvitemchg_t
        it_item_control   TYPE bapiobdlvitemctrlchg_t
      CHANGING
        cs_dados          TYPE zclmm_exe_proc_wms=>ty_dados
        ct_return         TYPE bapiret2_t
        cv_erro           TYPE flag.
    METHODS preenche_dados_po_change
      IMPORTING
        is_dados   TYPE zclmm_exe_proc_wms=>ty_dados
      CHANGING
        ct_poitem  TYPE bapimepoitem_tp
        ct_poitemx TYPE bapimepoitemx_tp.
    METHODS preenche_dados_delivery_update
      IMPORTING
        is_dados   TYPE zclmm_exe_proc_wms=>ty_dados
        iv_picking TYPE lgmng
      CHANGING
        ct_vbpok   TYPE vbpok_t.
    METHODS preenche_dados_delivery_change
      IMPORTING
        is_dados               TYPE zclmm_exe_proc_wms=>ty_dados
        iv_picking             TYPE lgmng
      CHANGING
        ct_item_data           TYPE bapiobdlvitemchg_t
        ct_item_control        TYPE bapiobdlvitemctrlchg_t.
    METHODS processa_dados_edoc_background.
    METHODS submit_edoc_background
      IMPORTING
        iv_process TYPE edoc_process
        iv_action  TYPE edoc_action
        ir_bukrs   TYPE /bglocs/fi_range_bukrs
        ir_posdt   TYPE edoc_s_posting_date_tab
        ir_edoty   TYPE edoc_s_edoc_type_tab.
    METHODS salvar_dados.
ENDCLASS.



CLASS ZCLMM_EXE_PROC_WMS IMPLEMENTATION.


  METHOD processar.

    me->selecionar_dados( ir_vbeln = ir_vbeln
                          ir_ztipo = ir_ztipo
                          iv_zstat = iv_zstat ).

    me->processar_dados( ).

    me->salvar_dados(  ).

  ENDMETHOD.


  METHOD selecionar_dados.
    DATA: lv_overstatus  TYPE GBSTK VALUE 'C'.

    SELECT  vbeln, posnr,  werks,  lgort, ztipodoc, zstatusintegracao,
            menge, zqtrec, zqtpen, meins, cancel,   mblnr,  mjahr,
            movestloc,    ernam,  erdat, erzet,    aenam,  aedat,
            aezet,        type,   id,    znumber,  message,
            overallsdprocessstatus, lfimg, umvkz,  umvkn, vrkme,  lipsmeins,
            vgbel, vgpos, vgtyp, bukrs, process, edoc_type, posting_date
      FROM zi_mm_integracao_doc_remessa
      INTO TABLE @gt_dados
     WHERE vbeln                  IN @ir_vbeln
       AND ztipodoc               IN @ir_ztipo
       AND ZstatusIntegracao      =  @iv_zstat
       AND OverallSDProcessStatus <> @lv_overstatus
       AND Cancel                 =  @abap_false.

  ENDMETHOD.


  METHOD processar_dados.
    DATA: lt_return       TYPE TABLE OF bapiret2,
          lt_item_control TYPE TABLE OF bapiobdlvitemctrlchg,
          lt_item_data    TYPE TABLE OF bapiobdlvitemchg,
          lt_vbpok        TYPE TABLE OF vbpok,
          lt_prot         TYPE TABLE OF prott,
          lt_poitem       TYPE TABLE OF bapimepoitem,
          lt_poitemx      TYPE TABLE OF bapimepoitemx,
          lt_bapiret2     TYPE TABLE OF bapiret2.


    DATA: ls_header_data    TYPE bapiobdlvhdrchg,
          ls_header_control TYPE bapiobdlvhdrctrlchg,
          ls_delivery       TYPE bapiobdlvhdrchg-deliv_numb,
          ls_vbkok          TYPE vbkok,
          ls_vbpok          TYPE vbpok,
          ls_prot           TYPE prott,
          ls_poheader       TYPE bapimepoheader,
          ls_poheaderx      TYPE bapimepoheaderx.

    DATA: lv_picking_s     TYPE lgmng,
          lv_purchaseorder TYPE bapiekko-po_number,
          lv_deliv_change  TYPE flag,
          lv_po_change     TYPE flag,
          lv_erro          TYPE flag.

    DATA(lt_dados) = gt_dados[].

    SORT: gt_dados BY vbeln,
          lt_dados BY vbeln.

    DELETE ADJACENT DUPLICATES FROM lt_dados COMPARING vbeln.

    LOOP AT lt_dados ASSIGNING FIELD-SYMBOL(<fs_dados_cab>).
      CLEAR:  lt_return[],
              lt_item_control[],
              lt_item_data[],
              lt_vbpok[],
              lt_prot[],
              lt_poitem[],
              lt_poitemx[],
              lt_bapiret2[].

      CLEAR:  ls_header_data,
              ls_header_control,
              ls_delivery,
              ls_vbkok,
              ls_vbpok,
              ls_prot,
              ls_poheader,
              ls_poheaderx.

      CLEAR:  lv_picking_s,
              lv_purchaseorder,
              lv_deliv_change,
              lv_po_change.

      ls_header_data-deliv_numb    = <fs_dados_cab>-vbeln.
*      ls_header_control-deliv_numb = <fs_dados_cab>-vbeln.

      ls_vbkok-vbeln_vl            = <fs_dados_cab>-vbeln.

      ls_poheader-po_number        = <fs_dados_cab>-vgbel.
      ls_poheaderx-po_number       = 'X'.

      READ TABLE gt_dados
        WITH KEY vbeln = <fs_dados_cab>-vbeln
        TRANSPORTING NO FIELDS
        BINARY SEARCH.
      IF sy-subrc = 0.
        DATA(lv_tabix) = sy-tabix.
        LOOP AT gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>)
           FROM lv_tabix
          WHERE vbeln = <fs_dados_cab>-vbeln.
          lv_picking_s = <fs_dados>-zqtrec * ( <fs_dados>-umvkz / <fs_dados>-umvkn ).
          IF <fs_dados>-ztipodoc = 'T' OR
             <fs_dados>-ztipodoc = 'V'.
            "Processo de transferência ou venda
            lv_picking_s = <fs_dados>-zqtrec * <fs_dados>-umvkz.
            IF <fs_dados>-zqtrec <> <fs_dados>-lfimg.
              "Quantidade divergente
              lv_deliv_change = abap_true.
              me->preenche_dados_delivery_change( EXPORTING is_dados        = <fs_dados>
                                                            iv_picking      = lv_picking_s
                                                  CHANGING  ct_item_data    = lt_item_data[]
                                                            ct_item_control = lt_item_control[] ).
            ENDIF.
            "Se for pedido de transferência
            IF <fs_dados>-vgtyp = 'V'.
              lv_po_change = abap_true.
              me->preenche_dados_po_change( EXPORTING is_dados   = <fs_dados>
                                            CHANGING  ct_poitem  = lt_poitem[]
                                                      ct_poitemx = lt_poitemx[] ).
            ENDIF.
          ELSEIF <fs_dados>-ztipodoc = 'E'.
            "Processo de entrada
            lv_picking_s = <fs_dados>-lfimg * ( <fs_dados>-umvkz / <fs_dados>-umvkn ).
          ENDIF.

          me->preenche_dados_delivery_update( EXPORTING is_dados        = <fs_dados>
                                                        iv_picking      = lv_picking_s
                                              CHANGING  ct_vbpok        = lt_vbpok[] ).

        ENDLOOP.
      ENDIF.

      IF lv_deliv_change IS NOT INITIAL.
        me->processa_dados_delivery_change( EXPORTING is_header_data    = ls_header_data
                                                      is_header_control = ls_header_control
                                                      it_item_data      = lt_item_data[]
                                                      it_item_control   = lt_item_control[]
                                            CHANGING  cs_dados          = <fs_dados_cab>
                                                      ct_return         = lt_return[]
                                                      cv_erro           = lv_erro ).
      ENDIF.

      IF lv_erro IS INITIAL.
        me->processa_dados_delivery_update( EXPORTING is_vbkok        = ls_vbkok
                                                      it_vbpok        = lt_vbpok[]
                                            CHANGING  cs_dados        = <fs_dados_cab>
                                                      ct_return       = lt_return[]
                                                      cv_erro         = lv_erro ).
        IF lv_erro IS INITIAL AND
           lv_po_change IS NOT INITIAL.
          me->processa_dados_po_change( EXPORTING is_poheader     = ls_poheader
                                                  is_poheaderx    = ls_poheaderx
                                                  it_poitem       = lt_poitem[]
                                                  it_poitemx      = lt_poitemx[]
                                        CHANGING  cs_dados        = <fs_dados_cab>
                                                  ct_return       = lt_return[]
                                                  cv_erro         = lv_erro ).
        ENDIF.
      ENDIF.


      LOOP AT gt_dados ASSIGNING <fs_dados>
         FROM lv_tabix
        WHERE vbeln = <fs_dados_cab>-vbeln.

        <fs_dados>-type    = <fs_dados_cab>-type.
        <fs_dados>-id      = <fs_dados_cab>-id.
        <fs_dados>-znumber = <fs_dados_cab>-znumber.
        <fs_dados>-message = <fs_dados_cab>-message.
        <fs_dados>-zstatusintegracao = <fs_dados_cab>-zstatusintegracao.

      ENDLOOP.

    ENDLOOP.

    me->processa_dados_edoc_background(  ).


  ENDMETHOD.


  METHOD preenche_dados_delivery_change.
    APPEND VALUE #( deliv_numb      = is_dados-vbeln
                    deliv_item      = is_dados-posnr
                    dlv_qty         = is_dados-zqtrec
                    dlv_qty_imunit  = iv_picking
                    fact_unit_nom   = iv_picking
                    fact_unit_denom = is_dados-zqtrec
                    sales_unit      = is_dados-vrkme
                    sales_unit_iso  = is_dados-vrkme
                    base_uom        = is_dados-lipsmeins
                    base_uom_iso    = is_dados-lipsmeins )
              TO ct_item_data.

    APPEND VALUE #( deliv_numb      = is_dados-vbeln
                    deliv_item      = is_dados-posnr
                    chg_delqty      = 'X' )
              TO ct_item_control.

  ENDMETHOD.


  METHOD preenche_dados_delivery_update.
    APPEND VALUE #( vbeln_vl = is_dados-vbeln
                    posnr_vl = is_dados-posnr
                    vbeln    = is_dados-vbeln
                    posnn    = is_dados-posnr
                    pikmg    = iv_picking
                    lfimg    = iv_picking
                    lgmng    = iv_picking
                    vrkme    = is_dados-vrkme
                    meins    = is_dados-lipsmeins
                    vfdat    = sy-datum )
              TO ct_vbpok.

  ENDMETHOD.


  METHOD preenche_dados_po_change.
    APPEND VALUE #( po_item = is_dados-vgpos
                    deliv_compl = 'X' )
              TO ct_poitem.

    APPEND VALUE #( po_item = is_dados-vgpos
                    deliv_compl = 'X' )
              TO ct_poitemx.

  ENDMETHOD.


  METHOD processa_dados_delivery_change.
    DATA: lt_item_control TYPE TABLE OF bapiobdlvitemctrlchg,
          lt_item_data    TYPE TABLE OF bapiobdlvitemchg.

    lt_item_control[] = it_item_control[].
    lt_item_data[]    = it_item_data[].

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        header_data    = is_header_data
        header_control = is_header_control
        delivery       = is_header_data-deliv_numb
      TABLES
        item_data      = lt_item_data
        item_control   = lt_item_control
        return         = ct_return.

    SORT ct_return BY type.

    READ TABLE ct_return
    ASSIGNING FIELD-SYMBOL(<fs_return>)
    WITH KEY type = 'E'
     BINARY SEARCH.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      cv_erro = abap_true.

      cs_dados-type = <fs_return>-type.
      cs_dados-id = <fs_return>-id.
      cs_dados-znumber = <fs_return>-number.
      cs_dados-message = <fs_return>-message.
      cs_dados-zstatusintegracao = gc_status_integracao-erro_processo.
*        LOOP AT ct_return ASSIGNING <fs_return>
*          WHERE type EQ 'E'.
*        ENDLOOP.

    ELSE.

      cs_dados-type = 'S'.
      cs_dados-id = 'ZMM'.
      cs_dados-znumber = '001'.
      cs_dados-zstatusintegracao = gc_status_integracao-finalizado.
      MESSAGE ID cs_dados-id
            TYPE cs_dados-type
          NUMBER cs_dados-znumber
            INTO cs_dados-message.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD processa_dados_delivery_update.
    DATA: lt_prot TYPE STANDARD TABLE OF prott,
          lt_vbpok        TYPE TABLE OF vbpok,
          ls_prot TYPE prott.

    lt_vbpok[] = it_vbpok[].

    CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
      EXPORTING
        vbkok_wa               = is_vbkok " Header Data
        synchron               = abap_true
        commit                 = abap_true
        delivery               = is_vbkok-vbeln_vl
        update_picking         = abap_true
        if_database_update_1   = '1'
        if_error_messages_send = abap_false
      TABLES
        vbpok_tab              = lt_vbpok[] " Item Data
        prot                   = lt_prot. " Message Return Table
    SORT lt_prot BY msgty.
    READ TABLE lt_prot INTO ls_prot WITH KEY msgty = 'E' BINARY SEARCH.
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      cv_erro = abap_true.

      cs_dados-type    = ls_prot-msgty.
      cs_dados-id      = ls_prot-msgid.
      cs_dados-znumber = ls_prot-msgno.
      cs_dados-zstatusintegracao = gc_status_integracao-erro_processo.

      MESSAGE ID ls_prot-msgid
            TYPE ls_prot-msgty
          NUMBER ls_prot-msgno
            WITH ls_prot-msgv1 ls_prot-msgv2 ls_prot-msgv3 ls_prot-msgv4
            INTO cs_dados-message.
    ELSE.

      cs_dados-type    = 'S'.
      cs_dados-id      = 'ZMM'.
      cs_dados-znumber = '001'.
      cs_dados-zstatusintegracao = gc_status_integracao-finalizado.
      MESSAGE ID cs_dados-id
            TYPE cs_dados-type
          NUMBER cs_dados-znumber
            INTO cs_dados-message.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDIF.

  ENDMETHOD.


  METHOD processa_dados_po_change.
    DATA:  lt_poitem       TYPE TABLE OF bapimepoitem,
           lt_poitemx      TYPE TABLE OF bapimepoitemx.

    lt_poitem[]   = it_poitem[].
    lt_poitemx[]  = it_poitemx[].

    CALL FUNCTION 'BAPI_PO_CHANGE'
      EXPORTING
        purchaseorder = is_poheader-po_number
        poheader      = is_poheader
        poheaderx     = is_poheaderx
      TABLES
        return        = ct_return
        poitem        = lt_poitem
        poitemx       = lt_poitemx.

    READ TABLE ct_return
    ASSIGNING FIELD-SYMBOL(<fs_return>)
    WITH KEY type = 'E'
     BINARY SEARCH.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      cv_erro = abap_true.

      cs_dados-type    = <fs_return>-type.
      cs_dados-id      = <fs_return>-id.
      cs_dados-znumber = <fs_return>-number.
      cs_dados-message = <fs_return>-message.
      cs_dados-zstatusintegracao = gc_status_integracao-erro_processo.

*        LOOP AT ct_return ASSIGNING <fs_return>
*          WHERE type EQ 'E'.
*        ENDLOOP.

    ELSE.

      cs_dados-type    = 'S'.
      cs_dados-id      = 'ZMM'.
      cs_dados-znumber = '001'.
      cs_dados-zstatusintegracao = gc_status_integracao-finalizado.
      MESSAGE ID cs_dados-id
            TYPE cs_dados-type
          NUMBER cs_dados-znumber
            INTO cs_dados-message.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.


  ENDMETHOD.


  METHOD processa_dados_edoc_background.

    DATA: lt_dados   TYPE TABLE OF ty_dados.

    DATA: lv_process TYPE edocument-process,
          lv_action  TYPE edoactionproc-edoaction.

    DATA: lr_bukrs TYPE RANGE OF edocument-bukrs,
          lr_posdt TYPE RANGE OF edocument-posting_date,
          lr_edoty TYPE RANGE OF edocument-edoc_type.

    DATA: ls_bukrs LIKE LINE OF lr_bukrs,
          ls_posdt LIKE LINE OF lr_posdt,
          ls_edoty LIKE LINE OF lr_edoty.

    SORT gt_dados BY zstatusintegracao.
    READ TABLE gt_dados
      WITH KEY zstatusintegracao = gc_status_integracao-finalizado
      TRANSPORTING NO FIELDS
      BINARY SEARCH.
    IF sy-subrc = 0.
      DATA(lv_tabix) = sy-tabix.
      LOOP AT gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados_temp>) FROM lv_tabix WHERE zstatusintegracao = gc_status_integracao-finalizado
                                                                               AND process IS NOT INITIAL.
        APPEND <fs_dados_temp> TO lt_dados.
      ENDLOOP.
    ENDIF.

    SORT: gt_dados BY process.
    SORT: lt_dados BY process.

    DELETE ADJACENT DUPLICATES FROM lt_dados COMPARING process.

    LOOP AT lt_dados ASSIGNING FIELD-SYMBOL(<fs_dados_cab>).
      CLEAR: lr_bukrs[],
             lr_posdt[],
             lr_edoty[],
             ls_bukrs,
             ls_posdt,
             ls_edoty.

      lv_process  = <fs_dados_cab>-process.
      lv_action   = 'CONTINUE'.

      ls_bukrs-sign   = 'I'.
      ls_bukrs-option = 'EQ'.

      ls_posdt-sign   = 'I'.
      ls_posdt-option = 'EQ'.

      ls_edoty-sign   = 'I'.
      ls_edoty-option = 'EQ'.

      READ TABLE gt_dados
        WITH KEY process = <fs_dados_cab>-process
        TRANSPORTING NO FIELDS
        BINARY SEARCH.
      IF sy-subrc = 0.
        lv_tabix = sy-tabix.
        LOOP AT gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>)
           FROM lv_tabix
          WHERE process = <fs_dados_cab>-process.

          ls_bukrs-low    = <fs_dados>-bukrs.
          COLLECT ls_bukrs INTO lr_bukrs.

          ls_posdt-low    = <fs_dados>-posting_date.
          COLLECT ls_posdt INTO lr_posdt.

          ls_edoty-low    = <fs_dados>-edoc_type.
          COLLECT ls_edoty INTO lr_edoty.
        ENDLOOP.
      ENDIF.
      me->submit_edoc_background( EXPORTING iv_process = lv_process
                                            iv_action  = lv_action
                                            ir_bukrs   = lr_bukrs[]
                                            ir_posdt   = lr_posdt[]
                                            ir_edoty   = lr_edoty[] ).

    ENDLOOP.

  ENDMETHOD.


  METHOD submit_edoc_background.
    DATA: lv_jobname  TYPE tbtco-jobname,
          lv_jobcount TYPE tbtco-jobcount.

    lv_jobname = 'JOB_EDOC_BACKGOUND'.

*   Open job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_jobname
      IMPORTING
        jobcount         = lv_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    CASE sy-subrc.
      WHEN 0.
      WHEN OTHERS.
        MESSAGE e208(00) WITH 'Error'(e01).
    ENDCASE.

*   Call report
    SUBMIT edoc_background
      USER sy-uname
      VIA JOB lv_jobname
    NUMBER lv_jobcount
      WITH p_proc = iv_process
      WITH p_act  = iv_action
      WITH so_bukrs IN ir_bukrs
      WITH so_posdt IN ir_posdt
      WITH so_edoty IN ir_edoty
      AND RETURN.

*   Close job
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = lv_jobcount
        jobname              = lv_jobname
        strtimmed            = 'X'  " start immediatly
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.
    IF sy-subrc <> 0.
      MESSAGE e208(00) WITH 'Error'(e01).
    ENDIF.
  ENDMETHOD.


  METHOD salvar_dados.

    IF gt_dados[] IS NOT INITIAL.
      SORT: gt_dados BY vbeln posnr.

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
            <fs_doc_rem_rec>-type    = ls_dados-type.
            <fs_doc_rem_rec>-id      = ls_dados-id.
            <fs_doc_rem_rec>-znumber = ls_dados-znumber.
            <fs_doc_rem_rec>-message = ls_dados-message.
            <fs_doc_rem_rec>-zstatus_integracao = ls_dados-zstatusintegracao.
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
ENDCLASS.
