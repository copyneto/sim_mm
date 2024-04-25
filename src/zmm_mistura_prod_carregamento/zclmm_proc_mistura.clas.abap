CLASS zclmm_proc_mistura DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_mkpf_key,
        vbeln TYPE ztmm_proc_mistur-vbeln,
        posnr TYPE ztmm_proc_mistur-posnr,
        cpudt TYPE mkpf-cpudt,
        usnam TYPE mkpf-usnam,
        bktxt TYPE mkpf-bktxt,
      END OF ty_mkpf_key .

    DATA:
      gt_proc_mistura TYPE TABLE OF ztmm_proc_mistur .
    DATA:
      gs_proc_mistura LIKE LINE OF gt_proc_mistura .
    DATA gv_result TYPE char1 .
    DATA gv_uname TYPE sy-uname .
    DATA gv_udate TYPE sy-datum .

    METHODS selecionar_dados
      IMPORTING
        !it_xlips TYPE shp_lips_t OPTIONAL .
    METHODS valida_mistura
      IMPORTING
        !it_xlips            TYPE shp_lips_t
      RETURNING
        VALUE(rv_mistura_ok) TYPE flag .
    METHODS process_mistura
      IMPORTING
        !it_xlips TYPE shp_lips_t
      EXPORTING
        !ev_error TYPE boolean .
    METHODS task_finish
      IMPORTING
        !p_task TYPE clike .
    METHODS process_create_mts .
    METHODS busca_docs_gerados .
    METHODS process_change_status .
    METHODS process_settlement_group
      IMPORTING
        !iv_status_suc TYPE ze_status_mistur
        !iv_status_err TYPE ze_status_mistur .
    METHODS proc_estorno
      IMPORTING
        !it_xlips TYPE shp_lips_t .
    METHODS process_settlement .
    METHODS change_status_cancel .
    METHODS valida_estorno
      IMPORTING
        !it_xlips        TYPE shp_lips_t
      RETURNING
        VALUE(rv_valida) TYPE flag .
    METHODS proc_repmanconf1_cancel .
    METHODS limpa_variaveis .
    METHODS salvar_dados .
    METHODS task_finish_single
      IMPORTING
        !p_task TYPE clike .
    METHODS task_finish_group
      IMPORTING
        !p_task TYPE clike .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS delete_dados_erro.
ENDCLASS.



CLASS zclmm_proc_mistura IMPLEMENTATION.


  METHOD selecionar_dados.

    DATA ls_proc_mistura LIKE LINE OF gt_proc_mistura.

    IF it_xlips IS NOT INITIAL.

      SELECT matnr, werks, stlan, stlnr, stlal
       FROM mast
       INTO TABLE @DATA(lt_mast)
       FOR ALL ENTRIES IN @it_xlips
       WHERE matnr = @it_xlips-matnr
       AND werks = @it_xlips-werks.

    ENDIF.

    IF sy-subrc IS INITIAL.

      IF it_xlips IS NOT INITIAL.

        SELECT werks, lgort, matnr, verid
         FROM ztmm_de_para_dep
         INTO TABLE @DATA(lt_verid)
         FOR ALL ENTRIES IN @it_xlips
         WHERE werks = @it_xlips-werks
           AND lgort = @it_xlips-lgort
           AND matnr = @it_xlips-matnr.

      ENDIF.

      SORT: lt_mast  BY matnr werks,
            lt_verid BY werks lgort matnr,
            gt_proc_mistura BY vbeln posnr.

      LOOP AT it_xlips ASSIGNING FIELD-SYMBOL(<fs_xlips>).

        READ TABLE lt_mast WITH KEY matnr = <fs_xlips>-matnr werks = <fs_xlips>-werks TRANSPORTING NO FIELDS BINARY SEARCH.

        IF sy-subrc IS INITIAL.

          READ TABLE gt_proc_mistura WITH KEY vbeln = <fs_xlips>-vbeln posnr = <fs_xlips>-posnr TRANSPORTING NO FIELDS BINARY SEARCH.

          IF sy-subrc IS NOT INITIAL.

            CLEAR ls_proc_mistura.

            ls_proc_mistura-vbeln = <fs_xlips>-vgbel.
            ls_proc_mistura-posnr = <fs_xlips>-vgpos.
            ls_proc_mistura-matnr = <fs_xlips>-matnr.
            ls_proc_mistura-werks = <fs_xlips>-werks.
            ls_proc_mistura-lgort = <fs_xlips>-lgort.
            ls_proc_mistura-lfimg = <fs_xlips>-lfimg.

            READ TABLE lt_verid ASSIGNING FIELD-SYMBOL(<fs_verid>) WITH KEY werks = <fs_xlips>-werks lgort = <fs_xlips>-lgort  matnr = <fs_xlips>-matnr BINARY SEARCH.

            IF sy-subrc IS INITIAL.

              ls_proc_mistura-verid = <fs_verid>-verid.

            ELSE.

              ls_proc_mistura-status = '3'.
              ls_proc_mistura-msg = TEXT-t06.

            ENDIF.

            APPEND ls_proc_mistura TO gt_proc_mistura.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

    DELETE gt_proc_mistura WHERE status = '1'.

  ENDMETHOD.


  METHOD valida_mistura.

    DATA: lv_status TYPE ztmm_proc_mistur-status,
          lt_likp   TYPE TABLE OF likp,
          lt_lips   TYPE TABLE OF lips,
          lt_mast   TYPE TABLE OF mast.

    rv_mistura_ok = abap_true.

    IF it_xlips IS NOT INITIAL.

      SELECT vbeln, posnr
       FROM ztmm_proc_mistur
       INTO TABLE @gt_proc_mistura
       FOR ALL ENTRIES IN @it_xlips
       WHERE vbeln = @it_xlips-vbeln
       AND posnr = @it_xlips-posnr.

    ENDIF.

    IF sy-subrc IS INITIAL.

      DELETE gt_proc_mistura WHERE status = '2' OR status = '3'.

      IF sy-subrc IS NOT INITIAL.

        rv_mistura_ok = abap_false.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD process_mistura.

    gv_uname = sy-uname.
    gv_udate = sy-datum.

*    IF valida_mistura( it_xlips = it_xlips ).

    selecionar_dados( it_xlips = it_xlips ).

    CHECK gt_proc_mistura[] IS NOT INITIAL.

    process_create_mts(  ).

    IF line_exists( gt_proc_mistura[ status = '3' ] ).
      ev_error = abap_true.
    ELSE.

      process_settlement_group( iv_status_suc = '1'
                              iv_status_err = '3'  ).
    ENDIF.

    salvar_dados( ).

*    ENDIF.

  ENDMETHOD.


  METHOD task_finish.

    RECEIVE RESULTS FROM FUNCTION 'ZFMMM_MISTURA_PROD_CARREGAMENT'
       CHANGING
          cs_proc_mistura = gs_proc_mistura.

    gv_result = abap_true .

  ENDMETHOD.


  METHOD process_create_mts.

    CHECK gt_proc_mistura[] IS NOT INITIAL.

    LOOP AT gt_proc_mistura ASSIGNING FIELD-SYMBOL(<fs_proc_mistura>) WHERE status <> '3'.

      CLEAR: gv_result.

      gs_proc_mistura = <fs_proc_mistura>.

      GET TIME STAMP FIELD DATA(lv_ts).

      DATA(lv_task) = |{ <fs_proc_mistura>-vbeln }{ <fs_proc_mistura>-posnr }{ lv_ts }|.

      CALL FUNCTION 'ZFMMM_MISTURA_PROD_CARREGAMENT'
        STARTING NEW TASK lv_task
        CALLING task_finish ON END OF TASK
        CHANGING
          cs_proc_mistura = gs_proc_mistura.

      WAIT FOR ASYNCHRONOUS TASKS UNTIL gv_result IS NOT INITIAL.

      <fs_proc_mistura> = gs_proc_mistura.

    ENDLOOP.

    busca_docs_gerados(  ).

  ENDMETHOD.


  METHOD busca_docs_gerados.

    DATA lt_mkpf_key TYPE TABLE OF ty_mkpf_key.

    lt_mkpf_key = VALUE #( FOR <fs_proc_mistura> IN gt_proc_mistura
                           ( vbeln   = <fs_proc_mistura>-vbeln
                             posnr   = <fs_proc_mistura>-posnr
                             cpudt   = gv_udate
                             usnam  = gv_uname
                             bktxt   = |{ <fs_proc_mistura>-vbeln }-{ <fs_proc_mistura>-posnr }| ) ).

    SORT lt_mkpf_key BY vbeln posnr.

    SELECT a~mblnr, a~mjahr, a~cpudt, a~usnam, a~bktxt, b~aufnr
    FROM mkpf AS a
    INNER JOIN mseg AS b ON b~mblnr = a~mblnr AND b~mjahr = a~mjahr AND b~bwart = '131'
    FOR ALL ENTRIES IN @lt_mkpf_key
    WHERE a~cpudt = @lt_mkpf_key-cpudt
    AND  a~usnam = @lt_mkpf_key-usnam
    AND  a~bktxt  = @lt_mkpf_key-bktxt
    INTO TABLE @DATA(lt_mkpf).

    IF lt_mkpf[] IS NOT INITIAL.

      SORT: lt_mkpf_key BY cpudt usnam bktxt,
            lt_mkpf     BY cpudt usnam bktxt.

      LOOP AT gt_proc_mistura ASSIGNING FIELD-SYMBOL(<fs_processa_mistura>) WHERE status <> '3' . "#EC CI_STDSEQ

        READ TABLE lt_mkpf_key
          ASSIGNING FIELD-SYMBOL(<fs_mkpf_key>)
          WITH KEY vbeln = <fs_processa_mistura>-vbeln
                   posnr = <fs_processa_mistura>-posnr
          BINARY SEARCH.

        IF sy-subrc IS INITIAL.

          READ TABLE lt_mkpf
            ASSIGNING FIELD-SYMBOL(<fs_mkpf>)
            WITH KEY cpudt  = <fs_mkpf_key>-cpudt
                     usnam = <fs_mkpf_key>-usnam
                     bktxt  = <fs_mkpf_key>-bktxt
            BINARY SEARCH.

          IF sy-subrc IS INITIAL.

            <fs_processa_mistura>-mblnr = <fs_mkpf>-mblnr.
            <fs_processa_mistura>-mjahr = <fs_mkpf>-mjahr.
            <fs_processa_mistura>-aufnr = <fs_mkpf>-aufnr.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD process_change_status.

    DATA lv_objnr  TYPE j_objnr.

    DATA(lt_proc_mistura) = gt_proc_mistura[].

    DELETE lt_proc_mistura WHERE aufnr IS INITIAL.

    CHECK lt_proc_mistura[] IS NOT INITIAL.

    SORT: lt_proc_mistura BY aufnr,
          gt_proc_mistura BY aufnr.

    DELETE ADJACENT DUPLICATES FROM lt_proc_mistura COMPARING aufnr.

    LOOP AT lt_proc_mistura ASSIGNING FIELD-SYMBOL(<fs_proc_mistura_aufnr>).

      lv_objnr = |OR{ <fs_proc_mistura_aufnr>-aufnr }|.

      CALL FUNCTION 'I_CHANGE_STATUS'
        EXPORTING
          objnr          = lv_objnr
          estat_inactive = 'I0002'
          estat_active   = 'I0045'
          stsma          = space
        EXCEPTIONS
          cannot_update  = 1
          OTHERS         = 2.

      IF sy-subrc <> 0.

        <fs_proc_mistura_aufnr>-status = '3'.
        <fs_proc_mistura_aufnr>-msg = TEXT-t01.

        READ TABLE gt_proc_mistura
          TRANSPORTING NO FIELDS
          WITH KEY aufnr = <fs_proc_mistura_aufnr>-aufnr
          BINARY SEARCH.

        IF sy-subrc IS INITIAL.

          DATA(lv_tabix) = sy-tabix.

          LOOP AT gt_proc_mistura ASSIGNING FIELD-SYMBOL(<fs_proc_mistura>) FROM lv_tabix WHERE aufnr = <fs_proc_mistura_aufnr>-aufnr.

            <fs_proc_mistura>-status =  <fs_proc_mistura_aufnr>-status.
            <fs_proc_mistura>-msg    =  <fs_proc_mistura_aufnr>-msg.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD process_settlement_group.

    DATA: lt_rem_obj   TYPE kabrt_objtab_table,
          lt_trace_obj TYPE kabrt_objtab_table,
          lt_setyptab  TYPE kabrt_setyp_table,
          lt_objtab    TYPE TABLE OF jsto_pre,
          lt_set       TYPE kabrt_aufsel_table,
          lt_late_obj  TYPE kabrt_objtab_table,
          lt_add_sdr   TYPE kabrt_objtab_table,
          lt_bel_all   TYPE kabrt_bel_all_table,
          lt_matnr     TYPE kabrt_cose_sel_table,
          lt_wsum      TYPE kabrt_wsum_table,
          lt_gsum      TYPE kabrt_gsum_table,
          ls_lst       TYPE kabr_lst,
          ls_auak      TYPE auak,
          ls_control   TYPE kabr_control,
          lv_objnr     TYPE j_objnr.

    CONSTANTS: lc_kokrs    TYPE auak-kokrs  VALUE '1000',
               lc_wrttp    TYPE auak-wrttp VALUE '04',
               lc_co_vaart TYPE auak-co_vaart VALUE '1',
               lc_wrttp_r  TYPE kabr_control-wrttp VALUE '04',
               lc_selart   TYPE kabr_control-selart VALUE 'OR1',
               lc_ok       TYPE kabr_control-ok VALUE 'AUSF',
               lc_prefixo  TYPE string VALUE 'OR'.

    DATA(lt_proc_mistura) = gt_proc_mistura[].

    DELETE lt_proc_mistura WHERE aufnr IS INITIAL.

    CHECK lt_proc_mistura[] IS NOT INITIAL.

    SORT: lt_proc_mistura BY aufnr,
          gt_proc_mistura BY aufnr.

    DELETE ADJACENT DUPLICATES FROM lt_proc_mistura COMPARING aufnr.

    LOOP AT lt_proc_mistura ASSIGNING FIELD-SYMBOL(<fs_proc_mistura_aufnr>).

      CLEAR: gv_result.

      gs_proc_mistura = <fs_proc_mistura_aufnr>.

      GET TIME STAMP FIELD DATA(lv_ts).

      DATA(lv_task) = |GROUP{ <fs_proc_mistura_aufnr>-aufnr }{ lv_ts }|.

      CALL FUNCTION 'ZFMMM_SETTLEMENT_GROUP'
        STARTING NEW TASK lv_task
        CALLING task_finish_group ON END OF TASK
        EXPORTING
          iv_status_suc   = iv_status_suc
          iv_status_err   = iv_status_err
        CHANGING
          cs_proc_mistura = gs_proc_mistura.

      WAIT FOR ASYNCHRONOUS TASKS UNTIL gv_result IS NOT INITIAL.

      <fs_proc_mistura_aufnr> = gs_proc_mistura.

      READ TABLE gt_proc_mistura
        TRANSPORTING NO FIELDS
        WITH KEY aufnr = <fs_proc_mistura_aufnr>-aufnr
        BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        DATA(lv_tabix) = sy-tabix.

        LOOP AT gt_proc_mistura ASSIGNING FIELD-SYMBOL(<fs_proc_mistura>) FROM lv_tabix WHERE aufnr = <fs_proc_mistura_aufnr>-aufnr. "#EC CI_NESTED

          <fs_proc_mistura>-status  = <fs_proc_mistura_aufnr>-status.
          <fs_proc_mistura>-msg     = <fs_proc_mistura_aufnr>-msg.
          <fs_proc_mistura>-uname   = <fs_proc_mistura_aufnr>-uname.
          <fs_proc_mistura>-data    = <fs_proc_mistura_aufnr>-data.

        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD proc_estorno.

    limpa_variaveis( ) .

    IF valida_estorno( it_xlips = it_xlips  ) .

      delete_dados_erro(  ).

      CHECK gt_proc_mistura[] IS NOT INITIAL.

      process_settlement( ).

      proc_repmanconf1_cancel(  ).

      salvar_dados( ).

    ENDIF.
  ENDMETHOD.


  METHOD process_settlement.
    DATA: ls_auak         TYPE auak,
          ls_control      TYPE kabr_control,
          lt_bel_all      TYPE kabrt_bel_all_table,
          lt_proc_mistura TYPE TABLE OF ztmm_proc_mistur,
          lv_aufnr        TYPE j_objnr,
          ls_e_lst        TYPE kabr_lst.

    DATA: lt_rem_obj   TYPE kabrt_objtab_table,
          lt_trace_obj TYPE kabrt_objtab_table,
          lt_setyptab  TYPE kabrt_setyp_table,
          lt_objtab    TYPE TABLE OF jsto_pre,
          lt_set       TYPE kabrt_aufsel_table,
          lt_late_obj  TYPE kabrt_objtab_table,
          lt_add_sdr   TYPE kabrt_objtab_table,
          lt_matnr     TYPE kabrt_cose_sel_table,
          lt_wsum      TYPE kabrt_wsum_table,
          lt_gsum      TYPE kabrt_gsum_table,
          ls_lst       TYPE kabr_lst,
          lv_objnr     TYPE j_objnr.

    CONSTANTS: lc_kokrs    TYPE auak-kokrs  VALUE '1000',
               lc_wrttp    TYPE auak-wrttp VALUE '04',
               lc_co_vaart TYPE auak-co_vaart VALUE '1',
               lc_wrttp_r  TYPE kabr_control-wrttp VALUE '04',
               lc_selart   TYPE kabr_control-selart VALUE 'OR1',
               lc_stor     TYPE kabr_control-ok VALUE 'STOR',
               lc_ok       TYPE kabr_control-ok VALUE 'AUSF',
               lc_prefixo  TYPE string VALUE 'OR'.

    "jogar os dados da gt_proc_mistura para uma tabela local, ordenar pelo campo aufnr e deletar os duplicados.

    CHECK gt_proc_mistura[] IS NOT INITIAL.

    MOVE-CORRESPONDING gt_proc_mistura TO lt_proc_mistura.

    SORT lt_proc_mistura BY status.

    READ TABLE lt_proc_mistura WITH KEY status = '4' TRANSPORTING NO FIELDS BINARY SEARCH.

    CHECK sy-subrc IS NOT INITIAL.

    SORT lt_proc_mistura BY aufnr.
    DELETE ADJACENT DUPLICATES FROM lt_proc_mistura COMPARING aufnr.

    LOOP AT lt_proc_mistura ASSIGNING FIELD-SYMBOL(<fs_processa_mistura_aufnr>).
      CLEAR: gv_result.

      CLEAR: ls_auak,
             ls_control,
             ls_lst,
             lt_rem_obj[],
             lt_trace_obj[],
             lt_setyptab[],
             lt_objtab[],
             lt_set[],
             lt_late_obj[],
             lt_add_sdr[],
             lt_bel_all[],
             lt_matnr[],
             lt_wsum[],
             lt_gsum[].

      lv_objnr   = |OR{ <fs_processa_mistura_aufnr>-aufnr }|.

      ls_auak-kokrs    = lc_kokrs.
      ls_auak-cpudt    = sy-datum.
      ls_auak-budat    = sy-datum.
      ls_auak-gjahr    = <fs_processa_mistura_aufnr>-data(4).
      ls_auak-bugjahr  = <fs_processa_mistura_aufnr>-data(4).
      ls_auak-perio    = <fs_processa_mistura_aufnr>-data+4(2).
      ls_auak-buperio  = <fs_processa_mistura_aufnr>-data+4(2).
      ls_auak-kurst    = 'M'.
      ls_auak-wrttp    = lc_wrttp.
      ls_auak-saprl    = sy-saprl.
      ls_auak-objnr    = lv_objnr.
      ls_auak-co_vaart = '1'.
      CONDENSE ls_auak-co_vaart NO-GAPS.

      ls_control-wrttp             = lc_wrttp_r.
      ls_control-detaillist        = abap_true.
      ls_control-revers            = abap_true.
      ls_control-selart            = lc_selart.
      ls_control-ok                = lc_stor.
      ls_control-count_com         = '1'.
      ls_control-zeile             = '1'.
      ls_control-afpo_pre_read     = abap_true.
      ls_control-coslv_read        = abap_true.
      ls_control-single_processing = abap_true.
      ls_control-mess_identif      = sy-uzeit.

      APPEND VALUE #( setyp = 'OR') TO lt_setyptab.
      APPEND VALUE #( objnr = lv_objnr ) TO lt_objtab.

      CALL FUNCTION 'MESSAGES_INITIALIZE'.

      CALL FUNCTION 'K_SETTLEMENT_GROUP_PROCESS'
        EXPORTING
          i_auak       = ls_auak
          i_control    = ls_control
          it_rem_obj   = lt_rem_obj
          it_trace_obj = lt_trace_obj
          it_setyptab  = lt_setyptab
        IMPORTING
          e_lst        = ls_lst
        TABLES
          it_objtab    = lt_objtab
        CHANGING
          ct_set       = lt_set
          ct_late_obj  = lt_late_obj
          ct_add_sdr   = lt_add_sdr
          ct_bel_all   = lt_bel_all
          ct_matnr     = lt_matnr
          ct_wsum      = lt_wsum
          ct_gsum      = lt_gsum.
      IF ls_e_lst-subrc <> 0.

        <fs_processa_mistura_aufnr>-status = '4'.
        <fs_processa_mistura_aufnr> = TEXT-t03."'Não foi possível estornar a liquidação da Ordem'.

      ENDIF.

      READ TABLE gt_proc_mistura
        TRANSPORTING NO FIELDS
        WITH KEY aufnr = <fs_processa_mistura_aufnr>-aufnr
        BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        DATA(lv_tabix) = sy-tabix.

        LOOP AT gt_proc_mistura ASSIGNING FIELD-SYMBOL(<fs_proc_mistura>) FROM lv_tabix WHERE aufnr = <fs_processa_mistura_aufnr>-aufnr. "#EC CI_NESTED

          <fs_proc_mistura>-status =  <fs_processa_mistura_aufnr>-status.
          <fs_proc_mistura>-msg    =  <fs_processa_mistura_aufnr>-msg.

        ENDLOOP.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD change_status_cancel.

    DATA: lv_objnr        TYPE j_objnr,
          lt_proc_mistura TYPE TABLE OF ztmm_proc_mistur.

    CHECK gt_proc_mistura[] IS NOT INITIAL.

    MOVE-CORRESPONDING gt_proc_mistura TO lt_proc_mistura.

    SORT lt_proc_mistura BY status.

    READ TABLE lt_proc_mistura WITH KEY status = '4' TRANSPORTING NO FIELDS BINARY SEARCH.

    CHECK sy-subrc IS NOT INITIAL.

    SORT lt_proc_mistura BY aufnr.
    SORT gt_proc_mistura BY aufnr.
    DELETE ADJACENT DUPLICATES FROM lt_proc_mistura COMPARING aufnr.

    LOOP AT lt_proc_mistura ASSIGNING FIELD-SYMBOL(<fs_processa_mistura_aufnr>).

      lv_objnr = 'OR' && <fs_processa_mistura_aufnr>-aufnr.

      CALL FUNCTION 'I_CHANGE_STATUS'
        EXPORTING
          objnr          = lv_objnr
          estat_inactive = 'I0045'
          estat_active   = 'I0002'
          stsma          = space
        EXCEPTIONS
          cannot_update  = 1
          OTHERS         = 2.

      IF sy-subrc <> 0.

        <fs_processa_mistura_aufnr>-status = '4'.
        <fs_processa_mistura_aufnr>-msg = TEXT-t05. "'Não foi possível estornar o encerramento da Ordem'.
      ELSE.
        <fs_processa_mistura_aufnr>-status = '2'.

      ENDIF.
      READ TABLE gt_proc_mistura
        TRANSPORTING NO FIELDS
        WITH KEY aufnr = <fs_processa_mistura_aufnr>-aufnr
        BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        DATA(lv_tabix) = sy-tabix.

        LOOP AT gt_proc_mistura ASSIGNING FIELD-SYMBOL(<fs_proc_mistura>) FROM lv_tabix WHERE aufnr = <fs_processa_mistura_aufnr>-aufnr. "#EC CI_NESTED

          <fs_proc_mistura>-status =  <fs_processa_mistura_aufnr>-status.
          <fs_proc_mistura>-msg    =  <fs_processa_mistura_aufnr>-msg.

        ENDLOOP.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD salvar_dados.

    CHECK gt_proc_mistura[] IS NOT INITIAL.

    MODIFY ztmm_proc_mistur FROM TABLE gt_proc_mistura.

  ENDMETHOD.


  METHOD valida_estorno.

    rv_valida = abap_false.
    IF it_xlips IS NOT INITIAL.
      SELECT mandt,vbeln,posnr,status,
             prtnr,mblnr,mjahr,uname,
             matnr,werks,lgort,lfimg,
             aufnr,data,msg,verid
        FROM ztmm_proc_mistur
        INTO TABLE @gt_proc_mistura
        FOR ALL ENTRIES IN @it_xlips
        WHERE vbeln = @it_xlips-vgbel
          AND posnr = @it_xlips-vgpos
          AND ( status = '1' OR
                status = '3' ).
      IF sy-subrc = 0.
        rv_valida = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD limpa_variaveis.

    CLEAR gt_proc_mistura.

  ENDMETHOD.


  METHOD proc_repmanconf1_cancel.
    DATA: lv_cancconfirmation TYPE  bapi_rm_datkey-confirmation,
          ls_return           TYPE  bapiret2.

    CHECK gt_proc_mistura[] IS NOT INITIAL.

    SORT gt_proc_mistura BY status.

    READ TABLE gt_proc_mistura WITH KEY status = '4' TRANSPORTING NO FIELDS BINARY SEARCH.

    CHECK sy-subrc IS NOT INITIAL.

    LOOP AT gt_proc_mistura ASSIGNING FIELD-SYMBOL(<fs_processa_mistura>).
      CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
        EXPORTING
          confirmation     = <fs_processa_mistura>-prtnr
          postdate         = sy-datum
        IMPORTING
          cancconfirmation = lv_cancconfirmation
          return           = ls_return.

      IF lv_cancconfirmation IS INITIAL.

        <fs_processa_mistura>-status = '4'.

        IF ls_return-message IS NOT INITIAL.

          <fs_processa_mistura>-msg = ls_return-message.

        ELSEIF ls_return-number <> 0.

          MESSAGE ID ls_return-id
                 TYPE ls_return-type
               NUMBER ls_return-number
                 INTO <fs_processa_mistura>-msg
                 WITH ls_return-message_v1
                      ls_return-message_v2
                      ls_return-message_v3
                      ls_return-message_v4.

        ELSE.

          <fs_processa_mistura>-msg = TEXT-t04. "'Não foi possível estornar a mistura realizada.'.

        ENDIF.
      ELSE.
        <fs_processa_mistura>-status = '2'.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD delete_dados_erro.
    DATA(lt_proc_mistura) = gt_proc_mistura[].

    DELETE lt_proc_mistura WHERE status = '1'.

    DELETE gt_proc_mistura WHERE status = '3'.

    IF lt_proc_mistura[] IS NOT INITIAL.
      DELETE ztmm_proc_mistur FROM TABLE lt_proc_mistura.
    ENDIF.

  ENDMETHOD.


  METHOD task_finish_single.

    RECEIVE RESULTS FROM FUNCTION 'ZFMMM_SETTLEMENT_SINGLE'
       CHANGING
          cs_proc_mistura = gs_proc_mistura.

    gv_result = abap_true .

  ENDMETHOD.


  METHOD task_finish_group.

    RECEIVE RESULTS FROM FUNCTION 'ZFMMM_SETTLEMENT_GROUP'
       CHANGING
          cs_proc_mistura = gs_proc_mistura.

    gv_result = abap_true .

  ENDMETHOD.
ENDCLASS.
