CLASS zclmm_replica_material DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_t001k TYPE STANDARD TABLE OF t001k WITH DEFAULT KEY .
    TYPES:
      ty_log_mat TYPE STANDARD TABLE OF ztmm_log_mat WITH DEFAULT KEY .

    CONSTANTS lc_modulo_mm TYPE ztca_param_mod-modulo VALUE 'MM' ##NO_TEXT.
    CONSTANTS lc_chave1_1 TYPE ztca_param_par-chave1 VALUE 'REPLICACAO_MATERIAL' ##NO_TEXT.
    CONSTANTS lc_chave2_1 TYPE ztca_param_par-chave2 VALUE 'PARAMETROS_CONVERSAO' ##NO_TEXT.
    CONSTANTS lc_chave3_1 TYPE ztca_param_par-chave3 VALUE 'MANDT_ORIGEM_DESTINO' ##NO_TEXT.
    CONSTANTS lc_chave1_2 TYPE ztca_param_par-chave1 VALUE 'REPLICACAO_MATERIAL' ##NO_TEXT.
    CONSTANTS lc_chave2_2 TYPE ztca_param_par-chave2 VALUE 'PARAMETROS_CONVERSAO' ##NO_TEXT.
    CONSTANTS lc_chave3_2 TYPE ztca_param_par-chave3 VALUE 'MATKL_ORIGEM_DESTINO' ##NO_TEXT.
    CONSTANTS lc_chave1_3 TYPE ztca_param_par-chave3 VALUE 'REPLICACAO_MATERIAL' ##NO_TEXT.
    CONSTANTS lc_chave2_3 TYPE ztca_param_par-chave3 VALUE 'PARAMETROS_FIXOS' ##NO_TEXT.
    CONSTANTS lc_chave3_3 TYPE ztca_param_par-chave3 VALUE 'BUKRS' ##NO_TEXT.
    CONSTANTS lc_chave1_4 TYPE ztca_param_par-chave3 VALUE 'REPLICACAO_MATERIAL' ##NO_TEXT.
    CONSTANTS lc_chave2_4 TYPE ztca_param_par-chave3 VALUE 'PARAMETROS_EXECUCAO' ##NO_TEXT.
    CONSTANTS lc_chave3_4 TYPE ztca_param_par-chave3 VALUE 'INTERVALO_TEMPO' ##NO_TEXT.
    CONSTANTS lc_chave1_5 TYPE ztca_param_par-chave3 VALUE 'REPLICACAO_MATERIAL' ##NO_TEXT.
    CONSTANTS lc_chave2_5 TYPE ztca_param_par-chave3 VALUE 'PARAMETROS_CONVERSAO' ##NO_TEXT.
    CONSTANTS lc_chave3_5 TYPE ztca_param_par-chave3 VALUE 'PRCTR_ORIGEM_DESTINO' ##NO_TEXT.
    CONSTANTS lc_chave1_6 TYPE ztca_param_par-chave3 VALUE 'REPLICACAO_MATERIAL' ##NO_TEXT.
    CONSTANTS lc_chave2_6 TYPE ztca_param_par-chave3 VALUE 'PARAMETROS_FIXOS' ##NO_TEXT.
    CONSTANTS lc_chave3_6 TYPE ztca_param_par-chave3 VALUE 'TIPO_MATERIAL' ##NO_TEXT.
    CONSTANTS lc_chave1_7 TYPE ztca_param_par-chave3 VALUE 'REPLICACAO_MATERIAL' ##NO_TEXT.
    CONSTANTS lc_chave2_7 TYPE ztca_param_par-chave3 VALUE 'PARAMETROS_FIXOS' ##NO_TEXT.
    CONSTANTS lc_chave3_7 TYPE ztca_param_par-chave3 VALUE 'GRUPO_COMPRADORES' ##NO_TEXT.
    CONSTANTS lc_chave1_8 TYPE ztca_param_par-chave3 VALUE 'REPLICACAO_MATERIAL' ##NO_TEXT.
    CONSTANTS lc_chave2_8 TYPE ztca_param_par-chave3 VALUE 'PARAMETROS_CONVERSAO' ##NO_TEXT.
    CONSTANTS lc_chave3_8 TYPE ztca_param_par-chave3 VALUE 'SPART_ORIGEM_DESTINO' ##NO_TEXT.
    CONSTANTS lc_chave1_9 TYPE ztca_param_par-chave3 VALUE 'REPLICACAO_MATERIAL' ##NO_TEXT.
    CONSTANTS lc_chave2_9 TYPE ztca_param_par-chave3 VALUE 'PARAMETROS_FIXOS' ##NO_TEXT.
    CONSTANTS lc_chave3_9 TYPE ztca_param_par-chave3 VALUE 'BUKRS_MTART' ##NO_TEXT.
    CONSTANTS lc_chave1_10 TYPE ztca_param_par-chave3 VALUE 'REPLICACAO_MATERIAL' ##NO_TEXT.
    CONSTANTS lc_chave2_10 TYPE ztca_param_par-chave3 VALUE 'PARAMETROS_FIXOS' ##NO_TEXT.
    CONSTANTS lc_chave3_10 TYPE ztca_param_par-chave3 VALUE 'VISAO_DEPOSITOS' ##NO_TEXT.
    DATA:
      gr_bukrs_mtart TYPE RANGE OF ZE_BUKRS_MTART .
    DATA:
      gr_mandt TYPE RANGE OF mandt .
    DATA:
      gr_matkl TYPE RANGE OF matkl .
    DATA:
      gr_prctr TYPE RANGE OF prctr .
    DATA:
      gr_mtart TYPE RANGE OF mtart .
    DATA:
      gr_ekgrp TYPE RANGE OF ekgrp .
    DATA:
      gr_spart TYPE RANGE OF spart .
    DATA:
      gt_mara TYPE STANDARD TABLE OF mara .
    DATA:
      gt_marc TYPE STANDARD TABLE OF marc .
    DATA:
      gt_mard TYPE STANDARD TABLE OF mard .
    DATA:
      gt_mvke TYPE STANDARD TABLE OF mvke .
    DATA:
      gt_mbew TYPE STANDARD TABLE OF mbew .
    DATA:
      gt_makt TYPE STANDARD TABLE OF makt .
    DATA:
      gt_mlan TYPE STANDARD TABLE OF mlan .
    DATA:
      gt_marm TYPE STANDARD TABLE OF marm .
    DATA:
    gt_tvko TYPE STANDARD TABLE OF tvko .
    DATA gs_headdata TYPE bapimathead .
    DATA gs_clientdata TYPE bapi_mara .
    DATA gs_clientdatax TYPE bapi_marax .
    DATA gs_plantdata TYPE bapi_marc .
    DATA gs_plantdatax TYPE bapi_marcx .
    DATA gs_storagelocationdata TYPE bapi_mard .
    DATA gs_storagelocationdatax TYPE bapi_mardx .
    DATA gs_valuationdata TYPE bapi_mbew .
    DATA gs_valuationdatax TYPE bapi_mbewx .
    DATA gs_salesdata TYPE bapi_mvke .
    DATA gs_salesdatax TYPE bapi_mvkex .
    DATA:
      gt_plantdata TYPE TABLE OF bapi_marc .
    DATA:
      gt_plantdatax TYPE TABLE OF bapi_marcx .
    DATA:
      gt_storagelocationdata TYPE TABLE OF bapi_mard .
    DATA:
      gt_storagelocationdatax TYPE TABLE OF bapi_mardx .
    DATA:
      gt_valuationdata TYPE TABLE OF bapi_mbew .
    DATA:
      gt_valuationdatax TYPE TABLE OF bapi_mbewx .
    DATA:
      gt_salesdata TYPE TABLE OF bapi_mvke .
    DATA:
      gt_salesdatax TYPE TABLE OF bapi_mvkex .
    DATA:
      gt_materialdescription  TYPE TABLE OF bapi_makt .
    DATA:
      gt_unitsofmeasure       TYPE TABLE OF bapi_marm .
    DATA:
      gt_unitsofmeasurex      TYPE TABLE OF bapi_marmx .
    DATA:
      gt_taxclassifications   TYPE TABLE OF bapi_mlan .
    DATA:
      gr_bukrs TYPE RANGE OF bukrs .
    DATA:
      gt_t001k TYPE TABLE OF t001k .
    DATA:
      gt_t001l TYPE TABLE OF t001l .
    DATA gv_time TYPE integer .
    DATA:
      gt_tvta TYPE TABLE OF tvta .
    DATA gv_mkve TYPE char1 .
    DATA gv_visao_dep TYPE abap_boolean .

    METHODS criar_dados
      IMPORTING
        !ir_material TYPE tbl_mat_range OPTIONAL
        !iv_manual   TYPE abap_bool
      EXPORTING
        !et_return   TYPE ty_log_mat .
    METHODS exibir_log
      CHANGING
        !ct_return TYPE ty_log_mat .
protected section.
private section.

  data GS_RETURN type BAPIRET2 .

  methods BUSCAR_DADOS
    importing
      !IV_DATA type TIMESTAMP optional
      !IR_MATERIAL type TBL_MAT_RANGE optional
      !IV_MANUAL type ABAP_BOOL optional .
  methods CONSTRUIR_DADOS
    importing
      !IS_MARA type MARA
      !IS_MARC type MARC
      !IS_MBEW type MBEW
      !IS_MVKE type MVKE .
  methods GRAVAR_LOG
    importing
      !IS_SALESDATA type BAPI_MVKE
      !IS_STORAGELOCATIONDATA type BAPI_MARD optional
      !IS_PLANTDATA type BAPI_MARC
    returning
      value(RS_RETURN) type ZTMM_LOG_MAT .
  methods OBTER_CENTROS .
  methods OBTER_DEPOSITOS .
  methods OBTER_AREA_DE_VENDAS .
  methods OBTER_TEMPO
    importing
      value(IV_DATA) type TIMESTAMP
    returning
      value(RV_DATA) type TIMESTAMP .
  methods LIMPAR_ATRIBUTOS .
  methods OBTER_CONSTANTES .
  methods OBTER_GRUPO_VENDAS .
ENDCLASS.



CLASS ZCLMM_REPLICA_MATERIAL IMPLEMENTATION.


  METHOD buscar_dados.

    DATA : lv_data TYPE d,
           lv_hora TYPE t.

    IF gr_mandt[] IS NOT INITIAL.
      DATA(lv_mandt) = VALUE #( gr_mandt[ high = sy-mandt ]-low ).
    ENDIF.

    DATA(lt_data) = obter_tempo( iv_data ).

    CONVERT TIME STAMP lt_data TIME ZONE sy-zonlo
    INTO DATE lv_data TIME lv_hora.

    DATA(lv_datum) = sy-datum.
    DATA(lv_uzeit) = sy-uzeit.

    IF iv_manual IS INITIAL.

      IF lv_data EQ lv_datum.

        SELECT *
          INTO TABLE @gt_mara
          FROM mara
          USING CLIENT @lv_mandt
          WHERE ersda >= @lv_data
*          AND   created_at_time >= @lv_hora
          AND mtart IN @gr_mtart
          AND matkl IN @gr_matkl
          .
        IF sy-subrc IS INITIAL.
          SORT gt_mara BY matnr.
        ENDIF.
        SELECT *
          APPENDING TABLE @gt_mara
          FROM mara
          USING CLIENT @lv_mandt
          WHERE laeda >= @lv_data
*          AND   last_changed_time >= @lv_hora
          AND mtart IN @gr_mtart
          AND matkl IN @gr_matkl
          .
        IF sy-subrc IS INITIAL.
          SORT gt_mara BY matnr.
        ENDIF.
      ENDIF.

      IF lv_data < lv_datum.
        SELECT *
          INTO TABLE @gt_mara
          FROM mara
          USING CLIENT @lv_mandt
          WHERE ( ersda EQ @lv_data  AND created_at_time >= @lv_hora )
          AND mtart IN @gr_mtart
          AND matkl IN @gr_matkl
          .
        IF sy-subrc IS INITIAL.
          SORT gt_mara BY matnr.
        ENDIF.
        SELECT *
          APPENDING TABLE @gt_mara
          FROM mara
          USING CLIENT @lv_mandt
          WHERE ersda EQ @lv_datum
          AND mtart IN @gr_mtart
          AND matkl IN @gr_matkl
          .
        IF sy-subrc IS INITIAL.
          SORT gt_mara BY matnr.
        ENDIF.
        SELECT *
          APPENDING TABLE @gt_mara
          FROM mara
          USING CLIENT @lv_mandt
          WHERE ( laeda EQ @lv_data  AND last_changed_time >= @lv_hora )
          AND mtart IN @gr_mtart
          AND matkl IN @gr_matkl
          .
        IF sy-subrc IS INITIAL.
          SORT gt_mara BY matnr.
        ENDIF.

        SELECT *
          APPENDING TABLE @gt_mara
          FROM mara
          USING CLIENT @lv_mandt
          WHERE laeda EQ @lv_datum
          AND mtart IN @gr_mtart
          AND matkl IN @gr_matkl
          .
        IF sy-subrc IS INITIAL.
          SORT gt_mara BY matnr.
        ENDIF.
      ENDIF.

    ELSE.

      SELECT *
         INTO TABLE @gt_mara
         FROM mara
         USING CLIENT @lv_mandt
         WHERE matnr IN @ir_material
         AND mtart IN @gr_mtart
         AND matkl IN @gr_matkl
         .
      IF sy-subrc IS INITIAL.
        SORT gt_mara BY matnr.
      ENDIF.

    ENDIF.

    IF gt_mara[] IS NOT INITIAL.

      SORT gt_mara BY matnr.
      DELETE ADJACENT DUPLICATES FROM gt_mara COMPARING matnr.
      IF gt_mara[] IS NOT INITIAL.

        SELECT *
          INTO TABLE @gt_marc
          FROM marc
              USING CLIENT @lv_mandt
          FOR ALL ENTRIES IN @gt_mara
           WHERE matnr EQ @gt_mara-matnr
          .
        IF sy-subrc IS INITIAL.

          SORT gt_marc BY matnr werks.
          DELETE ADJACENT DUPLICATES FROM gt_marc COMPARING matnr werks.

          IF gt_marc[] IS NOT INITIAL.

            SELECT *
              INTO TABLE @gt_mard
              FROM mard
              USING CLIENT @lv_mandt
              FOR ALL ENTRIES IN @gt_marc
              WHERE matnr EQ @gt_marc-matnr
              AND   werks EQ @gt_marc-werks
              .
            IF sy-subrc IS INITIAL.
              SORT gt_mard BY matnr.
              DELETE ADJACENT DUPLICATES FROM gt_mard COMPARING matnr.
            ENDIF.
            .
            SELECT *
             INTO TABLE @gt_mbew
             FROM mbew
             USING CLIENT @lv_mandt
             FOR ALL ENTRIES IN @gt_marc
             WHERE matnr EQ @gt_marc-matnr
             AND   bwkey EQ @gt_marc-werks
             .
            IF sy-subrc IS INITIAL.
              SORT gt_mbew BY matnr.
              DELETE ADJACENT DUPLICATES FROM gt_mbew COMPARING matnr.
            ENDIF.
          ENDIF.
        ENDIF.

        SELECT *
          INTO TABLE @gt_mvke
          FROM mvke
              USING CLIENT @lv_mandt
          FOR ALL ENTRIES IN @gt_mara
        WHERE matnr EQ @gt_mara-matnr
        .
        IF sy-subrc IS INITIAL.
          SORT gt_mvke BY matnr.
          DELETE ADJACENT DUPLICATES FROM gt_mvke COMPARING matnr.
        ENDIF.
        SELECT *
          INTO TABLE @gt_makt
          FROM makt
          USING CLIENT @lv_mandt
          FOR ALL ENTRIES IN @gt_mara
           WHERE matnr EQ @gt_mara-matnr
        .
        IF sy-subrc IS INITIAL.
          SORT gt_makt BY matnr.
        ENDIF.
        SELECT *
          INTO TABLE @gt_mlan
          FROM mlan
          USING CLIENT @lv_mandt
          FOR ALL ENTRIES IN @gt_mara
           WHERE matnr EQ @gt_mara-matnr
          .
        IF sy-subrc IS INITIAL.
          SORT gt_mlan BY matnr.
        ENDIF.
        SELECT *
          INTO TABLE @gt_marm
          FROM marm
          USING CLIENT @lv_mandt
          FOR ALL ENTRIES IN @gt_mara
           WHERE matnr EQ @gt_mara-matnr
          .
        IF sy-subrc IS INITIAL.
          SORT gt_marm BY matnr.
        ENDIF.
      ENDIF.
    ENDIF.

    obter_centros( ).
    obter_depositos( ).
    obter_area_de_vendas( ).
    obter_grupo_vendas( ).

  ENDMETHOD.


  METHOD construir_dados.

    IF gr_prctr[] IS NOT INITIAL.
      TRY.
          DATA(lv_prctr) = VALUE #( gr_prctr[ low = is_marc-prctr ]-high ).
        CATCH cx_sy_itab_line_not_found.
          lv_prctr = is_marc-prctr.
      ENDTRY.
    ENDIF.

    IF gr_matkl[] IS NOT INITIAL.
      TRY.
          DATA(lv_matkl) = VALUE #( gr_matkl[ low = is_mara-matkl ]-high ).
        CATCH cx_sy_itab_line_not_found.
          lv_matkl = is_mara-matkl.
      ENDTRY.
    ENDIF.

    IF gr_ekgrp[] IS NOT INITIAL.
      TRY.
          DATA(lv_ekgrp) = VALUE #( gr_ekgrp[ low = is_marc-ekgrp ]-high ).
        CATCH cx_sy_itab_line_not_found.
          lv_ekgrp = is_marc-ekgrp.
      ENDTRY.
    ENDIF.

    IF gr_spart[] IS NOT INITIAL.
      TRY.
          DATA(lv_spart) = VALUE #( gr_spart[ low = is_mara-spart ]-high ).
        CATCH cx_sy_itab_line_not_found.
          lv_spart = is_mara-spart.
      ENDTRY.
    ENDIF.

*** HEADDATA ***
    gs_headdata-material   = is_mara-matnr.
    gs_headdata-ind_sector = 'O'.
    gs_headdata-matl_type  = is_mara-mtart.
    gs_headdata-basic_view = abap_true.

    IF is_marc-pstat CS 'V'.
      gs_headdata-sales_view      = abap_true.
    ENDIF.
    IF is_marc-pstat CS 'E'.
      gs_headdata-purchase_view   = abap_true.
    ENDIF.
    IF is_marc-pstat CS 'D'.
      gs_headdata-mrp_view        = abap_true.
    ENDIF.
*    IF is_marc-pstat CS 'P'.
*      gs_headdata-forecast_view   = abap_true.
*    ENDIF.
    IF is_marc-pstat CS 'A'.
      gs_headdata-work_sched_view = abap_true.
    ENDIF.
    IF is_marc-pstat CS 'F'.
      gs_headdata-prt_view        = abap_true.
    ENDIF.
    IF is_marc-pstat CS 'L'.
      gs_headdata-storage_view    = abap_true.
    ENDIF.
    IF is_marc-pstat CS 'S'.
      gs_headdata-warehouse_view  = abap_true.
    ENDIF.
    IF is_marc-pstat CS 'Q'.
      gs_headdata-quality_view    = abap_true.
    ENDIF.
    IF is_marc-pstat CS 'B'.
      gs_headdata-account_view    = abap_true.
    ENDIF.
    IF is_marc-pstat CS 'G'.
      gs_headdata-cost_view       = abap_true.
    ENDIF.

*** CLIENTDATA ***
*    gs_clientdata-matnr       = is_mara-matnr.
    gs_clientdata-matl_group  = lv_matkl.
    gs_clientdata-old_mat_no  = is_mara-bismt.
    gs_clientdata-base_uom    = is_mara-meins.
    gs_clientdata-po_unit     = is_mara-bstme.
    gs_clientdata-net_weight  = is_mara-ntgew.
    gs_clientdata-unit_of_wt  = is_mara-gewei.
    gs_clientdata-trans_grp   = is_mara-tragr.
*    gs_clientdata-division    = is_mara-spart.
    gs_clientdata-division    = lv_spart.
    gs_clientdata-item_cat    = is_mara-mtpos_mara.

*** CLIENTDATAX ***
*    gs_clientdatax-matnr      = abap_true.
    gs_clientdatax-matl_group = abap_true.
    gs_clientdatax-old_mat_no = abap_true.
    gs_clientdatax-base_uom   = abap_true.
    gs_clientdatax-po_unit    = abap_true.
    gs_clientdatax-net_weight = abap_true.
    gs_clientdatax-unit_of_wt = abap_true.
    gs_clientdatax-trans_grp  = abap_true.
    gs_clientdatax-division   = abap_true.
    gs_clientdatax-item_cat   = abap_true.

*** PLANTDATA ***
*    gs_plantdata-matnr                          = is_mara-matnr.
    gt_plantdata = VALUE #( FOR ls_t001k IN gt_t001k  (
                                 abc_id                         = is_marc-maabc
                                 plant                          = ls_t001k-bwkey
                                 crit_part                      = is_marc-kzkri
                                 pur_group                      = lv_ekgrp
                                 issue_unit                     = is_marc-ausme
                                 mrpprofile                     = is_marc-dispr
                                 mrp_type                       = is_marc-dismm
                                 mrp_ctrler                     = is_marc-dispo
                                 plnd_delry                     = is_marc-plifz
                                 gr_pr_time                     = is_marc-webaz
                                 lotsizekey                     = is_marc-disls
                                 period_ind                     = is_marc-perkz
                                 proc_type                      = is_marc-beskz
                                 spproctype                     = is_marc-sobsl
                                 reorder_pt                     = is_marc-minbe
                                 safety_stk                     = is_marc-eisbe
                                 minlotsize                     = is_marc-bstmi
                                 maxlotsize                     = is_marc-bstma
                                 fixed_lot                      = is_marc-bstfe
                                 round_val                      = is_marc-bstrf
                                 max_stock                      = is_marc-mabst
                                 grp_reqmts                     = is_marc-kzbed
                                 mixed_mrp                      = is_marc-miskz
                                 production_scheduler           = is_marc-fevor
                                 proc_time                      = is_marc-bearz
                                 setuptime                      = is_marc-ruezt
                                 base_qty                       = is_marc-basmg
                                 inhseprodt                     = is_marc-dzeit
                                 stgeperiod                     = is_marc-maxlz
                                 stge_pd_un                     = is_marc-lzeih
                                 over_tol                       = is_marc-ueeto
                                 unlimited                      = is_marc-ueetk
                                 under_tol                      = is_marc-uneto
                                 replentime                     = is_marc-wzeit
                                 replace_pt                     = is_marc-atpkz
                                 ind_post_to_insp_stock         = is_marc-insmk
                                 ctrl_key                       = is_marc-ssqss
                                 doc_reqd                       = is_marc-kzdkz
                                 loadinggrp                     = is_marc-ladgr
                                 availcheck                     = is_marc-mtvfp
                                 batch_mgmt                     = is_marc-xchpf
                                 ship_proc_time                 = is_marc-vbeaz
                                 sup_source                     = is_marc-bwscl
                                 auto_p_ord                     = is_marc-kautb
                                 sourcelist                     = is_marc-kordb
                                 comm_code                      = is_marc-stawn
                                 countryori                     = is_marc-herkl
                                 regionorig                     = is_marc-herkr
                                 comm_co_un                     = is_marc-expme
                                 profit_ctr                     = lv_prctr
                                 ppc_pl_cal                     = is_marc-mrppp
                                 rep_manuf                      = is_marc-sauft
                                 pl_ti_fnce                     = is_marc-fxhor
                                 consummode                     = is_marc-vrmod
                                 alternative_bom                = is_marc-stlal
                                 bom_usage                      = is_marc-stlan
                                 planlistgrp                    = is_marc-plnnr
                                 planlistcnt                    = is_marc-aplal
                                 lot_size                       = is_marc-losgr
                                 specprocty                     = is_marc-sobsk
                                 prod_unit                      = is_marc-frtme
                                 iss_st_loc                     = is_marc-lgpro
                                 mrp_group                      = is_marc-disgr
                                 variance_key                   = is_marc-awsls
                                 serno_prof                     = is_marc-sernp
                                 neg_stocks                     = is_marc-xmcng
                                 plng_cycle                     = is_marc-lfrhy
                                 round_prof                     = is_marc-rdprf
                                 plan_strgp                     = is_marc-strgr
*                                 sloc_exprc                     = is_marc-lgfsb
                                 sloc_exprc                     = abap_false
                                 determ_grp                     = is_marc-eprio
                                 pur_status                     = is_marc-mmsta
                                 mat_cfop                       = is_marc-indus
                                 ctrl_code                      = is_marc-steuc
                                 ) ).

*** PLANTDATAX ***
    gt_plantdatax = VALUE #( FOR ls_plantdata IN gt_plantdata  (
*    gs_plantdatax-matnr                  = abap_true.
                                 plant                  = COND #( WHEN ls_plantdata-plant IS NOT INITIAL THEN ls_plantdata-plant )
                                 abc_id                 = COND #( WHEN ls_plantdata-abc_id IS NOT INITIAL THEN abap_true )
                                 crit_part              = COND #( WHEN ls_plantdata-crit_part IS NOT INITIAL THEN abap_true )
                                 pur_group              = COND #( WHEN ls_plantdata-pur_group IS NOT INITIAL THEN abap_true )
                                 issue_unit             = COND #( WHEN ls_plantdata-issue_unit IS NOT INITIAL THEN abap_true )
                                 mrpprofile             = COND #( WHEN ls_plantdata-mrpprofile IS NOT INITIAL THEN abap_true )
                                 mrp_type               = COND #( WHEN ls_plantdata-mrp_type IS NOT INITIAL THEN abap_true )
                                 mrp_ctrler             = COND #( WHEN ls_plantdata-mrp_ctrler IS NOT INITIAL THEN abap_true )
*                                 plnd_delry             = COND #( WHEN ls_plantdata-plnd_delry IS NOT INITIAL THEN abap_true )
                                 plnd_delry             = abap_true
                                 gr_pr_time             = COND #( WHEN ls_plantdata-gr_pr_time IS NOT INITIAL THEN abap_true )
                                 lotsizekey             = COND #( WHEN ls_plantdata-lotsizekey IS NOT INITIAL THEN abap_true )
                                 period_ind             = COND #( WHEN ls_plantdata-period_ind IS NOT INITIAL THEN abap_true )
                                 proc_type              = COND #( WHEN ls_plantdata-proc_type IS NOT INITIAL THEN abap_true )
                                 spproctype             = COND #( WHEN ls_plantdata-spproctype IS NOT INITIAL THEN abap_true )
                                 reorder_pt             = COND #( WHEN ls_plantdata-reorder_pt IS NOT INITIAL THEN abap_true )
                                 safety_stk             = COND #( WHEN ls_plantdata-safety_stk IS NOT INITIAL THEN abap_true )
                                 minlotsize             = COND #( WHEN ls_plantdata-minlotsize IS NOT INITIAL THEN abap_true )
                                 maxlotsize             = COND #( WHEN ls_plantdata-maxlotsize IS NOT INITIAL THEN abap_true )
                                 fixed_lot              = COND #( WHEN ls_plantdata-fixed_lot IS NOT INITIAL THEN abap_true )
                                 round_val              = COND #( WHEN ls_plantdata-round_val IS NOT INITIAL THEN abap_true )
                                 max_stock              = COND #( WHEN ls_plantdata-max_stock IS NOT INITIAL THEN abap_true )
                                 grp_reqmts             = COND #( WHEN ls_plantdata-grp_reqmts IS NOT INITIAL THEN abap_true )
                                 mixed_mrp              = COND #( WHEN ls_plantdata-mixed_mrp IS NOT INITIAL THEN abap_true )
                                 production_scheduler   = COND #( WHEN ls_plantdata-production_scheduler IS NOT INITIAL THEN abap_true )
                                 proc_time              = COND #( WHEN ls_plantdata-proc_time IS NOT INITIAL THEN abap_true )
                                 setuptime              = COND #( WHEN ls_plantdata-setuptime IS NOT INITIAL THEN abap_true )
                                 base_qty               = COND #( WHEN ls_plantdata-base_qty IS NOT INITIAL THEN abap_true )
                                 inhseprodt             = COND #( WHEN ls_plantdata-inhseprodt IS NOT INITIAL THEN abap_true )
                                 stgeperiod             = COND #( WHEN ls_plantdata-stgeperiod IS NOT INITIAL THEN abap_true )
                                 stge_pd_un             = COND #( WHEN ls_plantdata-stge_pd_un IS NOT INITIAL THEN abap_true )
                                 over_tol               = COND #( WHEN ls_plantdata-over_tol IS NOT INITIAL THEN abap_true )
                                 unlimited              = COND #( WHEN ls_plantdata-unlimited IS NOT INITIAL THEN abap_true )
                                 under_tol              = COND #( WHEN ls_plantdata-under_tol IS NOT INITIAL THEN abap_true )
                                 replentime             = COND #( WHEN ls_plantdata-replentime IS NOT INITIAL THEN abap_true )
                                 replace_pt             = COND #( WHEN ls_plantdata-replace_pt IS NOT INITIAL THEN abap_true )
                                 ind_post_to_insp_stock = COND #( WHEN ls_plantdata-ind_post_to_insp_stock IS NOT INITIAL THEN abap_true )
                                 ctrl_key               = COND #( WHEN ls_plantdata-ctrl_key IS NOT INITIAL THEN abap_true )
                                 doc_reqd               = COND #( WHEN ls_plantdata-doc_reqd IS NOT INITIAL THEN abap_true )
                                 loadinggrp             = COND #( WHEN ls_plantdata-loadinggrp IS NOT INITIAL THEN abap_true )
                                 availcheck             = COND #( WHEN ls_plantdata-availcheck IS NOT INITIAL THEN abap_true )
                                 batch_mgmt             = COND #( WHEN ls_plantdata-batch_mgmt IS NOT INITIAL THEN abap_true )
                                 ship_proc_time         = COND #( WHEN ls_plantdata-ship_proc_time IS NOT INITIAL THEN abap_true )
                                 sup_source             = COND #( WHEN ls_plantdata-sup_source IS NOT INITIAL THEN abap_true )
                                 auto_p_ord             = COND #( WHEN ls_plantdata-auto_p_ord IS NOT INITIAL THEN abap_true )
                                 sourcelist             = COND #( WHEN ls_plantdata-sourcelist IS NOT INITIAL THEN abap_true )
                                 comm_code              = COND #( WHEN ls_plantdata-comm_code IS NOT INITIAL THEN abap_true )
                                 countryori             = COND #( WHEN ls_plantdata-countryori IS NOT INITIAL THEN abap_true )
                                 regionorig             = COND #( WHEN ls_plantdata-regionorig IS NOT INITIAL THEN abap_true )
                                 comm_co_un             = COND #( WHEN ls_plantdata-comm_co_un IS NOT INITIAL THEN abap_true )
                                 profit_ctr             = COND #( WHEN ls_plantdata-profit_ctr IS NOT INITIAL THEN abap_true )
                                 ppc_pl_cal             = COND #( WHEN ls_plantdata-ppc_pl_cal IS NOT INITIAL THEN abap_true )
                                 rep_manuf              = COND #( WHEN ls_plantdata-abc_id IS NOT INITIAL THEN abap_true )
                                 pl_ti_fnce             = COND #( WHEN ls_plantdata-rep_manuf IS NOT INITIAL THEN abap_true )
                                 consummode             = COND #( WHEN ls_plantdata-consummode IS NOT INITIAL THEN abap_true )
                                 alternative_bom        = COND #( WHEN ls_plantdata-alternative_bom IS NOT INITIAL THEN abap_true )
                                 bom_usage              = COND #( WHEN ls_plantdata-bom_usage IS NOT INITIAL THEN abap_true )
                                 planlistgrp            = COND #( WHEN ls_plantdata-planlistgrp IS NOT INITIAL THEN abap_true )
                                 planlistcnt            = COND #( WHEN ls_plantdata-planlistcnt IS NOT INITIAL THEN abap_true )
                                 lot_size               = COND #( WHEN ls_plantdata-lot_size IS NOT INITIAL THEN abap_true )
                                 specprocty             = COND #( WHEN ls_plantdata-specprocty IS NOT INITIAL THEN abap_true )
                                 prod_unit              = COND #( WHEN ls_plantdata-prod_unit IS NOT INITIAL THEN abap_true )
                                 iss_st_loc             = COND #( WHEN ls_plantdata-iss_st_loc IS NOT INITIAL THEN abap_true )
                                 mrp_group              = COND #( WHEN ls_plantdata-mrp_group IS NOT INITIAL THEN abap_true )
                                 variance_key           = COND #( WHEN ls_plantdata-variance_key IS NOT INITIAL THEN abap_true )
                                 serno_prof             = COND #( WHEN ls_plantdata-serno_prof IS NOT INITIAL THEN abap_true )
                                 neg_stocks             = COND #( WHEN ls_plantdata-neg_stocks IS NOT INITIAL THEN abap_true )
                                 plng_cycle             = COND #( WHEN ls_plantdata-plng_cycle IS NOT INITIAL THEN abap_true )
                                 round_prof             = COND #( WHEN ls_plantdata-round_prof IS NOT INITIAL THEN abap_true )
                                 plan_strgp             = COND #( WHEN ls_plantdata-plan_strgp IS NOT INITIAL THEN abap_true )
                                 sloc_exprc             = COND #( WHEN ls_plantdata-sloc_exprc IS NOT INITIAL THEN abap_true )
                                 determ_grp             = COND #( WHEN ls_plantdata-determ_grp IS NOT INITIAL THEN abap_true )
                                 pur_status             = COND #( WHEN ls_plantdata-pur_status IS NOT INITIAL THEN abap_true )
                                 mat_cfop               = COND #( WHEN ls_plantdata-mat_cfop IS NOT INITIAL THEN abap_true )
                                 ctrl_code              = COND #( WHEN ls_plantdata-ctrl_code IS NOT INITIAL THEN abap_true )
                                 ) ).

    "Foi criado um parâmetro que HABILITA ou DESABILITA as ESTRUTURAS: STORAGELOCATIONDATA e STORAGELOCATIONDATAX
    "Parâmetro: MM - REPLICAÇÃO_MATERIAL - PARAMETROS_FIXOS – VISÃO_DEPÓSITOS
    " HABILITA = X / DESABILITA = ' '
    IF gv_visao_dep IS NOT INITIAL.
*** STORAGELOCATIONDATA ***
      gt_storagelocationdata = VALUE #( FOR ls_t001k5 IN gt_t001k
                                        FOR ls_t001l IN gt_t001l WHERE ( werks = ls_t001k5-bwkey ) (
*    gs_storagelocationdata-matnr = is_mara-matnr.
                                            plant    = ls_t001k5-bwkey
                                            stge_loc = ls_t001l-lgort
                                            ) ).

*** STORAGELOCATIONDATAX ***
      gt_storagelocationdatax = VALUE #( FOR ls_t001k6 IN gt_t001k
                                         FOR ls_t001l IN gt_t001l WHERE ( werks = ls_t001k6-bwkey ) (
*    gs_storagelocationdata-matnr    = abap_true.
                                             plant    = ls_t001k6-bwkey
                                             stge_loc = ls_t001l-lgort
                                             ) ).

    ENDIF.

*** VALUATIONDATA ***
    gt_valuationdata = VALUE #( FOR ls_t001k3 IN gt_t001k  (
*    gs_valuationdata-matnr       = is_mara-matnr.
                                    val_area    = ls_t001k3-bwkey
                                    val_type    = is_mbew-bwtar
                                    price_ctrl  = is_mbew-vprsv
                                    moving_pr   = is_mbew-verpr
                                    std_price   = is_mbew-stprs
                                    price_unit  = is_mbew-peinh
                                    val_class   = is_mbew-bklas
                                    ml_settle   = is_mbew-mlast
                                    matl_usage  = is_mbew-mtuse
                                    mat_origin  = is_mbew-mtorg
                                    in_house    = is_mbew-ownpr
                                    ) ).
*** VALUATIONDATAX ***
    gt_valuationdatax = VALUE #( FOR ls_valuationdata IN gt_valuationdata  (
*    gs_valuationdatax-matnr       = abap_true.
                                     val_area    = COND #( WHEN ls_valuationdata-val_area IS NOT INITIAL THEN ls_valuationdata-val_area )
                                     val_type    = COND #( WHEN ls_valuationdata-val_type IS NOT INITIAL THEN ls_valuationdata-val_type )
                                     price_ctrl  = COND #( WHEN ls_valuationdata-price_ctrl IS NOT INITIAL THEN abap_true )
                                     moving_pr   = COND #( WHEN ls_valuationdata-moving_pr IS NOT INITIAL THEN abap_true )
                                     std_price   = COND #( WHEN ls_valuationdata-std_price IS NOT INITIAL THEN abap_true )
                                     price_unit  = COND #( WHEN ls_valuationdata-price_unit IS NOT INITIAL THEN abap_true )
                                     val_class   = COND #( WHEN ls_valuationdata-val_class IS NOT INITIAL THEN abap_true )
                                     ml_settle   = COND #( WHEN ls_valuationdata-ml_settle IS NOT INITIAL THEN abap_true )
                                     matl_usage  = COND #( WHEN ls_valuationdata-matl_usage IS NOT INITIAL THEN abap_true )
                                     mat_origin  = COND #( WHEN ls_valuationdata-mat_origin IS NOT INITIAL THEN abap_true )
                                     in_house    = COND #( WHEN ls_valuationdata-in_house IS NOT INITIAL THEN abap_true )
                                     ) ) .
*** SALESDATA ***
    IF gv_mkve IS NOT INITIAL.
      gt_salesdata = VALUE #( FOR ls_tvta IN gt_tvta  (
*    gs_salesdata-matnr = is_mara-matnr.
                                  sales_org    = ls_tvta-vkorg
                                  distr_chan   = ls_tvta-vtweg
                                  matl_stats   = is_mvke-versg
                                  rebate_grp   = is_mvke-bonus
                                  comm_group   = is_mvke-provg
                                  cash_disc    = is_mvke-sktof
                                  min_order    = is_mvke-aumng
                                  min_dely     = is_mvke-lfmng
                                  dely_unit    = is_mvke-scmng
                                  dely_uom     = is_mvke-schme
                                  sales_unit   = is_mvke-vrkme
                                  item_cat     = is_mvke-mtpos
                                  delyg_plnt   = is_mvke-dwerk
                                  prod_hier    = is_mvke-prodh
                                  acct_assgt   = is_mvke-ktgrm
                                  matl_grp_1   = is_mvke-mvgr1
                                  matl_grp_2   = is_mvke-mvgr2
                                  matl_grp_3   = is_mvke-mvgr3
                                  matl_grp_4   = is_mvke-mvgr4
                                  matl_grp_5   = is_mvke-mvgr5
                                  prod_att_1   = is_mvke-prat1
                                  prod_att_2   = is_mvke-prat2
                                  prod_att_3   = is_mvke-prat3
                                  prod_att_4   = is_mvke-prat4
                                  prod_att_5   = is_mvke-prat5
                                  prod_att_6   = is_mvke-prat6
                                  prod_att_7   = is_mvke-prat7
                                  prod_att_8   = is_mvke-prat8
                                  prod_att_9   = is_mvke-prat9
                                  prod_att10   = is_mvke-prata
                                  round_prof   = is_mvke-rdprf
                                  unit_group   = is_mvke-megru
                                  ) ).

*** SALESDATAX ***
      gt_salesdatax = VALUE #( FOR ls_salesdata IN gt_salesdata  (
*    gs_salesdatax-matnr        = abap_true.
                                  sales_org    = COND #( WHEN ls_salesdata-sales_org IS NOT INITIAL THEN ls_salesdata-sales_org )
                                  distr_chan   = COND #( WHEN ls_salesdata-distr_chan IS NOT INITIAL THEN ls_salesdata-distr_chan )
                                  matl_stats   = COND #( WHEN ls_salesdata-matl_stats IS NOT INITIAL THEN abap_true )
                                  rebate_grp   = COND #( WHEN ls_salesdata-rebate_grp IS NOT INITIAL THEN abap_true )
                                  comm_group   = COND #( WHEN ls_salesdata-comm_group IS NOT INITIAL THEN abap_true )
                                  cash_disc    = COND #( WHEN ls_salesdata-cash_disc IS NOT INITIAL THEN abap_true )
                                  min_order    = COND #( WHEN ls_salesdata-min_order IS NOT INITIAL THEN abap_true )
                                  min_dely     = COND #( WHEN ls_salesdata-min_dely IS NOT INITIAL THEN abap_true )
                                  dely_unit    = COND #( WHEN ls_salesdata-dely_unit IS NOT INITIAL THEN abap_true )
                                  dely_uom     = COND #( WHEN ls_salesdata-dely_uom IS NOT INITIAL THEN abap_true )
                                  sales_unit   = COND #( WHEN ls_salesdata-sales_unit IS NOT INITIAL THEN abap_true )
                                  item_cat     = COND #( WHEN ls_salesdata-item_cat IS NOT INITIAL THEN abap_true )
                                  delyg_plnt   = COND #( WHEN ls_salesdata-delyg_plnt IS NOT INITIAL THEN abap_true )
                                  prod_hier    = COND #( WHEN ls_salesdata-prod_hier IS NOT INITIAL THEN abap_true )
                                  acct_assgt   = COND #( WHEN ls_salesdata-acct_assgt IS NOT INITIAL THEN abap_true )
                                  matl_grp_1   = COND #( WHEN ls_salesdata-matl_grp_1 IS NOT INITIAL THEN abap_true )
                                  matl_grp_2   = COND #( WHEN ls_salesdata-matl_grp_2 IS NOT INITIAL THEN abap_true )
                                  matl_grp_3   = COND #( WHEN ls_salesdata-matl_grp_3 IS NOT INITIAL THEN abap_true )
                                  matl_grp_4   = COND #( WHEN ls_salesdata-matl_grp_4 IS NOT INITIAL THEN abap_true )
                                  matl_grp_5   = COND #( WHEN ls_salesdata-matl_grp_5 IS NOT INITIAL THEN abap_true )
                                  prod_att_1   = COND #( WHEN ls_salesdata-prod_att_1 IS NOT INITIAL THEN abap_true )
                                  prod_att_2   = COND #( WHEN ls_salesdata-prod_att_2 IS NOT INITIAL THEN abap_true )
                                  prod_att_3   = COND #( WHEN ls_salesdata-prod_att_3 IS NOT INITIAL THEN abap_true )
                                  prod_att_4   = COND #( WHEN ls_salesdata-prod_att_4 IS NOT INITIAL THEN abap_true )
                                  prod_att_5   = COND #( WHEN ls_salesdata-prod_att_5 IS NOT INITIAL THEN abap_true )
                                  prod_att_6   = COND #( WHEN ls_salesdata-prod_att_6 IS NOT INITIAL THEN abap_true )
                                  prod_att_7   = COND #( WHEN ls_salesdata-prod_att_7 IS NOT INITIAL THEN abap_true )
                                  prod_att_8   = COND #( WHEN ls_salesdata-prod_att_8 IS NOT INITIAL THEN abap_true )
                                  prod_att_9   = COND #( WHEN ls_salesdata-prod_att_9 IS NOT INITIAL THEN abap_true )
                                  prod_att10   = COND #( WHEN ls_salesdata-prod_att10 IS NOT INITIAL THEN abap_true )
                                  round_prof   = COND #( WHEN ls_salesdata-round_prof IS NOT INITIAL THEN abap_true )
                                  unit_group   = COND #( WHEN ls_salesdata-unit_group IS NOT INITIAL THEN abap_true )
                                  ) ).
      CLEAR gv_mkve.
    ENDIF.

*** MATERIALDESCRIPTION ***
    gt_materialdescription = VALUE #( FOR ls_makt IN gt_makt WHERE ( matnr = is_mara-matnr )
                                   (
*                                      matnr = is_mara-matnr
                                      langu = ls_makt-spras
                                      matl_desc = ls_makt-maktx  ) ).

*** UNITSOFMEASURE ***
    gt_unitsofmeasure =  VALUE #( FOR ls_marm IN gt_marm WHERE ( matnr = is_mara-matnr )
                                   (
*                                   matnr       = is_mara-matnr
                                   alt_unit    = ls_marm-meinh
                                   numerator   = ls_marm-umrez
                                   denominatr  = ls_marm-umren
                                   ean_upc     = ls_marm-ean11
                                   length      = ls_marm-laeng
                                   width       = ls_marm-breit
                                   height      = ls_marm-hoehe
                                   unit_dim    = ls_marm-meabm
                                   volume      = ls_marm-volum
                                   volumeunit  = ls_marm-voleh
                                   gross_wt    = ls_marm-brgew
                                   unit_of_wt  = ls_marm-gewei     ) ).

*** UNITSOFMEASUREX ***
    gt_unitsofmeasurex =  VALUE #( FOR ls_unitsofmeasure IN gt_unitsofmeasure
                                   (
*                                   matnr       = abap_true
                                   alt_unit    = COND #( WHEN ls_unitsofmeasure-alt_unit IS NOT INITIAL THEN ls_unitsofmeasure-alt_unit )
                                   numerator   = COND #( WHEN ls_unitsofmeasure-numerator IS NOT INITIAL THEN abap_true )
                                   denominatr  = COND #( WHEN ls_unitsofmeasure-denominatr IS NOT INITIAL THEN abap_true )
                                   ean_upc     = COND #( WHEN ls_unitsofmeasure-ean_upc IS NOT INITIAL THEN abap_true )
                                   length      = COND #( WHEN ls_unitsofmeasure-length IS NOT INITIAL THEN abap_true )
                                   width       = COND #( WHEN ls_unitsofmeasure-width IS NOT INITIAL THEN abap_true )
                                   height      = COND #( WHEN ls_unitsofmeasure-height IS NOT INITIAL THEN abap_true )
                                   unit_dim    = COND #( WHEN ls_unitsofmeasure-unit_dim IS NOT INITIAL THEN abap_true )
                                   volume      = COND #( WHEN ls_unitsofmeasure-volume IS NOT INITIAL THEN abap_true )
                                   volumeunit  = COND #( WHEN ls_unitsofmeasure-volumeunit IS NOT INITIAL THEN abap_true )
                                   gross_wt    = COND #( WHEN ls_unitsofmeasure-gross_wt IS NOT INITIAL THEN abap_true )
                                   unit_of_wt  = COND #( WHEN ls_unitsofmeasure-unit_of_wt IS NOT INITIAL THEN abap_true )   ) ).

*** TAXCLASSIFICATIONS ***
    gt_taxclassifications = VALUE #( FOR ls_mlan3 IN gt_mlan WHERE ( matnr = is_mara-matnr )
                                   (
*                                     matnr = is_mara-matnr
                                     depcountry = ls_mlan3-aland
                                     tax_type_1 = 'IBRX'
                                     taxclass_1 = ls_mlan3-taxm1
                                    ) ).
* Binary Search
    SORT gt_plantdata BY plant.
    SORT gt_storagelocationdata BY plant.
    SORT gt_valuationdata BY val_area.
    SORT gt_salesdata BY sales_org.
    SORT gt_salesdatax BY sales_org distr_chan.

  ENDMETHOD.


  METHOD criar_dados.

    DATA ls_return TYPE ztmm_log_mat.

    limpar_atributos( ).
    obter_constantes( ).

    GET TIME STAMP FIELD DATA(lv_data).

    buscar_dados( EXPORTING ir_material = ir_material[]
                            iv_manual   = iv_manual
                            iv_data     = lv_data ).

    LOOP AT gt_mara INTO DATA(ls_mara).

      READ TABLE gt_marc INTO DATA(ls_marc) WITH KEY matnr = ls_mara-matnr
                                            BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        READ TABLE gt_mbew INTO DATA(ls_mbew) WITH KEY matnr = ls_mara-matnr
                                              BINARY SEARCH.
        CHECK sy-subrc = 0.
      ENDIF.
      READ TABLE gt_mvke INTO DATA(ls_mvke) WITH KEY matnr = ls_mara-matnr
                                            BINARY SEARCH.
*      CHECK sy-subrc = 0.
      IF sy-subrc IS INITIAL.

        gv_mkve = abap_true.

      ENDIF.

      construir_dados( EXPORTING is_mara = ls_mara
                                 is_marc = ls_marc
                                 is_mbew = ls_mbew
                                 is_mvke = ls_mvke ).

      "Foi criado um parâmetro que HABILITA ou DESABILITA as ESTRUTURAS: STORAGELOCATIONDATA e STORAGELOCATIONDATAX
      "Parâmetro: MM - REPLICAÇÃO_MATERIAL - PARAMETROS_FIXOS – VISÃO_DEPÓSITOS
      " HABILITA = X / DESABILITA = ' '
      IF gv_visao_dep IS NOT INITIAL.

        LOOP AT gt_plantdata INTO DATA(ls_plantdata).

          READ TABLE gt_t001k INTO DATA(ls_t001k) WITH KEY bwkey = ls_plantdata-plant
                                                           BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            IF line_exists( gr_bukrs_mtart[ low  = ls_t001k-bukrs
                                            high = ls_mara-mtart ] ).

              DATA(lv_index) = sy-tabix.

              READ TABLE gt_plantdatax INTO DATA(ls_plantdatax) WITH KEY plant = ls_plantdata-plant.
              CHECK sy-subrc = 0.

              READ TABLE gt_storagelocationdata WITH KEY plant = ls_plantdata-plant
                 BINARY SEARCH TRANSPORTING NO FIELDS.
              CHECK sy-subrc = 0.
              DATA(lv_tabix) = sy-tabix.


              LOOP AT gt_storagelocationdata FROM lv_tabix INTO DATA(ls_storagelocationdata).

                IF ls_storagelocationdata-plant <> ls_plantdata-plant.
                  EXIT.
                ENDIF.

                READ TABLE gt_valuationdata INTO DATA(ls_valuationdata) WITH KEY val_area = ls_plantdata-plant.
                DATA(lv_index2) = sy-tabix.
                CHECK sy-subrc = 0.

                READ TABLE gt_valuationdatax INTO DATA(ls_valuationdatax) INDEX lv_index2.
                CHECK sy-subrc = 0.

                READ TABLE gt_storagelocationdatax INTO DATA(ls_storagelocationdatax) WITH KEY plant    = ls_storagelocationdata-plant
                                                                                               stge_loc = ls_storagelocationdata-stge_loc.
                CHECK sy-subrc = 0.

*                READ TABLE gt_salesdata INTO DATA(ls_salesdata) INDEX 1.
*                IF sy-subrc IS INITIAL.
*                  DELETE gt_salesdata INDEX 1.
*                ENDIF.
*                READ TABLE gt_salesdatax INTO DATA(ls_salesdatax) INDEX 1.
*                IF sy-subrc IS INITIAL.
*                  DELETE gt_salesdatax INDEX 1.
*                ENDIF.
                READ TABLE gt_tvko INTO DATA(ls_tvko) WITH KEY bukrs = ls_t001k-bukrs
                                                      BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  READ TABLE gt_salesdata INTO DATA(ls_salesdata) WITH KEY sales_org = ls_tvko-vkorg
                                                                  BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    DATA(lv_index3) = sy-tabix.
                    DELETE gt_salesdata INDEX lv_index3.
                    READ TABLE gt_salesdatax INTO DATA(ls_salesdatax) WITH KEY sales_org = ls_salesdata-sales_org
                                                                               distr_chan = ls_salesdata-distr_chan
                                                                  BINARY SEARCH..
                    IF sy-subrc IS INITIAL.
                      DATA(lv_index3_x) = sy-tabix.
                      DELETE gt_salesdatax INDEX lv_index3_x.
                    ENDIF.
                  ENDIF.
                ENDIF.


                CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
                  EXPORTING
                    headdata             = gs_headdata
                    clientdata           = gs_clientdata
                    clientdatax          = gs_clientdatax
                    plantdata            = ls_plantdata
                    plantdatax           = ls_plantdatax
                    storagelocationdata  = ls_storagelocationdata
                    storagelocationdatax = ls_storagelocationdatax
                    valuationdata        = ls_valuationdata
                    valuationdatax       = ls_valuationdatax
                    salesdata            = ls_salesdata
                    salesdatax           = ls_salesdatax
                  IMPORTING
                    return               = gs_return
                  TABLES
                    materialdescription  = gt_materialdescription
                    unitsofmeasure       = gt_unitsofmeasure
                    unitsofmeasurex      = gt_unitsofmeasurex
                    taxclassifications   = gt_taxclassifications.
                IF gs_return-type EQ 'S'.

                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      wait = 'X'.

                ELSE.

                  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

                ENDIF.

                ls_return = gravar_log( is_plantdata           = ls_plantdata
                                        is_storagelocationdata = ls_storagelocationdata
                                        is_salesdata           = ls_salesdata ).

                APPEND ls_return TO et_return.

              ENDLOOP.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDLOOP.

      ELSE.
        LOOP AT gt_plantdata INTO DATA(ls_plantdata_aux).

          READ TABLE gt_t001k INTO DATA(ls_t001k_aux) WITH KEY bwkey = ls_plantdata_aux-plant
                                                 BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            IF line_exists( gr_bukrs_mtart[ low = ls_t001k_aux-bukrs
                                            high = ls_mara-mtart ] ).

              READ TABLE gt_plantdatax INTO DATA(ls_plantdatax_aux) WITH KEY plant = ls_plantdata_aux-plant.
              CHECK sy-subrc = 0.

              READ TABLE gt_valuationdata INTO DATA(ls_valuationdata_aux) WITH KEY val_area = ls_plantdata_aux-plant.
              DATA(lv_index4) = sy-tabix.
              CHECK sy-subrc = 0.

              READ TABLE gt_valuationdatax INTO DATA(ls_valuationdatax_aux) INDEX lv_index4.
              CHECK sy-subrc = 0.

              READ TABLE gt_tvko INTO DATA(ls_tvko_aux) WITH KEY bukrs = ls_t001k_aux-bukrs
                                                    BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                READ TABLE gt_salesdata INTO DATA(ls_salesdata_aux) WITH KEY sales_org = ls_tvko_aux-vkorg
                                                                BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  DATA(lv_index5) = sy-tabix.
                  DELETE gt_salesdata INDEX lv_index5.
                  READ TABLE gt_salesdatax INTO DATA(ls_salesdatax_aux) WITH KEY sales_org = ls_salesdata_aux-sales_org
                                                                                 distr_chan = ls_salesdata_aux-distr_chan
                                                  BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    DATA(lv_index5_x) = sy-tabix.
                    DELETE gt_salesdatax INDEX lv_index5_x.
                  ENDIF.
                ENDIF.
              ENDIF.


              CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
                EXPORTING
                  headdata            = gs_headdata
                  clientdata          = gs_clientdata
                  clientdatax         = gs_clientdatax
                  plantdata           = ls_plantdata_aux
                  plantdatax          = ls_plantdatax_aux
                  valuationdata       = ls_valuationdata_aux
                  valuationdatax      = ls_valuationdatax_aux
                  salesdata           = ls_salesdata_aux
                  salesdatax          = ls_salesdatax_aux
                IMPORTING
                  return              = gs_return
                TABLES
                  materialdescription = gt_materialdescription
                  unitsofmeasure      = gt_unitsofmeasure
                  unitsofmeasurex     = gt_unitsofmeasurex
                  taxclassifications  = gt_taxclassifications.
              IF gs_return-type EQ 'S'.

                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    wait = 'X'.

              ELSE.

                CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

              ENDIF.

              ls_return = gravar_log( is_plantdata           = ls_plantdata_aux
                                      is_salesdata           = ls_salesdata_aux ).

              APPEND ls_return TO et_return.
            ENDIF.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD exibir_log.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(lo_alv)
          CHANGING
            t_table        = ct_return

        ).
      CATCH cx_salv_msg.
    ENDTRY.
    lo_alv->display( ).

  ENDMETHOD.


  METHOD gravar_log.

    DATA(ls_log_mat) = VALUE ztmm_log_mat(
                                          matnr = gs_headdata-material
                                          werks = is_plantdata-plant
                                          lgort = is_storagelocationdata-stge_loc
                                          vkorg = is_salesdata-sales_org
                                          vtweg = is_salesdata-distr_chan
                                          ztype = gs_return-type
                                          zid = gs_return-id
                                          znumber = gs_return-number
                                          zmessage = gs_return-message
                                          zlog_no = gs_return-log_no
                                          zlog_msg_no = gs_return-log_msg_no
                                          zmessage_v1 = gs_return-message_v1
                                          zmessage_v2 = gs_return-message_v2
                                          zmessage_v3 = gs_return-message_v3
                                          zmessage_v4 = gs_return-message_v4
                                          usuario = sy-uname
                                          data = sy-datum
                                          hora = sy-uzeit
                                       ) .

    MODIFY ztmm_log_mat FROM ls_log_mat.

    rs_return = ls_log_mat.

  ENDMETHOD.


  METHOD OBTER_AREA_DE_VENDAS.

    SELECT *
      FROM tvta
      INTO TABLE @gt_tvta
      WHERE hide EQ @space
      AND   spart EQ '10'
      .

  ENDMETHOD.


  METHOD obter_centros.

    IF gr_bukrs[] IS NOT INITIAL.
      SELECT *
        FROM t001k
        INTO TABLE gt_t001k
        WHERE bukrs IN gr_bukrs
        .
      IF sy-subrc IS INITIAL.

        SORT gt_t001k BY bwkey.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD obter_depositos.
    IF gt_t001k[] IS NOT INITIAL.

      SELECT *
        FROM t001l
        INTO TABLE gt_t001l
        FOR ALL ENTRIES IN gt_t001k
        WHERE werks EQ gt_t001k-bwkey
        .
    ENDIF.
  ENDMETHOD.


  METHOD limpar_atributos.

    CLEAR:
    gr_mandt[],
    gr_matkl[],
    gr_prctr[],
    gt_mara[],
    gt_marc[],
    gt_mard[],
    gt_mvke[],
    gt_mbew[],
    gt_makt[],
    gt_mlan[],
    gt_marm[],
    gs_headdata,
    gs_clientdata,
    gs_clientdatax,
    gs_plantdata,
    gs_plantdatax,
    gs_storagelocationdata,
    gs_storagelocationdatax,
    gs_valuationdata,
    gs_valuationdatax,
    gs_salesdata,
    gs_salesdatax,
    gt_plantdata[],
    gs_plantdatax,
    gs_storagelocationdata,
    gs_storagelocationdatax,
    gs_valuationdata,
    gs_valuationdatax,
    gs_salesdata,
    gs_salesdatax,
    gt_plantdata[],
    gs_return,
    gt_plantdatax[],
    gt_storagelocationdata[],
    gt_storagelocationdatax[],
    gt_valuationdata[],
    gt_valuationdatax[],
    gt_salesdata[],
    gt_salesdatax[],
    gt_materialdescription[],
    gt_unitsofmeasure[],
    gt_unitsofmeasurex[],
    gt_taxclassifications[],
    gr_bukrs[],
    gt_t001k[],
    gt_t001l[],
    gv_time,
    gt_tvta[].

  ENDMETHOD.


  METHOD obter_constantes.

    DATA(lr_param) = zclca_tabela_parametros=>get_instance( ).

    TRY.
        lr_param->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_1
            iv_chave2 = lc_chave2_1
            iv_chave3 = lc_chave3_1
          IMPORTING
            et_range  = gr_mandt
        ).

      CATCH zcxca_tabela_parametros.

    ENDTRY.

    TRY.
        lr_param->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_2
            iv_chave2 = lc_chave2_2
            iv_chave3 = lc_chave3_2
          IMPORTING
            et_range  = gr_matkl
        ).

      CATCH zcxca_tabela_parametros.

    ENDTRY.

    TRY.
        lr_param->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_3
            iv_chave2 = lc_chave2_3
            iv_chave3 = lc_chave3_3
          IMPORTING
            et_range  = gr_bukrs
        ).

      CATCH zcxca_tabela_parametros.

    ENDTRY.

    TRY.
        lr_param->m_get_single(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_4
            iv_chave2 = lc_chave2_4
            iv_chave3 = lc_chave3_4
          IMPORTING
            ev_param  = gv_time
        ).

      CATCH zcxca_tabela_parametros.

    ENDTRY.

    TRY.
        lr_param->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_5
            iv_chave2 = lc_chave2_5
            iv_chave3 = lc_chave3_5
          IMPORTING
            et_range  = gr_prctr
        ).

      CATCH zcxca_tabela_parametros.

    ENDTRY.

    TRY.
        lr_param->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_6
            iv_chave2 = lc_chave2_6
            iv_chave3 = lc_chave3_6
          IMPORTING
            et_range  = gr_mtart
        ).

      CATCH zcxca_tabela_parametros.

    ENDTRY.
    TRY.
        lr_param->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_7
            iv_chave2 = lc_chave2_7
            iv_chave3 = lc_chave3_7
          IMPORTING
            et_range  = gr_ekgrp
        ).

      CATCH zcxca_tabela_parametros.

    ENDTRY.

    TRY.
        lr_param->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_8
            iv_chave2 = lc_chave2_8
            iv_chave3 = lc_chave3_8
          IMPORTING
            et_range  = gr_spart
        ).

      CATCH zcxca_tabela_parametros.

    ENDTRY.
    TRY.
        lr_param->m_get_range(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_9
            iv_chave2 = lc_chave2_9
            iv_chave3 = lc_chave3_9
          IMPORTING
            et_range  = gr_bukrs_mtart
        ).

      CATCH zcxca_tabela_parametros.

    ENDTRY.
    LOOP AT gr_bukrs_mtart ASSIGNING FIELD-SYMBOL(<fs_bukrs_mtart>).
      DATA(lv_bukrs_mtart) = <fs_bukrs_mtart>-low.
      SPLIT lv_bukrs_mtart AT '-' INTO <fs_bukrs_mtart>-low <fs_bukrs_mtart>-high.
    ENDLOOP.
    TRY.
        lr_param->m_get_single(
          EXPORTING
            iv_modulo = lc_modulo_mm
            iv_chave1 = lc_chave1_10
            iv_chave2 = lc_chave2_10
            iv_chave3 = lc_chave3_10
          IMPORTING
            ev_param  = gv_visao_dep
        ).

      CATCH zcxca_tabela_parametros.

    ENDTRY.



  ENDMETHOD.


  METHOD obter_tempo.

    DATA ls_tmstpl TYPE tzntstmpl.

    IF gv_time IS NOT INITIAL.
      DATA(lv_durat) = gv_time * 60.
    ENDIF.

    CALL METHOD cl_abap_tstmp=>subtractsecs
      EXPORTING
        tstmp   = iv_data
        secs    = lv_durat
      RECEIVING
        r_tstmp = ls_tmstpl.

     CALL METHOD cl_abap_tstmp=>move_to_short
      EXPORTING
        tstmp_src   = ls_tmstpl
      RECEIVING
        tstmp_out   = rv_data.

  ENDMETHOD.


  METHOD OBTER_GRUPO_VENDAS.

    SELECT *
      FROM tvko
      INTO TABLE gt_tvko
      WHERE bukrs IN gr_bukrs
      .
    IF sy-subrc IS INITIAL.
      SORT gt_tvko BY bukrs.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
