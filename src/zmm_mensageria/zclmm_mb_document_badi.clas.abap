class ZCLMM_MB_DOCUMENT_BADI definition
  public
  final
  create public .

*"* public components of class CL_EXM_IM_MB_DOCUMENT_BADI
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_MB_DOCUMENT_BADI .
protected section.
*"* protected components of class CL_EXM_IM_MB_DOCUMENT_BADI
*"* do not include other source files here!!!
private section.
*"* private components of class CL_EXM_IM_MB_DOCUMENT_BADI
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCLMM_MB_DOCUMENT_BADI IMPLEMENTATION.


method IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_BEFORE_UPDATE.
* Will make a protocol of material documents in table
* zmmdoc for users in table zspecialusers

*  DATA: zspecialusers TYPE zspecialusers,
*        l_zmmdoc      TYPE zmmdoc,
*        l_mkpf        TYPE mkpf,
*        l_special     TYPE zspecialusers.
*
*  CLEAR: l_zmmdoc.
*
*  READ TABLE xmkpf INTO l_mkpf INDEX 1.
*
*  SELECT SINGLE * FROM zspecialusers INTO l_special
*     WHERE special_user = l_mkpf-usnam.
** user found, update in update task
*  IF sy-subrc IS INITIAL.
*    MOVE-CORRESPONDING l_mkpf TO l_zmmdoc.
*    l_zmmdoc-special_user = l_mkpf-usnam.
*    CALL FUNCTION 'zupdate_zmmdoc' IN UPDATE TASK
*         EXPORTING
*              zmmdoc = l_zmmdoc.
*  ENDIF.

"{ Begin ENHO /SAPMP/ACCOUNTING_COPROD_BADI IS-MP-PP /SAPMP/SEPARATE_ACC_CO_BY_PROD }
*
*  CONSTANTS:
*        objektart_or TYPE ionr-obart  VALUE 'OR',
*        objektart_op TYPE ionr-obart  VALUE 'OP',
*        default_rule TYPE tkb2a-dfreg VALUE 'PP5',
*        lc_max_value TYPE i           VALUE 10.
*
*  DATA: i_order_header    TYPE          caufvd,
*        l_aufnr           TYPE          caufv-aufnr,
*        it_order_position TYPE TABLE OF afpob,
*        ls_position       TYPE          afpob,
*        lt_afpod          TYPE TABLE OF afpod,
*        afpod             TYPE          afpod,
*        bmeins            TYPE          afpo-meins,
*        lt_xmseg          TYPE TABLE OF mseg,
*        lt_xmseg_ord      TYPE TABLE OF mseg,
*        ls_xmseg          TYPE          mseg,
*        ls_xmkpf          TYPE          mkpf,
*        lt_xvm07m         TYPE TABLE OF vm07m,
*        ls_xvm07m         TYPE          vm07m,
*        ls_cobr_info      TYPE          cobr_info,
*        dfreg             TYPE          tkb2a-dfreg,
*        lt_cobra          TYPE TABLE OF cobra,
*        ls_cobra          TYPE          cobra,
*        lt_cobrb          TYPE TABLE OF cobrb,
*        ls_cobrb          TYPE          cobrb,
*        rule_used(1)      TYPE          c,
*        l_kokrs           TYPE          tka01-kokrs,
*        ls_t001w          TYPE          t001w,
*        ls_t001k          TYPE          t001k,
*        lt_ord_pre        TYPE TABLE OF ord_pre,
*        ls_ord_pre        TYPE          ord_pre,
*        diff_bmeins(1)    TYPE          c,
*        leave_badi(1)     TYPE          c,
*        l_wemng(1)        TYPE          c,
*        storno(1)         TYPE          c,
*        plan_quan(1)      TYPE          c,
*        afpod_read_db(1)  TYPE          c.
*
*  DATA: lt_aufnr TYPE STANDARD TABLE OF caufv-aufnr.
*
*  FIELD-SYMBOLS: <field> TYPE ANY.
*
*  DATA: lt_dftab      TYPE TABLE OF dftabelle,
*        ls_dftab      TYPE          dftabelle,
*        l_tabix       TYPE          sy-tabix,
*        lt_tmakg      TYPE TABLE OF makg,
*        tmakg         TYPE          makg,
*        lt_tmakz      TYPE TABLE OF makz,
*        tmakz         TYPE          makz,
*        ls_makv       TYPE          makv,
*        l_ratio       TYPE          i,
*        l_mod         TYPE          f,
*        l_bmenge      TYPE          afpod-bmeng,
*        l_vk          TYPE          i,
*        r_count       TYPE          i,
*        r_count_ratio TYPE          i,
*        l_value       TYPE          f.
*"{ End ENHO /SAPMP/ACCOUNTING_COPROD_BADI IS-MP-PP /SAPMP/SEPARATE_ACC_CO_BY_PROD }
*
*ENHANCEMENT-POINT CL_EXM_IM_MB_DOCUMENT_BADI_02 SPOTS ES_CL_EXM_IM_MB_DOCUMENT_BADI STATIC.
*
*"{ Begin ENHO /SAPMP/ACCOUNTING_COPROD_BADI IS-MP-PP /SAPMP/SEPARATE_ACC_CO_BY_PROD }
**
* LOOP AT xmseg INTO ls_xmseg.
*    APPEND ls_xmseg TO lt_xmseg.
*  ENDLOOP.
*
** sort after order number and position
*  SORT lt_xmseg BY aufnr aufps.
** get order numbers (only goods movements for production order)
*  LOOP AT lt_xmseg INTO ls_xmseg WHERE kzbew = 'F'.
*    l_aufnr = ls_xmseg-aufnr.
*    APPEND l_aufnr TO lt_aufnr.
*  ENDLOOP.
*
*  DELETE ADJACENT DUPLICATES FROM lt_aufnr.
*
*  CLEAR: l_aufnr.
** once for each order
*  LOOP AT lt_aufnr INTO l_aufnr.
*
** initialization
*    CLEAR: afpod,
*           plan_quan,
*           storno,
*           diff_bmeins,
*           rule_used,
*           dfreg,
*           ls_makv,
*           ls_ord_pre,
*           l_bmenge,
*           afpod_read_db.
*
*    REFRESH: lt_afpod,
*             lt_xmseg_ord,
*             lt_tmakg,
*             lt_tmakz,
*             lt_ord_pre.
*
** select entries to one order
*    LOOP AT lt_xmseg INTO ls_xmseg.
*      IF l_aufnr = ls_xmseg-aufnr.
*        APPEND ls_xmseg TO lt_xmseg_ord.
*      ENDIF.
*    ENDLOOP.
*
** check for cancellation
*    LOOP AT lt_xmseg_ord INTO ls_xmseg.
*      READ TABLE xvm07m INTO ls_xvm07m WITH KEY zeilv = ls_xmseg-zeile.
*      IF ls_xvm07m-xstor = '2' OR
*         ls_xmseg-shkzg  = 'H'.
*        storno = 'X'.
*      ENDIF.
*    ENDLOOP.
*
** read order header
*    CALL FUNCTION 'CO_BT_CAUFV_READ_WITH_KEY'
*      EXPORTING
*        aufnr_act  = l_aufnr
*      IMPORTING
*        caufvd_exp = i_order_header
*      EXCEPTIONS
*        not_found  = 1
*        OTHERS     = 2.
*
*    IF sy-subrc <> 0.
** read order from DB
*      MOVE l_aufnr TO ls_ord_pre-aufnr.
*      APPEND ls_ord_pre TO lt_ord_pre.
*
*      CALL FUNCTION 'CO_ZF_ORDER_READ'
*        EXPORTING
*          flg_enqueue       = ' '
*        TABLES
*          aufnr_tab_imp     = lt_ord_pre
*        EXCEPTIONS
*          order_not_found   = 1
*          release_no_change = 2
*          OTHERS            = 3.
*
** read order header
*      CALL FUNCTION 'CO_BT_CAUFV_READ_WITH_KEY'
*        EXPORTING
*          aufnr_act  = l_aufnr
*        IMPORTING
*          caufvd_exp = i_order_header
*        EXCEPTIONS
*          not_found  = 1
*          OTHERS     = 2.
*
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ELSE.
*        afpod_read_db = 'X'.
*      ENDIF.
*    ENDIF.
*
** check for co-products
*    CHECK i_order_header-flg_mltps = 'X'.
** check for production or process order
*    CHECK i_order_header-autyp = '10' OR
*          i_order_header-autyp = '40'.
** check for not combined order
*    CHECK i_order_header-atrkz <> 'Z'.
*
** check for status "closed" (I0046)
*    CALL FUNCTION 'STATUS_CHECK'
*      EXPORTING
*        objnr             = i_order_header-objnr
*        status            = 'I0046'
*      EXCEPTIONS
*        object_not_found  = 1
*        status_not_active = 2
*        OTHERS            = 3.
*
*    IF sy-subrc = 0.
*      CONTINUE.
*    ENDIF.
*
** check for status "deletion flag" (I0076)
*    CALL FUNCTION 'STATUS_CHECK'
*      EXPORTING
*        objnr             = i_order_header-objnr
*        status            = 'I0076'
*      EXCEPTIONS
*        object_not_found  = 1
*        status_not_active = 2
*        OTHERS            = 3.
*
*    IF sy-subrc = 0.
*      CONTINUE.
*    ENDIF.
*
** check if changes for settlement rules are allowed
*    CALL FUNCTION 'STATUS_CHANGE_FOR_ACTIVITY'
*      EXPORTING
*        check_only           = 'X'
*        objnr                = i_order_header-objnr
*        vrgng                = 'KABV'
*      EXCEPTIONS
*        activity_not_allowed = 1
*        object_not_found     = 2
*        status_inconsistent  = 3
*        status_not_allowed   = 4
*        wrong_input          = 5
*        warning_occured      = 6
*        OTHERS               = 7.
*
*    IF sy-subrc = 1.
*      CONTINUE.
*    ENDIF.
*
** get order items
*    IF it_order_position[] IS INITIAL OR
*       afpod_read_db       = 'X'.
*      REFRESH: it_order_position.
*      CALL FUNCTION 'CO_BT_AFPO_BT_FETCH'
*        TABLES
*          et_afpo_bt = it_order_position.
*
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
*
*    l_wemng = 'X'.
*    LOOP AT it_order_position INTO ls_position
*            WHERE aufnr = l_aufnr AND
*                  wepos = 'X'.
*      IF ls_position-vbkz <> 'X'.
*
*        IF afpod_read_db = 'X'.
*          CLEAR: ls_xmseg.
*
*          READ TABLE lt_xmseg INTO ls_xmseg
*                     WITH KEY aufnr = ls_position-aufnr
*                              aufps = ls_position-posnr.
** add actual quantity to DB quantity
*          IF sy-subrc          = 0              AND
*             storno            IS INITIAL       AND
*             ls_position-amein = ls_xmseg-bstme.
*            ls_position-wemng = ls_position-wemng + ls_xmseg-bstmg.
** subtract actual quantity from DB quantity
*          ELSEIF sy-subrc          = 0              AND
*                 storno            = 'X'            AND
*                 ls_position-amein = ls_xmseg-bstme.
*            ls_position-wemng = ls_position-wemng - ls_xmseg-bstmg.
*            IF ls_position-wemng < 0.
*              ls_position-wemng = 0.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*
*        MOVE-CORRESPONDING ls_position TO afpod.
*        IF NOT afpod-wemng IS INITIAL.
** at least one position with goods receipt quantity > 0
*          CLEAR: l_wemng.
*        ENDIF.
*
*        APPEND afpod TO lt_afpod.
*      ENDIF.
*    ENDLOOP.
*
*    IF l_wemng = 'X'     AND
*       storno IS INITIAL.
*      CONTINUE.
*    ELSEIF l_wemng = 'X' AND
*           storno  = 'X'.
** set planned quantities
*      plan_quan = 'X'.
*    ENDIF.
*
** check for same base quantity
*    LOOP AT it_order_position INTO ls_position
*            WHERE aufnr = l_aufnr AND
*                  wepos = 'X'     AND
*                  vbkz <> 'X'.
*
*      IF bmeins IS INITIAL.
*        bmeins = ls_position-meins.
*      ENDIF.
*      IF bmeins <> ls_position-meins.
*        diff_bmeins = 'X'.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*
*    IF diff_bmeins = 'X'.
*      CONTINUE.
*    ENDIF.
*
** fill ls_corb_info
*    CALL FUNCTION 'CO_TA_T001W_READ'
*      EXPORTING
*        t001w_werk = i_order_header-werks
*      IMPORTING
*        t001wwa    = ls_t001w.
*
*    CALL FUNCTION 'T001K_READ'
*      EXPORTING
*        bwkey  = ls_t001w-bwkey
*      IMPORTING
*        struct = ls_t001k.
*
** get controlling area with Bukrs/Gsber
*    CALL FUNCTION 'RK_KOKRS_FIND'
*      EXPORTING
*        bukrs = ls_t001k-bukrs
*        gsber = i_order_header-gsber
*      IMPORTING
*        kokrs = l_kokrs.
*
*    ls_cobr_info-kokrs = l_kokrs.
*    ls_cobr_info-bukrs = ls_t001k-bukrs.
*
** get settlement rules
** read data from DB into BT
*    CALL FUNCTION 'K_SETTLEMENT_RULE_CHECK'
*      EXPORTING
*        objnr             = i_order_header-objnr
*        test_bukrs        = ls_cobr_info-bukrs
*        test_kokrs        = ls_cobr_info-kokrs
*        rwin_aufruf       = ' '
*      EXCEPTIONS
*        error_occurred    = 1
*        no_rule_for_objnr = 2
*        warning_occurred  = 3
*        internal_problems = 4
*        OTHERS            = 5.
*
*    IF sy-subrc = 1 OR
*       sy-subrc = 2 OR
*       sy-subrc = 4.
*      CONTINUE.
*    ENDIF.
*
** read COBRA and COBRB
*    CALL FUNCTION 'K_SETTLEMENT_RULE_GET'
*      EXPORTING
*        objnr   = i_order_header-objnr
*      TABLES
*        e_cobra = lt_cobra
*        e_cobrb = lt_cobrb.
*
** COBRA has one entry
*    READ TABLE lt_cobra INTO ls_cobra INDEX 1.
*
** check if settlement rules were used
*    LOOP AT lt_cobrb INTO ls_cobrb.
*      IF dfreg          IS INITIAL AND
*         ls_cobrb-dfreg <> 'STR'.
*        dfreg = ls_cobrb-dfreg.
*      ENDIF.
*
*      clear afpod.
*      READ TABLE lt_afpod INTO afpod
*                  WITH KEY aufnr = ls_cobrb-aufnr
*                           posnr = ls_cobrb-posnr.
*
*      IF afpod-fxpru    =  'X'   AND
*         ls_cobrb-dfreg <> 'STR'.
*        CLEAR: afpod-fxpru.
*        MODIFY lt_afpod INDEX sy-tabix FROM afpod.
*      ENDIF.
*
*      IF NOT ( ls_cobrb-letja IS INITIAL AND
*               ls_cobrb-letpe IS INITIAL ) .
*        rule_used = 'X'.
*      ENDIF.
*    ENDLOOP.
** only for default rule PP5!
*    IF rule_used = 'X'  OR
*       NOT dfreg = default_rule.
*      CONTINUE.
*    ENDIF.
*
** Read cost apportionment structure
*    CALL FUNCTION 'MATERIAL_COSTS_SPLIT_READ'
*      EXPORTING
*        matnr                 = i_order_header-matnr
*        werks                 = i_order_header-werks
*        csplit                = i_order_header-csplit
*        datum                 = i_order_header-aufld
*      IMPORTING
*        makv_exp              = ls_makv
*      TABLES
*        tmakg                 = lt_tmakg
*        tmakz                 = lt_tmakz
*      EXCEPTIONS
*        costs_split_not_found = 01.
*
*    IF sy-subrc <> 0.
*      tmakg-crule = '2'.
*      APPEND tmakg TO lt_tmakg.
*    ENDIF.
*
** determine ratio
*    l_ratio = 1.
*    LOOP AT lt_afpod INTO afpod.
*      DO.
*        IF plan_quan IS INITIAL.
*          l_bmenge = afpod-wemng * afpod-umrez * l_ratio /
*                     afpod-umren.
*        ELSE.
*          l_bmenge = afpod-psmng * afpod-umrez * l_ratio /
*                     afpod-umren.
*
*        ENDIF.
*        l_mod = l_bmenge MOD 1.
*        IF l_mod > 0.
*          l_ratio = l_ratio * 10.
*        ELSE.
*          EXIT.
*        ENDIF.
*      ENDDO.
*    ENDLOOP.
** only for mill specific default rule PP5
*    IF sy-subrc = 0            AND
*       dfreg    = default_rule.
*      i_order_header-mill_ratio = l_ratio.
*      CALL FUNCTION 'CO_BT_CAUFV_UPD'
*        EXPORTING
*          caufvd_upd = i_order_header.
*
*    ENDIF.
** need for equivalence number overflow check (10 places)
*    l_value = l_ratio.
*    r_count_ratio = 0.
*    DO.
*      IF l_value > 1.
*        r_count_ratio = r_count_ratio + 1.
*        l_value = l_value / 10.
*      ELSE.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
** if another logic for determining equivalence numbers is wanted
** here is the right place  !!!
** the new equivalence numbers are stored in field: AFPOD-MILL_UA_BMENGE
**
** the field l_value is needed for buffer owerflow check (10 digits)
*
** determine new eqivalence numbers
*    LOOP AT lt_afpod INTO afpod.
*      l_tabix = sy-tabix.
** check for buffer overflow
*      IF plan_quan IS INITIAL.
*        l_value = afpod-wemng * afpod-umrez /
*                                afpod-umren.
*      ELSE.
** no goods received quantity exists (cancellation)
*        l_value = afpod-psmng * afpod-umrez /
*                                afpod-umren.
*      ENDIF.
*      r_count = 0.
*      DO.
*        IF l_value > 1.
*          r_count = r_count + 1.
*          l_value = l_value / 10.
*        ELSE.
*          EXIT.
*        ENDIF.
*      ENDDO.
*      l_vk = r_count + r_count_ratio.
*
*      IF l_vk > lc_max_value.
** field overflow by determination of equivalence numbers
*        leave_badi = 'X'.
*        EXIT.
*      ELSE.
** get new equivalence number
*        IF plan_quan IS INITIAL.
*          afpod-mill_ua_bmenge = afpod-wemng * afpod-umrez * l_ratio /
*                                 afpod-umren.
*        ELSE.
** no goods received quantity exists (cancellation)
** set planned quantity
*          afpod-mill_ua_bmenge = afpod-psmng * afpod-umrez * l_ratio /
*                                 afpod-umren.
*        ENDIF.
*        MODIFY lt_afpod FROM afpod INDEX l_tabix.
*      ENDIF.
*    ENDLOOP.
*
*    IF leave_badi = 'X'.
*      CONTINUE.
*    ENDIF.
*
** change only items for that a goods receipt take place
*    LOOP AT lt_afpod INTO afpod.
*      l_tabix = sy-tabix.
*      READ TABLE lt_xmseg TRANSPORTING NO FIELDS
*                 WITH KEY aufnr = afpod-aufnr
*                          aufps = afpod-posnr.
*      IF sy-subrc <> 0.
*        DELETE lt_afpod INDEX l_tabix.
*      ENDIF.
*    ENDLOOP.
*
*    CLEAR: l_tabix.
*
** ----- delete settlement rules from header to position ------ *
*    LOOP AT lt_afpod INTO afpod WHERE fxpru IS INITIAL.
*      CALL FUNCTION 'CK_F_POSTING_RULES_DELETE'
*        EXPORTING
*          p_objnr   = i_order_header-objnr
*          sel_posnr = afpod-posnr
*          sel_konty = objektart_op
*        EXCEPTIONS
*          OTHERS    = 1.
*    ENDLOOP.
*
** save new settlement rules (equivalence numbers)
*    CALL FUNCTION 'K_DEFAULT_RULE_READ'
*      EXPORTING
*        dfreg = dfreg
*        konty = objektart_op
*        obart = objektart_or
*      TABLES
*        dftab = lt_dftab.
*
** depending on the default rule
*    LOOP AT lt_afpod INTO afpod WHERE fxpru IS INITIAL.
** grouping
*      LOOP AT lt_tmakg INTO tmakg.
** distribution after equivalence numbers
** PP5: equivalenve number is stored in AFPOD-MILL_UA_BMENGE
*
** take fields from AFPOD into settlement rules
*        LOOP AT lt_dftab INTO ls_dftab.
*          l_tabix = sy-tabix.
*          ASSIGN (ls_dftab-fdnam) TO <field>.
*          MOVE <field> TO ls_dftab-fdinh.
*          MODIFY lt_dftab INDEX l_tabix FROM ls_dftab.
*        ENDLOOP.
*
*        CALL FUNCTION 'K_POSTING_RULE_INSERT'
*          EXPORTING
*            add_para               = ls_cobr_info
*            aprof                  = ls_cobra-aprof
*            dfreg                  = dfreg
*            konty                  = objektart_op
*            ursch                  = ls_makv-scnam
*            objnr                  = i_order_header-objnr
*          TABLES
*            dftab                  = lt_dftab
*          EXCEPTIONS
*            default_rule_not_found = 1
*            enqueue_failure        = 2
*            foreign_lock           = 3
*            message_occurred       = 4
*            objnr_not_found        = 5
*            receiver_missing       = 6
*            internal_problems      = 7
*            OTHERS                 = 8.
*
*        IF sy-subrc <> 0.
*          MESSAGE x899(mill).
*        ENDIF.
*      ENDLOOP.   " lt_tmakg
*    ENDLOOP.   " lt_afpod
*
*  ENDLOOP.   " lt_aufnr
*

"{ End ENHO /SAPMP/ACCOUNTING_COPROD_BADI IS-MP-PP /SAPMP/SEPARATE_ACC_CO_BY_PROD }

*ENHANCEMENT-POINT CL_EXM_IM_MB_DOCUMENT_BADI_01 SPOTS ES_CL_EXM_IM_MB_DOCUMENT_BADI.
return.

endmethod.


METHOD if_ex_mb_document_badi~mb_document_update.
* Will make a protocol of material documents in table
* zmmdoc for all postings with movent tpye 561

*  DATA: l_mkpf       TYPE mkpf,
*        l_mseg       TYPE mseg,
*        l_zmmdoc     TYPE zmmdoc.
*
*  CLEAR: l_zmmdoc.
*
*  READ TABLE xmseg INTO l_mseg
*     WITH KEY bwart = 561.
*  IF sy-subrc <> 0.
*    EXIT.
*  ENDIF.
*
*  READ TABLE xmkpf INTO l_mkpf INDEX 1.
*
*  MOVE-CORRESPONDING l_mkpf TO l_zmmdoc.
*  l_zmmdoc-special_user = l_mkpf-usnam.
*  INSERT INTO zmmdoc VALUES l_zmmdoc.

  IF xmseg[] IS NOT INITIAL.

    "Lógica método
    NEW zclsd_valida_mensageria(  )->valida_estoque_mm( it_xmseg = xmseg[] ).

  ENDIF.

ENDMETHOD.
ENDCLASS.
