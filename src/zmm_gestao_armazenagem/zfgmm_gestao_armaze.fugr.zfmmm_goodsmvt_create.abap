FUNCTION zfmmm_goodsmvt_create.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_GOODSMVT_ITEM) TYPE  TAB_BAPI_GOODSMVT_ITEM
*"     VALUE(IS_GOODSMVT_HEADER) TYPE  BAPI2017_GM_HEAD_01
*"     VALUE(IV_GOODSMVT_CODE) TYPE  BAPI2017_GM_CODE
*"     VALUE(IV_TESTRUN) TYPE  BAPI2017_GM_GEN-TESTRUN DEFAULT SPACE
*"     VALUE(IS_GOODSMVT_REF_EWM) TYPE  /SPE/BAPI2017_GM_REF_EWM
*"       OPTIONAL
*"     VALUE(IV_GOODSMVT_PRINT_CTRL) TYPE  BAPI2017_GM_PRINT OPTIONAL
*"     VALUE(IT_BASE_PROPR) TYPE  ZCTGMM_BASE_PROPR OPTIONAL
*"     VALUE(IT_BASE_TERC) TYPE  ZCTGMM_BASE_TERC OPTIONAL
*"  EXPORTING
*"     VALUE(ES_GOODSMVT_HEADRET) TYPE  BAPI2017_GM_HEAD_RET
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
  CONSTANTS: lc_gm_code TYPE bapi2017_gm_code VALUE '05',
             lc_id      TYPE symsgid VALUE 'ZMM',
             lc_no000   TYPE symsgno VALUE '000',
             lc_no004   TYPE symsgno VALUE '004',
             lc_type    TYPE bapi_mtype VALUE 'S'.

  DATA ls_return        LIKE LINE OF et_return.


  IF it_base_propr IS NOT INITIAL.
    MODIFY ztmm_base_propr FROM TABLE it_base_propr.
    COMMIT WORK AND WAIT.
  ENDIF.

  IF it_base_terc IS NOT INITIAL.
    MODIFY ztmm_base_terc FROM TABLE it_base_terc.
    COMMIT WORK AND WAIT.
  ENDIF.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = is_goodsmvt_header   " Material Document Header Data
      goodsmvt_code    = iv_goodsmvt_code     " Assign Code to Transaction for Goods Movement
    IMPORTING
      goodsmvt_headret = es_goodsmvt_headret  " Material Document Number/Material Document Year
    TABLES
      goodsmvt_item    = it_goodsmvt_item     " Material Document Items
      return           = et_return.            " Return Messages

  IF et_return IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = abap_on
      IMPORTING
        return = ls_return.

    IF ls_return IS INITIAL.
      et_return = VALUE bapiret2_t( (
                                      type = lc_type
                                      id = lc_id
                                      number = lc_no000
                                      message_v1 = abap_undefined
                                    )
                                    (
                                      type = lc_type
                                      id = lc_id
                                      number = lc_no004
                                      message_v1 = es_goodsmvt_headret-mat_doc
                                      message_v2 = es_goodsmvt_headret-doc_year
                                    )
                                  ).
    ENDIF.

  ENDIF.


ENDFUNCTION.
