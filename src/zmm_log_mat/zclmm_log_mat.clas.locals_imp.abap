CLASS lcl_ZI_MM_LOG_MAT DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zi_mm_log_mat RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_mm_log_mat RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_mm_log_mat RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zi_mm_log_mat.

    METHODS button FOR MODIFY
      IMPORTING keys FOR ACTION zi_mm_log_mat~button.

ENDCLASS.

CLASS lcl_ZI_MM_LOG_MAT IMPLEMENTATION.

  METHOD get_instance_features.
    RETURN.
  ENDMETHOD.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD lock.
    RETURN.
  ENDMETHOD.

  METHOD button.

    IF keys IS NOT INITIAL.

      DATA lt_matnr TYPE tbl_mat_range.
      DATA ls_matnr LIKE LINE OF lt_matnr.

      LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_keys>).
        ls_matnr-sign = 'I'.
        ls_matnr-option = 'EQ'.
        ls_matnr-matnr_low = <fs_keys>-matnr.
        APPEND ls_matnr TO lt_matnr.
      ENDLOOP.

      CALL FUNCTION 'ZFMMM_LOG_MAT'
        STARTING NEW TASK 'COMMIT'
        EXPORTING
          ir_matnr  = lt_matnr
          iv_manual = abap_true.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_ZI_MM_LOG_MAT2 DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lcl_ZI_MM_LOG_MAT2 IMPLEMENTATION.

  METHOD finalize.
    RETURN.
  ENDMETHOD.

  METHOD check_before_save.
    RETURN.
  ENDMETHOD.

  METHOD save.
    RETURN.
  ENDMETHOD.

  METHOD cleanup.
    RETURN.
  ENDMETHOD.

  METHOD cleanup_finalize.
    RETURN.
  ENDMETHOD.

ENDCLASS.
