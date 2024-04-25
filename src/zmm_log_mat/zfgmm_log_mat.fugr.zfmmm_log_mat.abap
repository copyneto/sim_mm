FUNCTION zfmmm_log_mat.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IR_MATNR) TYPE  TBL_MAT_RANGE
*"     VALUE(IV_MANUAL) TYPE  BOOLEAN
*"----------------------------------------------------------------------

  DATA lo_material TYPE REF TO zclmm_replica_material.

  IF lo_material IS NOT BOUND.
    CREATE OBJECT lo_material.
  ENDIF.

  lo_material->criar_dados( ir_material = ir_matnr iv_manual = iv_manual ).



ENDFUNCTION.
