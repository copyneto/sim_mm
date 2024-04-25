FUNCTION ZFMMM_WMS_ATUALIZA_TABELA.
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_NO_COMMIT) TYPE  BOOLEAN OPTIONAL
*"     VALUE(IT_ZTBMMXXX1) TYPE  ZCTGMM_DOC_REM_REC_TAB OPTIONAL
*"     VALUE(IT_ZTBMMXXX2) TYPE  ZCTGMM_DOC_MAT OPTIONAL
*"     VALUE(IS_ZTBMMXXX1) TYPE  ZSMM_DOC_REM_REC_TAB OPTIONAL
*"     VALUE(IS_ZTBMMXXX2) TYPE  ZSMM_DOC_MAT OPTIONAL
*"----------------------------------------------------------------------

  IF it_ztbmmxxx1 IS NOT INITIAL.
    MODIFY ztmm_doc_rem_rec FROM TABLE it_ztbmmxxx1.
  ENDIF.
  IF it_ztbmmxxx2 IS NOT INITIAL.
    MODIFY ztmm_doc_mat FROM TABLE it_ztbmmxxx2.
  ENDIF.
  IF is_ztbmmxxx1 IS NOT INITIAL.
    MODIFY ztmm_doc_rem_rec FROM is_ztbmmxxx1.
  ENDIF.
  IF is_ztbmmxxx2 IS NOT INITIAL.
    MODIFY ztmm_doc_mat FROM is_ztbmmxxx2.
  ENDIF.
*
*  IF iv_no_commit IS INITIAL.
*    COMMIT WORK.
*  ENDIF.




ENDFUNCTION.
