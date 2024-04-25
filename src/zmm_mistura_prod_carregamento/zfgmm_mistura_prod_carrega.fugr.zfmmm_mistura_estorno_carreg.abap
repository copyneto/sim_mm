FUNCTION zfmmm_mistura_estorno_carreg.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_XLIPS) TYPE  SHP_LIPS_T
*"----------------------------------------------------------------------
  DATA lo_proc_mistura TYPE REF TO zclmm_proc_mistura.
  CREATE OBJECT lo_proc_mistura.

  CALL METHOD lo_proc_mistura->proc_estorno
    EXPORTING
      it_xlips = it_xlips.
ENDFUNCTION.
