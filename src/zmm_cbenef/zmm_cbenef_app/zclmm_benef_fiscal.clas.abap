CLASS zclmm_benef_fiscal DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

    METHODS upload_file
      IMPORTING
        !iv_file     TYPE xstring
        !iv_filename TYPE string
      EXPORTING
        !et_return   TYPE bapiret2_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclmm_benef_fiscal IMPLEMENTATION.
  METHOD constructor.
    RETURN.
  ENDMETHOD.

  METHOD upload_file.

    DATA: lt_file   TYPE TABLE OF zsmm_benef_fiscal_file,
          lt_cbenef TYPE TABLE OF ztmm_cbenef.
    DATA: ls_cbenef_file TYPE zsmm_benef_fiscal_file.
    DATA: lv_message     TYPE string.
* ---------------------------------------------------------------------------
* Converte arquivo excel para tabela
* ---------------------------------------------------------------------------
    DATA(lo_excel) = NEW zclca_excel( iv_filename = iv_filename
                                      iv_file     = iv_file ).
    "lo_excel->gv_quant = abap_true.
    lo_excel->get_sheet( IMPORTING et_return = DATA(lt_return)              " Ignorar validação durante carga
                         CHANGING  ct_table  = lt_file[] ).

    IF line_exists( lt_return[ type = 'E' ] ).           "#EC CI_STDSEQ
      et_return = lt_return.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------------
* Prepara dados para salvar
* ---------------------------------------------------------------------------
    DELETE lt_file WHERE shipfrom   IS INITIAL
                      OR shipto     IS INITIAL
                      OR direct     IS INITIAL
*                      OR taxsit     IS INITIAL
                      OR cfop       IS INITIAL
                      OR cbenef     IS INITIAL
                      OR motdesicms IS INITIAL
                      OR tipo_calc  IS INITIAL.        "#EC CI_STDSEQ

    ls_cbenef_file-taxsit = 0.
    MODIFY lt_file FROM ls_cbenef_file TRANSPORTING taxsit WHERE taxsit IS INITIAL.  "#EC CI_STDSEQ

    IF lt_file IS NOT INITIAL.

      lt_cbenef = VALUE #( FOR ls_file IN lt_file ( mandt      = sy-mandt
                                                    shipfrom   = ls_file-shipfrom
                                                    shipto     = ls_file-shipto
                                                    direct     = ls_file-direct
                                                    taxsit     = ls_file-taxsit
                                                    cfop       = ls_file-cfop
                                                    nbm        = ls_file-nbm
                                                    matnr      = ls_file-matnr
                                                    cbenef     = ls_file-cbenef
                                                    motdesicms = ls_file-motdesicms
                                                    tipo_calc  = ls_file-tipo_calc ) ).

      IF lt_cbenef IS NOT INITIAL.

        MODIFY ztmm_cbenef FROM TABLE lt_cbenef.

        IF sy-subrc IS INITIAL.

          MESSAGE s000(zca_upload_excel) INTO lv_message.
          et_return[] = VALUE #( BASE et_return ( type = 'S' id = 'ZCA_UPLOAD_EXCEL' number = '000' message = lv_message ) ).

          COMMIT WORK AND WAIT.

        ENDIF.

      ENDIF.

    ELSE. " Erro

      MESSAGE e001(zca_upload_excel) INTO lv_message.
      et_return[] = VALUE #( BASE et_return ( type = 'S' id = 'ZCA_UPLOAD_EXCEL' number = '001' message = lv_message ) ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
