CLASS zclmm_nota_armazenagem DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_mseg IMPORTING is_mseg_key      TYPE mseg_key
                     RETURNING VALUE(rs_result) TYPE mseg.
    METHODS get_text IMPORTING iv_notafiscal     TYPE mseg-lfbnr
                               iv_notafiscalitem TYPE mseg-lfpos
                     RETURNING VALUE(rv_text)    TYPE string.
    METHODS get_preco_liquido IMPORTING iv_notafiscal           TYPE mseg-lfbnr
                                        iv_notafiscalitem       TYPE mseg-lfpos
                              RETURNING VALUE(rv_preco_liquido) TYPE j_1bnfpri.
    METHODS select_preco_liquido IMPORTING iv_notafiscal           TYPE mseg-lfbnr
                                           iv_notafiscalitem       TYPE mseg-lfpos
                                 RETURNING VALUE(rv_preco_liquido) TYPE j_1bnfpri.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLMM_NOTA_ARMAZENAGEM IMPLEMENTATION.


  METHOD get_mseg.
    SELECT SINGLE lfbnr,
                  lfpos
      FROM mseg
      INTO CORRESPONDING FIELDS OF @rs_result
      WHERE mblnr EQ @is_mseg_key-mblnr
        AND mjahr EQ @is_mseg_key-mjahr
        AND zeile EQ @is_mseg_key-zeile.
  ENDMETHOD.


  METHOD get_text.
    SELECT info_danfe
      FROM ztmm_base_propr
      INTO TABLE @DATA(lt_frete)
      WHERE notafiscal     = @iv_notafiscal
        AND notafiscalitem = @iv_notafiscalitem.

    SELECT info_danfe
      FROM ztmm_base_terc
      WHERE notafiscal     = @iv_notafiscal
        AND notafiscalitem = @iv_notafiscalitem
      APPENDING TABLE @lt_frete.

    IF lt_frete IS NOT INITIAL.
      DATA(ls_frete) = lt_frete[ 1 ].
      rv_text = ls_frete-info_danfe.
    ENDIF.
  ENDMETHOD.


  METHOD get_preco_liquido.
    CONSTANTS lc_select_preco_liquido TYPE seocpdname VALUE 'SELECT_PRECO_LIQUIDO'.

    CALL METHOD (lc_select_preco_liquido)
      EXPORTING
        iv_notafiscal     = iv_notafiscal
        iv_notafiscalitem = iv_notafiscalitem
      RECEIVING
        rv_preco_liquido  = rv_preco_liquido.
  ENDMETHOD.


  METHOD select_preco_liquido.
    SELECT SINGLE netpriceamount " br_nfpriceamountwithtaxes
      FROM i_br_nfitem
      INTO @rv_preco_liquido
      WHERE br_notafiscal     EQ @iv_notafiscal
        AND br_notafiscalitem EQ @iv_notafiscalitem.
  ENDMETHOD.
ENDCLASS.
