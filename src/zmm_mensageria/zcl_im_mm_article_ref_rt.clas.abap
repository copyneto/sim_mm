class ZCL_IM_MM_ARTICLE_REF_RT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_BADI_ARTICLE_REF_RT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_ARTICLE_REF_RT IMPLEMENTATION.


  method IF_EX_BADI_ARTICLE_REF_RT~ADD_INACTIVE_FIELDS.
  endmethod.


  method IF_EX_BADI_ARTICLE_REF_RT~IMPORT_AT_UPDATE_TASK.
  endmethod.


  method IF_EX_BADI_ARTICLE_REF_RT~REFERENZ.
  endmethod.


  METHOD if_ex_badi_article_ref_rt~referenz_after.
    CONSTANTS: lc_mm41 TYPE sy-tcode VALUE 'MM41',
               lc_mm42 TYPE sy-tcode VALUE 'MM42',
               lc_save TYPE sy-ucomm VALUE 'BU',
               lc_yes  TYPE sy-ucomm VALUE 'YES'.

    IF ( sy-tcode = lc_mm41 OR
         sy-tcode = lc_mm42 ) AND
       ( sy-ucomm = lc_save OR
         sy-ucomm = lc_yes  ).
      NEW zclsd_valida_mensageria(  )->valida_produto_mm( iv_matnr = badi_rmmg1-matnr ).
    ENDIF.
  ENDMETHOD.


  method IF_EX_BADI_ARTICLE_REF_RT~REFERENZ_BEFORE.
  endmethod.


  method IF_EX_BADI_ARTICLE_REF_RT~TRANSPORT_TO_UPDATE_TASK.
  endmethod.
ENDCLASS.
