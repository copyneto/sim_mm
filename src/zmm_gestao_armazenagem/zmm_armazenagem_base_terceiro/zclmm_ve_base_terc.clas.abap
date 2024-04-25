CLASS zclmm_ve_base_terc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_sadl_exit .
    INTERFACES if_sadl_exit_calc_element_read .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclmm_ve_base_terc IMPLEMENTATION.


  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA lt_data TYPE TABLE OF zi_mm_base_terceiro_aux.
    MOVE-CORRESPONDING it_original_data TO lt_data.
    SELECT _c~DeliveryDocument,
           _a~DeliveryDocumentItem,
           _a~ReferenceSDDocument,
           _a~ReferenceSDDocumentItem,
           _a~BR_NotaFiscal,
           BR_NotaFiscalItem,
           BR_NFPriceAmountWithTaxes,
           br_nfenumber,
           br_nfseries,
           DeliveryDocumentBySupplier
      FROM zi_mm_base_nf_aux AS _a
INNER JOIN I_DeliveryDocument AS _c ON _c~DeliveryDocument = _a~DeliveryDocument
INNER JOIN i_br_nfdocument AS _b ON _a~BR_NotaFiscal = _b~BR_NotaFiscal
      INTO TABLE @DATA(lt_cds).

    SORT lt_cds BY DeliveryDocument
                   DeliveryDocumentItem.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      READ TABLE lt_cds INTO DATA(ls_cds) WITH KEY DeliveryDocument = <fs_data>-DeliveryDocument
                                                   DeliveryDocumentItem = <fs_data>-DeliveryDocumentItem BINARY SEARCH.

      <fs_data>-NotaFiscal = ls_cds-BR_NotaFiscal.
      <fs_data>-NotaFiscalItem = ls_cds-BR_NotaFiscalItem.
*      <fs_data>-valorperdaganho = ls_cds-BR_NFPriceAmountWithTaxes * <fs_data>-ActualDeliveredQtyInBaseUnit - <fs_data>-descarga.
      <fs_data>-valorperdaganho = ls_cds-BR_NFPriceAmountWithTaxes * <fs_data>-perdaganho.
      <fs_data>-BR_NFPriceAmountWithTaxes = ls_cds-BR_NFPriceAmountWithTaxes.

    ENDLOOP.

    MOVE-CORRESPONDING lt_data to ct_calculated_data.

  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
    RETURN.
  ENDMETHOD.
ENDCLASS.
