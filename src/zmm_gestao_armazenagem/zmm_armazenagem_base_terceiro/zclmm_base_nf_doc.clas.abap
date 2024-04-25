CLASS zclmm_base_nf_doc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amdp_marker_hdb .
    INTERFACES if_oo_adt_classrun .
    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      ty_report   TYPE zi_mm_base_nf_doc,
      ty_t_report TYPE STANDARD TABLE OF ty_report.
ENDCLASS.



CLASS zclmm_base_nf_doc IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    RETURN.
  ENDMETHOD.


  METHOD if_rap_query_provider~select.

    DATA lt_report TYPE ty_t_report.
    DATA ls_report TYPE ty_report.

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

    LOOP AT Lt_cds ASSIGNING FIELD-SYMBOL(<fs_cds>).
      IF <fs_cds>-DeliveryDocumentBySupplier CS <fs_cds>-BR_NFeNumber.
        MOVE-CORRESPONDING <fs_cds> TO ls_report.
        ls_report-BR_NotaFiscalFormat = <fs_cds>-BR_NFeNumber.
        ls_report-BR_NotaFiscalItemFormat = <fs_cds>-BR_NFSeries.
      ENDIF.
    ENDLOOP.

    io_response->set_data( lt_report[] ).

  ENDMETHOD.
ENDCLASS.
