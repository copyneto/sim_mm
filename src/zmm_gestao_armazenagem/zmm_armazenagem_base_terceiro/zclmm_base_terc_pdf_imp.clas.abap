CLASS zclmm_base_terc_pdf_imp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zclmm_base_terc_pdf_imp.

    METHODS build
      IMPORTING
*                iv_SalesOrder     TYPE vdm_sales_order
*                iv_SalesOrderItem TYPE sales_order_item
*                iv_Deposito       TYPE lgort_d
                it_filters    TYPE  if_rap_query_request=>tt_parameters
      RETURNING VALUE(rv_pdf) TYPE ze_rawstring.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_instance TYPE REF TO zclmm_base_terc_pdf_imp.

    TYPES: BEGIN OF ty_filters,
             DeliveryDocument     TYPE vbeln_vl,
             DeliveryDocumentItem TYPE posnr_vl,
             qtdpopup             TYPE matnr,
           END OF ty_filters.

    DATA: gs_filters TYPE ty_filters.

    METHODS get_filters
      IMPORTING it_filters TYPE if_rap_query_request=>tt_parameters.

    "! Busca função
    "! @parameter rv_func | Retorna a função
    METHODS get_function
      RETURNING
        VALUE(rv_func) TYPE rs38l_fnam .

    "! Converte OTF para envio por email
    "! @parameter it_otf | Converte otf
    METHODS convert_otf
      IMPORTING
                 it_otf      TYPE tt_itcoo
      EXPORTING  ev_pdf_file TYPE xstring
      EXCEPTIONS not_convert_otf .
ENDCLASS.



CLASS ZCLMM_BASE_TERC_PDF_IMP IMPLEMENTATION.


  METHOD build.
    CONSTANTS: lc_zc TYPE ze_status_armaz VALUE 'ZC'.

    me->get_filters( it_filters ).

*    CHECK gs_filters IS NOT INITIAL.

    DATA(lo_object) = zclmm_notas_debito=>get_instance(  ).

    DATA lv_numc TYPE  dlydy.
    DATA lv_netw TYPE netwr.
    lv_numc = gs_filters-qtdpopup.

    SELECT SINGLE deliverydocument,
                  deliverydocumentitem,
                  notafiscal,
                  notafiscalitem,
                  semaforo,
                  deliverydocumentbysupplier,
                  documentdate,
                  plant,
                  werkscodename,
                  material,
                  materialname,
                  descarga,
                  actualdeliveredqtyinbaseunit,
                  perdaganho,
                  baseunit,
                  valorperdaganho,
                  br_nfpriceamountwithtaxes,
                  supplier,
                  docnumremessa,
                  status,
                  estoqueajustado,
                  docmatajest,
                  docnumretorno,
                  tipomov,
                  motivomov,
                  storagelocation,
                  bp_cliente,
                  agente_frete,
                  info_danfe,
                  placa,
                  modalidade_frete
        FROM zi_mm_base_terceiro_aux
        INTO @DATA(ls_base)
        WHERE DeliveryDocument = @gs_filters-deliverydocument
        AND   DeliveryDocumentItem = @gs_filters-deliverydocumentitem.

    SELECT SINGLE _c~DeliveryDocument,
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
      INTO @DATA(ls_cds)
      WHERE _c~deliverydocument = @ls_base-deliverydocument
        AND _a~deliverydocumentitem = @ls_base-deliverydocumentitem.

    ls_base-valorperdaganho = ls_cds-BR_NFPriceAmountWithTaxes * ls_base-perdaganho.
    lv_netw = abs( ls_base-valorperdaganho ).

    SELECT SINGLE  mandt,
                   deliverydocument,
                   deliverydocumentitem,
                   notafiscal,
                   notafiscalitem,
                   descarga,
                   docnumremessa,
                   docnumretorno,
                   docmatajest,
                   tipomov,
                   motivomov,
                   status,
                   estoqueajustado,
                   bp_cliente,
                   modalidade_frete,
                   agente_frete,
                   placa,
                   info_danfe
        FROM ztmm_base_terc
        INTO @DATA(ls_table)
        WHERE DeliveryDocument = @gs_filters-deliverydocument
        AND   DeliveryDocumentItem = @gs_filters-deliverydocumentitem.

    ls_table-status = lc_zc.

    MODIFY ztmm_base_terc FROM ls_table.

    data lv_docmat TYPE c LENGTH 20.
    lv_docmat = ls_table-docmatajest.

    rv_pdf = lo_object->m_main( iv_centro = ls_base-Plant
                                iv_dias = lv_numc
                                iv_grund = ls_base-motivomov
                                iv_supplier = ls_base-Supplier
                                iv_agente_frete = ls_base-agente_frete
                                iv_value = lv_netw
                                iv_tipomov = ls_table-tipomov
                                iv_motivomov = ls_table-motivomov
                                iv_docmatajest = lv_docmat
                                iv_DocumentItem = ls_base-DeliveryDocumentItem
                                ).

  ENDMETHOD.


  METHOD convert_otf.
    RETURN.
  ENDMETHOD.


  METHOD get_filters.
  CONSTANTS: lc_1 TYPE string value 'QTDPOPUP',
             lc_2 TYPE string VALUE 'DELIVERYDOCUMENT',
             lc_3 TYPE string VALUE 'DELIVERYDOCUMENTITEM'.
    LOOP AT it_filters ASSIGNING FIELD-SYMBOL(<fs_filters>).
      CASE <fs_filters>-parameter_name.
        WHEN lc_1.
          gs_filters-qtdpopup = <fs_filters>-value.
        WHEN lc_2.
          gs_filters-deliverydocument = <fs_filters>-value.
        WHEN lc_3.
          gs_filters-deliverydocumentitem = <fs_filters>-value.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_function.
    RETURN.
  ENDMETHOD.


  METHOD get_instance.
    IF ( go_instance IS INITIAL ).
      go_instance = NEW zclmm_base_terc_pdf_imp( ).
    ENDIF.

    ro_instance = go_instance.
  ENDMETHOD.
ENDCLASS.
