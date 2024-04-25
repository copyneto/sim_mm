CLASS zclmm_j_1bnfe_in DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class CL_J_1BNFE_IN_EXAMPLE
*"* do not include other source files here!!!

    INTERFACES if_badi_interface .
    INTERFACES if_j_1bnfe_in .
  PROTECTED SECTION.
*"* protected components of class CL_J_1BNFE_IN_EXAMPLE
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class CL_J_1BNFE_IN_EXAMPLE
*"* do not include other source files here!!!
ENDCLASS.



CLASS zclmm_j_1bnfe_in IMPLEMENTATION.


  METHOD if_j_1bnfe_in~change_sloc_and_valtype.
    RETURN.
  ENDMETHOD.


  METHOD if_j_1bnfe_in~check_invoice.
    RETURN.
  ENDMETHOD.


  METHOD if_j_1bnfe_in~inbound_deliv_change_header.
    RETURN.
  ENDMETHOD.


  METHOD if_j_1bnfe_in~inbound_deliv_delete_check.
    RETURN.
  ENDMETHOD.


  METHOD if_j_1bnfe_in~inbound_deliv_search.
    RETURN.
  ENDMETHOD.


  METHOD if_j_1bnfe_in~material_conversion.
    RETURN.
  ENDMETHOD.


  METHOD if_j_1bnfe_in~set_ekpo_buffer_refresh.
    RETURN.
  ENDMETHOD.


  METHOD if_j_1bnfe_in~unit_conversion.
* if system could not convert incoming unit of measure
* try to convert using other language

*    CHECK c_erfme IS INITIAL.

    DATA(lv_mseh3) = i_mseh3.
    IF lv_mseh3 IS NOT INITIAL.
      TRANSLATE lv_mseh3  TO UPPER CASE.
    ENDIF.

    SELECT SINGLE z~uom_int FROM i_purchaseorder AS item
            INNER JOIN I_PurchaseOrderItem AS header
            ON item~PurchaseOrder = header~PurchaseOrder
            INNER JOIN ztmm_conv_medida AS z
            ON   z~matnr = header~Material
            AND z~lifnr = item~supplier
            AND z~cprod = @i_xml_matnr
            AND z~uom_ext = @lv_mseh3
                WHERE header~PurchaseOrder  = @i_ebeln
                AND header~PurchaseOrderItem = @i_ebelp
                INTO @DATA(lv_uom_int).

    CHECK lv_uom_int IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = lv_uom_int
        language       = sy-langu
      IMPORTING
        output         = c_erfme
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    IF sy-subrc = 0.
      CALL FUNCTION 'UNIT_OF_MEASURE_SAP_TO_ISO'
        EXPORTING
          sap_code = c_erfme
        IMPORTING
          iso_code = c_erfme_iso.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
