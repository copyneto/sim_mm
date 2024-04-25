CLASS zclmm_pdm_material DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_hierar TYPE STANDARD TABLE OF bapi_wrf_hier_change_items WITH DEFAULT KEY,
      ty_return TYPE STANDARD TABLE OF bapireturn1 WITH DEFAULT KEY.

    DATA: gs_material TYPE zsmm_pdm_material_int.

    METHODS constructor
      IMPORTING
        is_material TYPE zsmm_pdm_material_int.

    METHODS: execute
      RETURNING VALUE(rt_return) TYPE ty_return.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      gt_hier   TYPE ty_hierar,
      gt_return TYPE ty_return.

    METHODS bapi_get_number.
    METHODS process_data.
    METHODS call_maintaindata.
    METHODS fill_header_main.
    METHODS fill_clientdata.
    METHODS fill_addnlclientdata.
    METHODS fill_materialdesc.
    METHODS fill_plantdata.
    METHODS fill_valutiondata.
    METHODS fill_postdata.
    METHODS fill_unitoftext.
    METHODS fill_salesdata.
    METHODS fill_international.
    METHODS fill_unitofmeasures.
    METHODS fill_charact.
    METHODS fill_hierarchy.
    METHODS fill_posdata.
    METHODS fill_unitofmeasurestext.
    METHODS call_fm_material.
    METHODS conv
      IMPORTING
        iv_unit        TYPE bapie1mamtrt-alt_unit
      RETURNING
        VALUE(rv_unit) TYPE bapie1mamtrt-alt_unit.
ENDCLASS.



CLASS zclmm_pdm_material IMPLEMENTATION.


  METHOD constructor.

    gs_material = is_material.

  ENDMETHOD.


  METHOD execute.

    IF gs_material-headdata-material IS INITIAL.
      bapi_get_number(  ).
    ENDIF.

    process_data(  ).

    rt_return = gt_return.

  ENDMETHOD.


  METHOD bapi_get_number.

    DATA:
      lt_ret    TYPE bapireturn1,
      lt_number TYPE STANDARD TABLE OF bapimatinr.

    CALL FUNCTION 'BAPI_MATERIAL_GETINTNUMBERRET'
      EXPORTING
        material_type   = gs_material-headdata-matl_type
      IMPORTING
        return          = lt_ret
      TABLES
        material_number = lt_number.

    IF lt_number IS NOT INITIAL.

      DATA(ls_material) = VALUE #( lt_number[ 1 ] OPTIONAL ).
      gs_material-headdata-material = ls_material-material.
      gs_material-headdata-material_external = ls_material-material_external.
      gs_material-headdata-material_guid = ls_material-material_guid.
      gs_material-headdata-material_long = ls_material-material_long.
      gs_material-headdata-material_version = ls_material-material_version.

    ENDIF.

  ENDMETHOD.


  METHOD process_data.

    call_maintaindata(  ).

  ENDMETHOD.


  METHOD call_maintaindata.

    fill_header_main(  ).
    fill_clientdata(  ).
    fill_addnlclientdata(  ).
    fill_materialdesc(  ).
    fill_plantdata(  ).
    fill_valutiondata( ).
    fill_postdata(  ).
    fill_unitoftext(  ).
    fill_salesdata(  ).
    fill_international(  ).
    fill_unitofmeasures(  ).
    fill_charact(  ).
    fill_hierarchy(  ).
    fill_posdata(  ).
    fill_unitofmeasurestext(  ).

    call_fm_material(  ).

  ENDMETHOD.


  METHOD fill_header_main.

    gs_material-headdata-no_appl_log = abap_true.
    gs_material-headdata-no_change_doc = abap_true.

    CHECK gs_material-headdata-material IS NOT INITIAL.

    UNPACK gs_material-headdata-material TO gs_material-headdata-material.

  ENDMETHOD.


  METHOD fill_clientdata.

    DATA: lv_count TYPE i.

    LOOP AT gs_material-clientdata ASSIGNING FIELD-SYMBOL(<fs_clientdata>).

      lv_count = lv_count + 1.

      <fs_clientdata>-material = COND #(  WHEN <fs_clientdata>-material IS INITIAL THEN gs_material-headdata-material
                                                                                                                                         ELSE |{ <fs_clientdata>-material ALPHA = IN }| ).

      <fs_clientdata>-base_uom = conv( <fs_clientdata>-base_uom ).

      gs_material-clientdatax[ lv_count ]-material = <fs_clientdata>-material.
      gs_material-clientdatax[ lv_count ]-base_uom = abap_true.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_addnlclientdata.

    DATA: lv_count TYPE i.

    LOOP AT gs_material-addnlclientdata ASSIGNING FIELD-SYMBOL(<fs_addnlclientdata>).

      lv_count = lv_count + 1.

      <fs_addnlclientdata>-material = COND #(  WHEN <fs_addnlclientdata>-material IS INITIAL THEN gs_material-headdata-material
                                                                                                                                         ELSE |{ <fs_addnlclientdata>-material ALPHA = IN }| ).
      gs_material-addnlclientdatax[ lv_count ]-material = <fs_addnlclientdata>-material.


    ENDLOOP.

  ENDMETHOD.


  METHOD fill_materialdesc.

    LOOP AT gs_material-materialdescription ASSIGNING FIELD-SYMBOL(<fs_materialdescription>).

      <fs_materialdescription>-material = COND #(  WHEN  <fs_materialdescription>-material  IS INITIAL THEN gs_material-headdata-material
                                                                                                                                                                         ELSE |{ <fs_materialdescription>-material  ALPHA = IN }| ).

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_plantdata.

    DATA: lv_count TYPE i.

    LOOP AT  gs_material-plantdata ASSIGNING FIELD-SYMBOL(<fs_plantdata>).

      lv_count = lv_count + 1.

      <fs_plantdata>-material = COND #(  WHEN  <fs_plantdata>-material  IS INITIAL THEN gs_material-headdata-material
                                                                                                                                         ELSE |{ <fs_plantdata>-material  ALPHA = IN }| ).
      gs_material-plantdatax[ lv_count ]-material = <fs_plantdata>-material.

    ENDLOOP..

  ENDMETHOD.


  METHOD fill_valutiondata.

    DATA: lv_count TYPE i.

    LOOP AT  gs_material-valuationdata ASSIGNING FIELD-SYMBOL(<fs_valuationdata>).

      lv_count = lv_count + 1.

      <fs_valuationdata>-material = COND #(  WHEN <fs_valuationdata>-material  IS INITIAL THEN gs_material-headdata-material
                                                                                                                                                      ELSE |{ <fs_valuationdata>-material  ALPHA = IN }| ).
      gs_material-valuationdatax[ lv_count  ]-material = <fs_valuationdata>-material.

    ENDLOOP..

  ENDMETHOD.


  METHOD fill_postdata.

    DATA: lv_count TYPE i.

    LOOP AT  gs_material-posdata ASSIGNING FIELD-SYMBOL(<fs_posdata>).

      lv_count = lv_count + 1.

      <fs_posdata>-material = COND #(  WHEN <fs_posdata>-material   IS INITIAL THEN gs_material-headdata-material
                                                                                                                                    ELSE |{ <fs_posdata>-material   ALPHA = IN }| ).
      gs_material-posdatax[ lv_count ]-material = <fs_posdata>-material.

    ENDLOOP..

  ENDMETHOD.


  METHOD fill_unitoftext.

    LOOP AT  gs_material-unitofmeasuretexts ASSIGNING FIELD-SYMBOL(<fs_unitofmeasuretexts>).

      <fs_unitofmeasuretexts>-alt_unit = conv( <fs_unitofmeasuretexts>-alt_unit ).

      <fs_unitofmeasuretexts>-material = COND #(  WHEN <fs_unitofmeasuretexts>-material  IS INITIAL THEN gs_material-headdata-material
                                                                                                                                                                        ELSE |{ <fs_unitofmeasuretexts>-material   ALPHA = IN }| ).
      <fs_unitofmeasuretexts>-text_id = '02'.
      <fs_unitofmeasuretexts>-consec_no = '01'.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_salesdata.

    DATA: lv_count TYPE i.

    LOOP AT gs_material-salesdata ASSIGNING FIELD-SYMBOL(<fs_salesdata>).

      lv_count = lv_count + 1.

      <fs_salesdata>-material = COND #(  WHEN <fs_salesdata>-material  IS INITIAL THEN gs_material-headdata-material
                                                                                                                                        ELSE |{ <fs_salesdata>-material   ALPHA = IN }| ).
      gs_material-salesdatax[ lv_count ]-material = <fs_salesdata>-material.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_international.

    LOOP AT  gs_material-internationalartnos ASSIGNING FIELD-SYMBOL(<fs_internationalartnos>).

      <fs_internationalartnos>-unit = conv( <fs_internationalartnos>-unit  ).

      <fs_internationalartnos>-material = COND #(  WHEN <fs_internationalartnos>-material IS INITIAL THEN gs_material-headdata-material
                                                                                                                                                                      ELSE |{ <fs_internationalartnos>-material  ALPHA = IN }| ).

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_unitofmeasures.

    DATA: lv_count TYPE i.

    LOOP AT gs_material-unitsofmeasure ASSIGNING FIELD-SYMBOL(<fs_unitsofmeasure>).

      lv_count = lv_count + 1.

      <fs_unitsofmeasure>-alt_unit = conv( <fs_unitsofmeasure>-alt_unit ).

      gs_material-unitsofmeasurex[ lv_count ]-alt_unit = conv( gs_material-unitsofmeasurex[ lv_count ]-alt_unit ).

      <fs_unitsofmeasure>-material = COND #(  WHEN <fs_unitsofmeasure>-material IS INITIAL THEN gs_material-headdata-material
                                                                                                                                                           ELSE |{ <fs_unitsofmeasure>-material ALPHA = IN }| ).
      gs_material-unitsofmeasurex[ lv_count ]-material = gs_material-headdata-material.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_charact.

    DATA: lv_count TYPE i.

    LOOP AT gs_material-characteristicvalue ASSIGNING FIELD-SYMBOL(<fs_characteristicvalue>).

      lv_count = lv_count + 1.

      <fs_characteristicvalue>-material  = COND #(  WHEN <fs_characteristicvalue>-material IS INITIAL THEN gs_material-headdata-material
                                                                                                                                                                      ELSE |{ <fs_characteristicvalue>-material ALPHA = IN }| ).
      gs_material-characteristicvaluex[ lv_count  ]-material = gs_material-headdata-material.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_hierarchy.

    LOOP AT gs_material-hierarchy_items ASSIGNING FIELD-SYMBOL(<fs_hierarchy_items>).

      REPLACE ALL OCCURRENCES OF '.' IN <fs_hierarchy_items>-date_from WITH space.
      REPLACE ALL OCCURRENCES OF '.' IN <fs_hierarchy_items>-date_to WITH space.

      CONDENSE: <fs_hierarchy_items>-date_from,
                          <fs_hierarchy_items>-date_to NO-GAPS.

      <fs_hierarchy_items>-date_from = <fs_hierarchy_items>-date_from+4(4) && <fs_hierarchy_items>-date_from+2(2)  &&  <fs_hierarchy_items>-date_from(2).
      <fs_hierarchy_items>-date_to = <fs_hierarchy_items>-date_to+4(4) && <fs_hierarchy_items>-date_to+2(2)  &&  <fs_hierarchy_items>-date_to(2).

      <fs_hierarchy_items>-matnr   = COND #(  WHEN <fs_hierarchy_items>-matnr IS INITIAL THEN gs_material-headdata-material
                                                                                                                                                      ELSE |{ <fs_hierarchy_items>-matnr  ALPHA = IN }| ).

      <fs_hierarchy_items>-matnr_long   = COND #(  WHEN <fs_hierarchy_items>-matnr_long IS INITIAL THEN gs_material-headdata-material
                                                                                                                                                                        ELSE |{ <fs_hierarchy_items>-matnr_long  ALPHA = IN }| ).

    ENDLOOP.

    gt_hier = CORRESPONDING #( gs_material-hierarchy_items ).

  ENDMETHOD.


  METHOD fill_posdata.

    DATA: lv_count TYPE i.

    LOOP AT gs_material-posdata ASSIGNING FIELD-SYMBOL(<fs_posdata>).

      lv_count = lv_count + 1.

      CHECK  <fs_posdata>-material IS INITIAL.

      <fs_posdata>-material = gs_material-headdata-material.
      gs_material-posdatax[ sy-tabix ]-material = gs_material-headdata-material.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_unitofmeasurestext.

    LOOP AT gs_material-unitofmeasuretexts ASSIGNING FIELD-SYMBOL(<fs_unitofmeasuretexts>).
      <fs_unitofmeasuretexts>-material = gs_material-headdata-material.
      <fs_unitofmeasuretexts>-text_id = '02'.
      <fs_unitofmeasuretexts>-consec_no = '01'.
    ENDLOOP.

  ENDMETHOD.


  METHOD call_fm_material.

    DATA:
      lv_main_data_error       TYPE boolean,
      lv_additional_data_error TYPE boolean,
      lt_return                TYPE STANDARD TABLE OF bapireturn1.

    CALL FUNCTION 'WRF_MATERIAL_MAINTAINDATA_RT'
      EXPORTING
        headdata                 = gs_material-headdata
        hierarchy_data           = gs_material-hierarchy_data
      IMPORTING
        ev_main_data_error       = lv_main_data_error
        ev_additional_data_error = lv_additional_data_error
      TABLES
        return                   = gt_return
        characteristicvalue      = gs_material-characteristicvalue
        characteristicvaluex     = gs_material-characteristicvaluex
        clientdata               = gs_material-clientdata
        clientdatax              = gs_material-clientdatax
        addnlclientdata          = gs_material-addnlclientdata
        addnlclientdatax         = gs_material-addnlclientdatax
        materialdescription      = gs_material-materialdescription
        plantdata                = gs_material-plantdata
        plantdatax               = gs_material-plantdatax
        unitsofmeasure           = gs_material-unitsofmeasure
        unitsofmeasurex          = gs_material-unitsofmeasurex
        unitofmeasuretexts       = gs_material-unitofmeasuretexts
        internationalartnos      = gs_material-internationalartnos
        valuationdata            = gs_material-valuationdata
        valuationdatax           = gs_material-valuationdatax
        salesdata                = gs_material-salesdata
        salesdatax               = gs_material-salesdatax
        posdata                  = gs_material-posdata
        posdatax                 = gs_material-posdatax
        hierarchy_items          = gt_hier.

    IF line_exists( gt_return[ type = 'E' ] ).

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD conv.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = iv_unit
        language       = 'P'
      IMPORTING
        output         = rv_unit
      EXCEPTIONS
        unit_not_found = 01.

    IF sy-subrc NE 0.
      rv_unit = iv_unit.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
