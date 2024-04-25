CLASS lcl_InventarioWMS DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR InventarioWMS RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR InventarioWMS RESULT result.

    METHODS enviardocs FOR MODIFY
      IMPORTING keys FOR ACTION InventarioWMS~enviardocs.

ENDCLASS.

CLASS lcl_InventarioWMS IMPLEMENTATION.

  METHOD get_instance_features.
    RETURN.
  ENDMETHOD.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD enviardocs.
    CHECK keys IS NOT INITIAL.

    READ ENTITIES OF zI_mm_wms_doc_inventario IN LOCAL MODE ENTITY InventarioWMS
     ALL FIELDS WITH CORRESPONDING #( keys )
  RESULT DATA(lt_result).

    DATA(lt_documentos) = CORRESPONDING zclmm_wms_integrar_docinv=>ty_t_doc_inventario( lt_result ).

    DATA(lt_return) = NEW zclmm_wms_integrar_docinv( )->exec_integracao(
      EXPORTING
        it_documentos = lt_documentos
    ).

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      reported-inventariowms = VALUE #(
        BASE reported-inventariowms ( %msg = new_message(
           id       = <fs_return>-id
           number   = <fs_return>-number
           v1       = <fs_return>-message_v1
           v2       = <fs_return>-message_v2
           v3       = <fs_return>-message_v3
           v4       = <fs_return>-message_v4
           severity = CONV #( <fs_return>-type )
      ) ) ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_ZI_MM_WMS_DOC_INVENTARIO DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lcl_ZI_MM_WMS_DOC_INVENTARIO IMPLEMENTATION.

  METHOD save_modified.
    RETURN.
  ENDMETHOD.

  METHOD cleanup_finalize.
    RETURN.
  ENDMETHOD.

ENDCLASS.
