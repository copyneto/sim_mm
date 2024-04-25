*&---------------------------------------------------------------------*
*& Include ZMMI_CHECK_FEATURES_ITEM
*&---------------------------------------------------------------------*
    CONSTANTS: gc_feature_id TYPE purchasing_document_feature VALUE 'ITEM_CAT'.

    DELETE ct_feature_check_result WHERE feature_id     = gc_feature_id
                                     AND is_advanced_po = abap_true.
