CLASS zclmm_movi_merc_imp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zclmm_movi_merc_imp .

    METHODS build
      IMPORTING
                it_filters         TYPE  if_rap_query_filter=>tt_name_range_pairs
      RETURNING VALUE(rt_messages) TYPE bapiret2_t.

    METHODS setup_messages
      IMPORTING
        VALUE(p_task) TYPE clike OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_filters,
             vbeln TYPE RANGE OF vbeln_vl,
           END OF ty_filters.

    CLASS-DATA go_instance TYPE REF TO zclmm_movi_merc_imp.

    DATA: gt_messages   TYPE bapiret2_t.
    DATA: gs_filters    TYPE ty_filters.
    DATA: gv_wait_async TYPE bool.

    METHODS get_filters
      IMPORTING it_filters TYPE if_rap_query_filter=>tt_name_range_pairs.

    METHODS call_job.

ENDCLASS.



CLASS zclmm_movi_merc_imp IMPLEMENTATION.

  METHOD get_instance.
    IF ( go_instance IS INITIAL ).
      go_instance = NEW zclmm_movi_merc_imp( ).
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.

  METHOD build.

    me->get_filters( it_filters ).

    CHECK gs_filters IS NOT INITIAL.

    me->call_job( ).

    "me->get_data( IMPORTING es_data = DATA(ls_data) ).

    "CHECK ls_data IS NOT INITIAL.

*    rv_pdf = me->get_smartform( is_header = ls_header
*                                it_item   = lt_item ).

    rt_messages = gt_messages.

  ENDMETHOD.

  METHOD get_filters.
    LOOP AT it_filters ASSIGNING FIELD-SYMBOL(<fs_filters>).

      CASE <fs_filters>-name.
        WHEN 'VBELN'.
          gs_filters-vbeln = CORRESPONDING #( <fs_filters>-range ).
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD call_job.

    gv_wait_async = abap_false.

    CALL FUNCTION 'ZFMMM_EXEC_MOVI_MERC'
      STARTING NEW TASK 'TASK'
      CALLING setup_messages ON END OF TASK
      EXPORTING
        it_r_vbeln    = gs_filters-vbeln.

    WAIT UNTIL gv_wait_async = abap_true.

  ENDMETHOD.

  METHOD setup_messages.

    RECEIVE RESULTS FROM FUNCTION 'ZFMMM_EXEC_MOVI_MERC'
          IMPORTING
            et_messages        = gt_messages.

    gv_wait_async = abap_true.

  ENDMETHOD.

ENDCLASS.
