CLASS zclmm_exec_processo_imp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zclmm_exec_processo_imp .

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
             vbeln             TYPE RANGE OF vbeln_vl,
             ztipo_doc         TYPE RANGE OF ze_tipo_doc,
             ZstatusIntegracao TYPE RANGE OF ze_status_int,
           END OF ty_filters.

    CLASS-DATA go_instance TYPE REF TO zclmm_exec_processo_imp.

    DATA: gt_messages   TYPE bapiret2_t.
    DATA: gs_filters    TYPE ty_filters.
    DATA: gv_wait_async TYPE bool.

    METHODS get_filters
      IMPORTING it_filters TYPE if_rap_query_filter=>tt_name_range_pairs.

    METHODS call_job.

ENDCLASS.



CLASS zclmm_exec_processo_imp IMPLEMENTATION.
  METHOD get_instance.
    IF ( go_instance IS INITIAL ).
      go_instance = NEW zclmm_exec_processo_imp( ).
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.

  METHOD build.

    me->get_filters( it_filters ).

    CHECK gs_filters IS NOT INITIAL.

    me->call_job( ).

    rt_messages = gt_messages.

  ENDMETHOD.

  METHOD get_filters.
    LOOP AT it_filters ASSIGNING FIELD-SYMBOL(<fs_filters>).

      CASE <fs_filters>-name.
        WHEN 'VBELN'.
          gs_filters-vbeln = CORRESPONDING #( <fs_filters>-range ).
        WHEN 'ZTIPODOC'.
          gs_filters-ztipo_doc = CORRESPONDING #( <fs_filters>-range ).
        WHEN 'ZSTATUSINTEGRACAO'.
          gs_filters-zstatusintegracao = CORRESPONDING #( <fs_filters>-range ).
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD call_job.
    DATA lv_status_int TYPE ze_status_int.
    gv_wait_async = abap_false.
    lv_status_int = gs_filters-zstatusintegracao[ 1 ]-low.

    CALL FUNCTION 'ZFMMM_EXEC_PROCE'
      STARTING NEW TASK 'TASK'
      CALLING setup_messages ON END OF TASK
      EXPORTING
        it_r_vbeln      = gs_filters-vbeln
        it_r_tipo_doc   = gs_filters-ztipo_doc
        iv_status_int   = lv_status_int.

    WAIT UNTIL gv_wait_async = abap_true.

  ENDMETHOD.

  METHOD setup_messages.

    RECEIVE RESULTS FROM FUNCTION 'ZFMMM_EXEC_PROCE'
          IMPORTING
            et_messages        = gt_messages.

    gv_wait_async = abap_true.

  ENDMETHOD.

ENDCLASS.

