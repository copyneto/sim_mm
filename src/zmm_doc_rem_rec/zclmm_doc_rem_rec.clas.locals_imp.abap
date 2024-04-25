CLASS lcl_lhc_ZI_MM_DOC_REM_REC DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zi_mm_doc_rem_rec RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_mm_doc_rem_rec RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_mm_doc_rem_rec RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zi_mm_doc_rem_rec.

    METHODS soma FOR MODIFY
      IMPORTING keys FOR ACTION zi_mm_doc_rem_rec~soma.

ENDCLASS.

CLASS lcl_lhc_ZI_MM_DOC_REM_REC IMPLEMENTATION.

  METHOD get_instance_features.
    RETURN.
  ENDMETHOD.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD read.
    RETURN.
  ENDMETHOD.

  METHOD lock.
    RETURN.
  ENDMETHOD.

  METHOD soma.
    IF keys IS NOT INITIAL.
      SELECT *                                          "#EC CI_SEL_DEL
          FROM I_DeliveryDocument
          INTO TABLE @DATA(lt_doc_eliminar)
          FOR ALL ENTRIES IN @keys
          WHERE deliverydocument = @keys-Vbeln.

      IF sy-subrc NE 0.
        reported-zi_mm_doc_rem_rec = VALUE #( ( %msg = new_message(
                                                         id       = 'ZMM_MESSAGES'
                                                         number   = '003'
                                                         v1       = keys[ 1 ]-vbeln
                                                         severity = if_abap_behv_message=>severity-error  ) ) ).

        RETURN.
      ENDIF.
      IF lt_doc_eliminar IS NOT INITIAL.


        SELECT vbeln,                              "#EC CI_NO_TRANSFORM
               posnr,
               werks,
               lgort,
               ZtipoDoc,
               zstatusintegracao,
               menge,
               zqtrec,
               zqtpen,
               meins,
               cancel,
               mblnr,
               mjahr,
               movestloc,
               ernam,
               erdat,
               erzet,
               aenam,
               aedat,
               aezet,
               type,
               id,
               znumber,
               message
          FROM zi_mm_doc_rem_rec
          INTO TABLE @DATA(lt_ztbmmxxx1_aux)
          FOR ALL ENTRIES IN @lt_doc_eliminar
         WHERE vbeln EQ @lt_doc_eliminar-deliverydocument
           AND cancel EQ @abap_false.

        IF sy-subrc NE 0.
          reported-zi_mm_doc_rem_rec = VALUE #( ( %msg = new_message(
                                                           id       = 'ZMM_MESSAGES'
                                                           number   = '003'
                                                           v1       = keys[ 1 ]-vbeln
                                                           severity = if_abap_behv_message=>severity-error  ) ) ).
          RETURN.
        ENDIF.

      ENDIF.

    ENDIF.

    SORT lt_doc_eliminar BY OverallSDProcessStatus.
    DELETE lt_doc_eliminar WHERE OverallSDProcessStatus EQ 'C'. "#EC CI_STDSEQ

    IF lt_doc_eliminar is initial.
      reported-zi_mm_doc_rem_rec = VALUE #( ( %msg = new_message(
                                                       id       = 'ZMM_MESSAGES'
                                                       number   = '006'
                                                       v1       = keys[ 1 ]-vbeln
                                                       severity = if_abap_behv_message=>severity-error  ) ) ).
      RETURN.
    ENDIF.
    SORT lt_doc_eliminar BY DeliveryDocument.
    DATA lt_ztbmmxxx1_fucntion TYPE TABLE OF ztmm_doc_rem_rec.
    DATA ls_ztbmmxxx1_function TYPE ztmm_doc_rem_rec.
    SORT lt_doc_eliminar BY DeliveryDocument.
    LOOP AT lt_ztbmmxxx1_aux ASSIGNING FIELD-SYMBOL(<fs_ztbmxxx1_aux>).
      READ TABLE lt_doc_eliminar TRANSPORTING NO FIELDS WITH KEY DeliveryDocument = <fs_ztbmxxx1_aux>-Vbeln BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_ztbmxxx1_aux>-Cancel = abap_true.
        MOVE-CORRESPONDING <fs_ztbmxxx1_aux> TO ls_ztbmmxxx1_function.
        APPEND ls_ztbmmxxx1_function TO lt_ztbmmxxx1_fucntion.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'ZFMMM_WMS_ATUALIZA_TABXX1'
      STARTING NEW TASK 'COMMIT'
      EXPORTING
        iv_no_commit = abap_false
        it_ztbmmxxx1 = lt_ztbmmxxx1_fucntion.

    reported-zi_mm_doc_rem_rec = VALUE #( ( %msg = new_message(
                                                     id       = 'ZMM_MESSAGES'
                                                     number   = '005'
                                                     v1       = keys[ 1 ]-vbeln
                                                     severity = if_abap_behv_message=>severity-success  ) ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_lsc_ZI_MM_DOC_REM_REC DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lcl_lsc_ZI_MM_DOC_REM_REC IMPLEMENTATION.

  METHOD finalize.
    RETURN.
  ENDMETHOD.

  METHOD check_before_save.
    RETURN.
  ENDMETHOD.

  METHOD save.
    RETURN.
  ENDMETHOD.

  METHOD cleanup.
    RETURN.
  ENDMETHOD.

  METHOD cleanup_finalize.
    RETURN.
  ENDMETHOD.

ENDCLASS.
