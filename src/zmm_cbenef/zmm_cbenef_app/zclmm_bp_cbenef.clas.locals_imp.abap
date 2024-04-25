CLASS lhc_ZI_MM_CBENEF DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_mm_cbenef RESULT result.
    METHODS valida_campos FOR VALIDATE ON SAVE
      IMPORTING keys FOR zi_mm_cbenef~valida_campos.

ENDCLASS.

CLASS lhc_ZI_MM_CBENEF IMPLEMENTATION.

  METHOD get_instance_authorizations.
    RETURN.
  ENDMETHOD.

  METHOD valida_campos.

    READ ENTITIES OF ZI_MM_CBENEF IN LOCAL MODE
        ENTITY ZI_MM_CBENEF
        ALL FIELDS WITH CORRESPONDING #( keys )
        RESULT DATA(lt_data)
        REPORTED DATA(lt_reported).



    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      IF <fs_data>-Nbm IS INITIAL AND <fs_data>-Matnr IS INITIAL.



        reported-zi_mm_cbenef = VALUE #( ( %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                           text     = TEXT-001 ) ) ).

*        reported-zi_mm_cbenef = VALUE #( ( %element-%field-Nbm = if_abap_behv=>mk-on
*                                           %element-%field-Matnr = if_abap_behv=>mk-on
*                                           %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error
*                                           text     = 'Informe o campo NCM ou Material' ) ) ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
