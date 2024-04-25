class ZCLMM_SHP_DELIVERY definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_DELIVERY_PROC .
protected section.
private section.
ENDCLASS.



CLASS ZCLMM_SHP_DELIVERY IMPLEMENTATION.


  METHOD if_ex_le_shp_delivery_proc~fill_delivery_item.
    IF is_likp-verur IS NOT INITIAL AND
      is_likp-lfart EQ 'EL' AND
      cs_lips-vgtyp EQ 'V'.
    ELSE.
      RETURN.
    ENDIF.
    DATA lv_vbeln TYPE lips-vbeln.
    lv_vbeln = is_likp-lifex.
    SELECT SINGLE b~bwart_next,
                  a~vbeln,
                  a~posnr,
                  a~bwart
           FROM lips AS a
           JOIN t156n AS b "#EC CI_BUFFJOIN
             ON b~bwart EQ a~bwart
          WHERE a~vbeln EQ @lv_vbeln
            AND a~posnr EQ @cs_lips-lifexpos
            AND b~fcode = 'WEUB'
   INTO @DATA(ls_bwart_next).

    IF sy-subrc EQ 0.
      cs_lips-bwart = ls_bwart_next-bwart_next.
    ELSE.
      CLEAR: ls_bwart_next.
    ENDIF.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~save_document_prepare.
    IF ct_xlips IS NOT INITIAL.
      SELECT  vbeln,
              posnr,
              werks,
              lgort,
              ztipo_doc,
              zstatus_integracao,
              menge,
              zqtrec,
              zqtpen,
              meins,
              cancel,
              mblnr,
              mjahr,
              move_stloc,
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
        FROM ztmm_doc_rem_rec
        INTO TABLE @DATA(lt_table)
        FOR ALL ENTRIES IN @ct_xlips
        WHERE vbeln =  @ct_xlips-vbeln
         AND cancel <> 'X'
         AND zstatus_integracao =  '01'.
      IF lt_table IS NOT INITIAL.
        DATA(lt_xlips) = ct_xlips.
        SORT lt_xlips BY vbeln.
        DATA(ls_table) = lt_table[ 1 ].
        READ TABLE lt_xlips INTO DATA(ls_xlips) WITH KEY vbeln = ls_table-vbeln BINARY SEARCH.

        IF sy-subrc IS INITIAL.

          DATA(lv_mensagem) = |{ TEXT-e01 }| && | { ls_xlips-vbeln } | && |{ TEXT-e02 }|.

          MESSAGE lv_mensagem TYPE 'E'.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~change_delivery_header.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~change_delivery_item.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~change_fcode_attributes.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~change_field_attributes.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~check_item_deletion.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~delivery_deletion.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~delivery_final_check.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~document_number_publish.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~fill_delivery_header.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~initialize_delivery.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~item_deletion.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~publish_delivery_item.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~read_delivery.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~save_and_publish_before_output.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_le_shp_delivery_proc~save_and_publish_document.
    DATA lt_ztbmmxxx1 TYPE zctgmm_doc_rem_rec_tab.

    DATA ls_ztbmmxxx1 TYPE ztmm_doc_rem_rec.

    DATA: lr_werks    TYPE RANGE OF lips-werks.

    CLEAR: lt_ztbmmxxx1[],
           ls_ztbmmxxx1,
           lr_werks[].

    SELECT low
      FROM zi_ca_get_parameter
    INTO TABLE @DATA(lt_werks)
      WHERE modulo EQ 'MM'
       AND chave1 EQ 'INTEGRACAO_WMS'
       AND chave2 EQ 'CENTRO'.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    lr_werks = VALUE #( FOR <fs_werks> IN lt_werks
                           ( sign   = 'I'
                             option = 'EQ'
                             low    = <fs_werks>-low ) ).

    SELECT SINGLE low
      FROM zi_ca_get_parameter
     INTO @DATA(ls_bwart)
        WHERE modulo EQ 'MM'
         AND chave1 EQ 'INTEGRACAO_WMS'
         AND chave2 EQ 'MOV_DEVOL_CLIENTE'.

    DATA(lt_xlips) = it_xlips.
    SORT lt_xlips BY vbeln posnr.

    DATA(lt_xlikp) = it_xlikp.
    SORT lt_xlikp BY vbeln.

    IF lt_xlips[] IS NOT INITIAL.
      SELECT vbeln,
             posnr
        FROM zi_mm_doc_rem_rec
        INTO TABLE @DATA(lt_check)
        FOR ALL ENTRIES IN @lt_xlips
        WHERE vbeln = @lt_xlips-vbeln
          AND posnr = @lt_xlips-posnr.
      IF lt_check[] IS NOT INITIAL.
        SORT lt_check BY vbeln posnr.
      ENDIF.
    ENDIF.

    READ TABLE lt_xlips ASSIGNING FIELD-SYMBOL(<fs_xlips>) INDEX 1.
    IF sy-subrc = 0.
      DATA(lv_tabix) = sy-tabix.
      LOOP AT lt_xlips
        INTO DATA(ls_xlips) FROM lv_tabix
        WHERE werks IN lr_werks.
        READ TABLE lt_xlikp INTO DATA(ls_xlikp) WITH KEY vbeln = ls_xlips-vbeln BINARY SEARCH.
        CHECK sy-subrc IS INITIAL.

        IF ls_xlikp-vbtyp = 'J' AND ls_xlips-vgtyp = 'V'.
          DATA(lv_tipo_doc) = 'T'.
        ELSEIF ls_xlikp-vbtyp = 'J' AND ls_xlips-vgtyp = 'C' AND ls_xlips-bwart = ls_bwart.
          lv_tipo_doc = 'R'.
        ELSEIF ls_xlikp-vbtyp = 'J' AND ls_xlips-vgtyp = 'C'.
          lv_tipo_doc = 'V'.
        ELSEIF ls_xlikp-vbtyp = '7' AND ls_xlips-vgtyp = 'V'.
          lv_tipo_doc = 'E'.
        ELSE.
          CONTINUE.
        ENDIF.
        READ TABLE lt_check
          ASSIGNING FIELD-SYMBOL(<fs_check>)
          WITH KEY vbeln = ls_xlips-vbeln
                   posnr = ls_xlips-posnr
          BINARY SEARCH.
        CHECK sy-subrc IS NOT INITIAL.

        ls_ztbmmxxx1-vbeln = ls_xlips-vbeln.
        ls_ztbmmxxx1-posnr = ls_xlips-posnr.
        ls_ztbmmxxx1-werks = ls_xlips-werks.
        ls_ztbmmxxx1-lgort = ls_xlips-lgort.
        ls_ztbmmxxx1-ztipo_doc = lv_tipo_doc.
        ls_ztbmmxxx1-zstatus_integracao = '00'.
        ls_ztbmmxxx1-menge = ls_xlips-lfimg.
        ls_ztbmmxxx1-meins = ls_xlips-vrkme.
        ls_ztbmmxxx1-ernam = sy-uname.
        ls_ztbmmxxx1-erdat = sy-datum.
        ls_ztbmmxxx1-erzet = sy-uzeit.
        APPEND ls_ztbmmxxx1 TO lt_ztbmmxxx1.
      ENDLOOP.
    ENDIF.


    IF lt_ztbmmxxx1[] IS NOT INITIAL.
      CALL FUNCTION 'ZFMMM_WMS_ATUALIZA_TABELA'
        IN UPDATE TASK
        EXPORTING
          iv_no_commit = abap_true                 " Campo de ligação para query de visão
          it_ztbmmxxx1 = lt_ztbmmxxx1.                 " Estrutura tabela ztmm_doc_rem_rec
    ENDIF.
  ENDMETHOD.
ENDCLASS.
