class ZCLMM_DOCUMENT_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_DOCUMENT_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCLMM_DOCUMENT_BADI IMPLEMENTATION.


  METHOD if_ex_mb_document_badi~mb_document_before_update.
    RETURN.
  ENDMETHOD.


  METHOD if_ex_mb_document_badi~mb_document_update.

    DATA ls_table TYPE zsmm_doc_mat.
    DATA lt_table TYPE zctgmm_doc_mat.
    DATA lv_i TYPE i VALUE 1.

    IF xmseg IS NOT INITIAL.
      SELECT Vbeln,
             Posnr,
             Werks,
             Lgort,
             ZtipoDoc,
             ZstatusIntegracao,
             Menge,
             Zqtrec,
             Zqtpen,
             Meins,
             Cancel,
             Mblnr,
             Mjahr,
             MoveStloc,
             Ernam,
             Erdat,
             Erzet,
             Aenam,
             Aedat,
             Aezet,
             Type,
             Id,
             Znumber,
             Message
        FROM zi_mm_doc_rem_rec
        FOR ALL ENTRIES IN @xmseg
        WHERE vbeln EQ @xmseg-vbeln_im
          AND posnr EQ @xmseg-vbelp_im
          AND ztipodoc EQ 'E' "docs de entrada
          AND zqtpen IS NOT INITIAL "somente se o campo estiver preenchido
          AND mblnr EQ @abap_false "considerar docs com movimentação devolução
          AND cancel EQ @abap_false "desconsiderar documentos cancelados
        INTO TABLE @DATA(lt_ztbmmxxx1).
    ENDIF.
    IF xmseg IS NOT INITIAL.
      SELECT Mblnr,
             Mjahr,
             Zeile,
             ZtipoDoc,
             ZstatusIntegracao,
             Zorigem,
             Matnr,
             Bwart,
             Shkzg,
             VbelnIm,
             VbelpIm,
             Werks,
             Lgort,
             Umwrk,
             Umlgo,
             Erfmg,
             Erfme,
             Zqtenv,
             Meins,
             Smbln,
             Sjahr,
             Smblp,
             Xnull,
             Ernam,
             Erdat,
             Erzet,
             Aenam,
             Aedat,
             Aezet,
             Type,
             Zid,
             Znumber,
             Zmessage
        FROM zi_mm_integracao_docs_mat
        FOR ALL ENTRIES IN @xmseg
       WHERE mblnr EQ @xmseg-mblnr
         AND mjahr EQ @xmseg-mjahr
         AND zeile EQ @xmseg-zeile
      INTO TABLE @DATA(lt_ztbmmxxx2).

    ENDIF.

    TYPES: BEGIN OF ty_bwart,
             mblnr TYPE Mblnr,
             mjahr TYPE mjahr,
             zeile TYPE mblpo,
             bwart TYPE ze_param_low,
           END OF ty_bwart.

    DATA  lt_bwart TYPE TABLE OF ty_bwart.

    MOVE-CORRESPONDING xmseg TO lt_bwart.

    SELECT low,
           chave3
    FROM zi_ca_get_parameter
   WHERE modulo EQ 'MM'
     AND chave1 EQ 'INTEGRACAO_WMS'
     AND ( chave2 EQ 'MOVIMENTO_DEVOLUCAO'
     OR chave2 EQ 'MOVIMENTO_ESTORNO' )
  INTO TABLE @DATA(lt_movimentos).

    SORT: lt_movimentos BY low,
          lt_ztbmmxxx2 BY mblnr
                          mjahr
                          zeile.
    DATA lv_zqenv TYPE int8.
    DATA lv_saldo TYPE int8.
    SORT lt_ztbmmxxx2 BY mblnr
                         mjahr
                         zeile.
    SORT lt_ztbmmxxx1 BY vbeln
                         posnr.

    LOOP AT xmseg ASSIGNING FIELD-SYMBOL(<fs_xmseg>).
      CHECK <fs_xmseg>-xauto IS INITIAL.
      CLEAR ls_table.
      READ TABLE lt_movimentos INTO DATA(ls_movimentos) WITH KEY low = <fs_xmseg>-bwart BINARY SEARCH..
      CHECK sy-subrc IS INITIAL.

      IF ls_movimentos-chave3 = 'D'.
        READ TABLE lt_ztbmmxxx2 INTO DATA(ls_ztbmmxxx2) WITH KEY mblnr = <fs_xmseg>-mblnr
                                                                 mjahr = <fs_xmseg>-mjahr
                                                                 zeile = <fs_xmseg>-zeile BINARY SEARCH.

        IF sy-subrc IS NOT INITIAL..
          READ TABLE lt_ztbmmxxx1 INTO DATA(ls_ztbmmxxx1) WITH KEY vbeln = <fs_xmseg>-vbeln_im
                                                                   posnr = <fs_xmseg>-vbelp_im BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            lv_saldo = ( ls_ztbmmxxx1-zqtrec - ls_ztbmmxxx2-erfmg ).
            IF lv_saldo > 0 AND lv_saldo > <fs_xmseg>-erfmg.
              lv_zqenv = lv_saldo - <fs_xmseg>-erfmg.
            ELSEIF lv_saldo > 0 AND lv_saldo < <fs_xmseg>-erfmg.
              lv_zqenv = <fs_xmseg>-erfmg - lv_saldo.
            ELSE.
              lv_zqenv = <fs_xmseg>-erfmg.
            ENDIF.
          ELSE.
            lv_zqenv = <fs_xmseg>-erfmg.
          ENDIF.
          ls_table-mandt              = <fs_xmseg>-mandt.
          ls_table-mblnr              = <fs_xmseg>-mblnr.
          ls_table-mjahr              = <fs_xmseg>-mjahr.
          ls_table-zeile              = <fs_xmseg>-zeile.
          ls_table-ztipo_doc          = ls_movimentos-chave3.
          IF lv_zqenv = 0.
            ls_table-zstatus_integracao = '07'.
          ELSE.
            ls_table-zstatus_integracao = '00'.
          ENDIF.
          ls_table-zorigem            = 'S'.
          ls_table-matnr              = <fs_xmseg>-matnr.
          ls_table-bwart              = <fs_xmseg>-bwart.
          ls_table-shkzg              = <fs_xmseg>-shkzg.
          ls_table-vbeln_im           = <fs_xmseg>-vbeln_im.
          ls_table-vbelp_im           = <fs_xmseg>-vbelp_im.
          ls_table-werks              = <fs_xmseg>-werks.
          ls_table-lgort              = <fs_xmseg>-lgort.
          ls_table-erfmg              = <fs_xmseg>-erfmg.
          ls_table-erfme              = <fs_xmseg>-erfme.
          ls_table-zqtenv             = lv_zqenv.
          ls_table-meins              = <fs_xmseg>-erfme.
          ls_table-ernam              = sy-uname.
          ls_table-erdat              = sy-datum.
          ls_table-erzet              = sy-uzeit.
          APPEND ls_table TO lt_table.
        ENDIF.
      ELSEIF ls_movimentos-chave3 = 'C'.
        ls_table-mandt              = <fs_xmseg>-mandt       .
        ls_table-mblnr              = <fs_xmseg>-mblnr       .
        ls_table-mjahr              = <fs_xmseg>-mjahr       .
        ls_table-zeile              = <fs_xmseg>-zeile       .
        ls_table-ztipo_doc          = ls_movimentos-chave3     .
        ls_table-zstatus_integracao = '00'                   .
        ls_table-zorigem            = 'S'                    .
        ls_table-matnr              = <fs_xmseg>-matnr       .
        ls_table-bwart              = <fs_xmseg>-bwart       .
        ls_table-shkzg              = <fs_xmseg>-shkzg       .
        ls_table-vbeln_im           = <fs_xmseg>-vbeln_im    .
        ls_table-vbelp_im           = <fs_xmseg>-vbelp_im    .
        ls_table-werks              = <fs_xmseg>-werks       .
        ls_table-lgort              = <fs_xmseg>-lgort       .
        ls_table-umwrk              = <fs_xmseg>-umwrk       .
        ls_table-umlgo              = <fs_xmseg>-umlgo       .
        ls_table-erfmg              = <fs_xmseg>-erfmg       .
        ls_table-erfme              = <fs_xmseg>-erfme       .
        ls_table-zqtenv             = <fs_xmseg>-erfmg       .
        ls_table-meins              = <fs_xmseg>-erfme       .
        ls_table-smbln              = <fs_xmseg>-smbln       .
        ls_table-sjahr              = <fs_xmseg>-sjahr       .
        ls_table-smblp              = <fs_xmseg>-smblp       .
        ls_table-ernam              = sy-uname               .
        ls_table-erdat              = sy-datum               .
        ls_table-erzet              = sy-uzeit               .
        APPEND ls_table TO lt_table.
      ENDIF.
    ENDLOOP.

    IF lt_table IS NOT INITIAL.
      CALL FUNCTION 'ZFMMM_WMS_ATUALIZA_TABELA'
        IN UPDATE TASK
        EXPORTING
          iv_no_commit = 'X'               " Campo de ligação para query de visão
*         it_ztbmmxxx1 =                  " Categoria de tabela para tabela ztmm_doc_rem_rec
          it_ztbmmxxx2 = lt_table              " Categoria de tabela para tabela ztmm_doc_mat
*         is_ztbmmxxx1 =                  " Estrutura tabela ztmm_doc_rem_rec
*         is_ztbmmxxx2 =                  " Estrutura para tabela ztmm_doc_mat
*        IMPORTING
*         et_return    =                  " Parâmetro de retorno
*         et_ztbmmxxx1 =                  " Categoria de tabela para tabela ztmm_doc_rem_rec
*         et_ztbmmxxx2 =                  " Categoria de tabela para tabela ztmm_doc_mat
        .
    ENDIF.

  ENDMETHOD.
ENDCLASS.
