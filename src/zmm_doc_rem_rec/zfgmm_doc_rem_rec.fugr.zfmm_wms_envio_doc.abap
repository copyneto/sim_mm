FUNCTION zfmm_wms_envio_doc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_NO_COMMIT) TYPE  BOOLEAN OPTIONAL
*"     VALUE(IT_ZTBMMXXX1) TYPE  ZCTGMM_DOC_REM_REC_TAB OPTIONAL
*"     VALUE(IT_ZTBMMXXX2) TYPE  ZCTGMM_DOC_MAT OPTIONAL
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2
*"     VALUE(ET_ZTBMMXXX1) TYPE  ZCTGMM_DOC_REM_REC_TAB
*"     VALUE(ET_ZTBMMXXX2) TYPE  ZCTGMM_DOC_MAT
*"     VALUE(ET_ZTBMMXXX3) TYPE  ZCTGMM_DOCUMENTO_RECEBIMENTO
*"     VALUE(ET_ZTBMMXXX4) TYPE  ZCTGMM_DADOS_NOTA_FISCAL
*"     VALUE(ET_ZTBMMXXX5) TYPE  ZCTGMM_DOCUMENTOS_REMESSA
*"----------------------------------------------------------------------

  IF it_ztbmmxxx1 IS NOT INITIAL.
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
      INTO TABLE @DATA(lt_ztbmmxxx1)
     FOR ALL ENTRIES IN @it_ztbmmxxx1
     WHERE vbeln EQ @it_ztbmmxxx1-vbeln
       AND werks EQ @it_ztbmmxxx1-werks
       AND erdat EQ @it_ztbmmxxx1-erdat
       AND ztipodoc EQ @it_ztbmmxxx1-ztipo_doc
       AND zstatusintegracao EQ '00' "este status indica os documentos que devem enviados
       AND cancel EQ @abap_false.
  ENDIF.
  IF it_ztbmmxxx2 IS NOT INITIAL.
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
     FOR ALL ENTRIES IN @it_ztbmmxxx2
     WHERE mblnr EQ @it_ztbmmxxx2-mblnr
       AND mjahr EQ @it_ztbmmxxx2-mjahr
       AND erdat EQ @it_ztbmmxxx2-erdat
       AND ztipodoc EQ @it_ztbmmxxx2-ztipo_doc
       AND zstatusintegracao EQ '00' "este status indica os documentos que devem enviados
       AND zqtenv IS NOT INITIAL "apenas documentos que possuem este campo preenchido (maior que zero)
       AND zorigem EQ 'S'
      INTO TABLE @DATA(lt_ztbmmxxx2)."considerar apenas os documentos com origem SAP.
  ENDIF.

  IF lt_ztbmmxxx1 IS NOT INITIAL.
  SELECT Mandt,                                         "#EC CI_NOFIELD
                                                        "#EC CI_NO_TRANSFORM
    Accesskey,
    Company_code,
    Delnum,
    Materialdoc,
    Materialdoc_year,
    Invnum,
    Invoice_fiscal_year,
    Nfenum,
    Series,
    Supplier,
    Supplier_cnpj_cpf,
    Supplier_cuf,
    Plant,
    Plant_cuf,
    Land1
FROM edobrincoming
FOR ALL ENTRIES IN @lt_ztbmmxxx1
WHERE delnum EQ @lt_ztbmmxxx1-vbeln
INTO TABLE @DATA(lt_ztbmmxxx4).
  ENDIF.



ENDFUNCTION.
