@AbapCatalog.sqlViewName: 'ZVMM_INT_REMESSA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Integracao Remessa'
/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define root view ZI_MM_Integracao_Doc_Remessa
  as select from    ztmm_doc_rem_rec as _Remessa
  
  left outer join I_DeliveryDocument as _DeliveryDocument on  _Remessa.vbeln = _DeliveryDocument.DeliveryDocument

  left outer join edobrincoming    as _Edobrincoming on _Edobrincoming.delnum = _Remessa.vbeln

  association [1..1] to edocument          as _Edocument        on  _Edocument.edoc_guid = _Edobrincoming.edoc_guid

  association [1..1] to lips               as _lips             on  _lips.vbeln = $projection.Vbeln
                                                                and _lips.posnr = $projection.Posnr
                                                                and _lips.lgort = $projection.Lgort


  association [1..1] to ZI_MM_DOC_ENTRADA_ESTORNO as _ENTRADA_ESTORNO on _ENTRADA_ESTORNO.vbeln_im = $projection.Vbeln
                                                                     and _ENTRADA_ESTORNO.vbelp_im = $projection.Posnr
  
  association [1..1] to dd07t as _ZD_DOC on _ZD_DOC.domname = 'ZD_DOC'
                                        and _ZD_DOC.ddlanguage = $session.system_language
                                        and _ZD_DOC.domvalue_l = $projection.ZtipoDoc
  
  association [1..1] to dd07t as _ZD_STATUS_INT on _ZD_STATUS_INT.domname = 'ZD_STATUS_INT'
                                               and _ZD_STATUS_INT.ddlanguage = $session.system_language
                                               and _ZD_STATUS_INT.domvalue_l = $projection.ZstatusIntegracao

{//ZD_STATUS_INT
  key _Remessa.vbeln              as Vbeln,
  key _Remessa.posnr              as Posnr,
      _Remessa.werks              as Werks,
      _Remessa.lgort              as Lgort,
      _Remessa.ztipo_doc          as ZtipoDoc,
      _ZD_DOC.ddtext              as ZtipoDocDesc,
      _Remessa.zstatus_integracao as ZstatusIntegracao,
      _ZD_STATUS_INT.ddtext       as ZstatusIntegracaoDesc,
      _Remessa.menge              as Menge,
      _Remessa.zqtrec             as Zqtrec,
      _Remessa.zqtpen             as Zqtpen,
      _Remessa.meins              as Meins,
      _Remessa.cancel             as Cancel,
      _Remessa.mblnr              as Mblnr,
      _Remessa.mjahr              as Mjahr,
      _Remessa.move_stloc         as MoveStloc,
      _Remessa.ernam              as Ernam,
      _Remessa.erdat              as Erdat,
      _Remessa.erzet              as Erzet,
      _Remessa.aenam              as Aenam,
      _Remessa.aedat              as Aedat,
      _Remessa.aezet              as Aezet,
      _Remessa.type               as Type,
      _Remessa.id                 as Id,
      _Remessa.znumber            as Znumber,
      _Remessa.message            as Message,

      _DeliveryDocument.OverallSDProcessStatus,
      _DeliveryDocument.DeliveryDocumentBySupplier,
      _DeliveryDocument.ShippingPoint,
      _DeliveryDocument.Supplier,
      _DeliveryDocument.ReceivingPlant,
      _DeliveryDocument.ShipToParty,

      _lips.arktx                 as DeliveryDocumentItemText,
      _lips.matnr                 as Material,
      _lips.matkl                 as MaterialGroup,
      _lips.lfimg                 as Lfimg,
      _lips.umvkz                 as Umvkz,
      _lips.umvkn                 as Umvkn,
      _lips.vrkme                 as Vrkme,
      _lips.meins                 as LipsMeins,
      _lips.vgbel                 as Vgbel,
      _lips.vgpos                 as Vgpos,
      _lips.vgtyp                 as Vgtyp,

      _Edocument.bukrs,
      _Edocument.process,
      _Edocument.edoc_type,
      _Edocument.posting_date,

      _Edobrincoming.accesskey,
      
      _ENTRADA_ESTORNO.mercadoria_entrada,
      _ENTRADA_ESTORNO.estorno_entrada,
      
      _lips
}
