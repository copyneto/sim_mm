@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface - Docs Material'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_INTEGRACAO_DOCS_MAT
  as select from ztmm_doc_mat

  association [0..1] to I_MaterialDocumentItem as _MaterialDocument on  _MaterialDocument.MaterialDocument     = $projection.Mblnr
                                                                    and _MaterialDocument.MaterialDocumentYear = $projection.Mjahr
                                                                    and _MaterialDocument.MaterialDocumentItem = $projection.Zeile

  association [0..1] to I_DeliveryDocumentItem as _DeliveryDocument on  _DeliveryDocument.DeliveryDocument     = $projection.VbelnIm
                                                                    and _DeliveryDocument.DeliveryDocumentItem = $projection.VbelpIm

  association [0..1] to I_MaterialText         as _Material         on  _Material.Material = $projection.Matnr
                                                                    and _Material.Language = $session.system_language
{
  key mblnr                                                      as Mblnr,
  key mjahr                                                      as Mjahr,
  key zeile                                                      as Zeile,
      ztipo_doc                                                  as ZtipoDoc,
      zstatus_integracao                                         as ZstatusIntegracao,
      zorigem                                                    as Zorigem,
      matnr                                                      as Matnr,
      bwart                                                      as Bwart,
      shkzg                                                      as Shkzg,
      vbeln_im                                                   as VbelnIm,
      vbelp_im                                                   as VbelpIm,
      werks                                                      as Werks,
      lgort                                                      as Lgort,
      umwrk                                                      as Umwrk,
      umlgo                                                      as Umlgo,
      @Semantics.quantity.unitOfMeasure: 'Meins'
      erfmg                                                      as Erfmg,
      erfme                                                      as Erfme,
      @Semantics.quantity.unitOfMeasure: 'Meins'
      zqtenv                                                     as Zqtenv,
      cast(meins as meins preserving type)                       as Meins,
      smbln                                                      as Smbln,
      sjahr                                                      as Sjahr,
      smblp                                                      as Smblp,
      xnull                                                      as Xnull,
      ernam                                                      as Ernam,
      erdat                                                      as Erdat,
      erzet                                                      as Erzet,
      aenam                                                      as Aenam,
      aedat                                                      as Aedat,
      aezet                                                      as Aezet,
      type                                                       as Type,
      zid                                                        as Zid,
      znumber                                                    as Znumber,
      zmessage                                                   as Zmessage,
      right(zeile,3)                                             as inventoryItem,


      _Material.MaterialName,

      _MaterialDocument.CompanyCode,
      _DeliveryDocument._DeliveryDocument.ShippingPoint,
      _DeliveryDocument._DeliveryDocument.Supplier,
      _DeliveryDocument._DeliveryDocument._Supplier.SupplierName as SupplierName,
      _DeliveryDocument._DeliveryDocument.MeansOfTransportType,
      _DeliveryDocument._DeliveryDocument.MeansOfTransport,
      //      _DeliveryDocument.Plant,
      _DeliveryDocument.StorageLocation,
      @Semantics.quantity.unitOfMeasure: 'BaseUnit'
      _DeliveryDocument.ActualDeliveryQuantity,
      _DeliveryDocument.BaseUnit,
      _DeliveryDocument.DeliveryDocumentItemText,
      _DeliveryDocument.ReferenceSDDocument,

      _MaterialDocument,
      _DeliveryDocument
}
