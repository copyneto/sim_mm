@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Documentos de inventários p/ integração WMS'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
@Metadata.allowExtensions: true
define root view entity ZI_MM_WMS_DOC_INVENTARIO
  as select from    R_PhysInvtryDocumentItemTP as _InventoryItem

    inner join      ZI_CA_GET_PARAMETER        as _ParamCentro   on  _ParamCentro.Modulo = 'MM'
                                                                 and _ParamCentro.Chave1 = 'INTEGRACAO_WMS'
                                                                 and _ParamCentro.Chave2 = 'ENVIO_RECEBIMENTO_DOCUMENTOS'
                                                                 and _ParamCentro.Chave3 = 'CENTRO'
                                                                 and _ParamCentro.Low    = _InventoryItem.Plant

    left outer join ZI_MM_INTEGRACAO_DOCS_MAT  as _MatDocItemWMS on  _MatDocItemWMS.Mblnr         = _InventoryItem.PhysicalInventoryDocument
                                                                 and _MatDocItemWMS.Mjahr         = _InventoryItem.FiscalYear
                                                                 and _MatDocItemWMS.inventoryItem = _InventoryItem.PhysicalInventoryDocumentItem

{
  key _InventoryItem.FiscalYear,
  key _InventoryItem.PhysicalInventoryDocument,
  key _InventoryItem.PhysicalInventoryDocumentItem as PhysicalInventoryDocumentItem,
      _InventoryItem.Plant,
      _InventoryItem.StorageLocation,
      _InventoryItem.PostingDate,
      @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
      _InventoryItem.Quantity,
      _InventoryItem.MaterialBaseUnit,
      //      @Semantics.quantity.unitOfMeasure: 'UnitOfEntry'
      //      _InventoryItem.QuantityInUnitOfEntry,
      //      _InventoryItem.UnitOfEntry,
      _InventoryItem.PhysicalInventoryItemIsZero,
      _InventoryItem._PhysicalInventoryDocument.PhysicalInventoryCountStatus,
      _InventoryItem._PhysicalInventoryDocument.DocumentDate,
      _InventoryItem._PhysicalInventoryDocument.PhysInvtryAdjustmentPostingSts,
      _InventoryItem.Material,
      _InventoryItem._Material.MaterialType,
      _InventoryItem._Material.MaterialGroup,
      _InventoryItem._Material._Text[Language = $session.system_language ].MaterialName,
      _InventoryItem.MaterialDocument,
      _InventoryItem.MaterialDocumentItem,
      _InventoryItem.MaterialDocumentYear,
      _InventoryItem._ProductPlant.GoodsIssueUnit,

      _MatDocItemWMS._MaterialDocument.GoodsMovementType,
      _MatDocItemWMS._MaterialDocument.DebitCreditCode,

      case
        when ( _InventoryItem._ProductPlant.GoodsIssueUnit <> '' and _InventoryItem._ProductPlant.GoodsIssueUnit <> _InventoryItem.MaterialBaseUnit )
        then _InventoryItem._ProductPlant.GoodsIssueUnit
        else _InventoryItem.MaterialBaseUnit
      end                                          as MaterialUnitMeasure

}

where
  (
       _MatDocItemWMS.Mblnr                                                     is null
    or _MatDocItemWMS.ZstatusIntegracao                                         =  '05'
  )
  and  _InventoryItem._PhysicalInventoryDocument.PhysicalInventoryCountStatus   <> 'X'
  and  _InventoryItem._PhysicalInventoryDocument.PhysInvtryAdjustmentPostingSts <> 'X'
