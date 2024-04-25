@EndUserText.label: 'Documentos de inventários p/ integração WMS'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_MM_WMS_DOC_INVENTARIO
  provider contract transactional_query
  as projection on ZI_MM_WMS_DOC_INVENTARIO
{
  key FiscalYear,
  key PhysicalInventoryDocument,
  key PhysicalInventoryDocumentItem,
      Plant,
      StorageLocation,
      PostingDate,
      Quantity,
      MaterialBaseUnit,
      PhysicalInventoryItemIsZero,
      PhysicalInventoryCountStatus,
      DocumentDate,
      PhysInvtryAdjustmentPostingSts,
      @ObjectModel.text.element: [ 'MaterialName' ]
      Material,
      MaterialType,
      MaterialGroup,
      MaterialName,
      MaterialDocument,
      MaterialDocumentItem,
      MaterialDocumentYear,
      GoodsIssueUnit,
      MaterialUnitMeasure
}
