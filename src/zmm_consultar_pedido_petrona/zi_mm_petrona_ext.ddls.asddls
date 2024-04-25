@AbapCatalog.sqlViewAppendName: 'ZVMMPETRONA'
@EndUserText.label: 'Extensão Petrona'
extend view A_PurchaseOrder with ZI_MM_PETRONA_EXT
  association [1..1] to ZI_MM_PETRONA_ALL as _Petrona on _Petrona.ZZ1_PurchaseOrder = $projection.purchaseorder
{
  _Petrona.ZZ1_PlantName,
  _Petrona.ZZ1_CnpjPlant,
  _Petrona.ZZ1_SupplierName,
  _Petrona.ZZ1_CnpjSupplier,
  _Petrona.ZZ1_PaymentTermText 
}
