@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View basic - Petrona'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_PETRONA_ALL
  as select from ZI_MM_PEDIDO_PETRONA as Pet
  association [1..1] to dfkkbptaxnum as _dfkk on  _dfkk.partner = Pet.plant
                                              and _dfkk.taxtype = 'BR1'
{
  key  Pet.ZZ1_PurchaseOrder,
       Pet.ZZ1_PlantName,
       _dfkk.taxnum as ZZ1_CnpjPlant,
       Pet.ZZ1_SupplierName,
       Pet.ZZ1_CnpjSupplier,
       Pet.ZZ1_PaymentTermText
}
