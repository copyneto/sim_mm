@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View basic - Pedido petrona'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define root view entity ZI_MM_PEDIDO_PETRONA
  as select from I_PurchaseOrder     as _h
    inner join   I_PurchaseOrderItem as _i on _h.PurchaseOrder = _i.PurchaseOrder
  association [1..1] to I_Supplier      as _s on  _s.Supplier = _h.Supplier
  association [*]    to t052u           as _t on  _t.zterm = _h.PaymentTerms
                                              and _t.spras = 'P'
  association [1..1] to P_BusinessPlace as _P on  _P.bukrs  = _i.CompanyCode
                                              and _P.branch = _i.Plant
{
  key _h.PurchaseOrder                                          as ZZ1_PurchaseOrder,
      min(_P.name )                                             as ZZ1_PlantName,
      min( lpad( cast(_i.Plant as abap.char( 10 ) ), 10, '0' ) ) as plant,
      //      max(_dfkk.taxnum)            as ZZ1_CnpjPlant,
      max(_s.OrganizationBPName1)                               as ZZ1_SupplierName,
      max(_s.TaxNumber1)                                        as ZZ1_CnpjSupplier,
      max(_t.text1)                                             as ZZ1_PaymentTermText
}
group by
  _h.PurchaseOrder
