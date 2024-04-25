@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Associação Pedido ao XML'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_EDOC_BR_PURCHASE_ORDER

  as select from ekpo as _PurchaseOrderItem

  association [0..1] to ekko as _PurchaseOrder on _PurchaseOrder.ebeln = $projection.ebeln
{
  key _PurchaseOrderItem.ebeln                                as ebeln,
  key _PurchaseOrderItem.ebelp                                as ebelp,
      _PurchaseOrderItem.matnr                                as matnr,
      cast( ltrim( _PurchaseOrderItem.idnlf, '0' ) as idnlf ) as idnlf,
      _PurchaseOrderItem.j_1bnbm                              as j_1bnbm,

      _PurchaseOrder.lifnr                                    as lifnr,
      _PurchaseOrderItem.werks                                as werks,
      _PurchaseOrderItem.loekz                                as loekz,
      _PurchaseOrderItem.elikz                                as elikz
}
