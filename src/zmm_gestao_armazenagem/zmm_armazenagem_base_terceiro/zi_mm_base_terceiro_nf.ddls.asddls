@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Seleção de dados auxiliares'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_MM_BASE_TERCEIRO_NF
  as select from A_InbDeliveryItem as _item

{
  
  key _item.DeliveryDocument,
  key _item.DeliveryDocumentItem,
  concat('000', cast(ltrim(_item.ReferenceSDDocumentItem, '0' ) as abap.char(6))) as ReferenceSDDocumentItem,
  _item.ReferenceSDDocument
}
