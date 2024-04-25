@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Buscar dados de NF'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_BASE_NF_AUX
  as select from ZI_MM_BASE_TERCEIRO_NF
  association [1..*] to I_BR_NFItem as _nf on  _nf.PurchaseOrder     = $projection.ReferenceSDDocument
                                           and _nf.PurchaseOrderItem = $projection.ReferenceSDDocumentItem
{
  key DeliveryDocument,
  key DeliveryDocumentItem,
      ReferenceSDDocument,
      ReferenceSDDocumentItem,
      _nf.BR_NotaFiscal,
      _nf.BR_NotaFiscalItem,
      _nf.BR_NFPriceAmountWithTaxes
}
