@EndUserText.label: 'CDS value Help - STCD1'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_VH_CST60_STCD1
  as select from ztmm_hist_cst60
{
      @EndUserText.label: 'CNPJ Fornecedor'
  key stcd1

}
