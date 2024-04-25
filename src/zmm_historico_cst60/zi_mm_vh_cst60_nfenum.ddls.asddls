@EndUserText.label: 'CDS Value Help - NFENUM'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_VH_CST60_NFENUM
  as select from ztmm_hist_cst60
{
  key nfenum,
  key series,

      @EndUserText.label: 'Número Nota fiscal'
      concat(concat(nfenum,('-')),series ) as NumeroNFe
}
