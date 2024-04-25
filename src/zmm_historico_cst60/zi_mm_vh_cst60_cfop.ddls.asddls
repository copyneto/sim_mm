@EndUserText.label: 'CDS Value Help - CFOP'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_VH_CST60_CFOP
  as select from ztmm_hist_cst60
{

      @EndUserText.label: 'CFOP'
  key cfop as Cfop

}
group by
  cfop;
