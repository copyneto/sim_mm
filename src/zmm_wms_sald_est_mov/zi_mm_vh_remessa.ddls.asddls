@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help - Remessa'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_VH_REMESSA as select from ztmm_doc_mat
{
    key vbeln_im as Remessa
}group by vbeln_im
union select from ztmm_doc_rem_rec
{
    key vbeln as remessa
}group by vbeln
