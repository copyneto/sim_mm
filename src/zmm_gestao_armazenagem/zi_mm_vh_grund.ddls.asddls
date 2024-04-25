@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help - Grund'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_VH_GRUND
  as select from t157e
  
{
  key cast( grund as abap.numc(4) ) as motivomov,
  key bwart,
      grtxt as Grtxt
      
}
where
    spras = $session.system_language
and ( bwart = 'Z11' )
group by
  grund,
  grtxt,
  bwart
