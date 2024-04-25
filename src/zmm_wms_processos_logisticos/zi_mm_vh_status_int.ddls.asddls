@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Value Help - ZStatusIntegracao'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity ZI_MM_VH_Status_Int as select from dd07t 
{
    key domvalue_l as ZstatusIntegracao,
    ddtext as ZstatusIntegracaoDesc 
    
}
where domname = 'ZD_STATUS_INT'
  and ddlanguage = $session.system_language
