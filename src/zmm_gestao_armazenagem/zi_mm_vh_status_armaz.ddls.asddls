@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help - Status Armazenagem'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS -- drop down menu for value help
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity ZI_MM_VH_STATUS_ARMAZ as select from dd07t {
    @ObjectModel.text.element: ['StatusText'] -- descricao da chave
    key domvalue_l as Status,
    ddtext         as StatusText
    
}
    where domname = 'ZD_STATUS_ARMAZ'
      and ddlanguage = $session.system_language
