@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Gerar retorno de armazenagem - Status'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS -- drop down menu for value help
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity ZI_MM_GERAR_RETOR_ARMAZ_STATUS as select from dd07t {
    @ObjectModel.text.element: ['StatusText'] -- descricao da chave
    key domvalue_l as Status,
    ddtext         as StatusText
    
}
    where domname = 'ZD_JCS_STARUS_RETONO'
      and ddlanguage = $session.system_language
