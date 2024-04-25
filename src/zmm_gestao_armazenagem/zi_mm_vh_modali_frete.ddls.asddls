@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help - J_1B_FREIGHT_MODE'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity ZI_MM_VH_MODALI_FRETE 
    as select from dd07t {
    ///@ObjectModel.text.element: ['ModalidadeFreteText'] -- descricao da chave
    key cast(domvalue_l as j_1b_freight_mode) as ModalidadeFrete,
    ddtext         as ModalidadeFreteText
    
}
    where domname = 'J_1B_FREIGHT_MODE'
      and ddlanguage = $session.system_language
