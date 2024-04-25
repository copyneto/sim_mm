@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cds de Value Help - Seleção em Dominios'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_VH_CADASTRO_PMPF_DOMINIO
  as select from    t006  as _t006
    left outer join t006a as _t006a on  _t006a.msehi  = _t006.msehi
                                    and _t006a.spras = $session.system_language

{
      @ObjectModel.text.element: ['Text_unit']
      @Search.ranking: #MEDIUM
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
  key _t006.msehi  as Unit,
      _t006a.mseht as Text_unit
}
