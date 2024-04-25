@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cds de Value Help - Cadastro PMPF'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_VH_CADASTRO_PMPF
  as select from    t005  as _t005
    left outer join t005t as _t005t on  _t005t.land1 = _t005.land1
                                    and _t005t.spras  = $session.system_language

{
      @ObjectModel.text.element: ['Pais_Text']
      @Search.ranking: #MEDIUM
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
  key _t005.land1  as Pais,
      _t005t.landx as Pais_Text
}
