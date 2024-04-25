@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cds de Value Help Cadastro PMPF - TXREG'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_VH_CADASTRO_PMPF_TXREG
  as select from    j_1btregx  as _J_1BTREGX
    left outer join j_1btregxt as _J_1BTREGXT on  _J_1BTREGXT.txreg  = _J_1BTREGX.txreg
                                              and _J_1BTREGXT.spras = $session.system_language

{
      @ObjectModel.text.element: ['Regiao_Txt']
      @Search.ranking: #MEDIUM
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
  key _J_1BTREGX.txreg as Regiao_fiscal,
      _J_1BTREGXT.txt  as Regiao_Txt
}
