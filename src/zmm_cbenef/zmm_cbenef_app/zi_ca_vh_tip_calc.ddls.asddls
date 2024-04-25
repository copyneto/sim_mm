@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help - Tipo Cálculo'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_CA_VH_TIP_CALC
as  select from dd07l as Objeto
    
    join         dd07t as Text on  Text.domname  = Objeto.domname
                               and Text.as4local = Objeto.as4local
                               and Text.valpos   = Objeto.valpos
                               and Text.as4vers  = Objeto.as4vers  

{
  @ObjectModel.text.element: ['ObjetoText']
  @Search.ranking: #MEDIUM
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.8
  key cast ( substring( Objeto.domvalue_l, 1, 1 ) as abap.char(1) ) as ObjetoId,
  @Semantics.text: true
  @Search.defaultSearchElement: true
  @Search.ranking: #HIGH
  @Search.fuzzinessThreshold: 0.7  
  Text.ddtext as ObjetoText     
}
where
      Objeto.domname  = 'ZD_TIPO_CALC'
  and Objeto.as4local = 'A' 
  and Text.ddlanguage = $session.system_language
