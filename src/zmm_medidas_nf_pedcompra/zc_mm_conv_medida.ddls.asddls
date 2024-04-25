@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZI_MM_CONV_MEDIDA'
@ObjectModel.semanticKey: [ 'Matnr' ]
@Search.searchable: true
define root view entity ZC_MM_CONV_MEDIDA
  provider contract transactional_query
  as projection on ZI_MM_CONV_MEDIDATP
{
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.90
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_CA_VH_MATNR', element: 'Material' } } ]
  key Matnr,
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.90
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_CA_VH_LIFNR', element: 'Fornecedor' } } ]
  key Lifnr,
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.90
  key Cprod,
      UomExt,
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.90
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_CA_VH_MEINS', element: 'Medida' } } ]
      UomInt,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt

}
