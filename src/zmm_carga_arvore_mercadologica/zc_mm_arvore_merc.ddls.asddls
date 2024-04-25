@EndUserText.label: 'CDS - Programa de Carga - Árvore Mercadológica'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_MM_ARVORE_MERC
  provider contract transactional_query
  as projection on ZI_MM_ARVORE_MERC
{
  key Uuid,
      @EndUserText.label: 'Nome do Arquivo'
      Filename,

      @EndUserText.label: 'Criado por'
      @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_CA_VH_USERS', element: 'UserID' } }]
      CreatedBy,

      @EndUserText.label: 'Data de Criação'
      @Consumption.filter.selectionType: #INTERVAL
      CreatedDate,

      @EndUserText.label: 'Hora de Criação'
      @Consumption.filter.selectionType: #INTERVAL
      CreatedAt,

      @EndUserText.label: 'Modificado por'
      @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_CA_VH_USERS', element: 'UserID' } }]
      LastChangedBy,

      @EndUserText.label: 'Data de Modificação'
      @Consumption.filter.selectionType: #INTERVAL
      LastChangedDate,

      @EndUserText.label: 'Hora de Modificação'
      @Consumption.filter.selectionType: #INTERVAL
      LastChangedAt,
      LocalLastChangedAt,
      
      @EndUserText.label: 'Status'
      StatusText,
      StatusCor,

      @ObjectModel.filter.enabled: false
      _Mensagens
}
