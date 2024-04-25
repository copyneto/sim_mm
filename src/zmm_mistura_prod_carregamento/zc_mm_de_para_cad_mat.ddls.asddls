@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Consumo para Aprovadores'
@Metadata.allowExtensions: true
define root view entity ZC_MM_DE_PARA_CAD_MAT
  provider contract transactional_query
  as projection on ZI_MM_DE_PARA_CAD_MATERIAL
{
  @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_CA_VH_WERKS', element: 'WerksCode' }}]
  key Centro,
  @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_CA_VH_MATNR', element: 'Material' }}]
  key Material,
  @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_CA_VH_LGORT', element: 'StorageLocation' }}]
  key Deposito_rev,
  @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_MM_VH_VERID', element: 'Versao' }}]
      Versao,
      Criado_por,
      Criado_em,
      Modificado_por,
      Modificado_em,
      Hora
}
