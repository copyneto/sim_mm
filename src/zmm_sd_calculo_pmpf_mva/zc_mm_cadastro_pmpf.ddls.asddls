@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de Consumo para Cadastro de PMPF'
@Metadata.allowExtensions: true
define root view entity ZC_MM_CADASTRO_PMPF
  provider contract transactional_query
  as projection on ZI_MM_CADASTRO_PMPF
{
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_MM_VH_CADASTRO_PMPF', element: 'Pais'} }]
  key Chave_pais,

      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_MM_VH_CADASTRO_PMPF_TXREG', element: 'Regiao_fiscal'} }]
  key Regiao_fiscal,

      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_CA_VH_MATNR', element: 'Material' }}]
  key Material,
  key Valido_des,
      Valido_ate,
      Preco,
      Num_uni,

      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_MM_VH_CADASTRO_PMPF_DOMINIO', element: 'Unit' }}]
      Uni_preco,
      Criado_por,
      Criado_em,
      Modificado_por,
      Modificado_em,
      Hora
}
