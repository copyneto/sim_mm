@EndUserText.label: 'CDS - Armazenagem base própria'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_MM_ARMAZ_BASE_PROPRIA 
    provider contract transactional_query
    as projection on ZI_MM_ARMAZ_BASE_PROPRIA 
{
    
   key MaterialDocYear,
   @EndUserText.label: 'Documento do Material Entrada'   
   @Consumption.semanticObject: 'Material' 
   key MaterialDoc,
   key MaterialDocItem,
       
       @EndUserText.label: 'Número e série Nfe remessa'
       NfeRemessa,
       
       @EndUserText.label: 'Data'
       @Consumption.filter.selectionType: #INTERVAL  --  VH pra Data
       @Consumption.filter.mandatory: true -- Filtro Obrigatorio
       Data,
       
    @EndUserText.label: 'Centro'    
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_CA_VH_WERKS', element: 'WerksCode' } }]   
    @Consumption.filter.mandatory: true -- Filtro Obrigatorio   
    Centro,   
       
    @EndUserText.label: 'Nome Centro'   
    NomeCentro,   
       
    @EndUserText.label: 'Material'   
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_CA_VH_MATNR', element: 'Material' } }]   
    @Consumption.semanticObject: 'Material'    
    Material,   
       
    @EndUserText.label: 'Descrição Material'      
    MaterialName,   
       
    @EndUserText.label: 'Quantidade Descarga'    
    QtdDescarga,   
       
    @EndUserText.label: 'Quantidade Faturada'        
    QtsFaturada,   
       
    @EndUserText.label: 'Perda/Ganho'   
    PerdaGanho,   
    UniMedidaBasica,   
       
    @EndUserText.label: 'valor Perda/Ganho'   
    valorPerdaGanho,   
    SalesDocumentCurrency,   
    
    @EndUserText.label: 'Tipo de Movimento'
    TyMove,
       
    @EndUserText.label: 'Motivo'    
    MotivoMov,
    
//  @EndUserText.label: 'Motivo Descrição'  
//  MotivoMovTxt, 
       
    @EndUserText.label: 'Parceiro de Negócios'    
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'I_EWM_BusinessPartner', element: 'BusinessPartner' } }]           
    @Consumption.semanticObject: 'BusinessPartner'   
    BusinessPartner,   
       
    @EndUserText.label: 'Nome do BP'     
    NomeBP,   
    
    @EndUserText.label: 'Documento do Material Retorno'   
    @Consumption.semanticObject: 'Material'   
    Doc_Material_Retorno,

    @EndUserText.label: 'Ano do Material Retorno'
    Ano_Material_Retorno,  
       
    @EndUserText.label: 'Status'  
    @ObjectModel.text.element: ['StatusText'] -- descricao da chave
    @UI.textArrangement: #TEXT_ONLY 
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_MM_GERAR_RETOR_ARMAZ_STATUS', element: 'Status' } }]   
    Status,
    StatusText,   
       
    @EndUserText.label: 'Estoque Ajustado?'  
    @ObjectModel.text.element: ['Estoque_Ajustado_Desc'] -- descricao da chave
    @UI.textArrangement: #TEXT_ONLY    
    Estoque_Ajustado, 
    Estoque_Ajustado_Desc,  
       
    @EndUserText.label: 'Documento do Material Ajustado'     
    @Consumption.semanticObject: 'Material'   
    Doc_Material_Ajuste,   
       
    @EndUserText.label: 'Ano do Material Ajustado'   
    Ano_Material_Ajuste,   
       
    @EndUserText.label: 'Documento de Retorno'   
    @Consumption.semanticObject: 'NotaFiscal'   
    BR_NotaFiscal,   
    
    
    @EndUserText.label: 'Parâmetro Nav. Doc de Entrada' 
    MaterialDocumentEntrada, 
    @EndUserText.label: 'Parâmetro Nav. Doc de Retorno' 
    MaterialDocumentRetorno,   
    @EndUserText.label: 'Parâmetro Nav. Doc de Ajuste' 
    MaterialDocumentAjuste, 
    
    @EndUserText.label: 'Parâmetro Nav. Ano Doc de Entrada' 
    MaterialDocumentYearEntrada,
    @EndUserText.label: 'Parâmetro Nav. Ano Doc de Retorno' 
    MaterialDocumentYearRetorno,
    @EndUserText.label: 'Parâmetro Nav. Ano Doc de Ajuste' 
    MaterialDocumentYearAjuste,    
    Empresa, 
    QtdReceb,
    Criticality,
    /* Associations */   
    _Material,   
    _Plant   
}
