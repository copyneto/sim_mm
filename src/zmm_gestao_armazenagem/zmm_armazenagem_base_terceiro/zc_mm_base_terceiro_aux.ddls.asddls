@EndUserText.label: 'CDS - Gestao de Armazenagem base terc'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_MM_BASE_TERCEIRO_AUX 
provider contract transactional_query
as projection on ZI_MM_BASE_TERCEIRO_AUX
{
    key DeliveryDocument,
    key DeliveryDocumentItem,
    semaforo,
    DeliveryDocumentBySupplier,
    DocumentDate,
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_CA_VH_WERKS', element: 'WerksCode' } }]   
    Plant,
    bukrs,
    WerksCodeName,
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_CA_VH_MATNR', element: 'Material' } }]   
    @Consumption.semanticObject: 'Material'
    Material,
    MaterialName,
    descarga,
    ActualDeliveredQtyInBaseUnit,
    perdaganho,
    @EndUserText.label: 'Documento de Retorno'   
    @Consumption.semanticObject: 'NotaFiscal'
    MaterialDocumentRertorno,
    BaseUnit,
    valorperdaganho,   
    Supplier,
    @Consumption.semanticObject: 'NotaFiscal'    
    BR_NotaFiscal,
    docnumremessa,
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_MM_VH_STATUS_ARMAZ', element: 'StatusText' } }]  
    status,
    estoqueajustado,
    @EndUserText.label: 'Documento do Material Ajustado'     
    @Consumption.semanticObject: 'Material'
    MaterialDocumentAjuste,
    docmatajest,
    @EndUserText.label: 'Documento do Material Ajustado'    
    @Consumption.semanticObject: 'NotaFiscal' 
    docnumretorno,
    @EndUserText.label: 'Tipo do Movimento'  
    tipomov,
    motivomov,
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'I_EWM_BusinessPartner', element: 'BusinessPartner' } }]           
    @Consumption.semanticObject: 'BusinessPartner'
    BusinessPartner
}
