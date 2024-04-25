@EndUserText.label: 'CDS - App para movimentar mercadorias'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_MM_WMS_EXEC_MOVI_MERC as projection on ZI_MM_Integracao_Doc_Remessa 
    
{
    key Vbeln,
    key Posnr,
    Werks,
    Lgort,
    @ObjectModel.text.element: ['ZtipoDocDesc'] -- descricao da chave
    ZtipoDoc,
    ZtipoDocDesc,
    @ObjectModel.text.element: ['ZstatusIntegracaoDesc'] -- descricao da chave
    ZstatusIntegracao,
    ZstatusIntegracaoDesc,
    @Semantics.quantity.unitOfMeasure : 'Meins'
    Menge,
    Zqtrec,
    Zqtpen,
    Meins,
    Cancel,
    Mblnr,
    Mjahr,
    MoveStloc,
    Ernam,
    Erdat,
    Erzet,
    Aenam,
    Aedat,
    Aezet,
    Type,
    Id,
    Znumber,
    Message,
    OverallSDProcessStatus,
    mercadoria_entrada,
    estorno_entrada
    
}
    where ZtipoDoc = 'E'
    and ZstatusIntegracao = '03'
    and mercadoria_entrada = 'X'
    and estorno_entrada is initial 
    and Mblnr is initial
    and Zqtpen is not initial
