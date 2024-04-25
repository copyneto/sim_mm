@EndUserText.label: 'CDS - App execução de processos'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_MM_WMS_EXEC_PROCESSO as 
    projection on ZI_MM_Integracao_Doc_Remessa 
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
    bukrs,
    process,
    edoc_type,
    posting_date,
    /* Associations */
    _lips
}
    where Cancel is initial
    and OverallSDProcessStatus != 'C' 

