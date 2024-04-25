@EndUserText.label: 'CDS de projeção'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_MM_DOC_REM_REC 
provider contract transactional_query 
as projection on ZI_MM_DOC_REM_REC
{
    @EndUserText.label: 'Remessa'
    key Vbeln,
    @EndUserText.label: 'Item de remessa'
    key Posnr,
    @EndUserText.label: 'Centro'
    Werks,
    @EndUserText.label: 'Depósito'
    Lgort,
    @EndUserText.label: 'Tipo documento'
    ZtipoDoc,
    @EndUserText.label: 'Status da integração'
    ZstatusIntegracao,
    @EndUserText.label: 'Quantidade do documento'
    Menge,
    @EndUserText.label: 'Quantidade Recebida'
    Zqtrec,
    @EndUserText.label: 'Quantidade Pendente'
    Zqtpen,
    @EndUserText.label: 'Unidade de medida básica'
    Meins,
    @EndUserText.label: 'Estornado'
    Cancel,
    @EndUserText.label: 'Número do documento do material'
    Mblnr,
    @EndUserText.label: 'Ano do documento do material'
    Mjahr,
    @EndUserText.label: 'Depósito de recebimento/de saída'
    MoveStloc,
    @EndUserText.label: 'Nome do responsável que criou o objeto'
    Ernam,
    @EndUserText.label: 'Data de criação do registro'
    Erdat,
    @EndUserText.label: 'Hora do registro'
    Erzet,
    @EndUserText.label: 'Nome do responsável pela modificação'
    Aenam,
    @EndUserText.label: 'Data da última modificação'
    Aedat,
    @EndUserText.label: 'Hora da última modificação'
    Aezet,
    @EndUserText.label: 'Categoria Mensagem'
    Type,
    @EndUserText.label: 'Classe de mensagem'
    Id,
    @EndUserText.label: 'Nº mensagem'
    Znumber,
    @EndUserText.label: 'Texto de mensagem'
    Message
}
