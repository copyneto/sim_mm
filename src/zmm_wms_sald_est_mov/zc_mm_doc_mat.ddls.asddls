@EndUserText.label: 'CDS de Projeção - Docs Material'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_MM_DOC_MAT provider contract transactional_query as projection on ZI_MM_DOC_MAT
{
    @EndUserText.label: 'Número do documento do material'
    key Mblnr,
    @EndUserText.label: 'Ano do documento do material'
    key Mjahr,
    @EndUserText.label: 'Item no documento do material'
    key Zeile,
    @EndUserText.label: 'Tipo documento'
    ZtipoDoc,
    @EndUserText.label: 'Status da integração'
    ZstatusIntegracao,
    @EndUserText.label: 'Sistema de Origem'
    Zorigem,
    @EndUserText.label: 'Material'
    Matnr,
    @EndUserText.label: 'Descrição Material'
    ProductDescription,
    @EndUserText.label: 'Tipo de Material'
    ProductType,
    @EndUserText.label: 'Grupo de Mercadorias'
    ProductGroup,
    @EndUserText.label: 'Tipo de movimento'
    Bwart,
    @EndUserText.label: 'Código débito/crédito'
    Shkzg,
    @EndUserText.label: 'Remessa'
    VbelnIm,
    @EndUserText.label: 'Item de remessa'
    VbelpIm,
    @EndUserText.label: 'Centro'
    Werks,
    @EndUserText.label: 'Depósito'
    Lgort,
    @EndUserText.label: 'Centro receptor/emissor'
    Umwrk,
    @EndUserText.label: 'Depósito de recebimento/de saída'
    Umlgo,
    @EndUserText.label: 'Qtd na unidade de medida do registro'
    Erfmg,
    @EndUserText.label: 'Unidade de medida do registro'
    Erfme,
    @EndUserText.label: 'Quantidade a ser enviada'
    Zqtenv,
    @EndUserText.label: 'Unidade de medida básica'
    Meins,
    @EndUserText.label: ''
    Smbln,
    @EndUserText.label: ''
    Sjahr,
    @EndUserText.label: ''
    Smblp,
    @EndUserText.label: 'Contagem zero'
    Xnull,
    @EndUserText.label: 'Criado por'
    Ernam,
    @EndUserText.label: 'Data de criação do registro'
    Erdat,
    @EndUserText.label: 'Hora do registro'
    Erzet,
    @EndUserText.label: 'Modifcado por'
    Aenam,
    @EndUserText.label: 'Data da última modificação'
    Aedat,
    @EndUserText.label: 'Hora da última modificação'
    Aezet,
    @EndUserText.label: 'Categoria Mensagem'
    Type,
    @EndUserText.label: 'Classe de mensagem'
    Zid,
    @EndUserText.label: 'Nº mensagem'
    Znumber,
    @EndUserText.label: 'Texto de mensagem'
    Zmessage
}
