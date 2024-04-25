@EndUserText.label: 'CDS de projeção log material'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_MM_LOG_MAT provider contract transactional_query as projection on ZI_MM_LOG_MAT
{
    key Matnr,
    key Werks,
    key Lgort,
    key Vkorg,
    key Vtweg,
    Ztype,
    Zid,
    Znumber,
    Zmessage,
    ZlogNo,
    ZlogMsgNo,
    ZmessageV1,
    ZmessageV2,
    ZmessageV3,
    ZmessageV4,
    Usuario,
    Data,
    Hora
}
