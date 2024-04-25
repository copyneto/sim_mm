//@AbapCatalog.sqlViewName: 'ZVMM_LOG_MAT'
//@AbapCatalog.compiler.compareFilter: true
//@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface log material'
define root view entity ZI_MM_LOG_MAT as select from ztmm_log_mat
{
    key matnr as Matnr,
    key werks as Werks,
    key lgort as Lgort,
    key vkorg as Vkorg,
    key vtweg as Vtweg,
    ztype as Ztype,
    zid as Zid,
    znumber as Znumber,
    zmessage as Zmessage,
    zlog_no as ZlogNo,
    zlog_msg_no as ZlogMsgNo,
    zmessage_v1 as ZmessageV1,
    zmessage_v2 as ZmessageV2,
    zmessage_v3 as ZmessageV3,
    zmessage_v4 as ZmessageV4,
    usuario as Usuario,
    data as Data,
    hora as Hora
}
