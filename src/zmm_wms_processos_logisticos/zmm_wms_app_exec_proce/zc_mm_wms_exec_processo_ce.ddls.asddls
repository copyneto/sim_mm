@EndUserText.label: 'CDS Custon Entity-App exec de processos'
@ObjectModel.query.implementedBy: 'ABAP:ZCLMM_EXEC_PROCESSO'
define custom entity ZC_MM_WMS_EXEC_PROCESSO_CE 
 //with parameters parameter_name : parameter_type 
{
  key message       : abap.char( 40 );
  Vbeln             : vbeln_vl;
  ZtipoDoc          : ze_tipo_doc;
  ZstatusIntegracao : ze_status_int;
  
}
