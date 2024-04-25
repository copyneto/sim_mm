@EndUserText.label: 'CDS App Movimentar Mercadoria'
@ObjectModel.query.implementedBy: 'ABAP:ZCLMM_MOVI_MERC'
define custom entity ZC_MM_WMS_EXEC_MOVI_MERC_CE 
// with parameters parameter_name : parameter_type 
{
  key message        : abap.char( 40 );
      Vbeln          : vbeln_vl;
  
}
