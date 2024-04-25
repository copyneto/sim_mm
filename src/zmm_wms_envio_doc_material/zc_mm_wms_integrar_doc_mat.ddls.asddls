@EndUserText.label: 'App envio documentos de materiais'
@ObjectModel.query.implementedBy: 'ABAP:ZCLMM_WMS_INTEGRAR_DOCMATERIAL'
define custom entity ZC_MM_WMS_INTEGRAR_DOC_MAT
{
      @UI.selectionField: [{ position: 10 }]
      @UI.lineItem     : [{ position: 10 }]
      @EndUserText.label: 'Documento'
  key Documento        : mblnr;
      @UI.lineItem     : [{ position: 20 }]
      AnoDocumento     : mjahr;
      //      @UI.lineItem  : [{ position: 30 }]
      //      ItemDocumento : mblpo;
      @UI.selectionField  : [{ position: 20 }]
      @UI.lineItem     : [{ position: 30 }]
      DataDocumento    : erdat;
      @UI.selectionField  : [{ position: 30 }]
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_MM_VH_TIPO_DOC', element: 'ZtipoDoc' } }]
      TipoDocumento    : ze_tipo_doc;
//      @UI.selectionField  : [{ position: 40 }]
//      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_MM_VH_STATUS_INT', element: 'ZstatusIntegracao' } }]
//      StatusIntegracao : ze_status_int;
      @UI.hidden       : true
      Centro           : werks_d;
      @UI.selectionField: [{ position: 50 }]
      @EndUserText.label: 'Modo Teste'
      ModoTeste        : xfeld;
      @UI.lineItem     : [{ position: 40 }]
      @EndUserText.label: 'Tipo mensagem'
      Tipo             : abap.char(20);
      @UI.lineItem     : [{ position: 50 }]
      Mensagem         : bapi_msg;
}
