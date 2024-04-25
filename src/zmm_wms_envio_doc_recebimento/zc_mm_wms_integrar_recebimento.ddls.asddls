@EndUserText.label: 'App envio documentos de recebimento'
@ObjectModel.query.implementedBy: 'ABAP:ZCLMM_WMS_INTEGRAR_RECEBIMENTO'
define custom entity ZC_MM_WMS_INTEGRAR_RECEBIMENTO
{
      @UI.selectionField: [{ position: 10 }]
      @UI.lineItem  : [{ position: 10 }]
      @EndUserText.label: 'Documento'
  key Documento     : vbeln_vl;
      @UI.selectionField  : [{ position: 20 }]
      DataDocumento : erdat;
      @UI.selectionField  : [{ position: 30 }]
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_MM_VH_TIPO_DOC', element: 'ZtipoDoc' } }]
      TipoDocumento : ze_tipo_doc;
      @UI.hidden    : true
      Centro        : werks_d;
      @UI.lineItem  : [{ position: 20 }]
      Tipo          : abap.char(20);
      @UI.selectionField: [{ position: 50 }]
      @EndUserText.label: 'Modo Teste'
//      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_CA_VH_YESNO', element: 'Value' } }]
      ModoTeste     : xfeld;
      @UI.lineItem  : [{ position: 30 }]
      Mensagem      : bapi_msg;
}
