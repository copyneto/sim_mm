@EndUserText.label: 'CDS - Custom entitiy PDF'
@ObjectModel.query.implementedBy: 'ABAP:ZCLMM_BASE_TERC_PDF'
define custom entity ZI_MM_BASE_TERC_PDF
  with parameters
    DeliveryDocument     : vbeln_vl,
    DeliveryDocumentItem : posnr_vl,
    qtdpopup             : matnr
{
  key stream_data : ze_rawstring;
}
