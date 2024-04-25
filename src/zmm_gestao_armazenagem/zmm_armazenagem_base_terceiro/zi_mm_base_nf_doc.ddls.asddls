@EndUserText.label: 'CDS - Buscar dados de NF'
@ObjectModel.query.implementedBy: 'ABAP:ZCLMM_BASE_NF_DOC'
define custom entity ZI_MM_BASE_NF_DOC
{
  key DeliveryDocument          : vbeln_vl;
  key DeliveryDocumentItem      : posnr_vl;
      ReferenceSDDocumentItem   : vgpos;
      ReferenceSDDocument       : vgbel;
      BR_NotaFiscal             : j_1bdocnum;
      BR_NotaFiscalItem         : j_1bitmnum;
      BR_NFPriceAmountWithTaxes : j_1bnfpri;
      BR_NotaFiscalFormat       : logbr_nfnum9;
      BR_NotaFiscalItemFormat   : logbr_nf_series;
}
