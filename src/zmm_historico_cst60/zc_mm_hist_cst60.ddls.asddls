@EndUserText.label: 'CDS de consumo para MM_HIST_CST60'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_MM_HIST_CST60
  provider contract transactional_query
  as projection on ZI_MM_HIST_CST60
{
  key nfenum,
  key series,
//      @ObjectModel.text.element: ['BusinessPartnerName']
  key stcd1,
//      BusinessPartnerName,
      NumeroNFe,
      @Consumption.filter.selectionType: #INTERVAL
      credat,
      MaterialName,
      matnr,
      cfop,
      menge,
      PlantName,
      werks,
      cst_icms,
      waerk,
      bc_icms,
      v_icms_pr,
      bc_icms_st,
      v_icms_st,
      bc_icms_st_fcp,
      v_icms_st_fcp,
      v_icms_rate,
      v_icms_rate_st,
      v_icms_rate_fcp
}
