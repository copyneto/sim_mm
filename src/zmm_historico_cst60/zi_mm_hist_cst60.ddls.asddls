@EndUserText.label: 'CDS de interface para MM_HIST_CST60'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZI_MM_HIST_CST60
  as select distinct from ztmm_hist_cst60 as _hist_cst60
  //    left outer join j_1bnfdoc       as _1bnfdoc on _hist_cst60.nfenum = _1bnfdoc.nfenum
  association [1..1] to I_MaterialText as _Material on  _Material.Material = _hist_cst60.matnr
                                                    and _Material.Language = $session.system_language

  //  association [1..*] to I_Businesspartnertaxnumber as _Bp       on  _Bp.BPTaxNumber = _hist_cst60.stcd1
  //                                                                and (
  //                                                                   _Bp.BPTaxType    = 'BR1'
  //                                                                   or _Bp.BPTaxType = 'BR2'
  //                                                                 )

  association [1..1] to I_Plant        as _Plant    on  _Plant.Plant = _hist_cst60.werks
{
  key _hist_cst60.nfenum,
  key _hist_cst60.series,
  key _hist_cst60.stcd1,
      @EndUserText.label: 'Nome do parceiro'
      //      _Bp._BusinessPartner.BusinessPartnerName,
      concat(concat(_hist_cst60.nfenum,('-')),_hist_cst60.series ) as NumeroNFe,
      _hist_cst60.credat,
      @EndUserText.label: 'Nome do material'
      _Material.MaterialName,
      @ObjectModel.text.element: ['MaterialName']
      _hist_cst60.matnr,
      _hist_cst60.cfop,
      _hist_cst60.menge,
      @EndUserText.label: 'Nome centro'
      _Plant.PlantName,
      @ObjectModel.text.element: ['PlantName']
      _hist_cst60.werks,
      cast( _hist_cst60.cst_icms as logbr_taxsit )                 as cst_icms,
      _hist_cst60.waerk,
      @Semantics.amount.currencyCode: 'waerk'
      _hist_cst60.bc_icms,
      @Semantics.amount.currencyCode: 'waerk'
      _hist_cst60.v_icms_pr,
      @Semantics.amount.currencyCode: 'waerk'
      _hist_cst60.bc_icms_st,
      @Semantics.amount.currencyCode: 'waerk'
      _hist_cst60.v_icms_st,
      @Semantics.amount.currencyCode: 'waerk'
      _hist_cst60.bc_icms_st_fcp,
      @Semantics.amount.currencyCode: 'waerk'
      _hist_cst60.v_icms_st_fcp,

      _hist_cst60.v_icms_rate,
      _hist_cst60.v_icms_rate_st,
      _hist_cst60.v_icms_rate_fcp
}
