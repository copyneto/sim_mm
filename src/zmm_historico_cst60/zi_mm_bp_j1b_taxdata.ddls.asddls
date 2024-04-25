@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Informações BP para leitura dinâmica de imposto'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZI_MM_BP_J1B_TAXDATA
  as select from kna1
{
  key cast(kunnr as bu_partner preserving type) as BusinessPartner,
      icmstaxpay                                as IcmsTaxPay,
      indtyp                                    as IndustrySector,
      'BP_CUST'                                 as BPType
}
union all select from lfa1
{
  key cast(lifnr as bu_partner preserving type) as BusinessPartner,
      icmstaxpay                                as IcmsTaxPay,
      indtyp                                    as IndustrySector,
      'BP_VEND'                                 as BPType
}
