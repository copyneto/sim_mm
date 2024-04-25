@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface Benefício Fiscal'

define root view entity ZI_MM_CBENEF
  as select from ztmm_cbenef 
  
  left outer join ZI_CA_VH_REGIO as _RegioFrom on  ztmm_cbenef.shipfrom = _RegioFrom.Regio
  
  left outer join ZI_CA_VH_REGIO as _RegioTo on  ztmm_cbenef.shipto = _RegioTo.Regio
  
  left outer join ZI_CA_VH_DIRECT as _Direct on  ztmm_cbenef.direct = _Direct.ObjetoId
  
  left outer join ZI_CA_VH_TAXSIT as _Taxsit on  ztmm_cbenef.taxsit = _Taxsit.ObjetoId
  
  left outer join ZI_CA_VH_CFOP as _Cfop on  ztmm_cbenef.cfop = _Cfop.CFOP
  
  left outer join ZI_CA_VH_NBM as _Nbm on  ztmm_cbenef.nbm = _Nbm.NBM
  
  left outer join ZI_CA_VH_MATNR as _Material on  ztmm_cbenef.matnr = _Material.Material
  
  left outer join ZI_CA_VH_BEN_FIS as _Cbenef on  ztmm_cbenef.cbenef = _Cbenef.CB
  
  left outer join ZI_CA_VH_MOT_DES as _Motdesicms on  ztmm_cbenef.motdesicms = _Motdesicms.ObjetoId
  
  left outer join ZI_CA_VH_TIP_CALC as _TipoCalc on  ztmm_cbenef.tipo_calc = _TipoCalc.ObjetoId     
  
{
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_CA_VH_REGIO', element: 'Regio' } } ]
  key ztmm_cbenef.shipfrom                     as Shipfrom,
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_CA_VH_REGIO', element: 'Regio' } } ]
  key ztmm_cbenef.shipto                       as Shipto,
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_CA_VH_DIRECT', element: 'ObjetoId' } } ]
  key ztmm_cbenef.direct                       as Direct,
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_CA_VH_TAXSIT', element: 'ObjetoId' } } ]  
  key cast(ztmm_cbenef.taxsit as abap.char(1)) as Taxsit,  
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_CA_VH_CFOP', element: 'CFOP' } } ]
  key cast(ztmm_cbenef.cfop as abap.char(10))  as Cfop,
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_CA_VH_NBM', element: 'NBM' } } ]
  key ztmm_cbenef.nbm                           as Nbm,
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_CA_VH_MATNR', element: 'Material' } } ]
  key ztmm_cbenef.matnr                         as Matnr,  
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_CA_VH_BEN_FIS', element: 'CB' } } ]  
  ztmm_cbenef.cbenef                            as Cbenef,
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_CA_VH_MOT_DES', element: 'ObjetoId' } } ]     
  ztmm_cbenef.motdesicms                        as Motdesicms,
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_CA_VH_TIP_CALC', element: 'ObjetoId' } } ]  
  ztmm_cbenef.tipo_calc                         as TipoCalc,
  
  _RegioFrom.RegioText as RegioTextFrom,  
  _RegioTo.RegioText as RegioTextTo,
  _Direct.ObjetoText as DirectText,
  _Taxsit.ObjetoText as TaxsitText,
  _Cfop.CFOPText,
  _Nbm.NBMText,
  _Material.Text as MaterialText,     
  _Cbenef.CBText,
  _Motdesicms.ObjetoText as MotDesIcmsText,
  _TipoCalc.ObjetoText as TipCalcText  
  
//  _RegioFrom,
//  _RegioTo,
//  _Direct,
//  _Taxsit,
//  _Cfop,
//  _Nbm,
//  _Material,
//  _Cbenef,
//  _Motdesicms,
//  _TipoCalc
}
