@EndUserText.label: 'CDS de consumo Benefício Fiscal'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_MM_CBENEF
  provider contract transactional_query
  as projection on ZI_MM_CBENEF
{
  @EndUserText.label: 'UF Origem'
  @ObjectModel.text.element: ['RegioTextFrom']  
  key Shipfrom,
  @EndUserText.label: 'UF Destino'
  @ObjectModel.text.element: ['RegioTextTo']
  key Shipto,
  @EndUserText.label: 'Direção'
  @ObjectModel.text.element: ['DirectText']
  key Direct,
  @EndUserText.label: 'CST ICMS'
  @ObjectModel.text.element: ['TaxsitText']
  key Taxsit,
  @EndUserText.label: 'CFOP'
  @ObjectModel.text.element: ['CFOPText']
  key Cfop,
  @EndUserText.label: 'NCM'
  @ObjectModel.text.element: ['NBMText']
  key Nbm,
  @EndUserText.label: 'Material'
  @ObjectModel.text.element: ['MaterialText']
  key Matnr,         
  @EndUserText.label: 'Cod Benef Fiscal'
  @ObjectModel.text.element: ['CBText']
  Cbenef,  
  @EndUserText.label: 'Motivo Desoneração'
  @ObjectModel.text.element: ['MotDesIcmsText']      
  Motdesicms,  
  @EndUserText.label: 'Tipo de cálculo'
  @ObjectModel.text.element: ['TipCalcText']
  TipoCalc,
  
  RegioTextFrom,  
  RegioTextTo,
  DirectText,
  TaxsitText,
  CFOPText,
  NBMText,
  MaterialText,     
  CBText,
  MotDesIcmsText,
  TipCalcText      
  
//  _RegioFrom.RegioText as RegioTextFrom,  
//  _RegioTo.RegioText as RegioTextTo,
//  _Direct.ObjetoText as DirectText,
//  _Taxsit.ObjetoText as TaxsitText,
//  _Cfop.CFOPText,
//  _Nbm.NBMText,
//  _Material.Text as MaterialText,     
//  _Cbenef.CBText,
//  _Motdesicms.ObjetoText as MotDesIcmsText,
//  _TipoCalc.ObjetoText as TipCalcText,  
}
