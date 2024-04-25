@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Soma QTD Retorno'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_GERAR_RETOR_SOMA
  as select from matdoc
{

  key lfbnr               as DocReferencia,
      @Semantics.quantity.unitOfMeasure : 'meins'
      abs(sum(stock_qty)) as Qtd_retorno,
      meins
}
group by
  lfbnr,
  meins
