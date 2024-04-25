@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - NotaFiscal / Doc Material'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_Nota_Doc_Material 
       as select from I_BR_NFItem  as NFItem 
   
       association [1..1] to I_BR_NFDocument  as _BR_NotaFiscal  on $projection.BR_NotaFiscal = _BR_NotaFiscal.BR_NotaFiscal                                              
       
{ 
   key BR_NotaFiscal,   
   key BR_NotaFiscalItem,  
       BR_NFSourceDocumentNumber,
       substring(BR_NFSourceDocumentNumber, 1,10) as DocMaterial,
       substring(BR_NFSourceDocumentNumber, 11,4) as AnoDocMaterial,
       substring(BR_NFSourceDocumentItem, 3,4)    as BR_NFSourceDocumentItem,
       BR_NFPriceAmountWithTaxes,
       @Semantics.amount.currencyCode:'SalesDocumentCurrency'
       BR_NFValueAmountWithTaxes,
       NetPriceAmount,
       SalesDocumentCurrency,
      _BR_NotaFiscal.BR_NFIsCanceled,
       
      _BR_NotaFiscal
}
