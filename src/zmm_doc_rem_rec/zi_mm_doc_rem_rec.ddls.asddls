@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_MM_DOC_REM_REC
  as select from ztmm_doc_rem_rec
{
  key vbeln              as Vbeln,
  key posnr              as Posnr,
      werks              as Werks,
      lgort              as Lgort,
      ztipo_doc          as ZtipoDoc,
      zstatus_integracao as ZstatusIntegracao,
      @Semantics.quantity.unitOfMeasure: 'Meins'
      menge              as Menge,
      zqtrec             as Zqtrec,
      zqtpen             as Zqtpen,
      meins              as Meins,
      cancel             as Cancel,
      mblnr              as Mblnr,
      mjahr              as Mjahr,
      move_stloc         as MoveStloc,
      ernam              as Ernam,
      erdat              as Erdat,
      erzet              as Erzet,
      aenam              as Aenam,
      aedat              as Aedat,
      aezet              as Aezet,
      type               as Type,
      id                 as Id,
      znumber            as Znumber,
      message            as Message
}
