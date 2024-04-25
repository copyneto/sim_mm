@AbapCatalog.sqlViewName: 'ZIWLK1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View WLK'
define view ZI_WLK1
  as select from wlk1 
{
  key filia                               as Filia,
  key artnr                               as Artnr,
  key vrkme                               as Vrkme,
  key datbi                               as Datbi,
  key cast( CONCAT('00',lfdnr) as lfdnr5  ) as Lfdnr,
      datab                               as Datab,
      ursac                               as Ursac,
      quell                               as Quell,
      pflkn                               as Pflkn,
      anzal                               as Anzal,
      datae                               as Datae,
      negat                               as Negat,
      aktio                               as Aktio,
      thema                               as Thema,
      strli                               as Strli,
      sstat                               as Sstat,
      lifnr                               as Lifnr,
      strnr                               as Strnr
}
