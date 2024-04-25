@AbapCatalog.sqlViewName: 'ZIMATNRSORT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sortimento Material'
define view ZI_MATERIAL_SORT
  as select from ZI_WLK1 as WLK1
    inner join   wrsz on  WLK1.Filia = wrsz.asort
                      and WLK1.Lfdnr = wrsz.lfdnr 

{
  key  WLK1.Artnr as matnr,
       WLK1.Datbi,
       WLK1.Filia,
       WLK1.Lfdnr,
       WLK1.Datab,
       WLK1.Ursac,
       WLK1.Quell,
       WLK1.Datae,
       wrsz.locnr,
       wrsz.vkorg,
       wrsz.vtweg,
       wrsz.spart,
       wrsz.sonut
}
where
  WLK1.Datbi >= $session.system_date
