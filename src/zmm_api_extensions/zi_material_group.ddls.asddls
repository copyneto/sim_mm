@AbapCatalog.sqlViewName: 'ZIMATGROUP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Grupo de materiais'
define view zi_material_group
  as select from wrf_matgrp_prod  as _Matgroup
    inner join   wrf_matgrp_strct as _MatgroupText on  _Matgroup.hier_id = _MatgroupText.hier_id
                                                   and _Matgroup.node    = _MatgroupText.node
{
  key _Matgroup.matnr,
      _Matgroup.hier_id,
      _Matgroup.node,
      _Matgroup.date_from,
      _Matgroup.date_to,
      _Matgroup.mainflg,
      _Matgroup.hiernode1,
      _Matgroup.hiernode2,
      _Matgroup.hiernode3,
      _Matgroup.hiernode4,
      _Matgroup.hiernode5,
      _MatgroupText.ltextg 
}
where
      _MatgroupText.spras = $session.system_language 
  and _Matgroup.date_to   >= $session.system_date
  and _Matgroup.date_from <= $session.system_date
