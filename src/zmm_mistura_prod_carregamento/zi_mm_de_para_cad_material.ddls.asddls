@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'de-para para dep dif do cad material'
define root view entity ZI_MM_DE_PARA_CAD_MATERIAL
  as select from ztmm_de_para_dep as _material
{
  key werks                 as Centro,
  key matnr                 as Material,
  key lgort                 as Deposito_rev,
      verid                 as Versao,
      @Semantics.user.createdBy: true
      created_by            as Criado_por,
      @Semantics.systemDateTime.createdAt: true
      created_at            as Criado_em,
      @Semantics.user.lastChangedBy: true
      last_changed_by       as Modificado_por,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as Modificado_em,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as Hora
}
