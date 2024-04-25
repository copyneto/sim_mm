@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'CDS View for ZI_MM_CONV_MEDIDA'
define root view entity ZI_MM_CONV_MEDIDATP
  as select from ztmm_conv_medida
{
  key matnr                 as Matnr,
  key lifnr                 as Lifnr,
  key cprod                 as Cprod,
      uom_ext               as UomExt,
      uom_int               as UomInt,
      @Semantics.user.createdBy: true
      created_by            as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at            as CreatedAt,
      @Semantics.user.lastChangedBy: true
      last_changed_by       as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt

}
