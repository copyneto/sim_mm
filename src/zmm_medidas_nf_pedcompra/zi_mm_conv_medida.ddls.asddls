@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection View for ZI_MM_CONV_MEDIDA'
define root view entity ZI_MM_CONV_MEDIDA
  provider contract transactional_interface
  as projection on ZI_MM_CONV_MEDIDATP
{
  key Matnr,
  key Lifnr,
  key Cprod,
      UomExt,
      UomInt,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt

}
