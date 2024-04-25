@AbapCatalog.sqlViewName: 'ZIMATCHARMER'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Caracteristica de Material'
define view ZI_MATERIAL_CHARMER
  as select from mara
{
  key matnr,
      cast('' as atnam)      as NAME_CHAR, 
      cast('' as atbez)      as DESCR_CHAR,
      cast('' as relevanzkz) as RELEVANCY,
      cast('' as atwrt30)    as CHAR_VALUE,
      cast('' as atwtb30)    as DESCR_CVAl,
      cast('' as atwrt70)    as CHAR_VALUE_LONG,
      cast('' as atwtb70)    as DESCR_CVAL_LONG
}
