@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS Value Help - ZTipoDoc'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity ZI_MM_VH_TIPO_DOC as select from dd07t
{        
      key domvalue_l as ZtipoDoc,
          ddtext as ZtipoDocDesc
}
where domname = 'ZD_DOC'
  and ddlanguage = $session.system_language
