@AbapCatalog.sqlViewAppendName: 'ZIPRODUCTEXT'
@EndUserText.label: 'A_PRODUCT Extension'
extend view A_Product with ZI_PRODUCT_EXT
  association [1..1] to mara                as _Material           on $projection.product = _Material.matnr
  association [0..*] to zi_material_group   as _MatgroupExt        on $projection.product = _MatgroupExt.matnr
  association [0..*] to ZI_MATERIAL_SORT    as _MatsortExt         on $projection.product = _MatsortExt.matnr
  association [0..*] to ZI_MATERIAL_CHARMER as _MatCharacteriscExt on $projection.product = _MatCharacteriscExt.matnr

{
  _Material.oihmtxgr,
  _MatgroupExt,
  _MatsortExt,
  _MatCharacteriscExt 
}
