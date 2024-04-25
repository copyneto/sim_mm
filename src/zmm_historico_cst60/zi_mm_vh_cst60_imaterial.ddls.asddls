@AbapCatalog.viewEnhancementCategory: [#NONE]
@EndUserText.label: 'CDS Value Help - I_MaterialText'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_VH_CST60_IMATERIAL
  as select from ztmm_hist_cst60

  association [1..*] to I_MaterialText as _Material on _Material.Material = $projection.Matnr


{


        //  key nfenum                 as Nfenum,

        @ObjectModel.text.element: ['MaterialName']
        @EndUserText.label: 'Material'
  key   matnr                  as Matnr,

        @EndUserText.label: 'Nome do material'
        _Material.MaterialName as MaterialName

}
group by
  matnr,
  _Material.MaterialName;
