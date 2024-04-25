@EndUserText.label: 'CDS Value Help - I_PLANT'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity ZI_MM_VH_CST60_IPLANT
  as select from ztmm_hist_cst60

  association [1..1] to I_Plant as _Plant on _Plant.Plant = $projection.Werks
{
      //  key nfenum           as Nfenum,

      @EndUserText.label: 'Centro'
      @ObjectModel.text.element: [ 'PlantName' ]
  key werks            as Werks,

      @EndUserText.label: 'Nome do centro'
      _Plant.PlantName as PlantName
}

group by
  werks,
  _Plant.PlantName;
