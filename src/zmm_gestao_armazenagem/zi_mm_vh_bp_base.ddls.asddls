@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help - J_1BPARID'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MM_VH_BP_BASE as select from lfa1 {
    key lifnr as Partner,
        name1 as PartnerName
}

//j_1bnfdoc {
//   key parid  as Partner,
//       name1  as PartnerName
//}
//group by parid,
//         name1
