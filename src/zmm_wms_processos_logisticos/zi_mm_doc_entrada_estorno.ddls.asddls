@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Doc mercadorias de entrada/estorno'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define view entity ZI_MM_DOC_ENTRADA_ESTORNO as select from nsdm_e_mseg

  association[1..1] to nsdm_e_mseg as _mseg_h on _mseg_h.smbln = $projection.mblnr 
                                             and _mseg_h.sjahr = $projection.mjahr
                                             and _mseg_h.smblp = $projection.zeile
                                             and _mseg_h.shkzg = 'H'

{   
      key mblnr,
      key _mseg_h.smbln, //
      key mjahr,
      key _mseg_h.sjahr,
      key zeile,
      key _mseg_h.smblp,
      vbeln_im,
      vbelp_im,
      shkzg,
      _mseg_h.shkzg as estorno,
      case 
        when shkzg = 'S' then 'X'
        else ''
      end as mercadoria_entrada,
      
      case 
        when _mseg_h.shkzg = 'H' then 'X'
        else ''
      end as estorno_entrada
    
} 
  //where shkzg = 'S'
