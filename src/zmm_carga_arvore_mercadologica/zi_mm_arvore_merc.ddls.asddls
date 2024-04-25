@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Programa de Carga - Árvore Mercadológica'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define root view entity ZI_MM_ARVORE_MERC
  as select from ztmm_arvore_merc

  association to balhdr                             as _LogHeader on  _LogHeader.object    = 'ZMM_ARVORE_MERC'
                                                                  and _LogHeader.subobject = 'CARGA'
                                                                  and _LogHeader.extnumber = $projection.Uuid

  composition [0..*] of ZC_MM_ARVORE_MERCADORIA_LOG as _Mensagens

{
  key uuid                  as Uuid,
      filename              as Filename,
      created_by            as CreatedBy,
      created_date          as CreatedDate,
      created_at            as CreatedAt,
      last_changed_by       as LastChangedBy,
      last_changed_date     as LastChangedDate,
      last_changed_at       as LastChangedAt,
      local_last_changed_at as LocalLastChangedAt,
      
      case 
        when _LogHeader.msg_cnt_al is not initial and _LogHeader.msg_cnt_e is initial
         then 'Importado com sucesso'
        when _LogHeader.msg_cnt_e is not initial
         then 'Importado com erros' 
         else ''
      end as StatusText,
      
      case 
        when _LogHeader.msg_cnt_al is not initial and _LogHeader.msg_cnt_e is initial
         then 3
        when _LogHeader.msg_cnt_e is not initial
         then 1 
         else 0
      end as StatusCor,

      _Mensagens
}
