@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Seleção de dados auxiliares'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_MM_BASE_TERCEIRO_AUX
  as select from    A_InbDeliveryItem as _item
    left outer join ztmm_base_terc    as _edit   on  _edit.deliverydocument     = _item.DeliveryDocument
                                                 and _edit.deliverydocumentitem = _item.DeliveryDocumentItem
    left outer join I_BR_NFDocument   as _header on _header.BR_NotaFiscal = _edit.docnumremessa

  association [0..1] to ZI_CA_VH_WERKS      as _plant      on  _plant.WerksCode = $projection.Plant
  association [0..1] to I_Material          as _material   on  _material.Material = $projection.Material
  association [0..*] to ZI_CA_GET_PARAMETER as _param      on  _param.Chave1 = 'COCKPIT_ARMAZENAGEM'
                                                           and _param.Chave2 = 'PERCENTUAL'
                                                           and _param.Modulo = 'MM'
  //  association [0..1] to ZI_MM_BASE_NF_DOC   as _nf         on  _nf.DeliveryDocument     = $projection.DeliveryDocument
  //                                                           and _nf.DeliveryDocumentItem = $projection.DeliveryDocumentItem
  //  association [0..1] to but000              as _bpname   on  _bpname.partner = $projection.supplier

  association [1..*] to ZI_CA_GET_PARAMETER as _ParameterP on  _ParameterP.Modulo = 'MM'
                                                           and _ParameterP.Chave1 = 'ZMM_GESTAO_ARMAZENAGEM'
                                                           and _ParameterP.Chave2 = 'BWART'
                                                           and _ParameterP.Chave3 = 'PERDA'

  association [1..*] to ZI_CA_GET_PARAMETER as _ParameterG on  _ParameterG.Modulo = 'MM'
                                                           and _ParameterG.Chave1 = 'ZMM_GESTAO_ARMAZENAGEM'
                                                           and _ParameterG.Chave2 = 'BWART'
                                                           and _ParameterG.Chave3 = 'GANHO'
  association [1..*] to I_BR_NFItem         as _nfitem     on  _nfitem.BR_ReferenceNFNumber = _edit.docnumremessa

  association [1..1] to I_SupDmndOvwPlant   as _bukrs      on  _bukrs.Plant = _item.Plant

{
  key _item.DeliveryDocument,
  key _item.DeliveryDocumentItem,
      @ObjectModel.virtualElement: true
      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCLMM_VE_BASE_TERC'
      //            cast('' as vgbel)                                                                     as NotaFiscal,
      cast( _edit.notafiscal  as vgbel)                                                     as NotaFiscal,
      @ObjectModel.virtualElement: true
      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCLMM_VE_BASE_TERC'
      //      cast('' as vgpos)                                                                     as NotaFiscalItem,
      cast( _edit.notafiscalitem as vgpos)                                                  as NotaFiscalItem,
      //semaforo
      //      case when _edit.descarga is null and cast( _item.ActualDeliveredQtyInBaseUnit as float ) > ( -1 * ( cast( cast( replace(_param.Low, ',', '.') as abap.numc(255,2) ) as float ) * cast( 100 as float ) ) ) and cast( _item.ActualDeliveredQtyInBaseUnit as float ) - cast( _edit.descarga as float ) < 0 then 2
      //           when _edit.descarga is null and cast( _item.ActualDeliveredQtyInBaseUnit as float ) > 0 then 1
      //           when cast( _item.ActualDeliveredQtyInBaseUnit as float ) - cast( _edit.descarga as float ) > ( -1 * ( cast( cast( replace(_param.Low, ',', '.') as abap.numc(255,2) ) as float ) * cast( 100 as float ) ) ) and cast( _item.ActualDeliveredQtyInBaseUnit as float ) - cast( _edit.descarga as float ) < 0 then 2
      //           when cast( _item.ActualDeliveredQtyInBaseUnit as float ) - cast( _edit.descarga as float ) > 0 then 1
      //           else 3 end                                                                                                                            as semaforo,
      //      1 as semaforo,
      case when _edit.descarga is null or _edit.descarga is initial then 3
           when abs(_edit.descarga) > abs(_item.ActualDeliveredQtyInBaseUnit) then 3
      //           when cast( _item.ActualDeliveredQtyInBaseUnit as float ) / cast( _edit.descarga as float ) > ( 1 * ( cast( cast( replace(_param.Low, ',', '.') as abap.numc(255,2) ) as float ) * cast( 100 as float ) ) )  then 1
      //           when cast( _item.ActualDeliveredQtyInBaseUnit as float ) / cast( _edit.descarga as float ) <= ( 1 * ( cast( cast( replace(_param.Low, ',', '.') as abap.numc(255,2) ) as float ) * cast( 100 as float ) ) )  then 2
           when abs(cast( _edit.descarga as float ) - cast( _item.ActualDeliveredQtyInBaseUnit as float ) ) > abs(( 1 * ( cast( cast( replace(_param.Low, ',', '.') as abap.numc(255,2) ) as float ) ) )  * cast( _item.ActualDeliveredQtyInBaseUnit as float ))  then 1
           when abs(cast( _edit.descarga as float )- cast( _item.ActualDeliveredQtyInBaseUnit as float )  ) <= abs(( 1 * ( cast( cast( replace(_param.Low, ',', '.') as abap.numc(255,2) ) as float ) ) )  * cast( _item.ActualDeliveredQtyInBaseUnit as float )) then 2
           else 3 end                                                                       as semaforo,
      //Número e série nfe remessa
      _item._DeliveryDocument.DeliveryDocumentBySupplier,
      //Data
      _item._DeliveryDocument.DocumentDate,
      //Centro
      _item.Plant,
      _bukrs.CompanyCode                                                                    as bukrs,
      //Nome centro
      _plant.WerksCodeName,
      //Material
      _item.Material,
      //Descrição material
      _material._Text[ Language = $session.system_language ].MaterialName,
      //Quantidade descarga
      _edit.descarga,
      @Semantics.quantity.unitOfMeasure: 'BaseUnit'
      //Quantidade faturada
      _item.ActualDeliveredQtyInBaseUnit,
      //Perda/ganho
      cast( _edit.descarga as float ) - cast( _item.ActualDeliveredQtyInBaseUnit as float ) as perdaganho,
      //Unidade de medida básica
      _item.BaseUnit,
      //Valor perda/ganho
      @ObjectModel.virtualElement: true
      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCLMM_VE_BASE_TERC'
      //      cast( _nf.BR_NFPriceAmountWithTaxes as float ) * ( cast( _item.ActualDeliveredQtyInBaseUnit as float ) - cast( _edit.descarga as float ) ) as valorperdaganho,
      cast(0 as j_1bnfpri)                                                                  as valorperdaganho,
      @ObjectModel.virtualElement: true
      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCLMM_VE_BASE_TERC'
      //      cast(0 as abap.dec(15,3)) as valorperdaganho,
      cast( 0 as j_1bnfpri)                                                                 as BR_NFPriceAmountWithTaxes,
      //Parceiro de negócios
      _item._DeliveryDocument.Supplier,

      _item._DeliveryDocument.Supplier                                                      as BusinessPartner,
      //Docnum remessa
      _edit.docnumremessa,
      //Status
      case
        when ( _edit.estoqueajustado = '' or _edit.estoqueajustado is null )
         and ( _item.ActualDeliveredQtyInBaseUnit <> _edit.descarga or _edit.descarga is null )
            then 'Ajuste de estoque pendente'
        when _edit.status = ''
          or ( _edit.status = '' and _item.ActualDeliveredQtyInBaseUnit = _edit.descarga and _edit.descarga is not null )
          or ( _edit.status <> '' and _header.BR_NFIsCanceled = 'X' )
          or ( _edit.docnumremessa is not initial and _header.BR_NFIsCanceled = 'X' )
          or ( _edit.status is not initial and _edit.docnumremessa is initial )
            then 'Remessa pendente'
        when _edit.status = 'ND'
            then 'Pendente ND'
        else 'Retorno concluído'
      end                                                                                   as status,
      //Estoque ajustado
      case when _edit.docmatajest = '' or _edit.docmatajest is null then 'Não'
        else 'Sim'
      end                                                                                   as estoqueajustado,
      //Documento material ajuste do estoque,
      _edit.docmatajest,
      _edit.docmatajest                                                                     as MaterialDocumentAjuste,
      //Docnum retorno
      _nfitem.BR_NotaFiscal                                                                 as docnumretorno,
      case when _header.BR_NFIsCanceled = ''
        then _edit.docnumremessa
        else cast( '' as abap.numc( 10 ) ) end                                              as BR_NotaFiscal,
      case
        when cast( _edit.descarga as float ) - cast( _item.ActualDeliveredQtyInBaseUnit as float )  < 0 then _ParameterP.Low
        when cast( _edit.descarga as float ) - cast( _item.ActualDeliveredQtyInBaseUnit as float ) > 0 then _ParameterG.Low
        else ''
       end                                                                                  as tipomov,
      _edit.motivomov,
      _item.StorageLocation,
      _edit.bp_cliente,
      _edit.agente_frete,
      _edit.info_danfe,
      _edit.placa,
      _edit.modalidade_frete,
      _edit.docnumremessa                                                                   as MaterialDocumentRertorno
}
where
  _material.MaterialType = 'ZFER'
