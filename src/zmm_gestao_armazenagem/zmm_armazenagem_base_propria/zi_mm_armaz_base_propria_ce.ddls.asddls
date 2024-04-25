@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS - Gestao de Armazenagem base própria'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
 
/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define root view entity ZI_MM_ARMAZ_BASE_PROPRIA_CE 
as select from I_MaterialDocumentItem             as _MaterialDocItem 
           left outer join  ztmm_base_propr                as _T_BASE_PROPRIA on _T_BASE_PROPRIA.materialdocyear        = _MaterialDocItem.MaterialDocumentYear
                                                                             and _T_BASE_PROPRIA.materialdoc            = _MaterialDocItem.MaterialDocument    
                                                                             and _T_BASE_PROPRIA.materialdocitem        = _MaterialDocItem.MaterialDocumentItem
                                                                              
    association[1..1] to ZI_MM_Nota_Doc_Material           as _NFItem         on _NFItem.DocMaterial                    = $projection.MaterialDoc
                                                                             and _NFItem.AnoDocMaterial                 = $projection.MaterialDocYear
                                                                             and _NFItem.BR_NFSourceDocumentItem        = $projection.MaterialDocItem
                          
    association[1..1] to ZI_MM_Nota_Doc_Material           as _NFDocRetorno   on _NFDocRetorno.DocMaterial              = $projection.Doc_Material_Retorno
                                                                             and _NFDocRetorno.AnoDocMaterial           = $projection.Ano_Material_Retorno
                                                                             and _NFDocRetorno.BR_NFSourceDocumentItem  = $projection.MaterialDocItem  
                                                                             and _NFDocRetorno.BR_NFIsCanceled          is initial
                                                                             
    association[1..1] to ZI_MM_GERAR_RETOR_SOMA            as _Soma           on _Soma.DocReferencia = $projection.NotaFiscal
                                                                    
    association[1..1] to ZI_MM_GERAR_RETOR_ARMAZ_STATUS    as _Status         on _Status.Status                         = $projection.Status
    
    
    
                                                     
{
       /////// Campos Chaves  
   key _MaterialDocItem.MaterialDocumentYear                       as MaterialDocYear,
   key _MaterialDocItem.MaterialDocument                           as MaterialDoc,
   key _MaterialDocItem.MaterialDocumentItem                       as MaterialDocItem,
       
       ////// Nota do documento de material
       _NFItem.BR_NotaFiscal                                       as NotaFiscal,
       _NFItem.BR_NotaFiscalItem                                   as NotaFiscalItem,
       _NFItem.BR_NFPriceAmountWithTaxes                           as PreLiq,
       @Semantics.amount.currencyCode:'SalesDocumentCurrency'
       _NFItem.BR_NFValueAmountWithTaxes,    
       _NFItem.NetPriceAmount,                                      
       _NFItem.SalesDocumentCurrency,
      
       ////// 
       _MaterialDocItem.Plant                                      as Centro,
       _MaterialDocItem.Material                                   as Material,
       _MaterialDocItem.Customer                                   as BusinessPartner,
       _MaterialDocItem.GoodsMovementType                          as GoodsMovementType, 
       _MaterialDocItem.StorageLocation                            as Deposito,
       _MaterialDocItem.CompanyCode                                as Empresa,
       _MaterialDocItem.MaterialBaseUnit                           as UniMedidaBasica,  
       @Semantics.quantity.unitOfMeasure: 'UniMedidaBasica'
       _MaterialDocItem.QuantityInBaseUnit                         as QtsFaturada, 
       //@Semantics.quantity.unitOfMeasure: 'UniMedidaBasica'
       //_MaterialDocItem.QuantityInBaseUnit                         as QtdReceb,                         
       _MaterialDocItem._BusinessPartner.BusinessPartnerName       as NomeBP, 
       _MaterialDocItem.ReversedMaterialDocument                   as ReversedMaterialDocument,
       /////////
          
       _T_BASE_PROPRIA.motivomov                                   as MotivoMov,                                                             
       _T_BASE_PROPRIA.doc_material_ajuste                         as Doc_Material_Ajuste,
       _T_BASE_PROPRIA.ano_material_ajuste                         as Ano_Material_Ajuste,
       _T_BASE_PROPRIA.doc_material_retorno                        as Doc_Material_Retorno,
       _T_BASE_PROPRIA.ano_material_retorno                        as Ano_Material_Retorno,
       _T_BASE_PROPRIA.estoque_ajustado                            as Estoque_Ajustado,
       _T_BASE_PROPRIA.bp_cliente                                  as Bp_Cliente,      
       _T_BASE_PROPRIA.modalidade_frete                            as Modalidade_Frete,
       _T_BASE_PROPRIA.agente_frete                                as Agente_Frete,    
       _T_BASE_PROPRIA.placa                                       as Placa,           
       _T_BASE_PROPRIA.info_danfe                                  as Info_Danfe,      
       ////// Nota do documento de material de retorno
      _NFDocRetorno.BR_NotaFiscal,
      _NFDocRetorno.BR_NFIsCanceled,
      
      /////  campos calculados
      @Semantics.quantity.unitOfMeasure: 'UniMedidaBasica'
       case 
        when _T_BASE_PROPRIA.qtddescarga is null then cast( 0 as menge_d )
        else _T_BASE_PROPRIA.qtddescarga  
       end                                                         as QtdDescarga, // (coluna editável), 
       
       @Semantics.quantity.unitOfMeasure: 'UniMedidaBasica'
       case 
        when _T_BASE_PROPRIA.qtddescarga is null    then cast( 0 as menge_d ) // (_MaterialDocItem.QuantityInBaseUnit)
        when _T_BASE_PROPRIA.qtddescarga is initial then cast( 0 as menge_d ) //(_MaterialDocItem.QuantityInBaseUnit)
        else (_T_BASE_PROPRIA.qtddescarga - _MaterialDocItem.QuantityInBaseUnit) 
       end                                                                                  as PerdaGanho,
                        
       case _T_BASE_PROPRIA.estoque_ajustado                 
        when 'X' then 'SIM'                 
        else 'NÃO'                       
       end                                                         as Estoque_Ajustado_Desc, 
                        
       case                  
        when _NFDocRetorno.BR_NotaFiscal is initial then ''
        when _NFDocRetorno.BR_NotaFiscal is null    then ''                 
        else 'X'                     
       end                                                         as Status,
       
      
      // Associations
      _Status,
      _NFItem,
      _NFDocRetorno,
      _Soma
}
