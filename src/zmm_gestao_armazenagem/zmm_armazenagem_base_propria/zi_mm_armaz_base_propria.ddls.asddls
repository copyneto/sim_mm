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
define root view entity ZI_MM_ARMAZ_BASE_PROPRIA as select from ZI_MM_ARMAZ_BASE_PROPRIA_CE as _T_BASE_PROPRIA 
                                                               
        association[1..1] to I_MaterialDocumentHeader as _MaterialDocHeader on $projection.MaterialDocYear = _MaterialDocHeader.MaterialDocumentYear
                                                                           and $projection.MaterialDoc     = _MaterialDocHeader.MaterialDocument    
          
        association[1..1] to I_BR_Plant               as _Plant             on _Plant.Plant = $projection.Centro
        association[1..1] to I_Material               as _Material          on _Material.Material = $projection.Material
        association[1..1] to I_EWM_BusinessPartner    as _BusinessPartner   on _BusinessPartner.BusinessPartner = $projection.BusinessPartner
              
        association[1..1] to ZI_CA_GET_PARAMETER      as _ParameterP        on _ParameterP.Modulo = 'MM'
                                                                           and _ParameterP.Chave1 = 'ZMM_GESTAO_ARMAZENAGEM'
                                                                           and _ParameterP.Chave2 = 'BWART'
                                                                           and _ParameterP.Chave3 = 'PERDA'
                                                                             
        association[1..1] to ZI_CA_GET_PARAMETER      as _ParameterG        on _ParameterG.Modulo = 'MM'
                                                                           and _ParameterG.Chave1 = 'ZMM_GESTAO_ARMAZENAGEM'
                                                                           and _ParameterG.Chave2 = 'BWART'
                                                                           and _ParameterG.Chave3 = 'GANHO'
{
   key MaterialDocYear,
   key MaterialDoc,
   key MaterialDocItem,
       NotaFiscal,  
       NotaFiscalItem, 
       ////////////////////////////////   Campos de relatorio /////////////////////////////////////                      
      _MaterialDocHeader.ReferenceDocument                 as NfeRemessa, 
      _MaterialDocHeader.DocumentDate                      as Data,      
       Centro,                                             // Centro
      _Plant.PlantName                                     as NomeCentro, 
       Material,                                           // Material
      _Material._Text[ Language =  $session.system_language ].MaterialName,
       @Semantics.quantity.unitOfMeasure: 'UniMedidaBasica'
       QtdDescarga,                                        // Quantidade descarga(coluna editável), 
       @Semantics.quantity.unitOfMeasure: 'UniMedidaBasica'
       QtsFaturada, // (não editável),                     // Quantidade Faturada
       
       ////////////////////////////////////////////////////////////////////////////////////////////////////
       @Semantics.quantity.unitOfMeasure: 'UniMedidaBasica'
       _Soma.Qtd_retorno,
       @Semantics.quantity.unitOfMeasure: 'UniMedidaBasica'
       cast(( coalesce(QtsFaturada, 0) - coalesce(_Soma.Qtd_retorno, 0)) as menge_d)  as   QtdReceb,
       /////////////////////////////////////////////////////////////////////////////////////////////////////
       @Semantics.quantity.unitOfMeasure: 'UniMedidaBasica'
       PerdaGanho                                          as PerdaGanho, 
       @Semantics.amount.currencyCode:'SalesDocumentCurrency'
       cast(PerdaGanho * PreLiq as j_1bnfpri)              as valorPerdaGanho, 
       MotivoMov,                                          // Motivo do Movimento
       BusinessPartner,                                    // Parceiro                 
      _BusinessPartner.BusinessPartnerName                 as NomeBP, 
       Doc_Material_Retorno,
       Ano_Material_Retorno,
       Status,                                             // Status, 
      _Status.StatusText,                                              
       Estoque_Ajustado,                                   // Estoque Ajustado, 
       Estoque_Ajustado_Desc,
       Doc_Material_Ajuste,                                // Doc Material Ajuste,
       Ano_Material_Ajuste,                                // Ano Material Ajuste,
       BR_NotaFiscal,                                      // Documento de retorno
       /////////////////////////////////////////////////////////////////////////////////////////
       ////      Navegação para APP Entrada de mercadorias Documento de material         ///////
       MaterialDoc                                         as MaterialDocumentEntrada,
       Doc_Material_Retorno                                as MaterialDocumentRetorno,
       Doc_Material_Ajuste                                 as MaterialDocumentAjuste, 
       MaterialDocYear                                     as MaterialDocumentYearEntrada,
       Ano_Material_Retorno                                as MaterialDocumentYearRetorno,
       Ano_Material_Ajuste                                 as MaterialDocumentYearAjuste,
       /////////////////////////////////////////////////////////////////////////////////////////
       SalesDocumentCurrency,
       UniMedidaBasica,
       Deposito,
       Empresa,
       @Semantics.amount.currencyCode:'SalesDocumentCurrency'
       PreLiq,       
       
       Bp_Cliente,      
       Modalidade_Frete,
       Agente_Frete,    
       Placa,           
       Info_Danfe,      
      _Material.MaterialType,
       GoodsMovementType,
       ReversedMaterialDocument,
       
       
       ////////////////  calcula o tipo de movimento de acordo com o saldo ////////////////////
       case 
        when PerdaGanho < 0 then _ParameterP.Low
        when PerdaGanho > 0 then _ParameterG.Low
        else ''
       end                                                 as TyMove,
       
       case 
        when PerdaGanho < 0 then 'Error'
        when PerdaGanho > 0 then 'Success'
        else ''
       end                                                 as Criticality,
      
      // Associations /////////////////////////////////////////////////////////////////////////
      _Plant,  
      _Material
}
    where _Material.MaterialType = 'ZFER'
      and GoodsMovementType = 'ZBP'
      and ReversedMaterialDocument is initial

