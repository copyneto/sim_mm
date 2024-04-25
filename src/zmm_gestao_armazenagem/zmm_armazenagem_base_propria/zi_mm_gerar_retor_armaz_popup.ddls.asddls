@EndUserText.label: 'CDS - Gerar retorno de armazenagem'
define abstract entity ZI_MM_GERAR_RETOR_ARMAZ_POPUP 
{   
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_MM_VH_BP_BASE', element: 'Partner' } }]
    BpCliente      :  j_1bparid;
    @Consumption.valueHelpDefinition: [{ entity:{ name: 'ZI_MM_VH_MODALI_FRETE', element: 'ModalidadeFrete' } }]
    ModalidadeFrete: j_1b_freight_mode;
    AgenteFrete    : j_1bparid;
    Placa          : j_1b_vehicle_lic_pl;
    InfoDanfe      : gho_alloc_url;
    QtdRes         : abap.dec(15,3);
    
}
