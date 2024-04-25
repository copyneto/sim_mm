@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'App para Cadastro de Condições PMPF'

define root view entity ZI_MM_CADASTRO_PMPF
  as select from ztmm_pmpf
{
  key    land1                 as Chave_pais,
  key    txreg                 as Regiao_fiscal,
  key    matnr                 as Material,
  key    validfrom             as Valido_des,
         validto               as Valido_ate,
         price                 as Preco,
         factor                as Num_uni,
         unit                  as Uni_preco,
         @Semantics.user.createdBy: true
         created_by            as Criado_por,
         @Semantics.systemDateTime.createdAt: true
         created_at            as Criado_em,
         @Semantics.user.lastChangedBy: true
         last_changed_by       as Modificado_por,
         @Semantics.systemDateTime.lastChangedAt: true
         last_changed_at       as Modificado_em,
         @Semantics.systemDateTime.localInstanceLastChangedAt: true
         local_last_changed_at as Hora
}
