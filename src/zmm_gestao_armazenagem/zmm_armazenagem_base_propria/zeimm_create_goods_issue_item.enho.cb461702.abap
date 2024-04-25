"Name: \FU:MB_CREATE_GOODS_ISSUE_ITEM\SE:BEGIN\EI
ENHANCEMENT 0 ZEIMM_CREATE_GOODS_ISSUE_ITEM.
IF ( mseg-bwart EQ 'ZZZ'
  OR mseg-bwart EQ 'ZEB')
AND mseg-lfbnr IS NOT INITIAL.
  DATA(lo_nota_arm) = NEW zclmm_nota_armazenagem( ).
  DATA(lv_preco_liquido) = lo_nota_arm->get_preco_liquido( iv_notafiscal = mseg-lfbnr iv_notafiscalitem = mseg-lfpos ).
  IF lv_preco_liquido IS NOT INITIAL.
    mseg-mwskz      = 'KA'.
    mseg-j_1bexbase = mseg-erfmg * lv_preco_liquido.
  ELSE.
    MESSAGE e006(zmm_base_message).
  ENDIF.
ENDIF.
ENDENHANCEMENT.
