"Name: \FU:J_1B_NFE_PROCESS_OUTBOUND\SE:BEGIN\EI
ENHANCEMENT 0 ZEIMM_J_1B_NFE_PROCESS_OUT.

*FIELD-SYMBOLS <fs_mseg> TYPE ANY TABLE.
*DATA: lt_mseg     TYPE TABLE OF mseg,
*      ls_parnad   TYPE j_1binnad,
*      ls_address  TYPE sadr,
*      ls_address1 TYPE addr1_val.
*
*ASSIGN ('(SAPMM07M)XMSEG[]') TO <fs_mseg>.
*IF <fs_mseg> IS ASSIGNED.
*  lt_mseg = <fs_mseg>.
*
*  DATA(ls_mseg) = lt_mseg[ 1 ].
*
*  IF ( ls_mseg-bwart EQ 'ZZZ'
*    OR ls_mseg-bwart EQ 'ZEB')
*  AND ls_mseg-lfbnr IS NOT INITIAL.
*
*    SELECT bp_cliente,
*           modalidade_frete,
*           agente_frete,
*           placa
*      FROM ztmm_base_propr
*      WHERE notafiscal     = @ls_mseg-lfbnr
*        AND notafiscalitem = @ls_mseg-lfpos
*
*    UNION
*
*    SELECT bp_cliente,
*           modalidade_frete,
*           agente_frete,
*           placa
*      FROM ztmm_base_terc
*      WHERE notafiscal     = @ls_mseg-lfbnr
*        AND notafiscalitem = @ls_mseg-lfpos
*      INTO TABLE @DATA(lt_frete).
*    IF sy-subrc EQ 0.
*      DATA(ls_frete) = lt_frete[ 1 ].
*
*      cs_header-placa    = ls_frete-placa.
*      cs_header-modfrete = ls_frete-modalidade_frete.
*
**      CALL FUNCTION 'J_1B_NF_VENDOR_READ'
**        EXPORTING
**          partner_id        = ls_frete-agente_frete
**          read_address      = 'X'
**        IMPORTING
**          parnad            = ls_parnad
**          address           = ls_address
**          address1          = ls_address1
**        EXCEPTIONS
**          partner_not_found = 1
**          address_not_found = 2
**          OTHERS            = 3.
**      IF sy-subrc IS INITIAL.
**        DATA(ls_partner) = CORRESPONDING j_1bnfnad( ls_parnad ).
**        ls_partner-docnum = i_docnum.
**        ls_partner-parvw  = 'SP'.
**        ls_partner-parid  = ls_frete-agente_frete.
**        ls_partner-partyp = 'V'.
**        APPEND ls_partner TO wk_partner.
**      ENDIF.
*
*    ENDIF.
*  ENDIF.
*ENDIF.
ENDENHANCEMENT.
