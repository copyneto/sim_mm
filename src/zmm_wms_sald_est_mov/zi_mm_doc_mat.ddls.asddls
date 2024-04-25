@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS de interface - Docs Material'
define root view entity ZI_MM_DOC_MAT as select from ztmm_doc_mat as _main
association[1..1] to I_Product as _Product on _Product.Product = $projection.Matnr
association[1..1] to I_ProductDescription as _Desc on _Desc.Product = $projection.Matnr
                                                  and _Desc.Language = $session.system_language
{
    key _main.mblnr as Mblnr,
    key _main.mjahr as Mjahr,
    key _main.zeile as Zeile,
    _main.ztipo_doc as ZtipoDoc,
    _main.zstatus_integracao as ZstatusIntegracao,
    _main.zorigem as Zorigem,
    _main.matnr as Matnr,
    _Desc.ProductDescription,
    _Product.ProductType,
    _Product.ProductGroup,
    _main.bwart as Bwart,
    _main.shkzg as Shkzg,
    _main.vbeln_im as VbelnIm,
    _main.vbelp_im as VbelpIm,
    _main.werks as Werks,
    _main.lgort as Lgort,
    _main.umwrk as Umwrk,
    _main.umlgo as Umlgo,
    _main.erfmg as Erfmg,
    _main.erfme as Erfme,
    _main.zqtenv as Zqtenv,
    _main.meins as Meins,
    _main.smbln as Smbln,
    _main.sjahr as Sjahr,
    _main.smblp as Smblp,
    _main.xnull as Xnull,
    _main.ernam as Ernam,
    _main.erdat as Erdat,
    _main.erzet as Erzet,
    _main.aenam as Aenam,
    _main.aedat as Aedat,
    _main.aezet as Aezet,
    _main.type as Type,
    _main.zid as Zid,
    _main.znumber as Znumber,
    _main.zmessage as Zmessage,
    _Product,
    _Desc
}
