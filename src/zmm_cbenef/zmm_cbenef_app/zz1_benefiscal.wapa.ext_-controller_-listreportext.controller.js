sap.ui.define(["sap/m/MessageToast","sap/ui/export/Spreadsheet","sap/ui/export/library"],function(e,a,s){"use strict";return sap.ui.controller("br.com.redesim.manutencaobeneffiscal.ext.controller.ListReportExt",{onUpload:function(e){var a=this;var s=new +
sap.m.Dialog({contentWidth:"300px",resizable:true,type:"Message"});s.setTitle("Carregar arquivo Excel");var t="/sap/opu/odata/sap/ZMM_BENEF_FISCAL_SRV/";var r=new sap.ui.model.odata.ODataModel(t,false);var o=t+"UploadFileSet";var i=new sap.ui.unified.Fil+
eUploader({width:"100%",fileType:["xlsx","xls","csv"],typeMissmatch:this.handleTypeMissmatch});i.setName("Simple Uploader");i.setUploadUrl(o);i.setSendXHR(true);i.setUseMultipart(false);i.setUploadOnChange(false);var n=new sap.ui.unified.FileUploaderPara+
meter({name:"x-csrf-token",value:r.getSecurityToken()});i.insertHeaderParameter(n);var p=new sap.m.Button({text:"Upload",type:"Accept",press:function(){var e=i.getFocusDomRef();var t=e.files[0];if(i.getValue()===""){sap.m.MessageToast.show("Favor selecio+
nar um arquivo");return}var r=new sap.ui.unified.FileUploaderParameter({name:"Content-Type",value:t.type});i.insertHeaderParameter(r);i.insertHeaderParameter(new sap.ui.unified.FileUploaderParameter({name:"SLUG",value:i.getValue()}));var o=function(e){re+
turn new Promise(function(e,t){i.attachUploadComplete(function(r){s.close();s.destroy();if(r.mParameters.status=="200"||r.mParameters.status=="201"||r.mParameters.status=="204"){a.showLog(r.mParameters.responseRaw);e()}else{a.showLog(r.mParameters.respon+
seRaw);t()}a.templateBaseExtension.getExtensionAPI().refreshTable()}.bind(this));i.upload()}.bind(this))}.bind(this);var n={sActionLabel:"Upload de Arquivo",dataloss:false};this.extensionAPI.securedExecution(o,n)}.bind(this)});var u=new sap.m.Button({tex+
t:"Cancelar",type:"Reject",press:function(){s.close();s.destroy();this.getView().removeAllDependents()}.bind(this)});s.addContent(i);s.setBeginButton(u);s.setEndButton(p);s.open()},handleTypeMissmatch:function(e){var a=e.getSource().getFileType();jQuery.+
each(a,function(e,s){a[e]="*."+s});var s=a.join(", ");sap.m.MessageToast.show("Tipo de arquivo *."+e.getParameter("fileType")+" nao ? suportado. Escolha um dos tipos a seguir: "+s)},showLog:function(e){let a=[];for(const s of e.matchAll(/(?:<d:message>(.+
*?)<[/]d:message>)/gm)){a.push(s[1])}let s=[];for(const a of e.matchAll(/(?:<d:severity>(.*?)<[/]d:severity>)/gm)){switch(a[1]){case"E":s.push(sap.ui.core.MessageType.Error);break;case"I":s.push(sap.ui.core.MessageType.Information);break;case"S":s.push(s+
ap.ui.core.MessageType.Success);break;case"W":s.push(sap.ui.core.MessageType.Warning);break;default:s.push(sap.ui.core.MessageType.None);break}}for(let e=0;e<a.length;e++){if(a[e]=="Ocorreu uma exceção"){continue}sap.ui.getCore().getMessageManager().addM+
essages(new sap.ui.core.message.Message({message:a[e],persistent:true,type:s[e]}))}}})});                                                                                                                                                                      
//# sourceMappingURL=ListReportExt.controller.js.map                                                                                                                                                                                                           