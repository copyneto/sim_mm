sap.ui.define(["sap/ui/comp/library","sap/ui/core/mvc/Controller","sap/ui/model/type/String","sap/m/ColumnListItem","sap/m/Label","sap/m/SearchField","sap/m/Token","sap/ui/model/Filter","sap/ui/model/FilterOperator","sap/ui/model/odata/v2/ODataModel","sa+
p/ui/table/Column","sap/m/Column","sap/m/Text"],function(e,t,a,n,i,r,l,o,s,u,p,c,d){"use strict";return{that:this,onInit:function(){},valueHelpContruct:function(e){var t=new d({text:{path:"Partner"},renderWhitespace:true});this._oBasicSearchField2=new sa+
p.m.SearchField({search:function(){this.oWhitespaceDialog2.getFilterBar().search()}.bind(this)});this.pWhitespaceDialog2=e.loadFragment({name:"br.com.simrede.basepropria.ext.fragment.valueHelpBpCliente"});this.pWhitespaceDialog2.then(function(a){var r=a.+
getFilterBar();var l=e.getView().getModel("i18n").getResourceBundle();this.oWhitespaceDialog2=a;if(this._bWhitespaceDialogInitialized2){a.open();return}e.getView().addDependent(a);r.setFilterBarExpanded(false);r.setBasicSearch(this._oBasicSearchField2);r+
.determineFilterItemByName("Partner").getControl().setTextFormatter(this.inputTextFormatter);a.getTableAsync().then(function(r){r.setModel(e.oModel);r.setSelectionMode("Single");if(r.bindRows){r.addColumn(new p({label:l.getText("BpCliente"),template:t}))+
;r.addColumn(new p({label:l.getText("BpClienteDesc"),template:"PartnerName"}));r.bindAggregation("rows",{path:"/BpBase",events:{dataReceived:function(){a.update()}}})}if(r.bindItems){r.addColumn(new c({header:new i({text:l.getText("BpCliente")})}));r.add+
Column(new c({header:new i({text:l.getText("BpClienteDesc")})}));r.bindItems({path:"/BpBase",template:new n({cells:[new i({text:"{Partner}"}),new i({text:"{PartnerName}"})]}),events:{dataReceived:function(){a.update()}}})}a.update()}.bind(this));this._bW+
hitespaceDialogInitialized2=true;this.ValueHelpDialogCancel=function(){a.close();a.destroy()};this.ValueHelpDialogOk=function(e){var t=e.getParameter("tokens");var n=t[0].getKey();a.close();a.destroy();return n};this.SuggestionItemSelected=function(e){le+
t t;t=[new o("Partner",s.EQ,`'${e.mParameters.selectedItem.mProperties.key}'`)];this.filterTable(new o({filters:t,and:true}))};this.Search=function(e){var t=`'${this._oBasicSearchField2.getValue()}'`,a=e.getParameter("selectionSet");var n=a.reduce(functi+
on(e,t){if(t.getValue()){e.push(new o({path:t.getName(),operator:s.Contains,value1:t.getValue()}))}return e},[]);n.push(new o({filters:[new o({path:"Partner",operator:s.Contains,value1:t}),new o({path:"PartnerName",operator:s.Contains,value1:t})],and:fal+
se}));this.filterTable(new o({filters:n,and:true}))};this.filterTable=function(e){a.getTableAsync().then(function(t){if(t.bindRows){t.getBinding("rows").filter(e)}if(t.bindItems){t.getBinding("items").filter(e)}a.update()})};this.inputTextFormatter=funct+
ion(e){var t=e.getText(),a=" ",n=" ";if(typeof t!=="string"){return t}return t.replaceAll(a+n,a+a)};a.open()}.bind(this));this.onSearchMotivo},onFilterBarSearch:function(e){},onInputSuggestionItemSelected:function(e){},_filterTable:function(e){},onValueH+
elpDialogCancel:function(){},onValueHelpDialogPopUpRetornoRemessaOk:function(e){},onInputSuggestionItemSelected2:function(e){}}});                                                                                                                             
//# sourceMappingURL=ValueHelpBpClienteExt.controller.js.map                                                                                                                                                                                                   