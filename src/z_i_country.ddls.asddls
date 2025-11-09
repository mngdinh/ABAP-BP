@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'T005T - Country Search Help'
@Metadata.ignorePropagatedAnnotations: true
define view entity Z_I_COUNTRY as select from t005t
{
    key spras as Spras,
    @Search.defaultSearchElement: true
    key land1 as Land1,
    landx50 as Landx50,
    landx as Landx,
    natio as Natio
}
where spras = $session.system_language
