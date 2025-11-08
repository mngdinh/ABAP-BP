@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'TB003 - BP Role Search Help'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable: true
define view entity Z_I_TB003 as select from tb003
{
    @Search.defaultSearchElement: true
    key role as Role,
    rolecategory as Rolecategory,
    stnd_rolecat as StndRolecat,
    bpview as Bpview
}
