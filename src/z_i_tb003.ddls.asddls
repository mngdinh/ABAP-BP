@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'TB003 - BP Role Search Help'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable: true
define view entity Z_I_TB003 
    as select from tb003 as a
    inner join tb003t as b on a.role = b.role
    
{
    @Search.defaultSearchElement: true
    key a.role as Role,
    a.rolecategory as Rolecategory,
    a.stnd_rolecat as StndRolecat,
    a.bpview as Bpview,
    b.rltitl as Rltitl,
    b.rltxt as Rltxt
}
where b.spras = $session.system_language
