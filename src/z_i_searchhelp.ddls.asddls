@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View - SearchHelp'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable: true
define view entity Z_I_SEARCHHELP 
as select from Z_I_ROOT as BP
left outer join Z_I_ROLE as Role on Role.Partner = BP.Partner
{
    @Search.defaultSearchElement: true
    @EndUserText.label: 'Business Partner'
    key BP.Partner,
    
    @Search.defaultSearchElement: true
    @EndUserText.label: 'BP Category'
    BP.Type,
    
    @Search.defaultSearchElement: true
    @EndUserText.label: 'BP Group'
    BP.BuGroup,
    
    @Search.defaultSearchElement: true
    @EndUserText.label: 'Search Term'
    BP.BuSort1,
    
    BP.NameOrg1,
    
    BP.NameOrg2,
    
    BP.NameLast,
    
    BP.NameFirst,
    
    BP.Namemiddle,
    
    Role.Rltyp
}
