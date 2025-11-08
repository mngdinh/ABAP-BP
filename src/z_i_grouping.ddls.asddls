@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'TB001 - Grouping Search Help'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable: true
define view entity Z_I_GROUPING 
    as select from tb001
{
    @Search.defaultSearchElement: true
    @EndUserText.label: 'Grouping'
    key bu_group as BuGroup,
    nrrng as Nrrng
}
