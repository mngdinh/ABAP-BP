@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'TB001 - Grouping Search Help'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable: true
define view entity Z_I_GROUPING 
    as select from tb001
    inner join tb002 on tb001.bu_group = tb002.bu_group
{
    @Search.defaultSearchElement: true
    @EndUserText.label: 'Grouping'
    key tb001.bu_group as BuGroup,
    tb001.nrrng as Nrrng,
    tb002.txt15 as Txt15,
    tb002.txt40 as Txt40
}
where tb002.spras = $session.system_language
