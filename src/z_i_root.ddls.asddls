@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BUT000 - CDS Root View Entity'
@Metadata.ignorePropagatedAnnotations: true
define root view entity Z_I_ROOT 
    as select from but000
    composition [0..*] of Z_I_ROLE as _Role
    composition [0..*] of Z_I_ADRR as _Addr
{
    key partner as Partner,
    type as Type,
    bu_group as BuGroup,
    bu_sort1 as BuSort1,
    bu_sort2 as BuSort2,
    title as Title,
    name_org1 as NameOrg1,
    name_org2 as NameOrg2,
    name_last as NameLast,
    name_first as NameFirst,
    namemiddle as Namemiddle,
    crusr as Crusr,
    crdat as Crdat,
    crtim as Crtim,
    chusr as Chusr,
    chdat as Chdat,
    chtim as Chtim,
    xdele as DeletionFlag,
    _Role,
    _Addr
}
where xdele <> 'X';
