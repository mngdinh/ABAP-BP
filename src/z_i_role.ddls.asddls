@AbapCatalog.viewEnhancementCategory: [#NONE]   
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BUT100 - CDS View Entity'
@Metadata.ignorePropagatedAnnotations: true
define view entity Z_I_ROLE 
    as select from but100 as b
    inner join tb003t as t on b.rltyp = t.role
    association to parent Z_I_ROOT as _Root on $projection.Partner = _Root.Partner
{
    key b.partner as Partner,
    key b.rltyp as Rltyp,
    key b.dfval as Dfval,
    b.valid_from as ValidFrom,
    b.valid_to as ValidTo,
    t.rltxt as RoleName,
//    role as Role,
//    authority as Authority
    _Root
}
where t.spras = $session.system_language
