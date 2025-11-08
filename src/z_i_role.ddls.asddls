@AbapCatalog.viewEnhancementCategory: [#NONE]   
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BUT100 - CDS View Entity'
@Metadata.ignorePropagatedAnnotations: true
define view entity Z_I_ROLE 
    as select from but100
    association to parent Z_I_ROOT as _Root on $projection.Partner = _Root.Partner
{
    key partner as Partner,
    key rltyp as Rltyp,
    key dfval as Dfval,
    valid_from as ValidFrom,
    valid_to as ValidTo,
//    role as Role,
//    authority as Authority
    _Root
}
