@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Role - View Projection'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity Z_C_ROLE as projection on Z_I_ROLE
{
    key Partner,
    key Rltyp,
    key Dfval,
    ValidFrom,
    ValidTo,
    /* Associations */
    _Root: redirected to parent Z_C_ROOT
}
