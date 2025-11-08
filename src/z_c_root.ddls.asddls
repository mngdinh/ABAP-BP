@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Root - View Projection'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity Z_C_ROOT as projection on Z_I_ROOT
{
    key Partner,
    Type,
    BuGroup,
    BuSort1,
    BuSort2,
    Title,
    NameOrg1,
    NameOrg2,
    NameLast,
    NameFirst,
    Namemiddle,
    Crusr,
    Crdat,
    Crtim,
    Chusr,
    Chdat,
    Chtim,
    DeletionFlag,
    /* Associations */
    _Role: redirected to composition child Z_C_ROLE,
    _Addr: redirected to composition child Z_C_ADDR
}
