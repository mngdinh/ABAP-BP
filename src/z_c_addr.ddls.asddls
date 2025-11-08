@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Address - View Projection'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity Z_C_ADDR
  as projection on Z_I_ADRR
{
  key Partner,
  key Addrnumber,
      Country,
      Region,
      PostalCode,
      Street,
      City,
      NameCompany,
      HouseNumber,
      /* Associations */
      _Root : redirected to parent Z_C_ROOT
}
