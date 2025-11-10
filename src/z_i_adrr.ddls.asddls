@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Address - CDS View Entity'
@Metadata.ignorePropagatedAnnotations: true
define view entity Z_I_ADRR
  as select from but020 as b
    inner join   adrc   as a on b.addrnumber = a.addrnumber
  association to parent Z_I_ROOT as _Root on $projection.Partner = _Root.Partner
{
  key b.partner    as Partner,
  key b.addrnumber as Addrnumber,
      a.country    as Country,
      a.region     as Region,
      a.post_code1 as PostalCode,
      a.street     as Street,
      a.city1      as City,
      a.name_co    as NameCompany,
      a.house_num1 as HouseNumber,
      b.xdfadr as StandardAddress,
      _Root
}
