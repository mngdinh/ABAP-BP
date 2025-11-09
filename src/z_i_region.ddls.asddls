@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'T005S - Region Search Help'
@Metadata.ignorePropagatedAnnotations: true
define view entity Z_I_REGION
  as select from t005s as a
    inner join   t005u as b on  a.land1 = b.land1
                            and a.bland = b.bland
{
  key a.land1 as Land1,
      @Search.defaultSearchElement: true
  key a.bland as Bland,
      b.bezei as Bezei
}
where
  b.spras = $session.system_language
