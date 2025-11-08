@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help for Business Partner Title'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable: true
define view entity Z_I_TITLE
  as select from tsad3t
{
  key langu as Langu,
  
  @Search.defaultSearchElement: true
  @EndUserText.label: 'Title Code'
  key title as Title,

  @Search.defaultSearchElement: true
  @EndUserText.label: 'Title Description'
  title_medi as TitleText
}
where langu = $session.system_language
