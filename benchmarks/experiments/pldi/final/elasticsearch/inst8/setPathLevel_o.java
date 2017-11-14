private void setPathLevel ()
{
  ObjectMapper objectMapper = parseContext.nestedScope().getObjectMapper();
  if (objectMapper == null)
  {
    parentFilter = parseContext.bitsetFilter(Queries.newNonNestedFilter());
  }
  else
  {
    parentFilter = parseContext.bitsetFilter(objectMapper.nestedTypeFilter());
  }
  childFilter = parseContext.bitsetFilter(nestedObjectMapper.nestedTypeFilter());
  parentObjectMapper = parseContext.nestedScope().nextLevel(nestedObjectMapper);
  return;
}