private void setPathLevel ()
{
  ObjectMapper objectMapper = shardContext.nestedScope().getObjectMapper();
  if (objectMapper == null)
  {
    parentFilter = shardContext.bitsetFilter(Queries.newNonNestedFilter());
  }
  else
  {
    parentFilter = shardContext.bitsetFilter(objectMapper.nestedTypeFilter());
  }
  childFilter = shardContext.bitsetFilter(nestedObjectMapper.nestedTypeFilter());
  parentObjectMapper = shardContext.nestedScope().nextLevel(nestedObjectMapper);
  return;
}