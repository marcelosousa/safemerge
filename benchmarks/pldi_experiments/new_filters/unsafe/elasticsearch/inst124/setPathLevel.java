protected BitDocIdSetFilter childFilter;
protected boolean filterFound = false;
private boolean filterParsed = false;
private Query innerFilter;
private Query innerQuery;
protected ObjectMapper nestedObjectMapper;
protected BitDocIdSetFilter parentFilter;
private ObjectMapper parentObjectMapper;
protected final QueryParseContext parseContext;
protected String path;
protected boolean queryFound = false;
private boolean queryParsed = false;
private BytesReference source;
protected final QueryShardContext shardContext;
protected Filter childFilter;
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
  childFilter = nestedObjectMapper.nestedTypeFilter();
  parentObjectMapper = shardContext.nestedScope().nextLevel(nestedObjectMapper);
  return;
}
