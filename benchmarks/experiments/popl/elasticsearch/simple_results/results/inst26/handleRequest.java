@Override
 protected UpdateByQueryRequest handleRequest (RestRequest request, RestChannel channel, Client client) throws Exception
{
  UpdateByQueryRequest internalRequest = new UpdateByQueryRequest(new SearchRequest());
  int scrollSize = internalRequest.getSearchRequest().source().size();
  internalRequest.getSearchRequest().source().size(SIZE_ALL_MATCHES);
  BytesReference bodyContent = null;
  if (RestActions.hasBodyContent(request) == 0)
  {
    bodyContent = RestActions.getRestContent(request);
    Tuple<XContentType, Map<String, Object>> body = XContentHelper.convertToMap(bodyContent, false);
    boolean modified = false;
    String conflicts = (String) body.v2().remove("conflicts");
    if (conflicts != null)
    {
      internalRequest.setConflicts(conflicts);
      modified = true;
    }
    else
      ;
    @SuppressWarnings("unchecked")
    Map<String, Object> script = (Map<String, Object>) body.v2().remove("script");
    if (script != null)
    {
      internalRequest.setScript(Script.parse(script, false, parseFieldMatcher));
      modified = true;
    }
    else
      ;
    if (modified)
    {
      XContentBuilder builder = XContentFactory.contentBuilder(body.v1());
      builder.map(body.v2());
      bodyContent = builder.bytes();
    }
    else
      ;
  }
  else
    ;
  RestSearchAction.parseSearchRequest(internalRequest.getSearchRequest(), indicesQueriesRegistry, request, parseFieldMatcher, aggParsers, suggesters, bodyContent);
  String conflicts = request.param("conflicts");
  if (conflicts != null)
  {
    internalRequest.setConflicts(conflicts);
  }
  else
    ;
  parseCommon(internalRequest, request);
  internalRequest.setSize(internalRequest.getSearchRequest().source().size());
  internalRequest.setPipeline(request.param("pipeline"));
  internalRequest.getSearchRequest().source().size(request.paramAsInt("scroll_size", scrollSize));
  if (request.hasParam("search_timeout") == 0)
  {
    internalRequest.getSearchRequest().source().timeout(request.paramAsTime("search_timeout", null));
  }
  else
    ;
  execute(request, internalRequest, channel);
  return internalRequest;
}
