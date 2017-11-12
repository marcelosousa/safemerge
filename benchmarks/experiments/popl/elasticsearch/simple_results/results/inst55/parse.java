public static final String NAME = "wildcard";
@Override
 public Query parse (QueryParseContext parseContext) throws IOException, QueryParsingException
{
  XContentParser parser = parseContext.parser();
  XContentParser.Token token = parser.nextToken();
  if (token != XContentParser.Token.FIELD_NAME)
  {
    throw new QueryParsingException(parseContext, "[wildcard] query malformed, no field");
  }
  else
    ;
  String fieldName = parser.currentName();
  String rewriteMethod = null;
  String value = null;
  float boost = AbstractQueryBuilder.DEFAULT_BOOST;
  String queryName = null;
  token = parser.nextToken();
  if (token == XContentParser.Token.START_OBJECT)
  {
    String currentFieldName = null;
    token = parser.nextToken();
    while (token != XContentParser.Token.END_OBJECT)
    {
      if (token == XContentParser.Token.FIELD_NAME)
      {
        currentFieldName = parser.currentName();
      }
      else
      {
        if ("wildcard".equals(currentFieldName) == 1)
        {
          value = parser.text();
        }
        else
          if ("value".equals(currentFieldName) == 1)
          {
            value = parser.text();
          }
          else
            if ("boost".equals(currentFieldName) == 1)
            {
              boost = parser.floatValue();
            }
            else
              if ("rewrite".equals(currentFieldName) == 1)
              {
                rewriteMethod = parser.textOrNull();
              }
              else
                if ("_name".equals(currentFieldName) == 1)
                {
                  queryName = parser.text();
                }
                else
                {
                  throw new QueryParsingException(parseContext, "[wildcard] query does not support [" + currentFieldName + "]");
                }
      }
      token = parser.nextToken();
    }
    parser.nextToken();
  }
  else
  {
    value = parser.text();
    parser.nextToken();
  }
  if (value == null)
  {
    throw new QueryParsingException(parseContext, "No value specified for prefix query");
  }
  else
    ;
  BytesRef valueBytes;
  MappedFieldType fieldType = parseContext.fieldMapper(fieldName);
  if (fieldType != null)
  {
    fieldName = fieldType.names().indexName();
    valueBytes = fieldType.indexedValueForSearch(value);
  }
  else
  {
    valueBytes = new BytesRef(value);
  }
  WildcardQuery wildcardQuery = new WildcardQuery(new Term(fieldName, valueBytes));
  QueryParsers.setRewriteMethod(wildcardQuery, parseContext.parseFieldMatcher(), rewriteMethod);
  wildcardQuery.setRewriteMethod(QueryParsers.parseRewriteMethod(parseContext.parseFieldMatcher(), rewriteMethod));
  wildcardQuery.setBoost(boost);
  if (queryName != null)
  {
    parseContext.addNamedQuery(queryName, wildcardQuery);
  }
  else
    ;
  return wildcardQuery;
}
