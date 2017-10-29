public static final String NAME = "span_first";
static final SpanFirstQueryBuilder SPAN_FIRST_QUERY_BUILDER = new SpanFirstQueryBuilder(null, -1);
private float boost = -1;
private final int end;
private final SpanQueryBuilder matchBuilder;
private String queryName;
@Override
 protected void doXContent (XContentBuilder builder, Params params) throws IOException
{
  builder.startObject(NAME);
  builder.field("match");
  matchBuilder.toXContent(builder, params);
  builder.field("end", end);
  if (boost != -1)
  {
    builder.field("boost", boost);
  }
  else
    ;
  if (queryName != null)
  {
    builder.field("_name", queryName);
  }
  else
    ;
  builder.endObject();
  return;
}
