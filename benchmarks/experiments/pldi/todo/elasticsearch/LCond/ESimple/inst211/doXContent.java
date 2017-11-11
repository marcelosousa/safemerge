private float boost = -1;
private Boolean disableCoord;
private String execution;
private String minimumShouldMatch;
private final String name;
private String queryName;
private final Object values;
public static final String NAME = "terms";
static final TermsQueryBuilder PROTOTYPE = new TermsQueryBuilder(null, (Object) null);
private String lookupId;
private String lookupIndex;
private String lookupPath;
private String lookupRouting;
private String lookupType;
@Override
 public void doXContent (XContentBuilder builder, Params params) throws IOException
{
  builder.startObject(NAME);
  if (values == null)
  {
    builder.startObject(name);
    if (lookupIndex != null)
    {
      builder.field("index", lookupIndex);
    }
    else
      ;
    builder.field("type", lookupType);
    builder.field("id", lookupId);
    if (lookupRouting != null)
    {
      builder.field("routing", lookupRouting);
    }
    else
      ;
    builder.field("path", lookupPath);
    builder.endObject();
  }
  else
  {
    builder.field(name, values);
  }
  if (minimumShouldMatch != null)
  {
    builder.field("minimum_should_match", minimumShouldMatch);
  }
  else
    ;
  if (disableCoord != null)
  {
    builder.field("disable_coord", disableCoord);
  }
  else
    ;
  printBoostAndQueryName(builder);
  builder.endObject();
  return;
}
