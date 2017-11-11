private String analyzer;
private Float boost;
private Float cutoff_Frequency = null;
private Fuzziness fuzziness;
private String fuzzyRewrite = null;
private Boolean fuzzyTranspositions = null;
private Boolean lenient;
private Integer maxExpansions;
private String minimumShouldMatch;
private final String name;
private Operator operator;
private Integer prefixLength;
private String queryName;
private String rewrite = null;
private Integer slop;
private final Object text;
private Type type;
private ZeroTermsQuery zeroTermsQuery;
public static final String NAME = "match";
static final MatchQueryBuilder PROTOTYPE = new MatchQueryBuilder(null, null);
@Override
 public void doXContent (XContentBuilder builder, Params params) throws IOException
{
  builder.startObject(NAME);
  builder.startObject(name);
  builder.field("query", text);
  if (type != null)
  {
    builder.field("type", type.toString().toLowerCase(Locale.ENGLISH));
  }
  else
    ;
  if (operator != null)
  {
    builder.field("operator", operator.toString());
  }
  else
    ;
  if (analyzer != null)
  {
    builder.field("analyzer", analyzer);
  }
  else
    ;
  if (slop != null)
  {
    builder.field("slop", slop);
  }
  else
    ;
  if (fuzziness != null)
  {
    fuzziness.toXContent(builder, params);
  }
  else
    ;
  if (prefixLength != null)
  {
    builder.field("prefix_length", prefixLength);
  }
  else
    ;
  if (maxExpansions != null)
  {
    builder.field("max_expansions", maxExpansions);
  }
  else
    ;
  if (minimumShouldMatch != null)
  {
    builder.field("minimum_should_match", minimumShouldMatch);
  }
  else
    ;
  if (fuzzyRewrite != null)
  {
    builder.field("fuzzy_rewrite", fuzzyRewrite);
  }
  else
    ;
  if (fuzzyTranspositions != null)
  {
    builder.field("fuzzy_transpositions", fuzzyTranspositions);
  }
  else
    ;
  if (lenient != null)
  {
    builder.field("lenient", lenient);
  }
  else
    ;
  if (zeroTermsQuery != null)
  {
    builder.field("zero_terms_query", zeroTermsQuery.toString());
  }
  else
    ;
  if (cutoff_Frequency != null)
  {
    builder.field("cutoff_frequency", cutoff_Frequency);
  }
  else
    ;
  printBoostAndQueryName(builder);
  builder.endObject();
  builder.endObject();
  return;
}
