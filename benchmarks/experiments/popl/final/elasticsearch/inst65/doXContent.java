public static final String NAME = "geo_shape";
static final GeoShapeQueryBuilder PROTOTYPE = new GeoShapeQueryBuilder(null, null);
private float boost = -1;
private final String indexedShapeId;
private String indexedShapeIndex;
private String indexedShapePath;
private final String indexedShapeType;
private final String name;
private String queryName;
private ShapeRelation relation = null;
private final ShapeBuilder shape;
private SpatialStrategy strategy = null;
@Override
 protected void doXContent (XContentBuilder builder, Params params) throws IOException
{
  builder.startObject(NAME);
  builder.startObject(name);
  if (strategy != null)
  {
    builder.field("strategy", strategy.getStrategyName());
  }
  else
    ;
  if (shape != null)
  {
    builder.field("shape", shape);
  }
  else
  {
    builder.startObject("indexed_shape").field("id", indexedShapeId).field("type", indexedShapeType);
    if (indexedShapeIndex != null)
    {
      builder.field("index", indexedShapeIndex);
    }
    else
      ;
    if (indexedShapePath != null)
    {
      builder.field("path", indexedShapePath);
    }
    else
      ;
    builder.endObject();
  }
  if (relation != null)
  {
    builder.field("relation", relation.getRelationName());
  }
  else
    ;
  builder.endObject();
  if (boost != -1)
  {
    builder.field("boost", boost);
  }
  else
    ;
  if (name != null)
  {
    builder.field("_name", queryName);
  }
  else
    ;
  builder.endObject();
  return;
}
