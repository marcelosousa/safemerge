private XContentType contentType = Requests.INDEX_CONTENT_TYPE;
private String id;
private OpType opType = OpType.INDEX;
@Nullable
 private String parent;
private boolean refresh = 0;
@Nullable
 private String routing;
private BytesReference source;
@Nullable
 private String timestamp;
@Nullable
 private TimeValue ttl;
private String type;
private long version = Versions.MATCH_ANY;
private VersionType versionType = VersionType.INTERNAL;
private String pipeline;
@Override
 public void writeTo (StreamOutput out) throws IOException
{
  super.writeTo(out);
  out.writeOptionalString(type);
  out.writeOptionalString(id);
  out.writeOptionalString(routing);
  out.writeOptionalString(parent);
  out.writeOptionalString(timestamp);
  if (ttl == null)
  {
    out.writeBoolean(0);
  }
  else
  {
    out.writeBoolean(1);
    ttl.writeTo(out);
  }
  out.writeBytesReference(source);
  out.writeByte(opType.id());
  out.writeBoolean(refresh);
  out.writeLong(version);
  out.writeByte(versionType.getValue());
  out.writeOptionalString(pipeline);
  return;
}
@Override
 public void writeTo (StreamOutput out) throws IOException
{
  super.writeTo(out);
  out.writeString(type);
  out.writeOptionalString(id);
  out.writeOptionalString(routing);
  out.writeOptionalString(parent);
  out.writeOptionalString(timestamp);
  if (ttl == null)
  {
    out.writeBoolean(0);
  }
  else
  {
    out.writeBoolean(1);
    ttl.writeTo(out);
  }
  out.writeBytesReference(source);
  out.writeByte(opType.id());
  out.writeBoolean(refresh);
  out.writeLong(version);
  out.writeByte(versionType.getValue());
}
@Override
 public void writeTo (StreamOutput out) throws IOException
{
  super.writeTo(out);
  out.writeOptionalString(type);
  out.writeOptionalString(id);
  out.writeOptionalString(routing);
  out.writeOptionalString(parent);
  out.writeOptionalString(timestamp);
  if (ttl == null)
  {
    out.writeBoolean(0);
  }
  else
  {
    out.writeBoolean(1);
    ttl.writeTo(out);
  }
  out.writeBytesReference(source);
  out.writeByte(opType.id());
  out.writeBoolean(refresh);
  out.writeLong(version);
  out.writeByte(versionType.getValue());
}
@Override
 public void writeTo (StreamOutput out) throws IOException
{
  super.writeTo(out);
  out.writeString(type);
  out.writeOptionalString(id);
  out.writeOptionalString(routing);
  out.writeOptionalString(parent);
  out.writeOptionalString(timestamp);
  if (ttl == null)
  {
    out.writeBoolean(0);
  }
  else
  {
    out.writeBoolean(1);
    ttl.writeTo(out);
  }
  out.writeBytesReference(source);
  out.writeByte(opType.id());
  out.writeBoolean(refresh);
  out.writeLong(version);
  out.writeByte(versionType.getValue());
  out.writeOptionalString(pipeline);
}
@Override
 public void writeTo (StreamOutput out) throws IOException
{
  super.writeTo(out);
  out.writeOptionalString(type);
  out.writeOptionalString(id);
  out.writeOptionalString(routing);
  out.writeOptionalString(parent);
  out.writeOptionalString(timestamp);
  if (ttl == null)
  {
    out.writeBoolean(0);
  }
  else
  {
    out.writeBoolean(1);
    ttl.writeTo(out);
  }
  out.writeBytesReference(source);
  out.writeByte(opType.id());
  out.writeBoolean(refresh);
  out.writeLong(version);
  out.writeByte(versionType.getValue());
  out.writeOptionalString(pipeline);
}
public IndexRequest id (String id)
{
  this.id = id;
  return this;
}
