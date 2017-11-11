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
 public void readFrom (StreamInput in) throws IOException
{
  super.readFrom(in);
  type = in.readOptionalString();
  id = in.readOptionalString();
  routing = in.readOptionalString();
  parent = in.readOptionalString();
  timestamp = in.readOptionalString();
  ttl = in.readBoolean() ? TimeValue.readTimeValue(in) : null;
  source = in.readBytesReference();
  opType = OpType.fromId(in.readByte());
  refresh = in.readBoolean();
  version = in.readLong();
  versionType = VersionType.fromValue(in.readByte());
  pipeline = in.readOptionalString();
  return;
}
@Override
 public void readFrom (StreamInput in) throws IOException
{
  super.readFrom(in);
  type = in.readString();
  id = in.readOptionalString();
  routing = in.readOptionalString();
  parent = in.readOptionalString();
  timestamp = in.readOptionalString();
  ttl = in.readBoolean() ? TimeValue.readTimeValue(in) : null;
  source = in.readBytesReference();
  opType = OpType.fromId(in.readByte());
  refresh = in.readBoolean();
  version = in.readLong();
  versionType = VersionType.fromValue(in.readByte());
}
@Override
 public void readFrom (StreamInput in) throws IOException
{
  super.readFrom(in);
  type = in.readOptionalString();
  id = in.readOptionalString();
  routing = in.readOptionalString();
  parent = in.readOptionalString();
  timestamp = in.readOptionalString();
  ttl = in.readBoolean() ? TimeValue.readTimeValue(in) : null;
  source = in.readBytesReference();
  opType = OpType.fromId(in.readByte());
  refresh = in.readBoolean();
  version = in.readLong();
  versionType = VersionType.fromValue(in.readByte());
}
@Override
 public void readFrom (StreamInput in) throws IOException
{
  super.readFrom(in);
  type = in.readString();
  id = in.readOptionalString();
  routing = in.readOptionalString();
  parent = in.readOptionalString();
  timestamp = in.readOptionalString();
  ttl = in.readBoolean() ? TimeValue.readTimeValue(in) : null;
  source = in.readBytesReference();
  opType = OpType.fromId(in.readByte());
  refresh = in.readBoolean();
  version = in.readLong();
  versionType = VersionType.fromValue(in.readByte());
  pipeline = in.readOptionalString();
}
@Override
 public void readFrom (StreamInput in) throws IOException
{
  super.readFrom(in);
  type = in.readOptionalString();
  id = in.readOptionalString();
  routing = in.readOptionalString();
  parent = in.readOptionalString();
  timestamp = in.readOptionalString();
  ttl = in.readBoolean() ? TimeValue.readTimeValue(in) : null;
  source = in.readBytesReference();
  opType = OpType.fromId(in.readByte());
  refresh = in.readBoolean();
  version = in.readLong();
  versionType = VersionType.fromValue(in.readByte());
  pipeline = in.readOptionalString();
}
