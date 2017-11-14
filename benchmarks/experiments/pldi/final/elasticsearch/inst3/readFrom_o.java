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
  return;
}