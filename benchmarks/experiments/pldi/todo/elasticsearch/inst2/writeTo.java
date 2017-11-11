private int forcedRefresh;
private String id;
private long seqNo;
private ShardId shardId;
private String type;
private long version;
@Override
 public void writeTo (StreamOutput out) throws IOException
{
  super.writeTo(out);
  shardId.writeTo(out);
  out.writeString(type);
  out.writeString(id);
  out.writeZLong(version);
  out.writeZLong(seqNo);
  out.writeBoolean(forcedRefresh);
  return;
}
