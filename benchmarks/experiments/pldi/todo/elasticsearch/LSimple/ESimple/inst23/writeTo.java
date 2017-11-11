private String id;
private ShardId shardId;
private String type;
private long version;
private long seqNo;
private boolean forcedRefresh;
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
