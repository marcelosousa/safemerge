private String id;
private ShardId shardId;
private String type;
private long version;
private long seqNo;
private boolean forcedRefresh;
@Override
 public void readFrom (StreamInput in) throws IOException
{
  super.readFrom(in);
  shardId = ShardId.readShardId(in);
  type = in.readString();
  id = in.readString();
  version = in.readZLong();
  seqNo = in.readZLong();
  forcedRefresh = in.readBoolean();
  return;
}
