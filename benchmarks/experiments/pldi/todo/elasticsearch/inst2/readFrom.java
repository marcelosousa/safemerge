private int forcedRefresh;
private String id;
private long seqNo;
private ShardId shardId;
private String type;
private long version;
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
