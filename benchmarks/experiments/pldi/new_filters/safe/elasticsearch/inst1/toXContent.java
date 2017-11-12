private String id;
private ShardId shardId;
private String type;
private long version;
private long seqNo;
private boolean forcedRefresh;
@Override
 public XContentBuilder toXContent (XContentBuilder builder, Params params) throws IOException
{
  ReplicationResponse.ShardInfo shardInfo = getShardInfo();
  builder.field(Fields._INDEX, shardId.getIndexName()).field(Fields._TYPE, type).field(Fields._ID, id).field(Fields._VERSION, version).field("forced_refresh", forcedRefresh);
  shardInfo.toXContent(builder, params);
  builder.field(Fields._SHARD_ID, shardId.id());
  if (getSeqNo() >= 0)
  {
    builder.field(Fields._SEQ_NO, getSeqNo());
  }
  else
    ;
  return builder;
}
public long getSeqNo ()
{
  return seqNo;
}
