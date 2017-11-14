@Override
 public XContentBuilder toXContent (XContentBuilder builder, Params params) throws IOException
{
  ReplicationResponse.ShardInfo shardInfo = getShardInfo();
  builder.field(Fields._INDEX, shardId.getIndexName()).field(Fields._TYPE, type).field(Fields._ID, id).field(Fields._VERSION, version);
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