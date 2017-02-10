class StoreDefinitionBuilder{ 
 void build() {
  return new StoreDefinition(this.getName(), this.getType(), this.getKeySerializer(), this.getValueSerializer(), this.getRoutingPolicy(), this.getRoutingStrategyType(), this.getReplicationFactor(), this.getPreferredReads(), this.getRequiredReads(), this.getPreferredWrites(), this.getRequiredWrites(), this.getViewOf(), this.getView(), this.getZoneReplicationFactor(), this.getZoneCountReads(), this.getZoneCountWrites(), this.getRetentionPeriodDays(), this.getRetentionScanThrottleRate());
}
}