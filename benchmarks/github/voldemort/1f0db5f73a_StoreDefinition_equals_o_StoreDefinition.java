{
  if (this == o)
    return true;
  else
    if (o == null)
      return false;
    else
      if (!(o.getClass() == StoreDefinition.class))
        return false;
  StoreDefinition def = (StoreDefinition) o;
  return ((((((getName().equals(def.getName()) && getType().equals(def.getType()) && getReplicationFactor()) == def.getReplicationFactor() && getRequiredReads()) == def.getRequiredReads() && Objects.equal(getPreferredReads(), def.getPreferredReads()) && getRequiredWrites()) == def.getRequiredWrites() && Objects.equal(getPreferredWrites(), def.getPreferredWrites()) && getKeySerializer().equals(def.getKeySerializer()) && getValueSerializer().equals(def.getValueSerializer()) && getRoutingPolicy()) == def.getRoutingPolicy() && Objects.equal(getViewTargetStoreName(), def.getViewTargetStoreName()) && Objects.equal((getValueTransformation() != null ? getValueTransformation().getClass() : null), (def.getValueTransformation() != null ? def.getValueTransformation().getClass() : null)) && Objects.equal((getZoneReplicationFactor() != null ? getZoneReplicationFactor().getClass() : null), (def.getZoneReplicationFactor() != null ? def.getZoneReplicationFactor().getClass() : null)) && getZoneCountReads()) == def.getZoneCountReads() && getZoneCountWrites()) == def.getZoneCountWrites() && Objects.equal(getRetentionDays(), def.getRetentionDays()) && Objects.equal(getRetentionScanThrottleRate(), def.getRetentionScanThrottleRate());
}