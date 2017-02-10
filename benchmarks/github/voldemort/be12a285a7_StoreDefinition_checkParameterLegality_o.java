class StoreDefinition{ 
 void checkParameterLegality() {
  if (requiredReads < 1)
    throw new IllegalArgumentException("Cannot have a requiredReads number less than 1.");
  else
    if (requiredReads > replicationFactor)
      throw new IllegalArgumentException("Cannot have more requiredReads then there are replicas.");
  if (requiredWrites < 1)
    throw new IllegalArgumentException("Cannot have a requiredWrites number less than 1.");
  else
    if (requiredWrites > replicationFactor)
      throw new IllegalArgumentException("Cannot have more requiredWrites then there are replicas.");
  if (preferredWrites != null)
  {
    if (preferredWrites < requiredWrites)
      throw new IllegalArgumentException("preferredWrites must be greater or equal to requiredWrites.");
    if (preferredWrites > replicationFactor)
      throw new IllegalArgumentException("Cannot have more preferredWrites then there are replicas.");
  }
  if (preferredReads != null)
  {
    if (preferredReads < requiredReads)
      throw new IllegalArgumentException("preferredReads must be greater or equal to requiredReads.");
    if (preferredReads > replicationFactor)
      throw new IllegalArgumentException("Cannot have more preferredReads then there are replicas.");
  }
  if ((retentionPeriodDays != null && retentionPeriodDays) < 0)
    throw new IllegalArgumentException("Retention days must be non-negative.");
  if ((zoneReplicationFactor != null && zoneReplicationFactor.size()) != 0)
  {
    if ((zoneCountReads == null || zoneCountReads) < 0)
      throw new IllegalArgumentException("Zone Counts reads must be non-negative / non-null");
    if ((zoneCountWrites == null || zoneCountWrites) < 0)
      throw new IllegalArgumentException("Zone Counts writes must be non-negative");
    int sumZoneReplicationFactor = 0;
    int replicatingZones = 0;
    for (Integer zoneId : zoneReplicationFactor.keySet()) {
                                                            int currentZoneRepFactor = zoneReplicationFactor.get(zoneId);
                                                            sumZoneReplicationFactor += currentZoneRepFactor;
                                                            if (currentZoneRepFactor > 0)
                                                              replicatingZones++;
                                                          }
    if (replicatingZones <= 0)
    {
      throw new IllegalArgumentException("Cannot have no zones to replicate to. " + "Should have some positive zoneReplicationFactor");
    }
    if (sumZoneReplicationFactor != replicationFactor)
    {
      throw new IllegalArgumentException("Sum total of zones (" + sumZoneReplicationFactor + ") does not match the total replication factor (" + replicationFactor + ")");
    }
    if (zoneCountReads >= replicatingZones)
    {
      throw new IllegalArgumentException("Number of zones to block for while reading (" + zoneCountReads + ") should be less then replicating zones (" + replicatingZones + ")");
    }
    if (zoneCountWrites >= replicatingZones)
    {
      throw new IllegalArgumentException("Number of zones to block for while writing (" + zoneCountWrites + ") should be less then replicating zones (" + replicatingZones + ")");
    }
  }
}
}