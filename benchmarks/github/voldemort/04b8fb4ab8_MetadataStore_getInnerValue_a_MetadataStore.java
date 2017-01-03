{
  List<Versioned<String>> values = innerStore.get(key, null);
  if (values.size() > 1)
    throw new VoldemortException("Inconsistent metadata found: expected 1 version but found " + values.size() + " for key:" + key);
  if (values.size() > 0)
  {
    return values.get(0);
  }
  throw new VoldemortException("No metadata found for required key:" + key);
}