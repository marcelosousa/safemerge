{
  lines = new ArrayList<Integer>();
  int index = -1;
  int count = StringUtils.countMatches(xmlString, "\n");
  for (int line = 1 ; line <= count ; line++)
  {
    lines.add((index + 1));
    index = xmlString.indexOf("\n", (index + 1));
  }
  lines.add((index + 1));
}