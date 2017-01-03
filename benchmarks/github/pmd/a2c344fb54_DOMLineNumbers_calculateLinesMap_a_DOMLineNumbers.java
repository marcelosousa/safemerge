{
  lines = new TreeMap();
  int index = -1;
  int count = StringUtils.countMatches(xmlString, "\n");
  for (int line = 1 ; line <= count ; line++)
  {
    lines.put(line, (index + 1));
    index = xmlString.indexOf("\n", (index + 1));
  }
  lines.put((count + 1), (index + 1));
}