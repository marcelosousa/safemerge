@Before
 public void setup () throws IOException
{
  Settings settings = ImmutableSettings.settingsBuilder().put("index.cache.filter.type", "none").put("name", "SimpleIndexQueryParserTests").build();
  IndexService indexService = createIndex("test", settings);
  MapperService mapperService = indexService.mapperService();
  String mapping = copyToStringFromClasspath("/org/elasticsearch/index/query/mapping.json");
  mapperService.merge("person", new CompressedString(mapping), true);
  ParsedDocument doc = mapperService.documentMapper("person").parse(new BytesArray(copyToBytesFromClasspath("/org/elasticsearch/index/query/data.json")));
  assertNotNull(doc.dynamicMappingsUpdate());
  client().admin().indices().preparePutMapping("test").setType("person").setSource(doc.dynamicMappingsUpdate().toString()).get();
  queryParser = indexService.queryParserService();
  return;
}