private AnalysisService analysisService;
protected final ESLogger logger = Loggers.getLogger(getClass());
@Test
 public void testSynonymsAnalysis () throws IOException
{
  InputStream synonyms = getClass().getResourceAsStream("synonyms.txt");
  InputStream synonymsWordnet = getClass().getResourceAsStream("synonyms_wordnet.txt");
  Path home = createTempDir();
  Path config = home.resolve("config");
  Files.createDirectory(config);
  Files.copy(synonyms, config.resolve("synonyms.txt"));
  Files.copy(synonymsWordnet, config.resolve("synonyms_wordnet.txt"));
  String json = "/org/elasticsearch/index/analysis/synonyms/synonyms.json";
  Settings settings = settingsBuilder().loadFromStream(json, getClass().getResourceAsStream(json)).put("path.home", home).put(IndexMetaData.SETTING_VERSION_CREATED, Version.CURRENT).build();
  Index index = new Index("test");
  Injector parentInjector = new ModulesBuilder().add(new SettingsModule(settings), new EnvironmentModule(new Environment(settings)), new IndicesAnalysisModule()).createInjector();
  Injector injector = new ModulesBuilder().add(new IndexSettingsModule(index, settings), new IndexNameModule(index), new AnalysisModule(settings, parentInjector.getInstance(IndicesAnalysisService.class))).createChildInjector(parentInjector);
  analysisService = injector.getInstance(AnalysisService.class);
  match("synonymAnalyzer", "kimchy is the dude abides", "shay is the elasticsearch man!");
  match("synonymAnalyzer_file", "kimchy is the dude abides", "shay is the elasticsearch man!");
  match("synonymAnalyzerWordnet", "abstain", "abstain refrain desist");
  match("synonymAnalyzerWordnet_file", "abstain", "abstain refrain desist");
  match("synonymAnalyzerWithsettings", "kimchy", "sha hay");
  return;
}
