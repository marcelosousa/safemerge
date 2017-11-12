private static final AsciiBytes MANIFEST_ENTRY_NAME = new AsciiBytes("META-INF/MANIFEST.MF");
private static final Set<String> SKIPPED_NAMES = new HashSet<String>(Arrays.asList(".", ".."));
private Map<AsciiBytes, Entry> entries = new LinkedHashMap<AsciiBytes, Entry>();
private boolean filtered = false;
private Manifest manifest;
private final File root;
private File manifestFile;
private final boolean recursive;
@Override
 public URL getUrl () throws MalformedURLException
{
  return this.root.toURI().toURL();
  return;
}
