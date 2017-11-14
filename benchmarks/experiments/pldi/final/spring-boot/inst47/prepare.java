private static final Object INSTANCE_MONITOR = new Object();
private static final String[] NO_ARGS = { };
private final ClassLoader applicationClassLoader;
private final String[] args;
private final Map<String, Object> attributes = new HashMap<String, Object>();
private final ClassLoaderFiles classLoaderFiles = new ClassLoaderFiles();
private boolean enabled = true;
private final UncaughtExceptionHandler exceptionHandler;
private boolean finished = false;
private final boolean forceReferenceCleanup;
private URL[] initialUrls;
private static Restarter instance;
private final BlockingDeque<LeakSafeThread> leakSafeThreads = new LinkedBlockingDeque<LeakSafeThread>();
private Log logger = new DeferredLog();
private final String mainClassName;
private final Object monitor = new Object();
private volatile ConfigurableApplicationContext rootContext;
private final Lock stopLock = new ReentrantLock();
private final Set<URL> urls = new LinkedHashSet<URL>();
private final List<ConfigurableApplicationContext> rootContexts = new CopyOnWriteArrayList<ConfigurableApplicationContext>();
void prepare (ConfigurableApplicationContext applicationContext)
{
  if (applicationContext != null && applicationContext.getParent() != null)
  {
    return;
  }
  else
    ;
  if (applicationContext instanceof GenericApplicationContext)
  {
    ((GenericApplicationContext) applicationContext).setResourceLoader(new ClassLoaderFilesResourcePatternResolver(this.classLoaderFiles));
  }
  else
    ;
  this.rootContexts.add(applicationContext);
  return;
}
