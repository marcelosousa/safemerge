private Audio audio;
private final Lwjgl3Clipboard clipboard;
private final Lwjgl3ApplicationConfiguration config;
private volatile Lwjgl3Window currentWindow;
private static GLFWErrorCallback errorCallback;
private final Array<Runnable> executedRunnables = new Array<Runnable>();
private final Files files;
private static GLVersion glVersion;
private final Array<LifecycleListener> lifecycleListeners = new Array<LifecycleListener>();
private int logLevel = LOG_INFO;
private final Net net;
private final ObjectMap<String, Preferences> preferences = new ObjectMap<String, Preferences>();
private final Array<Runnable> runnables = new Array<Runnable>();
private volatile boolean running = true;
private final Array<Lwjgl3Window> windows = new Array<Lwjgl3Window>();
public Lwjgl3Window newWindow (ApplicationListener listener, Lwjgl3WindowConfiguration config)
{
  Lwjgl3ApplicationConfiguration appConfig = Lwjgl3ApplicationConfiguration.copy(this.config);
  appConfig.setWindowedMode(config.windowWidth, config.windowHeight);
  appConfig.setWindowPosition(config.windowX, config.windowY);
  appConfig.setWindowSizeLimits(config.windowMinWidth, config.windowMinHeight, config.windowMaxWidth, config.windowMaxHeight);
  appConfig.setResizable(config.windowResizable);
  appConfig.setDecorated(config.windowDecorated);
  appConfig.setWindowListener(config.windowListener);
  appConfig.setFullscreenMode(config.fullscreenMode);
  appConfig.setTitle(config.title);
  appConfig.setInitialBackgroundColor(config.initialBackgroundColor);
  appConfig.setInitialVisible(config.initialVisible);
  Lwjgl3Window window = createWindow(appConfig, listener, windows.get(0).getWindowHandle());
  windows.add(window);
  return window;
}
private Lwjgl3Window createWindow (Lwjgl3ApplicationConfiguration config, ApplicationListener listener, long sharedContext)
{
  long windowHandle = createGlfwWindow(config, sharedContext);
  Lwjgl3Window window = new Lwjgl3Window(windowHandle, listener, config);
  window.setVisible(true);
  return window;
}
private Lwjgl3Window createWindow (Lwjgl3ApplicationConfiguration config, ApplicationListener listener, long sharedContext)
{
  long windowHandle = createGlfwWindow(config, sharedContext);
  Lwjgl3Window window = new Lwjgl3Window(windowHandle, listener, config);
  window.setVisible(config.initialVisible);
  return window;
}
