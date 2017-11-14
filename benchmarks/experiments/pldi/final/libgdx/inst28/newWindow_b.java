public Lwjgl3Window newWindow (ApplicationListener listener, Lwjgl3WindowConfiguration config)
{
  Lwjgl3ApplicationConfiguration appConfig = Lwjgl3ApplicationConfiguration.copy(this.config);
  appConfig.setWindowedMode(config.windowWidth, config.windowHeight);
  appConfig.setWindowPosition(config.windowX, config.windowY);
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