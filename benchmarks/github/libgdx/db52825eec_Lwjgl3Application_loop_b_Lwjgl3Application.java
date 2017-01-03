{
  Array<Lwjgl3Window> closedWindows = new Array<Lwjgl3Window>();
  while ((running && windows.size) > 0)
  {
    if (audio instanceof OpenALAudio)
    {
      ((OpenALAudio) audio).update();
    }
    closedWindows.clear();
    for (Lwjgl3Window window : windows) {
                                          Gdx.graphics = window.getGraphics();
                                          Gdx.gl30 = window.getGraphics().getGL30();
                                          Gdx.gl20 = Gdx.gl30 != null ? Gdx.gl30 : window.getGraphics().getGL20();
                                          Gdx.gl = Gdx.gl30 != null ? Gdx.gl30 : Gdx.gl20;
                                          Gdx.input = window.getInput();
                                          GLFW.glfwMakeContextCurrent(window.getWindowHandle());
                                          currentWindow = window;
                                          synchronized (lifecycleListeners)
                                          {
                                            window.update(lifecycleListeners);
                                          }
                                          if (window.shouldClose())
                                          {
                                            closedWindows.add(window);
                                          }
                                        }
    synchronized (runnables)
    {
      executedRunnables.clear();
      executedRunnables.addAll(runnables);
      runnables.clear();
    }
    for (Runnable runnable : executedRunnables) {
                                                  runnable.run();
                                                }
    for (Lwjgl3Window closedWindow : closedWindows) {
                                                      closedWindow.dispose();
                                                      windows.removeValue(closedWindow, false);
                                                    }
  }
}