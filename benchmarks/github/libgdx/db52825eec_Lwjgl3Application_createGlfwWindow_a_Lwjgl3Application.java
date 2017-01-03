{
  GLFW.glfwDefaultWindowHints();
  GLFW.glfwWindowHint(GLFW.GLFW_VISIBLE, GLFW.GLFW_FALSE);
  GLFW.glfwWindowHint(GLFW.GLFW_RESIZABLE, (config.windowResizable ? GLFW.GLFW_TRUE : GLFW.GLFW_FALSE));
  if (sharedContextWindow == 0)
  {
    GLFW.glfwWindowHint(GLFW.GLFW_RED_BITS, config.r);
    GLFW.glfwWindowHint(GLFW.GLFW_GREEN_BITS, config.g);
    GLFW.glfwWindowHint(GLFW.GLFW_BLUE_BITS, config.b);
    GLFW.glfwWindowHint(GLFW.GLFW_ALPHA_BITS, config.a);
    GLFW.glfwWindowHint(GLFW.GLFW_STENCIL_BITS, config.stencil);
    GLFW.glfwWindowHint(GLFW.GLFW_DEPTH_BITS, config.depth);
    GLFW.glfwWindowHint(GLFW.GLFW_SAMPLES, config.samples);
  }
  long windowHandle = 0;
  if (config.fullscreenMode != null)
  {
    windowHandle = GLFW.glfwCreateWindow(config.fullscreenMode.width, config.fullscreenMode.height, config.title, config.fullscreenMode.getMonitor(), sharedContextWindow);
  }
  else
  {
    GLFW.glfwWindowHint(GLFW.GLFW_DECORATED, (config.windowDecorated ? GLFW.GLFW_TRUE : GLFW.GLFW_FALSE));
    windowHandle = GLFW.glfwCreateWindow(config.windowWidth, config.windowHeight, config.title, 0, sharedContextWindow);
  }
  if (windowHandle == 0)
  {
    throw new GdxRuntimeException("Couldn't create window");
  }
  if (config.fullscreenMode == null)
  {
    if ((config.windowX == -1 && config.windowY) == -1)
    {
      GLFWVidMode vidMode = GLFW.glfwGetVideoMode(GLFW.glfwGetPrimaryMonitor());
      GLFW.glfwSetWindowPos(windowHandle, ((vidMode.width() / 2 - config.windowWidth) / 2), ((vidMode.height() / 2 - config.windowHeight) / 2));
    }
    else
    {
      GLFW.glfwSetWindowPos(windowHandle, config.windowX, config.windowY);
    }
  }
  GLFW.glfwMakeContextCurrent(windowHandle);
  GLFW.glfwSwapInterval((config.vSyncEnabled ? 1 : 0));
  GL.createCapabilities();
  String version = GL11.glGetString(GL20.GL_VERSION);
  int glMajorVersion = Integer.parseInt(("" + version.charAt(0)));
  if (glMajorVersion <= 1)
    throw new GdxRuntimeException("OpenGL 2.0 or higher with the FBO extension is required. OpenGL version: " + version);
  if (glMajorVersion == 2 || version.contains("2.1"))
  {
    if ((GLFW.glfwExtensionSupported("GL_EXT_framebuffer_object") == GLFW.GLFW_FALSE && GLFW.glfwExtensionSupported("GL_ARB_framebuffer_object")) == GLFW.GLFW_FALSE)
    {
      throw new GdxRuntimeException("OpenGL 2.0 or higher with the FBO extension is required. OpenGL version: " + version + ", FBO extension: false");
    }
  }
  for (int i = 0 ; i < 2 ; i++)
  {
    GL11.glClearColor(config.initialBackgroundColor.r, config.initialBackgroundColor.g, config.initialBackgroundColor.b, config.initialBackgroundColor.a);
    GL11.glClear(GL11.GL_COLOR_BUFFER_BIT);
    GLFW.glfwSwapBuffers(windowHandle);
  }
  return windowHandle;
}