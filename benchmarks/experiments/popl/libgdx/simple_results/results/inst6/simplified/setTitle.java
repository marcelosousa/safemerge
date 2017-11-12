final BufferFormat bufferFormat;
JglfwApplicationConfiguration config;
float deltaTime;
int fps;
long frameStart;
int frames;
boolean fullscreen;
int fullscreenMonitorIndex;
GLCommon gl;
JglfwGL10 gl10;
JglfwGL11 gl11;
JglfwGL20 gl20;
static int glMajorVersion;
static int glMinorVersion;
volatile boolean isContinuous = true;
long lastTime;
volatile boolean requestRendering;
boolean resize;
boolean sync;
long window;
public void setTitle (String title)
{
  if (title == null)
    glfwSetWindowTitle(window, "");
  else
    ;
  glfwSetWindowTitle(window, title);
  config.title(title);
  return;
}
