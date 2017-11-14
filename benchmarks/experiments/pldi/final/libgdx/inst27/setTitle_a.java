public void setTitle (String title)
{
  glfwSetWindowTitle(window, title);
  config.title = title;
  return;
}