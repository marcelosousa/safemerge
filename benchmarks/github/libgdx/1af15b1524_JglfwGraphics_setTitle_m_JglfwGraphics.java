{
  if (title == null)
    glfwSetWindowTitle(window, "");
  glfwSetWindowTitle(window, title);
  config.title = title;
}