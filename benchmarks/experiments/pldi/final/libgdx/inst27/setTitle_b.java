public void setTitle (String title)
{
  if (title == null)
    glfwSetWindowTitle(window, "");
  else
    ;
  glfwSetWindowTitle(window, title);
  return;
}