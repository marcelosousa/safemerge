@Override
 public URL getUrl () throws MalformedURLException
{
  FilteredURLStreamHandler handler = this.filtered ? new FilteredURLStreamHandler() : null;
  return new URL("file", "", -1, this.root.toURI().toURL().getPath(), handler);
  return;
}