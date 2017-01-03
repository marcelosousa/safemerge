{
  if (!this.outerName.replace('.', '/').equals(outerName))
  {
    return;
  }
  if (innerClasses == null)
  {
    innerClasses = new ArrayList<String>();
  }
  if (!innerClasses.contains(name.replace('/', '.')))
  {
    innerClasses.add(name.replace('/', '.'));
  }
  packages.put(innerName, name.replace('/', '.'));
}