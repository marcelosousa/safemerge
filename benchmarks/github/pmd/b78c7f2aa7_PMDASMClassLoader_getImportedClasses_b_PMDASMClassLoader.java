{
  if (dontBother.contains(name))
  {
    throw new ClassNotFoundException(name);
  }
  try
  {
    ClassReader reader = new ClassReader(getResourceAsStream((name.replace('.', '/') + ".class")));
    PMDASMVisitor asmVisitor = new PMDASMVisitor();
    reader.accept(asmVisitor, 0);
    List<String> inner = asmVisitor.getInnerClasses();
    if (inner != null && !inner.isEmpty())
    {
      inner = new ArrayList(inner);
      for (String str : inner) {
                                 reader = new ClassReader(getResourceAsStream((str.replace('.', '/') + ".class")));
                                 reader.accept(asmVisitor, 0);
                               }
    }
    return asmVisitor.getPackages();
  }
  catch (IOException e)
  {
    dontBother.add(name);
    throw new ClassNotFoundException(name, e);
  }
}