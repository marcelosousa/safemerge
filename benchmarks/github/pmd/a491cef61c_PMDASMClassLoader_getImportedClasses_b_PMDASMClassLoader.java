{
  if (dontBother.contains(name))
  {
    throw new ClassNotFoundException(name);
  }
  InputStream stream = null;
  try
  {
    stream = getResourceAsStream((name.replace('.', '/') + ".class"));
    ClassReader reader = new ClassReader(stream);
    PMDASMVisitor asmVisitor = new PMDASMVisitor();
    reader.accept(asmVisitor, 0);
    List<String> inner = asmVisitor.getInnerClasses();
    if (inner != null && !inner.isEmpty())
    {
      inner = new ArrayList<String>(inner);
      for (String str : inner) {
                                 InputStream innerClassStream = null;
                                 try
                                 {
                                   innerClassStream = getResourceAsStream((str.replace('.', '/') + ".class"));
                                   reader = new ClassReader(innerClassStream);
                                   reader.accept(asmVisitor, 0);
                                 }
                                 finally {
                                           IOUtils.closeQuietly(innerClassStream);
                                         }
                               }
    }
    return asmVisitor.getPackages();
  }
  catch (IOException e)
  {
    dontBother.add(name);
    throw new ClassNotFoundException(name, e);
  }
  finally {
            IOUtils.closeQuietly(stream);
          }
}