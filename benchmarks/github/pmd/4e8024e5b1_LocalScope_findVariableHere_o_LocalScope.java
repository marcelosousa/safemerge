{
  if (occurrence.isThisOrSuper() || occurrence.isMethodOrConstructorInvocation())
  {
    return null;
  }
  ImageFinderFunction finder = new ImageFinderFunction(occurrence.getImage());
  Applier.apply(finder, getVariableDeclarations().keySet().iterator());
  return finder.getDecl();
}