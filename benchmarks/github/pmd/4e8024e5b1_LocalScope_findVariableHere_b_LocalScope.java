{
  if (occurrence.isThisOrSuper() || occurrence.isMethodOrConstructorInvocation())
  {
    return null;
  }
  DeclarationFinderFunction finder = new DeclarationFinderFunction(occurrence);
  Applier.apply(finder, getVariableDeclarations().keySet().iterator());
  return finder.getDecl();
}