{
  Set<NameDeclaration> result = new HashSet<NameDeclaration>();
  if (occurrence.isThisOrSuper() || occurrence.isMethodOrConstructorInvocation())
  {
    return result;
  }
  DeclarationFinderFunction finder = new DeclarationFinderFunction(occurrence);
  Applier.apply(finder, getVariableDeclarations().keySet().iterator());
  if (finder.getDecl() != null)
  {
    result.add(finder.getDecl());
  }
  return result;
}