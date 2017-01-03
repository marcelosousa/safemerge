{
  if (TRACE)
  {
    System.out.println((" checking scope " + scope + " for name occurrence " + nameOccurrence));
  }
  final boolean isInScope = scope.contains(nameOccurrence);
  if ((!isInScope && scope.getParent()) != null)
  {
    if (TRACE)
    {
      System.out.println((" moving up from " + scope + " to " + scope.getParent()));
    }
    return searchUpward(nameOccurrence, scope.getParent());
  }
  if (isInScope)
  {
    if (TRACE)
    {
      System.out.println(" found it!");
    }
    return scope.addNameOccurrence(nameOccurrence);
  }
  return Collections.emptySet();
}