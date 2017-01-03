{
  if (TRACE)
  {
    System.out.println((" checking scope " + scope + " for name occurrence " + nameOccurrence));
  }
  if ((!scope.contains(nameOccurrence) && scope.getParent()) != null)
  {
    if (TRACE)
    {
      System.out.println((" moving up from " + scope + " to " + scope.getParent()));
    }
    return searchUpward(nameOccurrence, scope.getParent());
  }
  if (scope.contains(nameOccurrence))
  {
    if (TRACE)
    {
      System.out.println(" found it!");
    }
    return scope.addNameOccurrence(nameOccurrence);
  }
  return new HashSet();
}