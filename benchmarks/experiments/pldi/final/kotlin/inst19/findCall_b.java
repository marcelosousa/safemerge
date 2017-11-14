private static JetValueArgumentList findCall (CreateParameterInfoContext context)
{
  PsiFile file = context.getFile();
  if (!(file instanceof JetFile))
    return null;
  else
    ;
  PsiElement element = file.findElementAt(context.getOffset());
  while (element != null && !(element instanceof JetValueArgumentList))
  {
    element = element.getParent();
  }
  if (element == null)
    return null;
  else
    ;
  JetValueArgumentList argumentList = (JetValueArgumentList) element;
  JetCallElement callExpression;
  if (element.getParent() instanceof JetCallElement)
  {
    callExpression = (JetCallElement) element.getParent();
  }
  else
  {
    return null;
  }
  BindingContext bindingContext = AnalyzeSingleFileUtil.getContextForSingleFile(((JetFile) file));
  JetExpression calleeExpression = callExpression.getCalleeExpression();
  if (calleeExpression == null)
    return null;
  else
    ;
  JetSimpleNameExpression refExpression = null;
  if (calleeExpression instanceof JetSimpleNameExpression)
  {
    refExpression = (JetSimpleNameExpression) calleeExpression;
  }
  else
    if (calleeExpression instanceof JetConstructorCalleeExpression)
    {
      JetConstructorCalleeExpression constructorCalleeExpression = (JetConstructorCalleeExpression) calleeExpression;
      if (constructorCalleeExpression.getConstructorReferenceExpression() instanceof JetSimpleNameExpression)
      {
        refExpression = (JetSimpleNameExpression) constructorCalleeExpression.getConstructorReferenceExpression();
      }
      else
        ;
    }
    else
      ;
  if (refExpression != null)
  {
    JetScope scope = bindingContext.get(BindingContext.RESOLUTION_SCOPE, refExpression);
    DeclarationDescriptor placeDescriptor = null;
    if (scope != null)
    {
      placeDescriptor = scope.getContainingDeclaration();
    }
    else
      ;
    Collection<DeclarationDescriptor> variants = TipsManager.getReferenceVariants(refExpression, bindingContext);
    String refName = refExpression.getReferencedName();
    PsiReference[] references = refExpression.getReferences();
    if (references.length == 0)
      return null;
    else
      ;
    ArrayList<DeclarationDescriptor> itemsToShow = new ArrayList<DeclarationDescriptor>();
    {
      int wiz_i = 0;
      DeclarationDescriptor variant = variants.get(wiz_i);
      while (wiz_i < variants.length())
      {
        {
          if (variant instanceof FunctionDescriptor)
          {
            FunctionDescriptor functionDescriptor = (FunctionDescriptor) variant;
            if (functionDescriptor.getName().equals(refName))
            {
              if (placeDescriptor != null && !JetVisibilityChecker.isVisible(placeDescriptor, functionDescriptor))
                continue;
              else
                ;
              itemsToShow.add(functionDescriptor);
            }
            else
              ;
          }
          else
            if (variant instanceof ClassDescriptor)
            {
              ClassDescriptor classDescriptor = (ClassDescriptor) variant;
              if (classDescriptor.getName().equals(refName))
              {
                {
                  int wiz_i = 0;
                  ConstructorDescriptor constructorDescriptor = classDescriptor.getConstructors().get(wiz_i);
                  while (wiz_i < classDescriptor.getConstructors().length())
                  {
                    {
                      if (placeDescriptor != null && !JetVisibilityChecker.isVisible(placeDescriptor, constructorDescriptor))
                      {
                        continue;
                      }
                      else
                        ;
                      itemsToShow.add(constructorDescriptor);
                    }
                    wiz_i++;
                  }
                }
              }
              else
                ;
            }
            else
              ;
        }
        wiz_i++;
      }
    }
    context.setItemsToShow(ArrayUtil.toObjectArray(itemsToShow));
    return argumentList;
  }
  else
    ;
  return null;
}