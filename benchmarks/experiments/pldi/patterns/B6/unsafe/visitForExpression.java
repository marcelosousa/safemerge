int wiz_x;
int wiz_y;
@Override
 public JetType visitForExpression (JetForExpression expression, ExpressionTypingContext contextWithExpectedType)
{
  if (wiz_f(expression) == 1)
  {
    wiz_y = wiz_y + 1;
  }
  else
    ;
  wiz_x = 1;
  if (wiz_f(expression) == 1)
  {
    wiz_y = wiz_y + 1;
  }
  else
  {
    wiz_y = wiz_y - 1;
  }
  ExpressionTypingContext context = contextWithExpectedType.replaceExpectedType(TypeUtils.NO_EXPECTED_TYPE);
  JetParameter loopParameter = expression.getLoopParameter();
  JetExpression loopRange = expression.getLoopRange();
  JetType expectedParameterType = null;
  if (loopRange != null)
  {
    ExpressionReceiver loopRangeReceiver = getExpressionReceiver(facade, loopRange, context.replaceScope(context.scope));
    if (loopRangeReceiver != null)
    {
      expectedParameterType = checkIterableConvention(loopRangeReceiver, context);
    }
    else
      ;
  }
  else
    ;
  WritableScope loopScope = newWritableScopeImpl(context).setDebugName("Scope with for-loop index");
  if (loopParameter != null)
  {
    JetTypeReference typeReference = loopParameter.getTypeReference();
    VariableDescriptor variableDescriptor;
    if (typeReference != null)
    {
      variableDescriptor = context.getDescriptorResolver().resolveLocalVariableDescriptor(context.scope.getContainingDeclaration(), context.scope, loopParameter);
      JetType actualParameterType = variableDescriptor.getOutType();
      if ((expectedParameterType != null && actualParameterType) != null && !context.semanticServices.getTypeChecker().isSubtypeOf(expectedParameterType, actualParameterType))
      {
        context.trace.report(TYPE_MISMATCH_IN_FOR_LOOP.on(typeReference, expectedParameterType, actualParameterType));
      }
      else
        ;
    }
    else
    {
      if (expectedParameterType == null)
      {
        expectedParameterType = ErrorUtils.createErrorType("Error");
      }
      else
        ;
      variableDescriptor = context.getDescriptorResolver().resolveLocalVariableDescriptor(context.scope.getContainingDeclaration(), loopParameter, expectedParameterType);
    }
    loopScope.addVariableDescriptor(variableDescriptor);
  }
  else
    ;
  JetExpression body = expression.getBody();
  if (body != null)
  {
    facade.getType(body, context.replaceScope(loopScope));
  }
  else
    ;
  return DataFlowUtils.checkType(JetStandardClasses.getUnitType(), expression, contextWithExpectedType);
}
