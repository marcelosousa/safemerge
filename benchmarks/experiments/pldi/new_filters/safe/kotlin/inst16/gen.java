private final BindingContext bindingContext;
private final Codegens factory;
private final JetTypeMapper typeMapper;
private final GenerationState state;
public GeneratedClosureDescriptor gen (JetFunctionLiteralExpression fun)
{
  JetNamedDeclaration container = PsiTreeUtil.getParentOfType(fun, JetNamespace.class, JetClass.class, JetObjectDeclaration.class);
  final Pair<String, ClassVisitor> nameAndVisitor;
  if (container instanceof JetNamespace)
  {
    nameAndVisitor = state.forClosureIn(((JetNamespace) container));
  }
  else
  {
    nameAndVisitor = state.forClosureIn(state.getBindingContext().getClassDescriptor(((JetClassOrObject) container)));
  }
  final FunctionDescriptor funDescriptor = (FunctionDescriptor) state.getBindingContext().getDeclarationDescriptor(fun);
  final ClassVisitor cv = nameAndVisitor.getSecond();
  final String name = nameAndVisitor.getFirst();
  SignatureWriter signatureWriter = new SignatureWriter();
  final List<ValueParameterDescriptor> parameters = funDescriptor.getUnsubstitutedValueParameters();
  final String funClass = getInternalClassName(funDescriptor);
  signatureWriter.visitClassType(funClass);
  {
    int wiz_i = 0;
    ValueParameterDescriptor parameter = parameters.get(wiz_i);
    while (wiz_i < parameters.length())
    {
      {
        appendType(signatureWriter, parameter.getOutType(), '=');
      }
      wiz_i + 1;
    }
  }
  appendType(signatureWriter, funDescriptor.getUnsubstitutedReturnType(), '=');
  signatureWriter.visitEnd();
  cv.visit(Opcodes.V1_6, Opcodes.ACC_PUBLIC, name, null, funClass, new String[0]);
  final Method constructor = generateConstructor(cv, funClass);
  generateBridge(name, funDescriptor, cv);
  generateBody(name, funDescriptor, cv, container.getProject(), fun.getFunctionLiteral().getBodyExpression().getStatements());
  cv.visitEnd();
  return new GeneratedClosureDescriptor(name, constructor);
}
private void generateBody (String className, FunctionDescriptor funDescriptor, ClassVisitor cv, Project project, List<JetElement> body)
{
  FunctionCodegen fc = new FunctionCodegen(null, cv, state);
  fc.generatedMethod(body, OwnerKind.IMPLEMENTATION, invokeSignature(funDescriptor), null, funDescriptor.getUnsubstitutedValueParameters(), null);
}
private void generateBody (String className, FunctionDescriptor funDescriptor, ClassVisitor cv, Project project, List<JetElement> body)
{
  FunctionCodegen fc = new FunctionCodegen(null, cv, JetStandardLibrary.getJetStandardLibrary(project), bindingContext, factory);
  fc.generatedMethod(body, OwnerKind.IMPLEMENTATION, invokeSignature(funDescriptor), null, funDescriptor.getUnsubstitutedValueParameters(), null);
}
