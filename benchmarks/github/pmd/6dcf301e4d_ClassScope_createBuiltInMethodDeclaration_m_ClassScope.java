{
  ASTMethodDeclaration methodDeclaration = new ASTMethodDeclaration(JavaParserTreeConstants.JJTMETHODDECLARATION);
  methodDeclaration.setPublic(true);
  methodDeclaration.setScope(this);
  ASTMethodDeclarator methodDeclarator = new ASTMethodDeclarator(JavaParserTreeConstants.JJTMETHODDECLARATOR);
  methodDeclarator.setImage(methodName);
  methodDeclarator.setScope(this);
  ASTFormalParameters formalParameters = new ASTFormalParameters(JavaParserTreeConstants.JJTFORMALPARAMETERS);
  formalParameters.setScope(this);
  methodDeclaration.jjtAddChild(methodDeclarator, 0);
  methodDeclarator.jjtSetParent(methodDeclaration);
  methodDeclarator.jjtAddChild(formalParameters, 0);
  formalParameters.jjtSetParent(methodDeclarator);
  for (int i = 0 ; i < parameterTypes.length ; i++)
  {
    ASTFormalParameter formalParameter = new ASTFormalParameter(JavaParserTreeConstants.JJTFORMALPARAMETER);
    formalParameters.jjtAddChild(formalParameter, i);
    formalParameter.jjtSetParent(formalParameters);
    ASTType type = new ASTType(JavaParserTreeConstants.JJTTYPE);
    formalParameter.jjtAddChild(type, 0);
    type.jjtSetParent(formalParameter);
    ASTVariableDeclaratorId variableDeclaratorId = new ASTVariableDeclaratorId(JavaParserTreeConstants.JJTVARIABLEDECLARATORID);
    variableDeclaratorId.setImage(("arg" + i));
    formalParameter.jjtAddChild(variableDeclaratorId, 1);
    variableDeclaratorId.jjtSetParent(formalParameter);
    if (PRIMITIVE_TYPES.contains(parameterTypes[i]))
    {
      ASTPrimitiveType primitiveType = new ASTPrimitiveType(JavaParserTreeConstants.JJTPRIMITIVETYPE);
      primitiveType.setImage(parameterTypes[i]);
      type.jjtAddChild(primitiveType, 0);
      primitiveType.jjtSetParent(type);
    }
    else
    {
      ASTReferenceType referenceType = new ASTReferenceType(JavaParserTreeConstants.JJTREFERENCETYPE);
      type.jjtAddChild(referenceType, 0);
      referenceType.jjtSetParent(type);
      ASTClassOrInterfaceType classOrInterfaceType = new ASTClassOrInterfaceType(JavaParserTreeConstants.JJTCLASSORINTERFACETYPE);
      classOrInterfaceType.setImage(parameterTypes[i]);
      referenceType.jjtAddChild(classOrInterfaceType, 0);
      classOrInterfaceType.jjtSetParent(referenceType);
    }
  }
  MethodNameDeclaration mnd = new MethodNameDeclaration(methodDeclarator);
  return mnd;
}