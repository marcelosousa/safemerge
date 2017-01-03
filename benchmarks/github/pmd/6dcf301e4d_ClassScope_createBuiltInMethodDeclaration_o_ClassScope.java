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
  for (int i = 0 ; i < parameterCount ; i++)
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
  }
  MethodNameDeclaration mnd = new MethodNameDeclaration(methodDeclarator);
  return mnd;
}