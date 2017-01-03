{
  if (isFirstChild(node, ASTStatement.class))
  {
    ASTStatement statement = (ASTStatement) node.jjtGetChild(0);
    if (isFirstChild(statement, ASTStatementExpression.class))
    {
      ASTStatementExpression stmtExp = (ASTStatementExpression) statement.jjtGetChild(0);
      ASTPrimaryPrefix primaryPrefix = stmtExp.getFirstDescendantOfType(ASTPrimaryPrefix.class);
      if (primaryPrefix != null)
      {
        ASTName name = primaryPrefix.getFirstChildOfType(ASTName.class);
        if (name != null)
        {
          String image = name.getImage();
          if (image.endsWith(".append"))
          {
            String variable = image.substring(0, image.indexOf('.'));
            if (isAStringBuilderBuffer(primaryPrefix, variable))
            {
              return variable;
            }
          }
        }
      }
    }
  }
  return null;
}