{
  if (isFirstChild(node, ASTStatement.class))
  {
    ASTStatement statement = (ASTStatement) node.jjtGetChild(0);
    if (isFirstChild(statement, ASTStatementExpression.class))
    {
      ASTStatementExpression stmtExp = (ASTStatementExpression) statement.jjtGetChild(0);
      if (stmtExp.jjtGetNumChildren() == 1)
      {
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
      else
      {
        final ASTExpression exp = stmtExp.getFirstDescendantOfType(ASTExpression.class);
        if (isFirstChild(exp, ASTPrimaryExpression.class))
        {
          final ASTPrimarySuffix primarySuffix = ((ASTPrimaryExpression) exp.jjtGetChild(0)).getFirstDescendantOfType(ASTPrimarySuffix.class);
          if (primarySuffix != null)
          {
            final String name = primarySuffix.getImage();
            if (name != null && name.equals("append"))
            {
              final ASTPrimaryExpression pExp = stmtExp.getFirstDescendantOfType(ASTPrimaryExpression.class);
              if (pExp != null)
              {
                final ASTName astName = stmtExp.getFirstDescendantOfType(ASTName.class);
                if (astName != null)
                {
                  final String variable = astName.getImage();
                  if (isAStringBuilderBuffer(primarySuffix, variable))
                  {
                    return variable;
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  else
    if (isFirstChild(node, ASTLocalVariableDeclaration.class))
    {
      ASTLocalVariableDeclaration lvd = (ASTLocalVariableDeclaration) node.jjtGetChild(0);
      ASTVariableDeclaratorId vdId = lvd.getFirstDescendantOfType(ASTVariableDeclaratorId.class);
      ASTExpression exp = lvd.getFirstDescendantOfType(ASTExpression.class);
      if (exp != null)
      {
        ASTPrimarySuffix primarySuffix = exp.getFirstDescendantOfType(ASTPrimarySuffix.class);
        if (primarySuffix != null)
        {
          final String name = primarySuffix.getImage();
          if (name != null && name.equals("append"))
          {
            String variable = vdId.getImage();
            if (isAStringBuilderBuffer(primarySuffix, variable))
            {
              return variable;
            }
          }
        }
      }
    }
  return null;
}