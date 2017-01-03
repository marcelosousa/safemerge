{
  List<ASTLocalVariableDeclaration> vars = node.findDescendantsOfType(ASTLocalVariableDeclaration.class);
  List<ASTVariableDeclaratorId> ids = new ArrayList<ASTVariableDeclaratorId>();
  for (ASTLocalVariableDeclaration var : vars) {
                                                 ASTType type = var.getTypeNode();
                                                 if (type.jjtGetChild(0) instanceof ASTReferenceType)
                                                 {
                                                   ASTReferenceType ref = (ASTReferenceType) type.jjtGetChild(0);
                                                   if (ref.jjtGetChild(0) instanceof ASTClassOrInterfaceType)
                                                   {
                                                     ASTClassOrInterfaceType clazz = (ASTClassOrInterfaceType) ref.jjtGetChild(0);
                                                     if (((clazz.getType() != null && types.contains(clazz.getType().getName()) || clazz.getType()) == null && simpleTypes.contains(toSimpleType(clazz.getImage())) && !clazz.isReferenceToClassSameCompilationUnit() || types.contains(clazz.getImage())) && !clazz.isReferenceToClassSameCompilationUnit())
                                                     {
                                                       ASTVariableDeclaratorId id = var.getFirstDescendantOfType(ASTVariableDeclaratorId.class);
                                                       ids.add(id);
                                                     }
                                                   }
                                                 }
                                               }
  for (ASTVariableDeclaratorId x : ids) {
                                          ensureClosed(((ASTLocalVariableDeclaration) x.jjtGetParent().jjtGetParent()), x, data);
                                        }
}