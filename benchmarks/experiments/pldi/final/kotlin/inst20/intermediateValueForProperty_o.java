public StackValue intermediateValueForProperty (PropertyDescriptor propertyDescriptor, final boolean directToField)
{
  boolean isStatic = propertyDescriptor.getContainingDeclaration() instanceof NamespaceDescriptor;
  final JetType outType = propertyDescriptor.getOutType();
  boolean isInsideClass = propertyDescriptor.getContainingDeclaration() == contextType;
  Method getter;
  Method setter;
  if (directToField)
  {
    getter = null;
    setter = null;
  }
  else
  {
    getter = (isInsideClass && propertyDescriptor.getGetter()) == null ? null : typeMapper.mapGetterSignature(propertyDescriptor);
    setter = (isInsideClass && propertyDescriptor.getSetter()) == null ? null : typeMapper.mapSetterSignature(propertyDescriptor);
  }
  String fieldOwner;
  String interfaceOwner;
  if (isInsideClass || isStatic)
  {
    fieldOwner = JetTypeMapper.getOwner(propertyDescriptor, contextKind);
    interfaceOwner = JetTypeMapper.getOwner(propertyDescriptor, contextKind);
  }
  else
  {
    fieldOwner = null;
    interfaceOwner = JetTypeMapper.getOwner(propertyDescriptor, OwnerKind.INTERFACE);
  }
  return StackValue.property(propertyDescriptor.getName(), fieldOwner, interfaceOwner, typeMapper.mapType(outType), isStatic, getter, setter);
}