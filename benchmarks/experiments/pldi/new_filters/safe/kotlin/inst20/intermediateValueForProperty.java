private static final String CLASS_COMPARABLE = "java/lang/Comparable";
private static final String CLASS_INT_RANGE = "jet/IntRange";
private static final String CLASS_ITERABLE = "java/lang/Iterable";
private static final String CLASS_ITERATOR = "java/util/Iterator";
private static final String CLASS_OBJECT = "java/lang/Object";
private static final String CLASS_STRING = "java/lang/String";
private static final String CLASS_STRING_BUILDER = "java/lang/StringBuilder";
private static final String INT_RANGE_CONSTRUCTOR_DESCRIPTOR = "(II)V";
private static final Type INT_RANGE_TYPE = Type.getType(IntRange.class);
private static final String ITERABLE_ITERATOR_DESCRIPTOR = "()Ljava/util/Iterator;";
private static final String ITERATOR_HASNEXT_DESCRIPTOR = "()Z";
private static final String ITERATOR_NEXT_DESCRIPTOR = "()Ljava/lang/Object;";
private static final Type ITERATOR_TYPE = Type.getType(Iterator.class);
private static final Type JET_OBJECT_TYPE = Type.getType(JetObject.class);
private final BindingContext bindingContext;
private final OwnerKind contextKind;
private final DeclarationDescriptor contextType;
private final Stack<Label> myBreakTargets = new Stack<Label>();
private final Stack<Label> myContinueTargets = new Stack<Label>();
private final FrameMap myMap;
private final Stack<StackValue> myStack = new Stack<StackValue>();
private final Type returnType;
private final JetTypeMapper typeMapper;
private final InstructionAdapter v;
private static final Type OBJECT_TYPE = Type.getType(Object.class);
public StackValue intermediateValueForProperty (PropertyDescriptor propertyDescriptor, final boolean directToField)
{
  boolean isStatic = propertyDescriptor.getContainingDeclaration() instanceof NamespaceDescriptorImpl;
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
    fieldOwner = typeMapper.getOwner(propertyDescriptor, contextKind);
    interfaceOwner = typeMapper.getOwner(propertyDescriptor, contextKind);
  }
  else
  {
    fieldOwner = null;
    interfaceOwner = typeMapper.getOwner(propertyDescriptor, OwnerKind.INTERFACE);
  }
  return StackValue.property(propertyDescriptor.getName(), fieldOwner, interfaceOwner, typeMapper.mapType(outType), isStatic, getter, setter);
}
