private final ReceiverDescriptor expectedThisObject;
private PropertyGetterDescriptor getter;
private final boolean isVar;
private final Modality modality;
private final PropertyDescriptor original;
private final Set<PropertyDescriptor> overriddenProperties = Sets.newLinkedHashSet();
private ReceiverDescriptor receiver;
private PropertySetterDescriptor setter;
private List<TypeParameterDescriptor> typeParemeters;
private final Visibility visibility;
private ReceiverDescriptor expectedThisObject;
@NotNull
 @Override
 public PropertyDescriptor copy (DeclarationDescriptor newOwner, boolean makeNonAbstract)
{
  PropertyDescriptor propertyDescriptor = new PropertyDescriptor(newOwner, Lists.newArrayList(getAnnotations()), DescriptorUtils.convertModality(modality, makeNonAbstract), visibility, isVar, getName());
  propertyDescriptor.setType(getInType(), getOutType(), DescriptorUtils.copyTypeParameters(propertyDescriptor, getTypeParameters()), expectedThisObject, (receiver.exists() ? receiver.getType() : null));
  PropertyGetterDescriptor newGetter = getter == null ? null : new PropertyGetterDescriptor(propertyDescriptor, Lists.newArrayList(getter.getAnnotations()), DescriptorUtils.convertModality(getter.getModality(), makeNonAbstract), getter.getVisibility(), getter.hasBody(), getter.isDefault());
  if (newGetter != null)
  {
    newGetter.initialize(getter.getReturnType());
  }
  else
    ;
  PropertySetterDescriptor newSetter = setter == null ? null : new PropertySetterDescriptor(DescriptorUtils.convertModality(setter.getModality(), makeNonAbstract), setter.getVisibility(), propertyDescriptor, Lists.newArrayList(setter.getAnnotations()), setter.hasBody(), setter.isDefault());
  if (newSetter != null)
  {
    newSetter.initialize(setter.getValueParameters().get(0).copy(newSetter));
  }
  else
    ;
  propertyDescriptor.initialize(newGetter, newSetter);
  return propertyDescriptor;
}
@Override
 public JetType getInType ()
{
  return super.getInType();
}
@Override
 @NotNull
 public JetType getOutType ()
{
  return super.getOutType();
}
@NotNull
 @Override
 public List<TypeParameterDescriptor> getTypeParameters ()
{
  return typeParemeters;
}
@NotNull
 @Override
 public PropertyDescriptor copy (DeclarationDescriptor newOwner, boolean makeNonAbstract)
{
  PropertyDescriptor propertyDescriptor = new PropertyDescriptor(newOwner, Lists.newArrayList(getAnnotations()), DescriptorUtils.convertModality(modality, makeNonAbstract), visibility, isVar, getName());
  propertyDescriptor.setType(getInType(), getOutType(), DescriptorUtils.copyTypeParameters(propertyDescriptor, getTypeParameters()), expectedThisObject, (receiver.exists() ? receiver.getType() : null));
  PropertyGetterDescriptor newGetter = getter == null ? null : new PropertyGetterDescriptor(propertyDescriptor, Lists.newArrayList(getter.getAnnotations()), DescriptorUtils.convertModality(getter.getModality(), makeNonAbstract), getter.getVisibility(), getter.hasBody(), getter.isDefault());
  if (newGetter != null)
  {
    newGetter.initialize(getter.getReturnType());
  }
  PropertySetterDescriptor newSetter = setter == null ? null : new PropertySetterDescriptor(DescriptorUtils.convertModality(setter.getModality(), makeNonAbstract), setter.getVisibility(), propertyDescriptor, Lists.newArrayList(setter.getAnnotations()), setter.hasBody(), setter.isDefault());
  if (newSetter != null)
  {
    newSetter.initialize(setter.getValueParameters().get(0).copy(newSetter));
  }
  propertyDescriptor.initialize(newGetter, newSetter);
  return propertyDescriptor;
}
public void initialize (@Nullable
                        PropertyGetterDescriptor getter, @Nullable
                                                         PropertySetterDescriptor setter)
{
  this.getter = getter;
  this.setter = setter;
}
@NotNull
 @Override
 public PropertyDescriptor copy (DeclarationDescriptor newOwner, boolean makeNonAbstract)
{
  PropertyDescriptor propertyDescriptor = new PropertyDescriptor(newOwner, Lists.newArrayList(getAnnotations()), DescriptorUtils.convertModality(modality, makeNonAbstract), visibility, isVar, expectedThisObject, getName());
  propertyDescriptor.setType(getInType(), getOutType(), DescriptorUtils.copyTypeParameters(propertyDescriptor, getTypeParameters()), (receiver.exists() ? receiver.getType() : null));
  PropertyGetterDescriptor newGetter = getter == null ? null : new PropertyGetterDescriptor(propertyDescriptor, Lists.newArrayList(getter.getAnnotations()), DescriptorUtils.convertModality(getter.getModality(), makeNonAbstract), getter.getVisibility(), getter.hasBody(), getter.isDefault());
  if (newGetter != null)
  {
    newGetter.initialize(getter.getReturnType());
  }
  PropertySetterDescriptor newSetter = setter == null ? null : new PropertySetterDescriptor(DescriptorUtils.convertModality(setter.getModality(), makeNonAbstract), setter.getVisibility(), propertyDescriptor, Lists.newArrayList(setter.getAnnotations()), setter.hasBody(), setter.isDefault());
  if (newSetter != null)
  {
    newSetter.initialize(setter.getValueParameters().get(0).copy(newSetter));
  }
  propertyDescriptor.initialize(newGetter, newSetter);
  return propertyDescriptor;
}
