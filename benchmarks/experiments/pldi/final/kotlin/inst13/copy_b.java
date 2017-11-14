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
  propertyDescriptor.initialize(newGetter, newSetter);
  return propertyDescriptor;
}