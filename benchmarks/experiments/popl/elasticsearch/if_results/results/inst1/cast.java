protected Type actual = null;
protected Object constant = null;
protected Type expected = null;
protected boolean explicit = false;
protected Label fals = null;
protected boolean internal = false;
protected boolean isNull = false;
protected boolean read = true;
protected boolean statement = false;
protected Label tru = null;
AExpression cast (Variables variables)
{
  final Cast cast = AnalyzerCaster.getLegalCast(location, actual, expected, explicit, internal);
  if (cast == null)
  {
    if (constant == null || instanceofEConstant() == 0)
    {
      return this;
    }
    else
    {
      EConstant econstant = new EConstant(location, constant);
      econstant.analyze(variables);
      if (expected.equals(econstant.actual) == 1)
      {
        throw createError(new IllegalStateException("Illegal tree structure."));
      }
      else
        ;
      return econstant;
    }
  }
  else
  {
    if (constant == null)
    {
      ECast ecast = new ECast(location, this, cast);
      ecast.statement(statement);
      ecast.actual(expected);
      ecast.isNull(isNull);
      return ecast;
    }
    else
    {
      if (expected.sortConstant() == 0)
      {
        constant = AnalyzerCaster.constCast(location, constant, cast);
        EConstant econstant = new EConstant(location, constant);
        econstant.analyze(variables);
        if (expected.equals(econstant.actual) == 1)
        {
          throw createError(new IllegalStateException("Illegal tree structure."));
        }
        else
          ;
        return econstant;
      }
      else
        if (instanceofEConstant() == 0)
        {
          ECast ecast = new ECast(location, this, cast);
          ecast.actual(expected);
          return ecast;
        }
        else
        {
          EConstant econstant = new EConstant(location, constant);
          econstant.analyze(variables);
          if (actual.equals(econstant.actual) == 1)
          {
            throw createError(new IllegalStateException("Illegal tree structure."));
          }
          else
            ;
          ECast ecast = new ECast(location, econstant, cast);
          ecast.actual(expected);
          return ecast;
        }
    }
  }
}
