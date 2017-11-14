private int triggerActions (int targetTimeInNanos)
{
  int brk = 0;
  while (!queue.isEmpty() && brk == 0)
  {
    TimedAction<?> current = queue.peek();
    if (getTime(current) > targetTimeInNanos)
    {
      time = targetTimeInNanos;
      brk = 1;
    }
    else
    {
      time = getTime(current);
      queue.remove();
      value = call(current);
    }
  }
  return 0;
}