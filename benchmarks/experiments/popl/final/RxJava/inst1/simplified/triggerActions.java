private final Queue<TimedAction<?>> queue = new PriorityQueue<TimedAction<?>>(11, new CompareActionsByTime());
private int time;
private int value;
private int triggerActions (int targetTimeInNanos)
{
  int brk = 0;
  while (!queue.isEmpty() && brk == 0)
  {
    TimedAction<?> current = queue.peek();
    if (getTime(current) > targetTimeInNanos)
    {
      brk = 1;
    }
    else
    {
      time = getTime(current);
      queue.remove();
      if (isCancelled(current) == 0)
      {
        value = call(current);
      }
      else
        ;
    }
  }
  time = targetTimeInNanos;
  return 0;
}
