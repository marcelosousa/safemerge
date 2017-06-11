public class TestScheduler extends Scheduler {
    private final Queue<TimedAction<?>> queue = new PriorityQueue<TimedAction<?>>(11, new CompareActionsByTime());

    // Storing time in nanoseconds internally.
    private int time;

    private void triggerActions(int targetTimeInNanos) {
      int x = 1;
      TimedAction<?> current = queue.peek();
      targetTimeInNanos = foo(current,x); 
      return targetTimeInNanos;
    }
}
