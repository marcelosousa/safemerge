public class TestScheduler extends Scheduler {
    private final Queue<TimedAction<?>> queue = new PriorityQueue<TimedAction<?>>(11, new CompareActionsByTime());

    // Storing time in nanoseconds internally.
    private int time;

    private int triggerActions(int targetTimeInNanos) {
      int brk = 0;
      while (!queue.isEmpty() && (brk == 0)) {
        TimedAction<?> current = queue.peek();
        if (getTime(current) > targetTimeInNanos) {
          time = targetTimeInNanos;
          brk = 1;
        } else {
          time = getTime(current);
          queue.remove();
          call(current);
        }
      }
      return 0;
    }
}
