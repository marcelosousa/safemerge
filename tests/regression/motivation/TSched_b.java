public class TestScheduler extends Scheduler {
    private final Queue<TimedAction<?>> queue = new PriorityQueue<TimedAction<?>>(11, new CompareActionsByTime());

    // Storing time in nanoseconds internally.
    private long time;

    private void triggerActions(long targetTimeInNanos) {
      int brk = 0;
      while (!queue.isEmpty() && (brk == 0)) {
        TimedAction<?> current = queue.peek();
        if (getTime(current) > targetTimeInNanos) {
          time = targetTimeInNanos;
          brk = 1;
        } else {
          time = getTime(current);
          queue.remove();
          if (isCancelled(current) == 0){
            call(current);
          }
        }
      }
    }
}
