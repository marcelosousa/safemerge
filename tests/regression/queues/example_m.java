public class TestScheduler extends Scheduler {
    private final Queue<TimedAction<?>> queue = new PriorityQueue<TimedAction<?>>(11, new CompareActionsByTime());

    // Storing time in nanoseconds internally.
    private long time;

    private void triggerActions(long targetTimeInNanos) {
      return targetTimeInNanos;
    }
}
