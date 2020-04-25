import java.util.concurrent.atomic.AtomicLongArray;

public class AcmeSafeState implements State {
    AtomicLongArray value;

    AcmeSafeState(int length) { value = new AtomicLongArray(length); }

    public int size() { return value.length(); }

    public long[] current() { // build an array and return
        long[] current_array = new long[value.length()];
        for (int i = 0; i < value.length(); i++) {
            current_array[i] = value.get(i);
        }
        return current_array;
    }

    public void swap(int i, int j) {
        value.getAndDecrement(i);
        value.getAndIncrement(j);
    }
}
