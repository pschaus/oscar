package oscar.algo;

import java.util.*;

/**
 * A mutable map for Integer keys based on a trie using the binary representation of keys
 *
 * Not thread-safe, at all.
 *
 * @param <V> values to be contained in the structure
 */
public class MutableIntMap<V> extends AbstractMap<Integer, V> implements NavigableMap<Integer, V> {
    protected int treeSize;
    protected int[] tree;

    protected int size;
    protected V[] values;

    // Contains all the available positions in content
    protected Stack<Integer> availableTreeNode;
    protected Stack<Integer> availableValuePos;

    // After each call to findIdx, pathBuffer contains the path to the key asked. Path ends prematurely with -1
    // if the key cannot be reached
    protected int[] pathBuffer = new int[32];

    /**
     * Convert key for internal use (flip the first bit)
     */
    protected int convertKey(int key) {
        // The idea here is to invert the first bit to allow easy comparison at the first level
        // Since all numbers are encoded as two's complement, this works.
        return key ^ (1 << 31);
    }

    public final class MapEntry implements Map.Entry<Integer, V> {
        int key;
        int contentIdx;
        V value;

        public MapEntry(int key, int contentIdx) {
            this.key = key;
            this.contentIdx = contentIdx;
            this.value = null;
        }

        public Integer getKey() {
            return key;
        }

        public V getValue() {
            if(value == null)
                return values[contentIdx];
            return value;
        }

        public V setValue(V value) {
            V old = values[contentIdx];
            values[contentIdx] = value;
            return old;
        }

        private void unbind() {
            value = values[contentIdx];
        }

        public boolean equals(Object o) {
            if (!(o instanceof Map.Entry))
                return false;
            Map.Entry<? extends Integer,?> e = (Map.Entry<? extends Integer,?>)o;
            return  getKey().equals(e.getKey()) &&
                    (getValue() == null ? e.getValue() == null : getValue().equals(e.getValue()));
        }

        public int hashCode() {
            return key ^ (getValue() == null ? 0 : getValue().hashCode());
        }

        public String toString() {
            return key + "=" + getValue();
        }
    }

    public MutableIntMap() {
        clear();
    }

    @Override
    public void clear() {
        tree = new int[128];
        values = (V[]) new Object[4];
        tree[0] = -1;
        tree[1] = -1;
        treeSize = 2;
        size = 0;
        availableTreeNode = new Stack<>();
        availableValuePos = new Stack<>();
    }


    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isEmpty() {
        return size() == 0;
    }

    /**
     * Create a node in the tree. Makes sure to reuse deleted entries.
     *
     * @return the id of the new node in the array tree
     */
    protected int createNode() {
        if (availableTreeNode.isEmpty()) {
            if(treeSize+2 > tree.length) {
                int[] newTree = new int[tree.length*2];
                System.arraycopy(tree, 0, newTree, 0, treeSize);
                tree = newTree;
            }
            tree[treeSize] = -1;
            tree[treeSize+1] = -1;
            treeSize += 2;
            return treeSize-2;
        }
        else {
            int pos = availableTreeNode.pop();
            tree[pos] = -1;
            tree[pos + 1] = -1;
            return pos;
        }
    }

    /**
     * Find some available place in `values`. Makes sure to reuse deleted entries.
     *
     * @return the id of the new value in the array values
     */
    protected int createValue() {
        if (availableValuePos.isEmpty()) {
            if(size == values.length) {
                V[] newArray = (V[]) new Object[values.length*2];
                System.arraycopy(values, 0, newArray, 0, size);
                values = newArray;
            }
            values[size] = null;
            size += 1;
            return size-1;
        }
        else {
            int pos = availableValuePos.pop();
            values[pos] = null;
            size += 1;
            return pos;
        }
    }

    /**
     * Find the index where a particular key is stored.
     * Also fills pathBuffer.
     *
     * @param key    the key to search
     * @param create weither to create the path if it doesn't exists or not
     * @return the index of the key in this.values. if create is false and the index doesn't exists, returns -level-1,
     * where level is the height of the tree where the search stopped
     */
    protected int findIdx(int key, boolean create) {
        pathBuffer[0] = 0;
        // Explore the first 31 levels
        for (int i = 0; i < 31; i++) {
            int node = pathBuffer[i];
            int direction = node + ((key >> (31 - i)) & 1);
            int nextNode = tree[direction];

            // Simply follow the path if it exists
            if (nextNode != -1) {
                pathBuffer[i + 1] = nextNode;
            }
            // If the direction does not exists yet and we shouldn't create, return -level, as the node does not exist
            else if (!create) {
                pathBuffer[i + 1] = -1;
                return -i-1;
            }
            // Else, if the direction does not exists yet, create it
            else {
                pathBuffer[i + 1] = createNode();
                tree[direction] = pathBuffer[i + 1];
            }
        }
        // Then the last level
        int lastNode = pathBuffer[31] + (key & 1);
        if(tree[lastNode] != -1)
            return tree[lastNode];
        else if(!create)
            return -32;
        else {
            tree[lastNode] = createValue();
            return tree[lastNode];
        }
    }

    @Override
    public boolean containsKey(Object v) {
        int idx = findIdx((int) v, false);
        return idx >= 0;
    }

    public V get(Object key) {
        int realKey = convertKey((int) key);
        int idx = findIdx(realKey, false);
        return idx >= 0 ? values[idx] : null;
    }

    public V put(Integer key, V value) {
        int realKey = convertKey(key);
        int idx = findIdx(realKey, true);
        V old = values[idx];
        values[idx] = value;
        return old;
    }

    @Override
    public V remove(Object v) {
        int realKey = convertKey((int) v);
        int idx = findIdx(realKey, false);
        if (idx < 0)
            return null;
        V originalValue = values[idx];
        availableValuePos.add(idx);
        size--;

        //Let us follow the path from the bottom to the top and remove
        //all nodes until we find a parent with two children
        for (int i = 31; i >= 0; i--) {
            int node = pathBuffer[i];
            if (i != 0 && (tree[node] == -1 || tree[node + 1] == -1)) {
                tree[node] = -1;
                tree[node+1] = -1;
                availableTreeNode.push(node);
            } else {
                // We found a node with two children. One of them has been deleted.
                if (i == 31) { // we are at the last depth
                    if(tree[node] == idx)
                        tree[node] = -1;
                    else
                        tree[node+1] = -1;
                } else if (tree[pathBuffer[i]] == pathBuffer[i + 1]) { //if the left entry is the last node deleted
                    tree[pathBuffer[i]] = -1;
                } else { //if the right entry is the last node deleted
                    tree[pathBuffer[i] + 1] = -1;
                }
                return originalValue;
            }
        }
        throw new RuntimeException("This should never be reached");
    }

    
    public Integer lowerKey(Integer key) {
        Map.Entry<Integer, V> entry = lowerEntry(key);
        if (entry != null)
            return entry.getKey();
        return null;
    }

    
    public Integer floorKey(Integer key) {
        Map.Entry<Integer, V> entry = floorEntry(key);
        if (entry != null)
            return entry.getKey();
        return null;
    }

    
    public Integer ceilingKey(Integer key) {
        Map.Entry<Integer, V> entry = ceilingEntry(key);
        if (entry != null)
            return entry.getKey();
        return null;
    }

    
    public Integer higherKey(Integer key) {
        Map.Entry<Integer, V> entry = higherEntry(key);
        if (entry != null)
            return entry.getKey();
        return null;
    }

    
    public Integer firstKey() {
        Map.Entry<Integer, V> entry = firstEntry();
        if (entry != null)
            return entry.getKey();
        return null;
    }

    
    public Integer lastKey() {
        Map.Entry<Integer, V> entry = lastEntry();
        if (entry != null)
            return entry.getKey();
        return null;
    }

    /**
     * Utility function used to find predecessor/successor of a given key.
     * The idea is to go back on the path until we find the first not-yet-used turn, take it, then
     * go as far as possible as our initial number
     * @param key
     * @param left
     * @param strict
     * @return
     */
    private MapEntry findLowestNodeGoingToXAndFollow(int key, boolean left, boolean strict) {
        // Fill pathBuffer
        int idx = findIdx(key, false);

        //If not strict and we have a value, return immediately
        if (!strict && idx >= 0)
            return new MapEntry(convertKey(key), idx);

        // Verify that the last node on the path does not already contain what we search
        boolean reachedLastLevel = idx >= 0 || idx == -32;
        if (reachedLastLevel && left && (key & 1) == 1 && tree[pathBuffer[31]] != -1) {
            key &= ~1;
            return new MapEntry(convertKey(key), tree[pathBuffer[31]]);
        }
        if (reachedLastLevel && !left && (key & 1) == 0 && tree[pathBuffer[31] + 1] != -1) {
            key |= 1;
            return new MapEntry(convertKey(key), tree[pathBuffer[31] + 1]);
        }

        // Let's find the first node, from the bottom, that can go left (or right if !left)
        int offset = left ? 0 : 1;
        int level = -1;
        int startLevel = reachedLastLevel ? 30 : -idx-1;
        for (int i = startLevel; i >= 0; i--) {
            int nextNode = tree[pathBuffer[i] + offset];
            if (nextNode != -1 && nextNode != pathBuffer[i + 1]) {
                level = i;
                break;
            }
        }
        if (level == -1)
            return null;

        //Let's go left (or right if !left)
        if (left) {
            pathBuffer[level + 1] = tree[pathBuffer[level]];
            key &= ~(1 << (31 - level));
        } else {
            pathBuffer[level + 1] = tree[pathBuffer[level] + 1];
            key |= (1 << (31 - level));
        }
        level++;

        //Find the rightmost (or leftmost if !left) value
        if (left) {
            for (int i = level; i < 31; i++) {
                if (tree[pathBuffer[i] + 1] != -1) {
                    pathBuffer[i + 1] = tree[pathBuffer[i] + 1];
                    key |= (1 << (31 - i));
                } else {
                    pathBuffer[i + 1] = tree[pathBuffer[i]];
                    key &= ~(1 << (31 - i));
                }
            }
            if (tree[pathBuffer[31] + 1] != -1)
                return new MapEntry(convertKey(key | 1), tree[pathBuffer[31] + 1]);
            else
                return new MapEntry(convertKey(key & ~1), tree[pathBuffer[31]]);
        } else {
            for (int i = level; i < 31; i++) {
                if (tree[pathBuffer[i]] != -1) {
                    pathBuffer[i + 1] = tree[pathBuffer[i]];
                    key &= ~(1 << (31 - i));
                } else {
                    pathBuffer[i + 1] = tree[pathBuffer[i] + 1];
                    key |= (1 << (31 - i));
                }
            }
            if (tree[pathBuffer[31]] != -1)
                return new MapEntry(convertKey(key & ~1), tree[pathBuffer[31]]);
            else
                return new MapEntry(convertKey(key | 1), tree[pathBuffer[31] + 1]);
        }
    }

    
    public Map.Entry<Integer, V> lowerEntry(Integer key) {
        if (size() == 0)
            return null;
        return findLowestNodeGoingToXAndFollow(convertKey(key), true, true);
    }

    
    public Map.Entry<Integer, V> floorEntry(Integer key) {
        if (size() == 0)
            return null;
        return findLowestNodeGoingToXAndFollow(convertKey(key), true, false);
    }

    
    public Map.Entry<Integer, V> higherEntry(Integer key) {
        if (size() == 0)
            return null;
        return findLowestNodeGoingToXAndFollow(convertKey(key), false, true);
    }

    
    public Map.Entry<Integer, V> ceilingEntry(Integer key) {
        if (size() == 0)
            return null;
        return findLowestNodeGoingToXAndFollow(convertKey(key), false, false);
    }

    
    public Map.Entry<Integer, V> firstEntry() {
        return ceilingEntry(Integer.MIN_VALUE);
    }

    
    public Map.Entry<Integer, V> lastEntry() {
        return floorEntry(Integer.MAX_VALUE);
    }

    
    public Map.Entry<Integer, V> pollFirstEntry() {
        MapEntry entry = (MapEntry)firstEntry();
        if (entry == null)
            return null;
        entry.unbind();
        remove(entry.getKey());
        return entry;
    }

    
    public Map.Entry<Integer, V> pollLastEntry() {
        MapEntry entry = (MapEntry)lastEntry();
        if (entry == null)
            return null;
        entry.unbind();
        remove(entry.getKey());
        return entry;
    }
    
    public Comparator<? super Integer> comparator() {
        return Comparator.naturalOrder();
    }

    private transient volatile Set<Map.Entry<Integer, V>> entrySet;
    public Set<Map.Entry<Integer, V>> entrySet() {
        if(entrySet == null)
            entrySet = new EntrySet(this);
        return entrySet;
    }

    class EntrySetIterator implements Iterator<Map.Entry<Integer, V>> {
        protected MapEntry current;
        protected MapEntry next;
        private EntrySetIterator() {
            current = null;
            next = (MapEntry)firstEntry();
        }

        @Override
        public boolean hasNext() {
            return next != null;
        }

        @Override
        public Map.Entry<Integer, V> next() {
            if(next == null)
                throw new NoSuchElementException();
            current = next;
            next = (MapEntry)higherEntry(current.getKey());
            return current;
        }

        @Override
        public void remove() {
            if(current == null)
                throw new IllegalStateException();
            current.unbind();
            MutableIntMap.this.remove(current.getKey());
        }
    }

    class EntrySet extends AbstractSet<Map.Entry<Integer, V>> implements Set<Map.Entry<Integer, V>> {
        MutableIntMap<V> parent;

        private EntrySet(MutableIntMap<V> parent){
            this.parent = parent;
        }

        @Override
        public boolean contains(Object o) {
            Map.Entry<Integer, V> entry = (Map.Entry<Integer, V>)o;
            V val = MutableIntMap.this.get(entry.getKey());
            return val != null && !val.equals(entry.getValue());
        }

        @Override
        public Iterator<Map.Entry<Integer, V>> iterator() {
            return new EntrySetIterator();
        }

        @Override
        public int size() {
            return parent.size();
        }
    }

    public NavigableSet<Integer> navigableKeySet() {
        throw new UnsupportedOperationException();
    }

    
    public NavigableSet<Integer> descendingKeySet() {
        throw new UnsupportedOperationException();
    }

    
    public NavigableMap<Integer, V> descendingMap() {
        throw new UnsupportedOperationException();
    }

    
    public NavigableMap<Integer, V> subMap(Integer fromKey, boolean fromInclusive, Integer toKey, boolean toInclusive) {
        throw new UnsupportedOperationException();
    }

    
    public NavigableMap<Integer, V> headMap(Integer toKey, boolean inclusive) {
        throw new UnsupportedOperationException();
    }

    
    public NavigableMap<Integer, V> tailMap(Integer fromKey, boolean inclusive) {
        throw new UnsupportedOperationException();
    }

    
    public SortedMap<Integer, V> subMap(Integer fromKey, Integer toKey) {
        throw new UnsupportedOperationException();
    }

    
    public SortedMap<Integer, V> headMap(Integer toKey) {
        throw new UnsupportedOperationException();
    }

    
    public SortedMap<Integer, V> tailMap(Integer fromKey) {
        throw new UnsupportedOperationException();
    }
}
