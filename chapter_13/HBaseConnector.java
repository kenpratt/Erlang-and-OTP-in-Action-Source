
import org.apache.hadoop.hbase.HBaseConfiguration;
import org.apache.hadoop.hbase.client.*;
import java.util.NavigableMap;

public class HBaseConnector {
  private HTable table;

  public HBaseConnector() throws Exception {
    super();
    table = new HTable(new HBaseConfiguration(), "cache");
  }

  public byte[] get(String key) throws Exception {
    Result result = table.get(new Get(key.getBytes()));
    NavigableMap<byte[], NavigableMap<byte[], byte[]>> map =
        result.getNoVersionMap();
    return map.get("value".getBytes()).get("".getBytes());
  }

  public void put(String key, byte[] value) throws Exception {
    Put put = new Put(key.getBytes());
    put.add("value".getBytes(), "".getBytes(), value);
    table.put(put);
  }

  public void delete(String key) throws Exception {
    Delete del = new Delete(key.getBytes());
    table.delete(del);
  }
}
