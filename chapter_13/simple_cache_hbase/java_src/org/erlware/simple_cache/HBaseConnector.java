package org.erlware.simple_cache;

import java.io.IOException;
import java.util.NavigableMap;

import org.apache.hadoop.hbase.HBaseConfiguration;
import org.apache.hadoop.hbase.client.Delete;
import org.apache.hadoop.hbase.client.Get;
import org.apache.hadoop.hbase.client.HTable;
import org.apache.hadoop.hbase.client.Put;
import org.apache.hadoop.hbase.client.Result;

/**
 * This class provides a direct get/put interface to our HBase tables
 * 
 * @author Eric Merritt
 *
 */
public class HBaseConnector {
	private HTable _table;
	
	public HBaseConnector() throws IOException {
		super();

		_table = new HTable(new HBaseConfiguration(), "cache");
	}

	/**
	 * Get a cache object from the table, by its filename.
	 * 
	 * @param fileName The filename/object name of the cached object
	 * @return The value of that object or null if it doesn't exist
	 * @throws IOException
	 */
	public byte[] get(String fileName) throws IOException {
		Result result = _table.get(new Get(fileName.getBytes()));
		NavigableMap<byte[], NavigableMap<byte[], byte[]>> res = result.getNoVersionMap();
		
		return res.get("value".getBytes()).get("".getBytes());
	}
	
	/**
	 * Put a new cached object into the table.
	 * 
	 * @param filename The name of the object
	 * @param value The binary value of the object
	 * @throws IOException
	 */
	public void put(String filename, byte[] value) throws IOException {
		Put put = new Put(filename.getBytes());
		put.add("value".getBytes(), "".getBytes(), value);
	
		_table.put(put);
	}
	
	/**
	 * Delete a key from the system
	 * @param filename The name of the file to delete.
	 * @throws IOException
	 */
	public void delete(String filename) throws IOException {
		Delete del = new Delete(filename.getBytes());
	
		_table.delete(del);
	}

}
