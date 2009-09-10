package org.erlware.simple_cache;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.hadoop.hbase.HBaseConfiguration;
import org.apache.hadoop.hbase.client.Get;
import org.apache.hadoop.hbase.client.HTable;
import org.apache.hadoop.hbase.client.Result;

public class HBaseConnector {

	public static Map retrievePost(String postId) throws IOException {
		HTable table = new HTable(new HBaseConfiguration(), "tester");
		Map post = new HashMap();

		Get g = new Get(postId.getBytes());
		Result rs = table
		.get(g);
		post = rs.getMap();
		
		System.out.println(post.get("value:binary"));
		
		
		return post;
	}

	public static void main(String[] args) throws IOException {
		Map blogpost = HBaseConnector.retrievePost("some/file");
		System.out.println(blogpost);
	}
}
