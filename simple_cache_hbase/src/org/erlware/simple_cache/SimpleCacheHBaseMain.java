package org.erlware.simple_cache;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.apache.hadoop.hbase.HBaseConfiguration;
import org.apache.hadoop.hbase.client.HTable;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public class SimpleCacheHBaseMain {

	private HTable _table;
	private ExecutorService _exe;
	private OtpNode _node;
	private OtpMbox _mbox;

	public SimpleCacheHBaseMain(String nodeName, String serverName)
			throws IOException {
		super();
		
		_table = new HTable(new HBaseConfiguration(), "tester");
		_exe = Executors.newFixedThreadPool(10);
		_node = new OtpNode(nodeName);
		_mbox = _node.createMbox();
		_mbox.registerName(serverName);
	}

	public void process() {

		OtpErlangObject o;
		OtpErlangTuple msg;
		OtpErlangPid from;
		String action;
		String key;
		byte[] data;

		while (true) {
			try {
				o = _mbox.receive();
				if (o instanceof OtpErlangTuple) {
					msg = (OtpErlangTuple) o;
					from = (OtpErlangPid) msg.elementAt(0);
					action = ((OtpErlangAtom) msg.elementAt(1)).toString();
					key = ((OtpErlangList) msg.elementAt(2)).toString();
					data = ((OtpErlangBinary) msg.elementAt(3)).binaryValue();
					_mbox.send(from, msg.elementAt(1));
				}
			} catch (Exception e) {
				System.out.println("" + e);
			}
		}

	}

	private static void usage() {
		System.out
				.println("You must provide the node name and the server name");
	}

	public static void main(String[] args) throws Exception {
		if (args.length < 2) {
			usage();
			return;
		}

		SimpleCacheHBaseMain main = new SimpleCacheHBaseMain(args[0], args[1]);
		main.process();

	}

}
