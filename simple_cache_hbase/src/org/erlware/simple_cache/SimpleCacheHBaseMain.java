package org.erlware.simple_cache;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

/**
 * The main class if interaction for our simple cache system.
 * 
 * @author Eric Merritt
 *
 */
public class SimpleCacheHBaseMain {

	private HBaseConnector _conn;
	private ExecutorService _exe;
	private OtpNode _node;
	private OtpMbox _mbox;

	public SimpleCacheHBaseMain(String nodeName, String serverName)
			throws IOException {
		super();

		_conn = new HBaseConnector();
		_exe = Executors.newFixedThreadPool(10);
		_node = new OtpNode(nodeName);
		_mbox = _node.createMbox();
		_mbox.registerName(serverName);
	}

	/**
	 * Provides an 'endless loop' to process all incoming messages.
	 * 
	 */
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
					
					HBaseTask task = null;
					if (msg.arity() == 3) {
						data = ((OtpErlangBinary) msg.elementAt(3))
								.binaryValue();

						task = new HBaseTask(_mbox, _conn,
								 from, action, key, data);
					} else if (msg.arity() == 2) {
						task = new HBaseTask(_mbox, _conn,
								 from, action, key, null);
					} else {
						OtpErlangTuple res = new OtpErlangTuple(new OtpErlangObject[] {
								new OtpErlangAtom("error"), new OtpErlangAtom("invalid_request") });
						_mbox.send(from, res);
						return;
					}
					
					// Submit the task to the executor. Asynchronous processing
					_exe.submit(task);
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
