package org.erlware.simple_cache;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
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

	public SimpleCacheHBaseMain(String nodeName, String cookie, String serverName)
			throws IOException {
		super();

		_conn = new HBaseConnector();
		_exe = Executors.newFixedThreadPool(10);
		_node = new OtpNode(nodeName, cookie);
		_mbox = _node.createMbox();
		_mbox.registerName(serverName);
	}

	/**
	 * Provides an 'endless loop' to process all incoming messages.
	 * 
	 */
	public void process() {

		byte[] data;

		while (true) {
			try {
				OtpErlangObject o = _mbox.receive();
				if (o instanceof OtpErlangTuple) {
					OtpErlangTuple msg = (OtpErlangTuple) o;
					OtpErlangPid from = (OtpErlangPid) msg.elementAt(0);
					OtpErlangRef ref = (OtpErlangRef) msg.elementAt(1);
					String action = ((OtpErlangAtom) msg.elementAt(2)).toString();
					String key = ((OtpErlangList) msg.elementAt(3)).toString();
					
					HBaseTask task = null;
					if (msg.arity() == 3) {
						data = ((OtpErlangBinary) msg.elementAt(3))
								.binaryValue();

						task = new HBaseTask(_mbox, _conn,
								 from, ref, action, key, data);
					} else if (msg.arity() == 2) {
						task = new HBaseTask(_mbox, _conn,
								 from, ref, action, key, null);
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
				System.out.println(e.getMessage());
			}
		}

	}

	private static void usage() {
		System.out
				.println("You must provide the node name, node cookie and the server name");
	}

	public static void main(String[] args) throws Exception {
		if (args.length < 3) {
			usage();
			return;
		}

		String nodeName = args[0];
		String cookie = args[1];
		String serverName = args[2];
		
		SimpleCacheHBaseMain main = new SimpleCacheHBaseMain(nodeName, cookie, serverName);
		main.process();

	}

}
