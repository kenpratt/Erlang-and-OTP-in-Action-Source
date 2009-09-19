package org.erlware.jinterface_example;

import java.io.IOException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

/**
 * The main class if interaction for our simple cache system.
 * 
 * @author Eric Merritt
 * 
 */
public class JinterfaceExample {


	private OtpNode _node;
	private OtpMbox _mbox;

	public JinterfaceExample(String nodeName, String serverName, String cookie)
			throws IOException {
		super();

		_node = new OtpNode(nodeName, cookie);
		_mbox = _node.createMbox();
		_mbox.registerName(serverName);
	}

	/**
	 * Provides an 'endless loop' to process all incoming messages.
	 * 
	 */
	public void process() {

		while (true) {
			try {
				OtpErlangObject o = _mbox.receive();
				if (o instanceof OtpErlangTuple) {
					OtpErlangTuple msg = (OtpErlangTuple) o;
					OtpErlangPid from = (OtpErlangPid) msg.elementAt(0);
					String name = ((OtpErlangString) msg.elementAt(1)).stringValue();
					
					
					OtpErlangString greetings = new OtpErlangString("Greetings to");
					
					OtpErlangTuple outMsg = new OtpErlangTuple(
							new OtpErlangObject[]{_mbox.self(),
							                  greetings,
								              new OtpErlangString(name)});
					
					_mbox.send(from, outMsg);
					
					
					
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
		if (args.length < 3) {
			usage();
			return;
		}

		JinterfaceExample main = new JinterfaceExample(args[0], args[1], args[2]);
		main.process();

	}

}
