package org.erlware.simple_cache;

import java.io.IOException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * This class provides an asynchronous mechanism by which a value can be gotten
 * from the database or put to it. 
 * 
 * @author Eric Merritt
 *
 */
public class HBaseTask implements Runnable {
	private HBaseConnector _conn;
	private OtpMbox _mbox;
	private OtpErlangPid _from;
	private String _action;
	private String _key;
	private byte[] _value;

	/**
	 * Creates and initializes the task
	 * 
	 * @param mbox The erlang mailbox to use for sending
	 * @param conn The connection object to our hbase system
	 * @param from The pid that the request came from
	 * @param action The action that was requested (get/put)
	 * @param key The key of the request
	 * @param value The value if it exists (may be null if get is the action)
	 */
	public HBaseTask(OtpMbox mbox, HBaseConnector conn, OtpErlangPid from,
			String action, String key, byte[] value) {
		super();

		_mbox = mbox;
		_conn = conn;
		_from = from;
		_action = action;
		_key = key;
		_value = value;
	}

	/**
	 * Get a value from the database and send it back to the requestor in the form
	 * 
	 * {get_result, <data>}
	 * 
	 * @throws IOException
	 */
	private void doGet() throws IOException {
		byte[] data = _conn.get(_key);

		OtpErlangTuple res = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("get_result"), new OtpErlangBinary(data) });

		_mbox.send(_from, res);

	}

	/**
	 * Put a value into the database. Sends result to the user in the form
	 * 
	 * {put_result, ok}
	 * @throws IOException
	 */
	private void doPut() throws IOException {
		_conn.put(_key, _value);

		OtpErlangTuple res = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("put_result"), new OtpErlangAtom("ok") });

		_mbox.send(_from, res);

	}
	

	/**
	 * The run task of the runnable interface. Actually kicks off and does the work.
	 * 
	 */
	@Override
	public void run() {
		try {
		if (_action.equals("get")) {
			doGet();
		} else if (_action.equals("put")) {
			doPut();
		}
		}catch(IOException ioe){
			
			// Let the caller know that we had a problem
			OtpErlangTuple res = new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("error"), new OtpErlangList(ioe.getMessage()) });
			_mbox.send(_from, res);
		}
	}

}
