import com.ericsson.otp.erlang.*;
import java.util.concurrent.*;

public class HBaseNode {
  private HBaseConnector conn;
  private ExecutorService exec;
  private OtpNode node;
  private OtpMbox mbox;

  public HBaseNode(String nodeName, String cookie)
  throws Exception {
    super();
    conn = new HBaseConnector();
    exec = Executors.newFixedThreadPool(10);
    node = new OtpNode(nodeName, cookie);
    mbox = node.createMbox("hbase_server");
  }

  public static void main(String[] args) throws Exception {
    if (args.length != 2) {
      System.out.println("wrong number of arguments");
      System.out.println("expected: nodeName cookie");
      return;
    }
    HBaseNode main = new HBaseNode(args[0],args[1]);
    main.process();
  }

  // message format: { Action, FromPID, UniqueRef, Key [, Value] }
  private void process() {
    byte[] data;
    while (true) {
      try {
	OtpErlangObject msg = mbox.receive();
	OtpErlangTuple t = (OtpErlangTuple) msg;
	String action = ((OtpErlangAtom) t.elementAt(0)).atomValue();
	OtpErlangPid from = (OtpErlangPid) t.elementAt(1);
	OtpErlangRef ref = (OtpErlangRef) t.elementAt(2);
	String key = ((OtpErlangList) t.elementAt(3)).toString();
	HBaseTask task = null;
	if (t.arity() == 5) {
	  data = ((OtpErlangBinary) t.elementAt(4)).binaryValue();
	  task = new HBaseTask(mbox, conn, from, ref, action, key, data);
	} else if (t.arity() == 4) {
	  task = new HBaseTask(mbox, conn, from, ref, action, key, null);
	} else {
	  System.out.println("invalid request: " + t);
	  continue;
	}
	exec.submit(task);
      } catch (Exception e) {
	  System.out.println("caught error: " + e);
      }
    }
  }
}
