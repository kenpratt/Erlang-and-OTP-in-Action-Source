import com.ericsson.otp.erlang.*;

public class HBaseTask implements Runnable {
  private OtpMbox mbox;
  private HBaseConnector conn;
  private OtpErlangPid from;
  private OtpErlangRef ref;
  private String action;
  private String key;
  private byte[] data;

  public HBaseTask(OtpMbox mbox, HBaseConnector conn,
                   OtpErlangPid from, OtpErlangRef ref,
                   String action, String key, byte[] data) {
    super();
    this.mbox = mbox;
    this.conn = conn;
    this.ref = ref;
    this.action = action;
    this.key = key;
    this.data = data;
  }

  public void run() {
    try {
      if (action.equals("get")) {
        doGet();
      } else if (action.equals("put")) {
        doPut();
      } else if (action.equals("delete")) {
        doDelete();
      }
    } catch (Exception e) {
   // FIXME this error message is not handled by the Erlang code!
        OtpErlangTuple reply =
          new OtpErlangTuple(new OtpErlangObject[] {
                                   new OtpErlangAtom("error"), ref,
                                   new OtpErlangList(e.getMessage())
                                 });
        mbox.send(from, reply);
    }
  }

  private void doGet() throws Exception {
    byte[] data = conn.get(key);
    OtpErlangTuple reply = new OtpErlangTuple(new OtpErlangObject[] {
                                 new OtpErlangAtom("reply"), ref, 
                                 new OtpErlangBinary(data)
                               });
    mbox.send(from, reply);
  }

  private void doPut() throws Exception {
    conn.put(key, data);
    OtpErlangTuple reply = new OtpErlangTuple(new OtpErlangObject[] {
                                 new OtpErlangAtom("reply"), ref, 
                                 new OtpErlangAtom("ok")
                               });
    mbox.send(from, reply);
  }

  private void doDelete() throws Exception {
    conn.delete(key);
    OtpErlangTuple reply = new OtpErlangTuple(new OtpErlangObject[] {
                                 new OtpErlangAtom("reply"), ref,
                                 new OtpErlangAtom("ok")
                               });
    mbox.send(from, reply);
  }
}

