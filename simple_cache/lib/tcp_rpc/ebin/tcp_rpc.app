%% This is the application resource file (.app file) for the telnet_server,
%% application.
{application, tcp_rpc, 
  [{description, "An RPC mechanism over TCP"},
   {vsn, "0.1.0"},
   {modules, [tr_app,
              tr_sup,
	      tr_server]},
   {registered,[tr_sup]},
   {applications, [kernel, stdlib]},
   {mod, {tr_app,[]}},
   {start_phases, []}]}.

