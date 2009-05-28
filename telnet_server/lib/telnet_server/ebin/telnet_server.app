%% This is the application resource file (.app file) for the telnet_server,
%% application.
{application, telnet_server, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [telnet_server_app,
              telnet_server_sup,
	      ts_server]},
   {registered,[telnet_server_sup]},
   {applications, [kernel, stdlib]},
   {mod, {telnet_server_app,[]}},
   {start_phases, []}]}.

