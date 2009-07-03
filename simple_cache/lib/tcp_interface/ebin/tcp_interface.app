%% This is the application resource file (.app file) for the telnet_server,
%% application.
{application, tcp_interface, 
  [{description, "An interface mechanism over TCP"},
   {vsn, "0.1.0"},
   {modules, [ti_app,
              ti_sup,
	      ti_server]},
   {registered,[ti_sup]},
   {applications, [kernel, stdlib]},
   {mod, {ti_app,[]}},
   {start_phases, []}]}.

