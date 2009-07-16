%% This is the application resource file (.app file) for the application.
{application, protobuffs_interface, 
  [{description, "An interface mechanism using Google protocol buffers"},
   {vsn, "0.1.0"},
   {modules, [pi_app,
              pi_sup,
	      pi_server]},
   {registered,[pi_sup]},
   {applications, [kernel, stdlib]},
   {mod, {pi_app,[]}},
   {start_phases, []}]}.

