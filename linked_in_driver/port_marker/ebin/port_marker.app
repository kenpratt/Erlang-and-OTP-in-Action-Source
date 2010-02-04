%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, port_marker,
 [{description, ""},
  {vsn, "1.0.0.0"},
  {modules, [pm_marker,
             pm_app,
             pm_sup]},
  {registered, [pm_sup, pm_marker]},
  {applications, []},
  {mod, {pm_app, []}}]}.
