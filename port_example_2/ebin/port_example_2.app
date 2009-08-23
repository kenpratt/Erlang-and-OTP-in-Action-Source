%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, port_example_2,
 [{description, "simple linked in driver example"},
  {vsn, "0.1.0"},
  {modules, [pe2_app,
             pe2_sup,
             pe2_c_drv]},
  {registered, [pe2_sup, pe2_c_drv]},
  {applications, [kernel, stdlib]},
  {mod, {pe2_app, []}}]}.
