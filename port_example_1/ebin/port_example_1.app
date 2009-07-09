%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, port_example_1,
 [{description, "simple port example"},
  {vsn, "0.1.0"},
  {modules, [pe1_app,
             pe1_sup,
             pe1_py_drv]},
  {registered, [pe_sup, py_drv]},
  {applications, [kernel, stdlib]},
  {mod, {pe1_app, []}}]}.
