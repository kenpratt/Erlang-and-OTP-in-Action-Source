%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
%% This is the application resource file for simple_cache

{application, simple_cache,
 [{description, "A simple caching system"},
  {vsn, "0.1.0.0"},
  {modules, [simple_cache,
             sc_app,
             sc_sup,
             sc_store,
             sc_element]},
  {registered, [sc_sup]},
  {applications, [kernel, stdlib]},
  {mod, {sc_app, []}}
 ]}.
