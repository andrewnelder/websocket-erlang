-module(ws).
-export([start/0]).

start() ->
  start(ws).

start(A) ->
  case application:start(A) of
    {error, {not_started, B}} ->
      start(B),
      start(A);
    ok ->
      ok;
    Error ->
      Error
  end.
