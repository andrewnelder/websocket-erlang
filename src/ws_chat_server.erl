-module(ws_chat_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_history/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_history() ->
  gen_server:call(?SERVER, request_history).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
  pg2:join(chatgroup, self()),
  {ok, []}.

handle_call(request_history, _From, State) ->
    {reply, lists:reverse(State), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({chatmessage, Msg}, State) ->
    {noreply, [Msg|lists:sublist(State, 4)]}.

terminate(_Reason, _State) ->
    pg2:leave(chatgroup, self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

