
%% - Start up the email fetcher
%% - Trigger preliminary analysis and potentially save every email

-module(email_analysis).
-behaviour(gen_server).

%% Interface
-export([start_link/2]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(MDB, email_analysis_pool).

start_link(Mailboxes, AnalyzeCallback) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Mailboxes, AnalyzeCallback], []).


init([Mailboxes, AnalyzeCallback]) ->
  mail_stream:start_link(Mailboxes),
  application:start(emongo),
  emongo:add_pool(?MDB, "localhost", 27017, "email_analysis", 1),
  {ok, AnalyzeCallback}.

handle_info({email, HeadDat, Bodies}, AnalyzeCallback) ->
  spawn(fun() -> process_callback(HeadDat, Bodies, AnalyzeCallback) end),
  {noreply, AnalyzeCallback}.

process_callback(HeadDat, Bodies, AnalyzeCallback) ->
  case AnalyzeCallback(HeadDat, Bodies) of
    store ->
      store_email(HeadDat ++ Bodies);
    {store, More} ->
      store_email(More++HeadDat++Bodies);
    completely_ignore ->
      ok
  end.

store_email(Data) ->
  Check = [
    {"message-id", proplists:get_value("message-id",Data)},
    {"date", proplists:get_value("date",Data)},
    {"from", proplists:get_value("from",Data)}
  ],
  emongo:update(?MDB, "email", Check, Data, true).




%% Unused callbacks
handle_call(_Req, _Fr, State) ->
  {reply, not_implemented, State}.
handle_cast(_Request, State) ->
  {noreply, State}.
terminate(_Reason, {Port, _Requestor, _Freq}) ->
  port_close(Port),
  ok.
code_change(_Old, State, _Extra) ->
  {ok, State}.
