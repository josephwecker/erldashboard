-module(mail_stream).

-behaviour(gen_server).

%% Interface
-export([
    start_link/1,
    start_link/2,
    start_link/3
  ]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

start_link(Conn) ->
  start_link(Conn, "Inbox").
start_link(Conn, MBox) ->
  start_link(Conn, MBox, 5000).
start_link(Conn, MBox, Freq) ->
  Self = self(),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Conn, MBox, Freq, Self], []).

init([{Host, Username, Password}, MBox, Freq, Requestor]) ->
  Port = open_port({spawn, "fetch.py"}, [{packet, 4}, binary]),
  port_command(Port, term_to_binary({connect, Host, Username, Password,
        MBox})),
  receive
    {Port, {data, Data}} ->
      case binary_to_term(Data) of
        ok ->
          self() ! check_mail,
          {ok, {Port, Requestor, Freq}};
        ErrReason ->
          {stop, ErrReason}
      end
  end.

handle_info(check_mail, {Port, _, _} = State) ->
  initiate_mail_check(Port),
  {noreply, State};

handle_info({Port, {data, Data}}, {Port, _, _} = State) ->
  port_response(binary_to_term(Data), State).

port_response({email, Data, Msgs}, {_, Requestor, _} = State) ->
  Data2 = [{K,proplists:get_all_values(K,Data)} ||
    K <- proplists:get_keys(Data)],
  Requestor ! {email, Data2, Msgs},
  {noreply, State};
port_response(done_getting_new, {_, _, Freq} = State) ->
  timer:send_after(Freq, self(), check_mail),
  {noreply, State}.

initiate_mail_check(Port) ->
  port_command(Port, term_to_binary({get_new})),
  receive
    {Port, {data, Data}} ->
      case binary_to_term(Data) of
        initiating_get_new ->
          ok;
        {error, Reason} ->
          erlang:error(Reason)
      end
  end.

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
