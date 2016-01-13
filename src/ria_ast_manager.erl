%%%-------------------------------------------------------------------
%%% @author sanr
%%% @copyright (C) 2014, <Ria.com>
%%% @doc
%%%
%%% @end
%%% Created : 22. авг 2014 16:38
%%%-------------------------------------------------------------------
-module(ria_ast_manager).
-author("sanr").

-behaviour(gen_server).

-export([start_link/2]).
-export([login/2]).
-export([getKey/2]).
-export([handle_event/1]).

%%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).
-export([analize_event/1]).%For testing

-record(state, {
  socket,
  pkg_acc,
  process_function = fun(_Message)->ok end
}).

-include("ast_mgr.hrl").

start_link(Host, Port) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port, []], []).

login(Name, Passwd) ->
  Login = action(<<"Login">>, [{<<"Username">>, Name},
    {<<"Secret">>, Passwd},
    {<<"Events">>, <<"call,hud,system">>}
  ]),
  Login,
  case send_cmd(Login) of
    {ok, _} -> ok;
    Error -> Error
  end.


init([Host, Port, _Options]) ->
  TCPOpts = [binary, {active, true}, {packet, line}],
%%   Connect = gen_tcp:connect(Host, Port, TCPOpts),
  case gen_tcp:connect(Host, Port, TCPOpts) of
    {ok, Socket} ->
      {ok, #state{socket = Socket,
        pkg_acc = []}};
    Error ->
      io:format("Error:~p~n", [Error]),
      timer:sleep(5000),
      {stop, Error}
  end.

handle_call({send, Cmd}, _From, #state{socket = Socket} = State) ->
  gen_tcp:send(Socket, Cmd),
  {reply, ok, State};

handle_call(ping, _From, State) ->
  {reply, pong, State};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({set_function,Function}, State)->
  NewState = case is_function(Function) of
    false->State;
    true->State#state{process_function=Function}
  end,
  {noreply, NewState};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({tcp, _Socket, <<"\r\n">>}, #state{pkg_acc = []} = State) ->
  {noreply, State};

handle_info({tcp, _Socket, <<"\r\n">>}, #state{pkg_acc = PkgAcc, process_function = Function} = State) ->
  ParsedEvents = ast_parser:parse_package(lists:reverse(PkgAcc)),
  Function(ParsedEvents),
%%   analize_event(ParsedEvents),
  {noreply, State#state{pkg_acc = []}};

handle_info({tcp, _Socket, Data}, #state{pkg_acc = Pkg} = State) ->
  Line = strip_nl(Data),
  {noreply, State#state{pkg_acc = [Line | Pkg]}};


handle_info(Info, State) ->
  io:format("~p~n", [Info]),
  {stop, {unknown_message, Info}, State}.

terminate(Reason, State) ->
  io:format("~p terminate. Reasone: ~p. State ~p", [?MODULE, Reason, State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


handle_event(_Event)->
  oi:format("Event ~n"),
  ok.
%% +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%%                                        CALL BLOCK
%% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
analize_event({event, 'Hangup', #hangup{cause_txt = CauseTxt, unique_id = UniqId} = _Record, _ActionID}) ->
  case CauseTxt == "User busy" of
    true -> ok;
    false ->
      free_lines:channel_stop(UniqId)
  end,
  ok;
analize_event({event, 'Newchannel', #channel{unique_id = _UniqueId, caller_id_num = Number, exten = "111"} = Record, _ActionID}) ->
  io:format("~p~n", [Record]),
  io:format("Exten: ~p~n", ["111"]),
  free_lines:number_hold(Number),
  ok;

analize_event({event, 'Newchannel', #channel{unique_id = _UniqueId, caller_id_num = Number, exten = "000"} = Record, _ActionID}) ->
  io:format("~p~n", [Record]),
  io:format("Exten: ~p~n", ["000"]),
  free_lines:number_idle(Number),
  ok;

analize_event({event, 'Newchannel', #channel{unique_id = UniqueId, caller_id_num = Number, exten = _Exten} = _Record, _ActionID}) ->
  free_lines:channel_set_number(UniqueId, Number),
  ok;

analize_event({event, 'Bridge', #bridge{bridgestate = "Link"} = Bridge, _ActionID}) ->
  free_lines:channel_start_call(Bridge#bridge.unique_id_1),
  free_lines:channel_start_call(Bridge#bridge.unique_id_2),
  ok;
%% +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%% ---------------------------------------------------------------------------------------------------------------------
%%                                  RENAME BLOCK
%% ---------------------------------------------------------------------------------------------------------------------
analize_event({event, 'Rename', #rename{unique_id = Channel} = Rename, _ActionID}) ->
  case Rename#rename.new_number==Rename#rename.old_number of
    true->ok;
    false->
      free_lines:channel_set_number(Channel, Rename#rename.new_number)
  end,
ok;


%% =====================================================================================================================
%%                                   SIP STATUS BLOCK
%% =====================================================================================================================
analize_event({event, 'Registry', #registry{channel = _Channel} = Registry, _ActionID}) ->
  io:format("Registry is:~p~n", [Registry]),
  ok;
analize_event({event, 'PeerStatus', #peer_status{peer_status = 'Reachable'} = PeerStatus, _ActionID}) ->
  io:format("PeerStatus is:~p~n", [PeerStatus]),
  case string:to_integer(string:sub_string(PeerStatus#peer_status.peer, 5, 7)) of
    {error, no_integer} -> ok;
    {Number, _} -> free_lines:number_idle(Number);
    _ -> error
  end,
  ok;
analize_event({event, 'PeerStatus', #peer_status{peer_status = 'Unreachable'} = PeerStatus, _ActionID}) ->
  io:format("PeerStatus is:~p~n", [PeerStatus]),
  case string:to_integer(string:sub_string(PeerStatus#peer_status.peer, 5, 7)) of
    {error, no_integer} -> ok;
    {Number, _} -> free_lines:number_inactive(Number);
    _ -> error
  end,
  ok;
analize_event({event, 'PeerStatus', #peer_status{peer_status = 'Unregistered'} = PeerStatus, _ActionID}) ->
  io:format("PeerStatus is:~p~n", [PeerStatus]),
  case string:to_integer(string:sub_string(PeerStatus#peer_status.peer, 5, 7)) of
    {error, no_integer} -> ok;
    {Number, _} -> free_lines:number_inactive(Number);
    _ -> error
  end,
  ok;
analize_event({event, 'PeerStatus', #peer_status{peer_status = _Status} = PeerStatus, _ActionID}) ->
  io:format("PeerStatus is:~p (ignored)~n", [PeerStatus]),
  ok;
%% =====================================================================================================================

analize_event(_Other) ->
  ok.

%% -----------------------------------------------------------------------------
%% @spec send_cmd(Cmd) -> mgr_response()
%% @doc
%% Sends a command package to Asterisk.
%% @end
%% -----------------------------------------------------------------------------
send_cmd(Cmd) ->
  gen_server:call(?MODULE, {send, Cmd}).

%% -----------------------------------------------------------------------------
%% @spec strip_nl(Binary::binary()) -> binary()
%% @doc
%% Strips newline characters from a binary, either \r\n or \n
%% @end
%% -----------------------------------------------------------------------------
strip_nl(Binary) ->
  Size = size(Binary),
  NLSize = Size - 1,
  CRNLSize = Size - 2,
  case Binary of
    <<Bytes:CRNLSize/binary, "\r\n">> ->
      Bytes;
    <<Bytes:NLSize/binary, "\n">> ->
      Bytes
  end.

%% -----------------------------------------------------------------------------
%% @spec action(Action::string(), Args) -> mgr_response()
%%              Args = [Arg]
%%              Arg  = {Name, Value}
%%              Name = string()
%%              Value = string() | atom() | integer()
%% @doc
%% Creates a package to be sent a command to Asterisk.
%% @end
%% -----------------------------------------------------------------------------
action(Action, Args) ->
  ArgsBin = args(Args),
%% <<"Action: ", <<"Login">>  [{<<"Username">>, Name}, {<<"Secret">>, Passwd},{<<"Events">>, <<"call,hud">>}], "\r\n">>
  <<"Action: ", Action/binary, "\r\n", ArgsBin/binary, "\r\n">>.

%% -----------------------------------------------------------------------------
%% @spec args(Pkg::string(), Args) -> string()
%%                Args = [Arg]
%%                Arg  = {Name, Value}
%%                Name = string()
%%                Value = string() | atom() | integer()
%% @doc
%% Makes a string/package of arguments.
%% @end
%% -----------------------------------------------------------------------------
args([{Name, Value} | Args]) ->
  C = <<Name/binary, ": ", Value/binary, "\r\n">>,
  args(C, Args).
args(Data, []) ->
  Data;
args(Data, [{Name, Value} | Args]) ->
  C = <<Data/binary, Name/binary, ": ", Value/binary, "\r\n">>,
  args(C, Args).

getKey([], _Key) -> empty;
getKey([First | Tail], Key) ->
  {Name, Value} = First,
  case Name == Key of
    true -> Value;
    false -> getKey(Tail, Key)
  end.
