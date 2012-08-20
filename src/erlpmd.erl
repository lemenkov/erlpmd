%% Copyright (c) 2011 Peter Lemenkov.
%%
%% The MIT License
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%

-module(erlpmd).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	?MODULE = ets:new(?MODULE, [public, named_table]),
	error_logger:warning_msg("ErlPMD: started.~n"),
	{ok, Args}.

handle_call(Request, From, State) ->
	error_logger:warning_msg("ErlPMD: strange call: ~p from: ~p.~n", [Request, From]),
	{reply, ok, State}.

handle_cast({msg,<<$x, PortNo:16, NodeType:8, Proto:8, HiVer:16, LoVer:16, NLen:16, Rest/binary>>, Fd, Ip, Port}, State) ->
	error_logger:warning_msg("ErlPMD: alive request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
	<<NodeName:NLen/binary, ELen:16, Extra/binary>> = Rest,
	Creation = random:uniform(3),
	error_logger:warning_msg("DUMP: ~p~n", [[PortNo, NodeType, Proto, HiVer, LoVer, NodeName, Extra, Creation]]),
	ets:lookup(?MODULE, NodeName) == [] orelse ets:delete(erlpmd, NodeName),
	ets:insert_new(?MODULE, {NodeName, {PortNo, NodeType, Proto, HiVer, LoVer, Extra, Creation, Fd}}),
	gen_server:cast(listener, {msg, <<$y, 0:8, Creation:16>>, Ip, Port}),
	{noreply, State};

handle_cast({msg,<<$z, NodeName/binary>>, Fd, Ip, Port}, State) ->
	error_logger:warning_msg("ErlPMD: port request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
	case ets:lookup(?MODULE, NodeName) of
		[] ->
			gen_server:cast(listener, {msg, <<$w, 1:8>>, Ip, Port});
		[{NodeName, {PortNo, NodeType, Proto, HiVer, LoVer, Extra, _, _}}] ->
			NLen = size(NodeName),
			ELen = size(Extra),
			gen_server:cast(listener, {msg, <<$w, 0:8, PortNo:16, NodeType:8, Proto:8, HiVer:16, LoVer:16, NLen:16, NodeName:NLen/binary, ELen:16, Extra:ELen/binary>>, Ip, Port})
	end,
	gen_server:cast(listener, {close, Ip, Port}),
	{noreply, State};

handle_cast({msg,<<$n>>, Fd, Ip, Port}, State) ->
	error_logger:warning_msg("ErlPMD: name request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
	Nodes = list_to_binary(lists:flatten([ io_lib:format("name ~s at port ~p~n", [X, Y]) || [X, Y] <- ets:match(erlpmd, {'$1', {'$2', 77, '_', '_', '_', '_', '_', '_'}})])),
	gen_server:cast(listener, {msg, <<4369:32, Nodes/binary>>, Ip, Port}),
	gen_server:cast(listener, {close, Ip, Port}),
	{noreply, State};

handle_cast({msg,<<$d>>, Fd, Ip, Port}, State) ->
	error_logger:warning_msg("ErlPMD: dump request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
	Nodes = list_to_binary(lists:flatten([ io_lib:format("active name     ~s at port ~p, fd = ~p ~n", [X, Y, F]) || [X, Y, F] <- ets:match(erlpmd, {'$1', {'$2', 77, '_', '_', '_', '_', '_', '$3'}})])),
	gen_server:cast(listener, {msg, <<4369:32, Nodes/binary>>, Ip, Port}),
	gen_server:cast(listener, {close, Ip, Port}),
	{noreply, State};

handle_cast({msg,<<$k>>, Fd, Ip, Port}, State) ->
	error_logger:warning_msg("ErlPMD: kill request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
	gen_server:cast(listener, {msg, <<"OK">>, Ip, Port}),
	gen_server:cast(listener, stop),
	{stop, normal, State};

handle_cast({msg,<<$s, _NodeName/binary>>, Fd, Ip, Port}, State) ->
	error_logger:warning_msg("ErlPMD: stop request from ~s:~p (IGNORED for now).~n", [inet_parse:ntoa(Ip), Port]),
%	gen_server:cast(listener, {msg, <<"NOEXIST">>, Ip, Port}),
	gen_server:cast(listener, {msg, <<"STOPPED">>, Ip, Port}),
	gen_server:cast(listener, {close, Ip, Port}),
	{noreply, State};

handle_cast({close, Fd}, State) ->
	error_logger:warning_msg("ErlPMD: closed connection: ~p.~n", [Fd]),
	case ets:match(erlpmd, {'$1', {'_', '_', '_', '_', '_', '_', '_', Fd}}) of
		[[NodeName]] -> ets:delete(erlpmd, NodeName);
		_ -> ok
	end,
	{noreply, State};

handle_cast(Msg, State) ->
	error_logger:warning_msg("ErlPMD: strange cast: ~p.~n", [Msg]),
	{noreply, State}.

handle_info(Info, State) ->
	error_logger:warning_msg("ErlPMD: strange info: ~p.~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	error_logger:warning_msg("ErlPMD: stopped.~n"),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

