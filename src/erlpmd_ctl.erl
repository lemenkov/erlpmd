%% Copyright (c) 2012 Peter Lemenkov.
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

-module(erlpmd_ctl).

-export([start/0]).

start() ->
	% Check environment variables first
	EnvAddr = case os:getenv("ERL_EPMD_ADDRESS") of
		false -> [{0,0,0,0}];
		AS -> addrstr_to_ip(AS)
	end,

	EnvPort = case os:getenv("ERL_EPMD_PORT") of
		false -> 4369;
		PS -> list_to_integer(PS)
	end,

	EnvRelaxedCommandCheck = case os:getenv("ERL_EPMD_RELAXED_COMMAND_CHECK") of
		false -> false;
		_ -> true
	end,

	% Now check for command-line switches
	Addr = case init:get_argument(address)of
		error -> EnvAddr;
		{ok,[[AddrStr]]} -> addrstr_to_ip(AddrStr)
	end,

	Port = case init:get_argument(port) of
		error -> EnvPort;
		{ok,[[PortStr]]} -> list_to_integer(PortStr)
	end,

	% Set debug level - ignored for now
	% init:get_argument(d),
	% init:get_argument(debug),

	% Run daemonised - ignored for now
	% init:get_argument(daemon),

	% Allow this instance of epmd to be killed with
	% epmd -kill even if there are registered nodes.
	% Also allows forced unregister (epmd -stop)
	RelaxedCommandCheck = case init:get_argument(relaxed_command_check) of
		error -> EnvRelaxedCommandCheck;
		_ -> true
	end,

	% FIXME
	% init:get_argument(packet_timeout),
	% init:get_argument(delay_accept),
	% init:get_argument(delay_write),

	application:set_env(erlpmd, address, Addr),
	application:set_env(erlpmd, port, Port),
	application:set_env(erlpmd, relaxed_command_check, RelaxedCommandCheck),

	application:start(erlpmd).


%%
%% Private functions
%%

addrstr_to_ip(AddrStr) ->
	[begin {ok, Y} = inet_parse:address(X), Y end || X <-string:tokens(AddrStr,",")].
