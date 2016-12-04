-module(file_utils).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/2, save_terms/1, file_exists/1, read/1]).

start(Dir, _Opt) ->
  %% start the named gen server
  gen_server:start({local, ?MODULE}, ?MODULE, Dir, []).

save_terms({File, List}) ->
  gen_server:call(?MODULE, {save_terms, File, List}).  

file_exists(File) ->
  gen_server:call(?MODULE, {file_exists, File}).  

read(File) ->
	gen_server:call(?MODULE, {read, File}).  	  

terminate(shutdown, _State) ->
    ok.

%% this will be called by gen_server:start - just pass the directory
init(Dir) ->
  Result = filelib:ensure_dir(Dir), 
  {ok, Dir}.

writeListToFile(Dir, File, L) ->
	FilePath = Dir ++ File,
	{ok, S} = file:open(FilePath, write),
	lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, L), 
	file:close(S).

handle_call({file_exists, File}, _From, Dir) ->
	FilePath = Dir ++ File, 
	Reply = filelib:is_regular(FilePath),
	{reply, Reply, Dir};

handle_call({read, File}, _From, Dir) ->
	FilePath = Dir ++ File, 
	{ok, Stocks} = file:consult(FilePath),
	{reply, Stocks, Dir};

handle_call({save_terms, File, List}, _From, Dir) ->
  writeListToFile(Dir, File, List),
  {reply, ok, Dir}.

%% you can ignore the rest - they are needed to be present
handle_cast(_Msg, State) ->
  {noreply, State}.
handle_info(_Info, State) ->
  {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.