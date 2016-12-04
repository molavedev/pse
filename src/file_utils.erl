-module(file_utils).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/2, save_terms/1, file_exists/1, read/1, dump/2]).
-export([get_one_year_return/1]).

start(Dir, _Opt) ->
  %% start the named gen server
  gen_server:start({local, ?MODULE}, ?MODULE, Dir, []).

get_one_year_return(File) ->
	Dir = get_dir(),
	FilePath = Dir ++ File,
	{ok, S} = file:open(FilePath, read),
	OneYear = get_one_year_return(loop, S),
	file:close(S),
	OneYear.	

get_one_year_return_from_line(Line, Index) ->
	Substr = string:sub_string(Line, Index, Index + 100),
	Index2 = string:rstr(Substr, "%"),
	Return = string:strip(string:sub_string(Substr, Index2-5, Index2-1)),
	io:format("~p~n", [Return]),
	{Float, _} = string:to_float(Return),
	Float.

get_one_year_return(loop, S) ->
	OneYearReturnStr = "1 Yr Return",
	Line = io:get_line(S, ''),
	case Line of
		eof -> no_return;
	 	_ -> 
	 		Index = string:rstr(Line, OneYearReturnStr),
	 		if 
	 			Index > 0 -> 
	 				get_one_year_return_from_line(Line, Index);
	 			Index == 0 -> get_one_year_return(loop, S) 
	 		end 
	end.		

dump(File, Data) ->
 	Dir = get_dir(),
	FilePath = Dir ++ File,
	file:write_file(FilePath, Data).

save_terms({File, List}) ->
  	Dir = get_dir(),
  	writeListToFile(Dir, File, List).  

writeListToFile(Dir, File, L) ->
	FilePath = Dir ++ File,
	{ok, S} = file:open(FilePath, write),
	lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, L), 
	file:close(S).  

file_exists(File) ->
  Dir = get_dir(),	
  FilePath = Dir ++ File, 
  filelib:is_regular(FilePath).  

read(File) ->
 	Dir = get_dir(),
    FilePath = Dir ++ File, 
	{ok, Stocks} = file:consult(FilePath),
	Stocks.  	  

get_dir() ->
	gen_server:call(?MODULE, {get_dir}).		

terminate(shutdown, _State) ->
    ok.

%% this will be called by gen_server:start - just pass the directory
init(Dir) ->
  Result = filelib:ensure_dir(Dir), 
  {ok, Dir}.

handle_call({get_dir}, _From, Dir) ->
	{reply, Dir, Dir}.

%% you can ignore the rest - they are needed to be present
handle_cast(_Msg, State) ->
  {noreply, State}.
handle_info(_Info, State) ->
  {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.