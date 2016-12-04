-module(stocks).

-export([momentum/0]).

momentum() ->
	%% it's required to call inets:start() before we can make http requests
	inets:start(),
	ssl:start(),
	file_utils:start(get_cache_dir(), []),
 	io:format("downloading phisix data from 'http://phisix-api.appspot.com/stocks.json' ~n"),
 	Stocks = download(),
 	io:format("filtering stocks with 1M daily value. ~n"),
    FilteredStocks = filter_stocks(Stocks),
    BloombergStocks = get_bloomberg_data(FilteredStocks),
    io:format("bloomberg Stocks: ~p~n",[BloombergStocks]),
 	io:format("for each stock, get quotes and 1-year returns from bloomberg~n"),
 	io:format("calculate 1-year return less last-month's returns~n"),
 	io:format("sort stocks by 1-year return less last-months return~n"),
 	io:format("display / save results~n").

get_bloomberg_stock(Stock) ->
	[{_, Name}, {_, [_, {_, Price}]}, _, {_, Volume}, {_, Symbol}] = Stock,
	Url = "http://www.bloomberg.com/quote/" ++ binary_to_list(Symbol) ++ ":PM",
	io:format("Url: ~p~n", [Url]),
	CacheFile = binary_to_list(Symbol) ++ ".txt",
	case file_utils:file_exists(CacheFile) of
		false -> 
			io:format("~p DOES NOT exist~n", [CacheFile]),
			Response = httpc:request(get, {Url, []}, [], []),
		 	Body = response_body(Response),
		 	file_utils:dump(CacheFile, Body),
		 	timer:sleep(10000);
		true ->
			io:format("~p exists~n", [CacheFile]),
			ok 	
	end,
	OneYearReturn = file_utils:get_one_year_return(CacheFile),
	io:format("one year return: ~p~n", [OneYearReturn]),
	{Name, Symbol, Price, Volume, OneYearReturn}.

get_bloomberg_data(Stocks) ->
	io:format("getting bloomberg....~n"),
	lists:map(fun(Stock) -> get_bloomberg_stock(Stock) end, Stocks).

greater_than_limit(Stock) ->
	[_Name, Price, _Change, {_, Volume}, _Symbol] = Stock,
	{_, [_currency, {_, Amount}]} = Price,
	greater_than_limit(Amount*Volume, 1000000).

greater_than_limit(X,Limit) when X >= Limit -> true;
greater_than_limit(_, _) -> false.
	

filter_stocks(Stocks) ->
	[Stock || Stock <- Stocks, greater_than_limit(Stock)].

%% dir MUST HAVE a slash "/" at the end for filelib:ensure_dir to work
get_cache_dir() ->
	{{Year, Month, Day}, _Time} = erlang:localtime(),
	io:format("year: ~p; month: ~p; day: ~p~n", [Year, Month, Day]),
	"pse_" ++ integer_to_list(Year) ++ "_" ++ 
	     integer_to_list(Month) ++ "_" ++ 
	     integer_to_list(Day) ++ "/".

read_stocks_from_file(CacheFile) ->
	file_utils:read(CacheFile).

download_stocks(CacheFile) ->
 	Url = "http://phisix-api.appspot.com/stocks.json",
 	Response = httpc:request(get, {Url, []}, [], []),
	Body = response_body(Response),
	Stocks = stocks_from_html_body(Body),
	file_utils:save_terms({"stocks.txt", Stocks}),
	Stocks.	

stocks_from_html_body(Body) ->
	[{_Stock, Stocks}, _AsOf] = jsx:decode(list_to_binary(Body)),
	Stocks.	

download() ->
	io:format("downloading...~n"),
	CacheFile = "stocks.txt",
	case file_utils:file_exists(CacheFile) of
		true ->
			io:format("cache file exists~n"), 
			read_stocks_from_file(CacheFile);
		false -> 
			io:format("cache file DOES NOT exists~n"),
			download_stocks(CacheFile)
	end.

response_body({ok, { _, _, Body}}) -> Body.