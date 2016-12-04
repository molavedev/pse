-module(stocks).

-export([momentum/0]).

momentum() ->
	%% it's required to call inets:start() before we can make http requests
	inets:start(),
	file_utils:start(getCacheDir(), []),
 	io:format("downloading phisix data from 'http://phisix-api.appspot.com/stocks.json' ~n"),
 	Stocks = download(),
 	io:format("filtering stocks with 1M daily value. ~n"),
    FilteredStocks = filterStocks(Stocks),
    io:format("Filtered Stocks: ~p~n",[FilteredStocks]),
    getBloombergData(FilteredStocks),
 	io:format("for each stock, get quotes and 1-year returns from bloomberg~n"),
 	io:format("calculate 1-year return less last-month's returns~n"),
 	io:format("sort stocks by 1-year return less last-months return~n"),
 	io:format("display / save results~n").

getBloombergData(Stocks) ->
	io:format("getting bloomberg...."). 	

greaterThanLimit(Stock) ->
	[_Name, Price, _Change, {_, Volume}, _Symbol] = Stock,
	{_, [_currency, {_, Amount}]} = Price,
	greaterThanLimit(Amount*Volume, 300000000).

greaterThanLimit(X,Limit) when X >= Limit -> true;
greaterThanLimit(_, _) -> false.
	

filterStocks(Stocks) ->
	[Stock || Stock <- Stocks, greaterThanLimit(Stock)].

%% dir MUST HAVE a slash "/" at the end for filelib:ensure_dir to work
getCacheDir() ->
	{{Year, Month, Day}, _Time} = erlang:localtime(),
	io:format("year: ~p; month: ~p; day: ~p~n", [Year, Month, Day]),
	"pse_" ++ integer_to_list(Year) ++ "_" ++ 
	     integer_to_list(Month) ++ "_" ++ 
	     integer_to_list(Day) ++ "/".

readStocksFromFile(CacheFile) ->
	file_utils:read(CacheFile).

downloadStocks(CacheFile) ->
 	Url = "http://phisix-api.appspot.com/stocks.json",
 	Response = httpc:request(get, {Url, []}, [], []),
	Body = response_body(Response),
	Stocks = stocksFromHtmlBody(Body),
	file_utils:save_terms({"stocks.txt", Stocks}),
	Stocks.	

stocksFromHtmlBody(Body) ->
	[{_Stock, Stocks}, _AsOf] = jsx:decode(list_to_binary(Body)),
	Stocks.	

download() ->
	io:format("downloading...~n"),
	CacheFile = "stocks.txt",
	case file_utils:file_exists(CacheFile) of
		true ->
			io:format("cache file exists~n"), 
			readStocksFromFile(CacheFile);
		false -> 
			io:format("cache file DOES NOT exists~n"),
			downloadStocks(CacheFile)
	end.

response_body({ok, { _, _, Body}}) -> Body.