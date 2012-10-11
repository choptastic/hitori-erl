-module(hitori).
-export([
	generate/1,
	solve/1,
	blacken_coord/3,
	row_list/2,
	col_list/2,
	find_duplicates_in_list/1,
	find_offending_squares/1
]).

%% Will be a list of lists (matrix) of tuples of the form {Number, black|white}
generate(Size) ->
	random:seed(now()),
	Range = lists:seq(1,Size),
	lists:map(fun(_) ->
		lists:map(fun(_) ->
			generate_square(Size)
		end,Range)
	end,Range).


generate_square(Max) ->
	{random:uniform(Max),white}.


solve(Puz) ->
	solve_worker(Puz, []).

%% replace the nth element of a list with a new element (Val)
set_nth(N, Val, List) ->
	{Head, [_ToDrop | Tail]} = lists:split(N-1, List),
	Head ++ [Val | Tail].

%% will properly throw a match error if we try to black out an already black square
blacken_square({Val, white}) ->
	{Val, black}.


blacken_coord(Puz, X, Y) ->
	Xrow = lists:nth(X, Puz),
	Square = lists:nth(Y, Xrow),
	BlackenedSquare = blacken_square(Square),

	%% Set the Yth element of the row to our newly blackedned square
	NewXrow = set_nth(Y, BlackenedSquare, Xrow),

	%% Put the new Row back into the list.
	_NewPuz = set_nth(X, NewXrow, Puz).

%% Ineligible will be a coordinate tuple {x, y}, indicating that this route did not work
%% in this configuration and so should not be attempted
solve_worker(Puz, Ineligible) ->
	case find_offending_squares(Puz) of
		[] -> Puz; %% No offending squares meens it's solved
		Offenders -> solve_workerblacken_and_continue(Offenders -- Ineligible)
	end.

solve_worker(Puz, Offenders, Ineligible) ->

%% Returns a list of squares which are part of duplicated rows or columns.
%% Is a list of coordinates {X, Y}
find_offending_squares(Puz) -> 
	OffendingRowCoords = find_offending_rows(Puz),
	OffendingColCoords = find_offending_cols(Puz),
	prioritize_offenders(lists:sort(OffendingRowCoords ++ OffendingColCoords)).

prioritize_offenders(List) ->
	prioritize_offenders(List, [], []).

prioritize_offenders([], HighPri, LowPri) ->
	HighPri	++ LowPri;
prioritize_offenders([H,H|List], HighPri, LowPri) ->
	%% if two of the same in a row, we have a hig priority coord
	%% (means it violates both verticslly and horizontally)
	%% this only works because there will only ever be at most two duplicates in a row
	prioritize_offenders(List, [H|HighPri], LowPri);
prioritize_offenders([H|List], HighPri,LowPri) ->
	%% it' not hig pri, so stic it in low pri list
	prioritize_offenders(List, HighPri, [H|LowPri]).


	
find_offending_rows(Puz) ->
	lists:foldl(fun({X, Row}, Acc) ->
		OffendingCoords = [{X, Y} || Y <- find_duplicates_in_list(Row)],
		OffendingCoords ++ Acc
		end,[],lists:zip(lists:seq(1,length(Puz)),Puz)).

find_offending_cols(Puz) ->
	%% Pivot the puzzle so we can work on the list sli this
	Cols = [col_list(Puz, N) || N <- lists:seq(1,length(Puz))],
	
	%% The following is *deceivingly* close to the find_offending_rows fun
	%% but notice the orientations of X and Y (that's what's different, since w're on cols
	lists:foldl(fun({Y, Col}, Acc) ->
		OffendingCoords = [{X, Y} || X <- find_duplicates_in_list(Col)],
		OffendingCoords ++ Acc
	end,[],lists:zip(lists:seq(1,length(Puz)),Cols)).

find_duplicates_in_list(List) ->
	lists:foldl(fun
		({_,{_,black}}, Acc) -> 
			%% It's black, it can't be bad, so we don't add anything to the accumulator
			Acc;
		({Position,{Val, white}},Acc) ->
			%% It's white, so let's found how many other white copies in this list
			case count_white_copies(List, Val) of
				1 ->  
					%% One copy in the list means just us, no need to add
					Acc;
				_ -> 
					%% More than one copy in the list, so let's store this position and move on
					[Position | Acc]
			end
	end,[],lists:zip(lists:seq(1,length(List)),List)).

count_white_copies(List, Val) ->
	length([N || {N, white} <- List, N==Val]).


is_next_to_black(Puz, X, Y) ->
	is_black(Puz, X-1,Y) orelse 
	is_black(Puz, X+1,Y) orelse 
	is_black(Puz, X, Y-1) orelse 
	is_black(Puz, X, Y+1).

is_black(Puz, X, Y) when X =< 0 orelse Y =< 0 orelse X > length(Puz) orelse Y > length(Puz) ->
	false;
is_black(Puz, X, Y) ->
	{_, Color} = get_coord(Puz, X, Y),
	Color == black.
	
get_coord(Puz, X, Y) ->
	Xrow = lists:nth(X, Puz),
	_Square = lists:nth(Y, Xrow).

row_list(Puz, N) ->
	lists:nth(N, Puz).

col_list(Puz, N) ->
	[lists:nth(N, Row) || Row <- Puz].
