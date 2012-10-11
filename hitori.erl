-module(hitori).
-export([
	generate/1,
	solve/1,
	blacken_coord/3,
	row_list/2,
	col_list/2,
	find_duplicates_in_list/1,
	find_offending_squares/1,
	from_list/1,
	render/1
]).

-compile(export_all).

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
	Solved = analyze_and_solve(Puz, []),
	[{orig, render(Puz)}, {solved, render(Solved)}].

render(undefined) -> no_solution;
render(Puz) ->
	lists:map(fun(Row) ->
		lists:map(fun
			({X, white}) -> X;
			({_, black}) -> x
		end,Row)
	end,Puz).

from_list(List) ->
	lists:map(fun(Row) ->
		[{X, white} || X <- Row]
	end,List).
		

%% replace the nth element of a list with a new element (Val)
set_nth(N, Val, List) ->
	{Head, [_ToDrop | Tail]} = lists:split(N-1, List),
	Head ++ [Val | Tail].

%% will properly throw a match error if we try to black out an already black square
tag_square({Val, white}, Tag) ->
	{Val, Tag}.

is_touched_or_black(Puz, Coord) ->
	{_, Color} = get_coord(Puz, Coord),
	Color == touched orelse Color == black.

blacken_coord(Puz, {X,Y}) ->
	blacken_coord(Puz, X, Y).

blacken_coord(Puz, X, Y) ->
	set_coord_tag(Puz, X, Y, black).

touch_coord(Puz, {X, Y}) ->
	set_coord_tag(Puz, X, Y, touched).

set_coord_tag(Puz, X, Y, Tag) ->

	Xrow = lists:nth(X, Puz),
	Square = lists:nth(Y, Xrow),
	BlackenedSquare = tag_square(Square,Tag),

	%% Set the Yth element of the row to our newly blackedned square
	NewXrow = set_nth(Y, BlackenedSquare, Xrow),

	%% Put the new Row back into the list.
	_NewPuz = set_nth(X, NewXrow, Puz).

%% Ineligible will be a coordinate tuple {x, y}, indicating that this route did not work
%% in this configuration and so should not be attempted
analyze_and_solve(Puz, Ineligible) ->
	case find_offending_squares(Puz) of
		[] -> Puz; %% No offending squares meens it's solved
		Offenders -> 
			io:format("Rows: ~s\r",[lists:duplicate(length(Puz)*length(Puz),32)]),
			io:format("Rows: ~s\r",[lists:duplicate(length(Offenders),$.)]),
			solve_worker(Puz, Offenders -- Ineligible, Ineligible)
	end.

solve_worker(_Puz, [], _) ->
	%% If this is called with no offenders, that means it was unable to find a path
	%% so let's return undefined to let the caller know that we failed
	undefined;
solve_worker(Puz, [FirstOffender | Offenders], Ineligible) ->
	case is_next_to_black(Puz, FirstOffender) of
		true -> 
			solve_worker(Puz, Offenders, [FirstOffender | Ineligible]);
		false -> 
			NewPuz = blacken_coord(Puz,FirstOffender),
			case are_all_white_cells_contiguous(NewPuz) of
				true -> 
					case analyze_and_solve(NewPuz, Ineligible) of
						undefined -> solve_worker(Puz, Offenders, [FirstOffender | Ineligible]);
						Final -> Final
					end;
				false -> 
					solve_worker(Puz, Offenders, [FirstOffender | Ineligible])
			end
	end.

are_all_white_cells_contiguous(Puz) ->
	Start = first_white_cell(Puz),
	Touched = spread_touch(Puz,Start),
	not(are_there_any_white_cells(Touched)).
	
are_there_any_white_cells(Puz) ->
	lists:any(fun(Row) ->
		lists:any(fun({_,Color}) -> Color==white end, Row)
	end,Puz).

spread_touch(Puz,{X,Y}) when X =< 0 orelse Y =< 0 orelse X > length(Puz) orelse Y > length(Puz) ->
	Puz;
spread_touch(Puz,{X,Y}) ->
	case is_touched_or_black(Puz, {X, Y}) of
		true -> Puz;
		false ->
			P1 = touch_coord(Puz, {X, Y}),
			P2 = spread_touch(P1, {X+1,Y}),
			P3 = spread_touch(P2, {X-1,Y}),
			P4 = spread_touch(P3, {X, Y+1}),
			_P5= spread_touch(P4, {X, Y-1})
	end.
	
first_white_cell(Puz) ->
	[{Pos,_}| _] = lists:dropwhile(fun({_,{_,Color}}) -> Color==black end,lists:zip(lists:seq(1,length(Puz)),hd(Puz))),
	{1,Pos}.

	

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


is_next_to_black(Puz, {X, Y}) ->
	is_black(Puz, X-1,Y) orelse 
	is_black(Puz, X+1,Y) orelse 
	is_black(Puz, X, Y-1) orelse 
	is_black(Puz, X, Y+1).

is_black(Puz, X, Y) when X =< 0 orelse Y =< 0 orelse X > length(Puz) orelse Y > length(Puz) ->
	false;
is_black(Puz, X, Y) ->
	{_, Color} = get_coord(Puz, X, Y),
	Color == black.


get_coord(Puz, {X, Y}) ->
	get_coord(Puz, X, Y).

get_coord(Puz, X, Y) ->
	Xrow = lists:nth(X, Puz),
	_Square = lists:nth(Y, Xrow).



row_list(Puz, N) ->
	lists:nth(N, Puz).

col_list(Puz, N) ->
	[lists:nth(N, Row) || Row <- Puz].



big() ->
	Orig = [	5
	,	5
	,	6
	,	2
	,	7
	,	7
	,	8
	,	8
	,	8
	,	5
	,	3
	,	1
	,	6
	,	2
	,	5
	,	4
	,	5
	,	1
	,	8
	,	2
	,	6
	,	3
	,	7
	,	2
	,	8
	,	6
	,	3
	,	7
	,	2
	,	7
	,	3
	,	5
	,	3
	,	5
	,	1
	,	3
	,	5
	,	7
	,	2
	,	5
	,	7
	,	4
	,	5
	,	8
	,	6
	,	6
	,	7
	,	1
	,	1
	,	1
	,	2
	,	3
	,	4
	,	8
	,	6
	,	7
	,	3
	,	3
	,	7
	,	4
	,	1
	,	7
	,	4
	,	4],
	from_list(squarify_list(Orig)).

squarify_list(List) ->
	Numrows = round(math:sqrt(length(List))),
	lists:map(fun(N) ->
		lists:sublist(List,N,Numrows)
	end,lists:seq(1,length(List),Numrows)).

big9() ->
G = [
	[7,5,2,8,4,2,5,6,8]
	,[5,7,7,6,3,6,2,1,4]
	,[9,4,1,7,6,7,8,8,5]
	,[6,1,3,5,8,4,7,2,7]
	,[4,8,1,9,2,6,5,8,2]
	,[2,8,6,7,7,9,1,4,6]
	,[3,2,4,7,7,5,5,9,6]
	,[4,9,5,2,1,3,6,7,8]
	,[6,6,1,4,2,7,3,1,9]
],
	from_list(G).

bigger() ->
	from_list(squarify_list(
		[7
	,		7
	,		9
	,		1
	,		8
	,		8
	,		4
	,		5
	,		7
	,		10
	,		1
	,		6
	,		4
	,		7
	,		7
	,		12
	,		6
	,		2
	,		9
	,		1
	,		11
	,		5
	,		3
	,		8
	,		11
	,		2
	,		8
	,		12
	,		4
	,		4
	,		1
	,		5
	,		12
	,		7
	,		9
	,		1
	,		11
	,		9
	,		7
	,		6
	,		8
	,		1
	,		2
	,		4
	,		10
	,		11
	,		11
	,		12
	,		9
	,		5
	,		10
	,		1
	,		3
	,		1
	,		8
	,		9
	,		2
	,		10
	,		7
	,		11
	,		7
	,		2
	,		6
	,		2
	,		3
	,		3
	,		8
	,		12
	,		9
	,		4
	,		8
	,		7
	,		10
	,		1
	,		8
	,		7
	,		12
	,		11
	,		5
	,		4
	,		8
	,		5
	,		6
	,		4
	,		9
	,		8
	,		11
	,		3
	,		10
	,		10
	,		10
	,		7
	,		4
	,		6
	,		2
	,		6
	,		5
	,		10
	,		3
	,		9
	,		7
	,		5
	,		11
	,		6
	,		10
	,		11
	,		4
	,		1
	,		12
	,		6
	,		9
	,		11
	,		1
	,		9
	,		4
	,		3
	,		7
	,		2
	,		8
	,		5
	,		2
	,		11
	,		4
	,		2
	,		5
	,		5
	,		3
	,		2
	,		1
	,		12
	,		10
	,		5
	,		10
	,		4
	,		12
	,		10
	,		11
	,		8
	,		6
	,		2
	,		5
	,		10
	,		1
	,		9])).
		

huge() -> 
	Huge = [[12,11,14,13,10,8,20,10,18,6,18,10,3,1,6,15,5,10,9,1]
	,[19,12,12,16,1,17,5,3,3,6,17,11,8,4,20,6,9,7,2,6]
	,[16,20,8,2,5,6,17,11,9,1,8,8,7,12,4,4,14,18,10,16]
	,[1,4,10,12,5,20,8,6,18,9,19,2,8,15,17,1,18,11,7,5]
	,[8,9,2,19,6,7,2,5,17,12,10,16,19,14,16,13,20,1,15,12]
	,[5,5,4,16,7,14,1,6,7,1,4,8,2,6,15,12,8,16,11,18]
	,[14,14,1,11,2,17,9,16,13,8,8,10,1,19,17,7,18,18,20,6]
	,[17,15,4,8,9,9,4,12,20,17,13,14,1,13,18,3,16,2,3,19]
	,[3,16,17,16,12,4,14,3,4,15,6,17,9,20,2,3,16,13,14,8]
	,[5,7,16,15,10,3,6,1,3,2,11,4,13,9,10,8,11,16,13,20]
	,[9,13,10,17,9,11,14,18,6,2,1,19,8,5,5,2,7,12,12,10]
	,[13,18,20,5,3,10,12,1,17,3,19,16,15,2,9,13,7,4,1,6]
	,[18,6,8,1,15,2,13,9,4,16,3,6,17,5,16,14,8,20,17,2]
	,[4,14,9,18,18,1,9,1,12,3,7,16,5,13,10,17,4,15,19,6]
	,[10,12,4,12,18,19,15,13,8,14,12,7,20,13,11,19,6,3,12,1]
	,[6,16,5,3,16,18,10,17,8,7,15,12,10,11,1,2,13,3,8,9]
	,[2,10,18,5,8,15,2,4,1,4,11,8,16,3,13,9,9,14,6,6]
	,[19,17,11,19,20,13,16,7,3,10,14,15,6,5,8,4,2,1,12,4]
	,[4,6,15,7,2,2,6,20,10,17,2,1,13,8,12,16,15,5,2,11]
	,[2,1,13,6,4,10,19,14,7,18,20,13,17,5,3,19,12,8,5,15]
	],
	from_list(Huge).
