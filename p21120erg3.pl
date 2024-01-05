:- discontiguous state/1.
:- discontiguous final_state/1.
:- discontiguous automata_numbers/2.
:- discontiguous automata_text/2.
:- discontiguous automata/2.
:- discontiguous automata_max_moves/4.


% number states 
state(s0).
state(s1).
state(s2).
state(s3).
final_state(sf).

% text states (some redundant) 
state(s1).
state(s2).
final_state(s3).
state(s4).

% numbers transitions 
trans(s0,5,s1).
trans(s0,10,s2).
trans(s0,20,sf).
trans(s1,5,s2).
trans(s1,10,s3).
trans(s2,5,s3).
trans(s2,10,sf).
trans(s3,5,sf).

% text transitions %
trans(s1,a,s1).
trans(s1,b,s1).
trans(s1,a,s2).
trans(s2,null,s4).
trans(s2,b,s3).
trans(s3,null,s1).
trans(s3,b,s4).


% check if X is number
check_number(X):-
	number(X).
% check for number to all elements on list
check_numbers(X):-
	maplist(number,X).

% check if X is not a number
check_not_number(X) :-
    \+ number(X).

% check if all elements on list are not numbers
check_not_numbers(X) :-
    maplist(check_not_number, X).

% check for epsilon transitions and get the next state
check_for_epsilon(State,NextState):-
	trans(State,null,NextState).

% decrement number by 1
decr(Num,DecrNum) :- DecrNum is Num-1.

% base cases for each category of automata -> final state and empty list
automata(sf,[]).
automata(s3,[]).
automata_numbers(sf,[]).
automata_text(s3,[]).
automata_max_moves(s3,[],_,_).


% automata/2 body for numbers
automata(State, [Transition|TransitionList]) :-
	nonvar(Transition),
	nonvar(TransitionList),
    check_number(Transition),
    check_numbers(TransitionList),
    automata_numbers(State,[Transition|TransitionList]).

% automata/2 body for not numbers
automata(State, [Transition|TransitionList]) :-
	nonvar(Transition),
	nonvar(TransitionList),
    check_not_number(Transition),
    check_not_numbers(TransitionList),
    automata_text(State,[Transition|TransitionList]).
	

% automata_numbers/2 numbers case 
automata_numbers(State,[Transition|TransitionList]):-
	trans(State,Transition,NextState),
	automata_numbers(NextState,TransitionList).


% automata_text/2 text case 
automata_text(State,[Transition|TransitionList]):-
	trans(State,Transition,NextState),
	automata_text(NextState,TransitionList).

% automata_text/2 text case check for epsilon transitions
automata_text(State,[Transition|TransitionList]):-
	check_for_epsilon(State,EpsilonNext),
	nonvar(EpsilonNext),
	automata_text(EpsilonNext,[Transition|TransitionList]).


% automata/3 check for circles on text automata 
automata(State,[Transition|TransitionList],Max_Moves):-
	nonvar(Transition),
	nonvar(TransitionList),
	nonvar(Max_Moves),
	number(Max_Moves),
	automata_max_moves(State,[Transition|TransitionList],Max_Moves,Max_Moves).

% automata_max_moves/3 case for circles, decrement moves by 1 
automata_max_moves(State,[Transition|TransitionList],Max_Moves,Moves):-
	trans(State,Transition,State),
	decr(Moves,MovesLeft),
	MovesLeft>=0,
	automata_max_moves(State,TransitionList,Max_Moves,MovesLeft).

 % automata_max_moves/3 case for non-circles, reset moves to max no circle found 
automata_max_moves(State,[Transition|TransitionList],Max_Moves,_):-
	trans(State,Transition,NextState),
	State \= NextState,
	automata_max_moves(NextState,TransitionList,Max_Moves,Max_Moves). 
	
% automata_max_moves/3 case for epsilon, check next state if found
automata_max_moves(State,[Transition|TransitionList],Max_Moves,Moves):-
	check_for_epsilon(State,EpsilonNext),
	nonvar(EpsilonNext),
	automata_max_moves(EpsilonNext,[Transition|TransitionList],Max_Moves,Moves).




