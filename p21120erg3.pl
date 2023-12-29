:- discontiguous state/1.
:- discontiguous final_state/1.
:- discontiguous automata_numbers/2.
:- discontiguous automata_text/2.
:- discontiguous automata/2.
:- discontiguous automata_max_moves/3.


% number states %
state(s0).
state(s1).
state(s2).
state(s3).
final_state(sf).

% text states (some redundant)%
state(s1).
state(s2).
final_state(s3).
state(s4).

% numbers transitions%
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


% helper checking for no integer transition
check_number(Transition):-
	number(Transition).
% check if all transitions are numbers
check_numbers(Transitions):-
	maplist(number,Transitions).

% helper to check if transition not number 
check_not_number(Transition) :-
    \+ number(Transition).

% check if all transitions are not numbers
check_not_numbers(Transitions) :-
    maplist(check_not_number, Transitions).

% check for epsilon transitions and get the next state
check_for_epsilon(State,NextState):-
	trans(State,null,NextState).

decr(Num,DecrNum) :- DecrNum is Num-1.

% automata/2 base cases -> we are at sf and list is empty
automata(sf,[]).
automata(s3,[]).
automata_numbers(sf,[]).
automata_text(s3,[]).
automata_max_moves(s3,[],_).


% automata/2 body for numbers
automata(State, [Transition|TransitionList]) :-
    check_number(Transition),
    check_numbers(TransitionList),
    automata_numbers(State,[Transition|TransitionList]).

% automata/2 body for not numbers
automata(State, [Transition|TransitionList]) :-
    check_not_number(Transition),
    check_not_numbers(TransitionList),
    automata_text(State,[Transition|TransitionList]).
	

% automata numbers case %
automata_numbers(State,[Transition|TransitionList]):-
	trans(State,Transition,NextState),
	automata_numbers(NextState,TransitionList).


% automata text case 
automata_text(State,[Transition|TransitionList]):-
	trans(State,Transition,NextState),
	automata_text(NextState,TransitionList).

% check for epsilon - null transitions - only for text
automata_text(State,[Transition|TransitionList]):-
	check_for_epsilon(State,EpsilonNext),
	trans(EpsilonNext,Transition,NextState),
	automata_text(NextState,TransitionList).


% check for circles with automata/3 - implemented for text only since numbers dont have circles
automata(State,[Transition|TransitionList],Max_Moves):-
	nonvar(Transition),
	nonvar(TransitionList),
	nonvar(Max_Moves),
	number(Max_Moves),
	automata_max_moves(State,[Transition|TransitionList],Max_Moves).


% automata case for circles - no epsilon
automata_max_moves(State,[Transition|TransitionList],Moves):-
	trans(State,Transition,NextState),
	decr(Moves,MovesLeft),
	MovesLeft>=0,
	automata_max_moves(NextState,TransitionList,MovesLeft).
	
% automata case for circles - with epsilon transitions
automata_max_moves(State,[Transition|TransitionList],Moves):-
	check_for_epsilon(State,EpsilonNext),
	trans(EpsilonNext,Transition,NextState),
	decr(Moves,MovesLeft),
	MovesLeft>=0,
	automata_max_moves(NextState,TransitionList,MovesLeft).





