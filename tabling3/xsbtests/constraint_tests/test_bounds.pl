
/*
Need example of lex_chain/1.
*/

:- op(760,yfx,(#<=>)).
:- op(750,xfy,(#=>)).
:- op(750,yfx,(#<=)).
:- op(740,yfx,(#\/)).
:- op(730,yfx,(#\)).
:- op(720,yfx,(#/\)).
:- op(710, fy,(#\)).
:- op(700,xfx,(#>)).
:- op(700,xfx,(#<)).
:- op(700,xfx,(#>=)).
:- op(700,xfx,(#=<)).
:- op(700,xfx,(#=)).
:- op(700,xfx,(#\=)).
:- op(700,xfx,(in)).
:- op(550,xfx,(..)).

?- [bounds].

:- import in/2, '#>'/2, tuples_in/2, all_different/1, label/1, '#='/2,
    '#>='/2, '#\='/2, '#<'/2, '#=<'/2, serialized/2, sum/3
    ,indomain/1,labeling/2,'#=>'/2
 from bounds.

test:- test0.
test:- test1.
test:- test2.
test:- test3.
test:- test4.
test:- test5.
test:- test6.
test:- test7.
test:- test8.
test:- test9.
test:- test10.
test:- test11.
test:- test12.
test:- test13.
test:- test14.

test0:- send(Money),writeln(money(Money)),fail.
test1:- path(A,D,Ps),writeln(path(A,D,Ps)),fail.
test2:- X in 1..2,X #\= 2, writeln(test2(X)),fail.
test3:- X in 1..2,X #> 1,writeln(test3(X)),fail.
test4:- X in 1..2,X #< 2,writeln(test4(X)),fail.
test5:- X in 1..3,X #>= 2, X #=< 2,writeln(test5(X)),fail.
test6:- X in 1..3,Y in 1..3,Z in 1..3,all_different([X,Y,Z]),X = 1, Y = 2,
	writeln(test6(X,Y,Z)),fail.
test7:- X in 1..3,Y in 1..3,Z in 1..3,all_different([X,Y,Z]),X = 1, Y = 1,
	writeln(error_test7),fail.
test8:- X in 1..3,Y in 1..3,Z in 1..3,
	sum([X,Y,Z],#=,9),writeln(test8(X,Y,Z)),fail.
test9:- findall(X,(X in 1..3,indomain(X)),Xs),writeln(test9(Xs)),fail.
test10:- findall(p(X,Y),(X in 1..5,Y in 1..5,X #< Y,label([X,Y])),Choices),
	writeln(test10(Choices)),fail.
test11:- findall(p(X,Y),(X in 1..5,Y in 1..5,X #< Y,
	 labeling([max(X)],[X,Y])),Choices),writeln(test11(Choices)),fail.
test12:- findall(p(X,Y),(X in 1..5,Y in 1..5,X #< Y,
	 labeling([max(X)],[X,Y])),Choices),writeln(test12(Choices)),fail.
test13:- X in 1..10, Y in 1..10, serialized([X,Y],[8,1]),label([X,Y]),
	 writeln(test13(X,Y)),fail.
test14:- X in 1..10, Y in 1..10,Z in 1..10, serialized([X,Y,Z],[8,1,1]), 
	X #\= Y #=> Z, writeln(test14(Z)).

%X in 1..10,Y in 1..10,X + 4 #< Y -4.

send([[S,E,N,D], [M,O,R,E], [M,O,N,E,Y]]) :-
    Digits = [S,E,N,D,M,O,R,Y],
    Carries = [C1,C2,C3,C4],
    Digits in 0..9,
    Carries in 0..1,
    M #= C4,
    O + 10 * C4 #= M + S + C3,
    N + 10 * C3 #= O + E + C2,
    E + 10 * C2 #= R + N + C1,
    Y + 10 * C1 #= E + D,
    M #>= 1,
    S #>= 1,
    all_different(Digits),
    label(Digits).


schedule(Ts) :-
    Ts = [[1,2,0,1],[2,3,4,5],[2,3,0,1],[3,4,5,6],[3,4,2,3],[3,4,8,9]].

path(A, D, Ps) :-
    schedule(Ts),
    Ps = [[A,B,_T0,T1],[B,C,T2,T3],[C,D,T4,_T5]],
    tuples_in(Ps, Ts),
    T2 #> T1,
    T4 #> T3.

table_test(X):-
	X in 2..3,p(X).

:- table p/1.
p(X):- X in 1..2.

end_of_file.

%-------------------
sudoku(Init,S):- 
	create_domains(S),
	initialize(Init,S),
	add_column_constraints(S),
	do_label(S).

create_domains(S):-
	create_row(1,9,S).

create_row(N,9,[]):-  N > 9.
create_row(N,9,S):- 
	S = [s(V_1,N,1),s(V_2,N,2),s(V_3,N,3),s(V_4,N,4),s(V_5,N,5),
	     s(V_6,N,6),s(V_7,N,7),s(V_8,N,8),s(V_9,N,9)|Rest],
	V_1 in 1..9,V_2 in 1..9,V_3 in 1..9,V_4 in 1..9,V_5 in 1..9,
	V_6 in 1..9,V_7 in 1..9,V_8 in 1..9,V_9 in 1..9,
	all_different([V_1,V_2,V_3,V_4,V_5,V_6,V_7,V_8,V_9]),
	Next is N + 1,
	create_row(Next,9,Rest).

initialize([],_Square).
initialize([H|T],Square):- 
	initialize_1(Square,H),
	initialize(T,Square).

initialize_1([],_).
initialize_1([s(X,Row,Col)|_],s(N,Row,Col)):- !,X = N.
initialize_1([_|Rest],s(N,Row,Col)):- initialize_1(Rest,s(N,Row,Col)).

add_column_constraints(S):-
        S = [s(V_11,1,1),s(V_12,1,2),s(V_13,1,3),s(V_14,1,4),s(V_15,1,5),
             s(V_16,1,6),s(V_17,1,7),s(V_18,1,8),s(V_19,1,9),
             s(V_21,2,1),s(V_22,2,2),s(V_23,2,3),s(V_24,2,4),s(V_25,2,5),
             s(V_26,2,6),s(V_27,2,7),s(V_28,2,8),s(V_29,2,9),
             s(V_31,3,1),s(V_32,3,2),s(V_33,3,3),s(V_34,3,4),s(V_15,3,5),
             s(V_36,3,6),s(V_37,3,7),s(V_38,3,8),s(V_39,3,9),
             s(V_41,4,1),s(V_42,4,2),s(V_43,4,3),s(V_44,4,4),s(V_15,4,5),
             s(V_46,4,6),s(V_47,4,7),s(V_48,4,8),s(V_49,4,9),
             s(V_51,5,1),s(V_52,5,2),s(V_53,5,3),s(V_54,5,4),s(V_15,5,5),
             s(V_56,5,6),s(V_57,5,7),s(V_58,5,8),s(V_59,5,9),
             s(V_61,6,1),s(V_62,6,2),s(V_63,6,3),s(V_64,6,4),s(V_15,6,5),
             s(V_66,6,6),s(V_67,6,7),s(V_68,6,8),s(V_69,6,9),
             s(V_71,7,1),s(V_72,7,2),s(V_73,7,3),s(V_74,7,4),s(V_15,7,5),
             s(V_76,7,6),s(V_77,7,7),s(V_78,7,8),s(V_79,7,9),
             s(V_81,8,1),s(V_82,8,2),s(V_83,8,3),s(V_84,8,4),s(V_15,8,5),
             s(V_86,8,6),s(V_87,8,7),s(V_88,8,8),s(V_89,8,9),
             s(V_91,9,1),s(V_92,9,2),s(V_93,9,3),s(V_94,9,4),s(V_15,9,5),
             s(V_96,9,6),s(V_97,9,7),s(V_98,9,8),s(V_99,9,9)],
        all_different([V_11,V_21,V_31,V_41,V_51,V_61,V_71,V_81,V_91]),
        all_different([V_12,V_22,V_32,V_42,V_52,V_62,V_72,V_82,V_92]),
        all_different([V_13,V_23,V_33,V_43,V_53,V_63,V_73,V_83,V_93]),
	writeln('3_columns'),
        all_different([V_14,V_24,V_34,V_44,V_54,V_64,V_74,V_84,V_94]),
        all_different([V_15,V_25,V_35,V_45,V_55,V_65,V_75,V_85,V_95]),
        all_different([V_16,V_26,V_36,V_46,V_56,V_66,V_76,V_86,V_96]),
	writeln('6_columns'),
        all_different([V_17,V_27,V_37,V_47,V_57,V_67,V_77,V_87,V_97]),
        all_different([V_18,V_28,V_38,V_48,V_58,V_68,V_78,V_88,V_98]),
        all_different([V_19,V_29,V_39,V_49,V_59,V_69,V_79,V_89,V_99]),
	writeln('9_columns'),
%
        all_different([V_11,V_21,V_31,V_12,V_22,V_32,V_13,V_23,V_33]),
	writeln('1_squares'),
        all_different([V_14,V_24,V_34,V_15,V_25,V_35,V_16,V_26,V_36]),
	writeln('2_squares'),
        all_different([V_17,V_27,V_37,V_18,V_28,V_38,V_19,V_29,V_39]),
	writeln('3_squares'),
        all_different([V_41,V_51,V_61,V_42,V_52,V_62,V_43,V_53,V_63]),
        all_different([V_44,V_54,V_64,V_45,V_55,V_65,V_46,V_56,V_66]),
        all_different([V_47,V_57,V_67,V_48,V_58,V_68,V_49,V_59,V_69]),
	writeln('6_squares'),
        all_different([V_71,V_81,V_91,V_72,V_82,V_92,V_73,V_83,V_93]),
	writeln('7_squares'),
        all_different([V_74,V_84,V_94,V_75,V_85,V_95,V_76,V_86,V_96]),
	writeln('8_squares'),
        all_different([V_77,V_87,V_97,V_78,V_88,V_98,V_79,V_89,V_99]),
	writeln('9_squares').

do_label(S):- 
	extract_vars(S,Svars),
	writeln(labelling),
	label(Svars).

extract_vars([],[]).
extract_vars([s(V,_,_)|R],[V|R1]):- var(V),!,
	extract_vars(R,R1).
extract_vars([_|R],R1):- 
	extract_vars(R,R1).

test(S):- 
sudoku([s(1,1,1),                                                      s(3,1,8),
                 s(2,2,2),                  s(1,2,5),                  
                          s(3,3,3),
                                   s(4,4,4),
                                            s(5,5,5),
                                                     s(6,6,6),
                                                              s(7,7,7),
                                                                       s(8,8,8),
                                                                                s(9,9,9)],S).
end_of_file.

