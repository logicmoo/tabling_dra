%%%%%  Built-in predicates for the "dra" interpreter  %%%%

%%% If the interpreted program invokes a built-in predicate, that predicate must
%%% be declared in the table "is_builtin/1" below.
%%% Every addition should be considered carefully: some built-ins might require
%%% special treatment by the interpreter.

%%  NOTE: findall/3 is not opaque to coinductive and tabled ancestors.

%%  NOTE: Just adding "!" won't do the trick, the main interpreter would
%%        have to be modified substantially (but first: what are the semantics?)
:-dynamic(is_builtin/4).
:-dynamic(is_builtin/1).

%is_builtin(_, ',',2          ).  % special treatment in solve/4
%is_builtin( (_ -> _)           ).  % special treatment in solve/4
%is_builtin( (_ -> _ ; _)       ).  % special treatment in solve/4
%is_builtin( (_ ; _)            ).  % special treatment in solve/4
%is_builtin( \+( _ )            ).  % special treatment in solve/4
%is_builtin( assert( _ )        ).  % special treatment in solve/4
%is_builtin( findall( _, _, _ ) ).  % special treatment in solve/4
%is_builtin( once( _ )          ).  % special treatment in solve/4
%is_builtin( retractall( _ )    ).  % special treatment in solve/4
is_builtin( _ < _              ).
is_builtin( _ = _              ).
is_builtin( _ =:= _            ).
is_builtin( _ =< _             ).
is_builtin( _ =\= _            ).
is_builtin( _ > _              ).
is_builtin( _ >= _             ).
is_builtin( _ \= _             ).
is_builtin( _ is _             ).
is_builtin( append( _, _, _ )  ).
is_builtin( atom( _ )          ).
is_builtin( call( _ )          ).
is_builtin( fail               ).
is_builtin( false              ).
is_builtin( length( _, _ )     ).
is_builtin( member( _, _ )     ).
is_builtin( nl                 ).
is_builtin( read( _ )          ).
is_builtin( set_flag( _, _ )   ).
is_builtin( sort( _, _ )       ).
is_builtin( true               ).
is_builtin( var( _ )           ).
is_builtin( write( _ )         ).
is_builtin( write_term( _, _ ) ).
is_builtin( writeln( _ )       ).
is_builtin( 'C'( _, _, _ )     ).  % for DCG's on some Prolog systems
is_builtin( set_print_depth( _ ) ).          % not a real built-in, see "top_level"

is_builtin(_, delete(_,_,_)  ,delete, 3).
is_builtin(_,G,F,A):- (var(G)->functor(G,F,A);true), is_builtin(G).
