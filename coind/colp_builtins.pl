%%%%%  Built-in predicates for the "colp" interpreter.  %%%%

%%% If the interpreted program invokes a built-in predicate, that predicate must
%%% be declared in the table "builtin/1" below.
%%% Every addition should be considered carefully: some built-ins might require
%%% special treatment by the interpreter.

%%  NOTE: Just adding "!" won't do the trick, the main interpreter would
%%        have to be modified substantially.

builtin( true               ).
builtin( false              ).
builtin( fail               ).
builtin( _ = _              ).
builtin( _ \= _             ).
builtin( \+( _ )            ).
builtin( once( _ )          ).   % special treatment in solve/2
builtin( (_ ->_ ; _)        ).   % special treatment in solve/2
builtin( (_ ; _)            ).   % special treatment in solve/2
builtin( (_ , _)            ).   % special treatment in solve/2
builtin( writeln( _ )       ).
builtin( write( _ )         ).
builtin( write_term( _, _ ) ).
builtin( nl                 ).
builtin( set_print_depth( _, _ ) ).     % not a real built-in, see "top_level"

