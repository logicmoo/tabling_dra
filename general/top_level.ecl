%%%                                                                          %%%
%%%  A simple (!) general top level for metainterpreters.                    %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (January 2009).                       %%%
%%%                                                                          %%%
%%%  Last update: 2 February 2009.                                           %%%
%%%                                                                          %%%
%%%  NOTE: Some of the code may be Eclipse-specific and may require          %%%
%%%        minor tweaking for other Prolog systems.                          %%%
%%%                                                                          %%%

%%% NOTE:
%%%
%%%    0. The clauses read in by the top level are loaded into the module
%%%       "interpreted".  This is done to avoid conflicts with predicates
%%%       used in the metainterpreter (and the top level).  The metainterpreter
%%%       must access them by using
%%%          clause( ... ) @ interpreted.
%%%
%%%
%%%    1. To use this top level, just include it in your the file that
%%%       contains the code for your metainterpreter:
%%%
%%%           :- ensure_loaded( '../general/top_level' ).
%%%
%%%       Then load the metainterpreter into your logic programming system.
%%%
%%%
%%%    2. To begin execution by loading a new program, invoke
%%%
%%%           prog( filename ).
%%%
%%%       If the filename has no extension, the default extension will be
%%%       used if provided (see the description of "default_extension" below).
%%%
%%%       As the file is loaded, directives and queries are executed on-the-fly
%%%       by invoking the metainterpreter (except the ":- op ..." directive,
%%%       which is interpreted directly). A query is evaluated to give all
%%%       solutions (it is as if the user kept responding with a semicolon):
%%%       to avoid that use the built-in predicate once/1 .
%%%
%%%       After the file is loaded (and all the directives and queries it
%%%       contains are executed), interactive mode is started.  This is very
%%%       much like the usual top-level loop, except that the associated
%%%       metainterpreter -- and not the underlying Logic Programming system --
%%%       is used to evaluate queries and directives.
%%%
%%%
%%%       To just enter interactive mode invoke
%%%
%%%           top
%%%
%%%       To exit interactive mode enter end of file (^D), or just write
%%%
%%%           quit.
%%%
%%%       (the former method appears not to work with tkeclipse).
%%%
%%%
%%%    3. To include files (interactively or from other files) use
%%%       the usual Prolog syntax:
%%%
%%%           :- [ file1, file2, ... ].
%%%
%%%       Please note that there is a difference between "prog( file )" and
%%%       ":- [ file ].".  If the former is used, the metainterpreter is
%%%       (re)initialised before loading the file (see description of
%%%       initialise/0  below); if the latter is used, the file is just loaded.
%%%
%%%
%%%    4. The metainterpreter should provide the following predicates
%%%       ("hooks" that will be called by the top level:
%%%
%%%          - default_extension/1:
%%%                 This predicate is optional.  If present, its argument
%%%                 should be a string that describes the extension to be
%%%                 added to file names that do not already have an extension.
%%%                 (The string should begin with a period!)
%%%                 For example, a metainterpreter for coinductive logic
%%%                 programming might contain the following fact:
%%%                      default_extension( ".clp" ).
%%%
%%%          - initialise/0:
%%%                 This will be called before loading a new program,
%%%                 giving the metainterpreter an opportunity to
%%%                 (re)initialise its data structures.
%%%
%%%          - legal_directive/1:
%%%                 Whenever the top level encounters a directive
%%%                 (of the form ":- D."), it will call "legal_directive( D )".
%%%                 If the call succeeds, the interpreter will be given
%%%                 a chance to process the directive (see below), otherwise
%%%                 the directive will be ignored (with a suitable warning).
%%%
%%%          - execute_directive/1:
%%%                 Whenever the top level encounters a legal directive
%%%                 ":- D" (see above), it invokes "execute_directive( D )"
%%%                 to give the interpreter a chance to act upon the
%%%                 directive.
%%%
%%%          - query/1:
%%%                 This would be the main entry point of the metainterpreter.
%%%                 Whenever the top level encounters a query (of the form
%%%                 "?- Q."), it will display the query and then call
%%%                 "query( Q )".  Depending on the result, it will then
%%%                 display "No", or "Yes" (preceded by a display of bindings
%%%                 acquired by the variables occurring in "Q"); in the latter
%%%                 case it will also backtrack to obtain more solutions.
%%%
%%%
%%%    5. The metainterpreter can also define hooks of its own.  A hook
%%%       predicate should be declared in a fact of "hook_predicate/1".
%%%       For example,
%%%
%%%           hook_predicate( essence_hook( _, _ ) ).
%%%
%%%       declares that "essence_hook/2" is a metainterpreter hook.  A hook
%%%       predicate (essence_hook/2 in this case) should be dynamic.  When
%%%       the top level encounters a clause whose head matches a hook predicate
%%%       declaration, the clause is asserted at the front (!) of the predicate
%%%       (in the module of the running program, not in "interpreted").
%%%
%%%       NOTE: If the interpreter does not use hook predicates, it must contain
%%%             the definition
%%%                 hook_predicate( '' ).


:- ensure_loaded( utilities ).


% If p/k has already been seen (and declared as dynamic), the fact is recorded
% as known( p, k ).

:- dynamic known/2 .



%% prog( + file name ):
%% Initialise, then load a program from this file, processing directives and
%% queries.  After this is done, enter interactive mode.


prog( FileName ) :-
        retractall( known( _, _ ) ),
        erase_modules,
        create_modules,
        initialise,                              % provided by a metainterpreter
        process_file( FileName ),
        top.



%% NOTE: In order to avoid name conflicts with the numerous built-in predicates
%%       of Eclipse, the only thing "interpreted" imports is the module
%%       "interface".  The module "interface" is created by the top-level from
%%       a declaration of built-ins allowed by the metainterpreter (and provided
%%       by the latter in table "builtin").  The difficulty is that Eclipse does
%%       not allow direct exportation of built-ins: this is solved by defining
%%       yet another module, called "interface_aux".
%%       The exact mechanics are best explained by means of an example:
%%
%%       1. Let the metainterpreter contain a declaration of only one built-in:
%%             builtin( writeln( _ ) ).
%%
%%       2. The top level will add the following clause to module
%%          "interface_aux" (which imports all the built-ins by default):
%%             xxx_writeln( X ) :-  writeln( X ).
%%
%%       3. "xxx_writeln/1" will be exported by "interface_aux".
%%
%%       4. Module "interface" will import only "interface_aux", and will be
%%          closed to default importation of built-ins.  It will define clause:
%%             writeln( X) :- xxx_writeln( X ).
%%
%%       5. "writeln/1" is now a user-defined predicate and can be exported from
%%          "interface".
%%
%%       6. "interpreted" will import only "interface", and will be closed to
%%          default importation of built-ins.  The interpreted program can use
%%          "writeln/1" and no other built-ins.  It can also define predicates
%%          whose names would normally clash with names of built-ins (e.g.,
%%          "connect/2", which might be useful in, say, a graph-processing
%%          application).


%% erase_modules:
%% Erase the modules that might be there after interpreting the previous
%% program.

erase_modules :-
        erase_module( interpreted ),
        erase_module( interface ),
        erase_module( interface_aux ).

%% create_modules:
%% Create the modules.

create_modules :-
        create_module( interface_aux ),
        create_module( interface  , [], [ interface_aux ] ),
        create_module( interpreted, [], [ interface     ] ),
        fill_interface_modules.

%
fill_interface_modules :-
        builtin( Pattern ),
        functor( Pattern, F, K ),
        concat_atoms( 'xxx_', F, ExtF ),
        mk_pattern( ExtF, K, ExtPattern ),
        ExtPattern =.. [ _ | Args ],
        Pattern    =.. [ _ | Args ],     % i.e., unify arguments of the patterns
        assert( (ExtPattern :- Pattern) ) @ interface_aux,
        assert( (Pattern :- ExtPattern) ) @ interface,
        export( ExtF / K ) @ interface_aux,
        export( F    / K ) @ interface,
        fail.

fill_interface_modules.




%% process_file( + file name ):
%% Load a program from this file, processing directives and queries.

:- mode process_file( + ).

process_file( FileName ) :-
        ensure_filename_is_an_atom( FileName ),
        ensure_extension( FileName, FullFileName ),
        open( FullFileName, read, ProgStream ),
        process_input( ProgStream ),
        close( ProgStream ).


%% process_input( + input stream ):
%% Read the stream, processing directives and queries and storing clauses.

:- mode process_input( + ).

process_input( ProgStream ) :-
        repeat,
        readvar( ProgStream, Term, VarDict ),
        verify_program_item( Term ),
        process_term( Term, VarDict ),
        Term = end_of_file,
        !.


%% ensure_extension( + file name, - ditto possibly extended ):
%% If the file name has no extension, add the default extension, if any

:- mode ensure_extension( +, - ).

ensure_extension( FileName, FullFileName ) :-
        atom_string( FileName, FileNameString ),
        \+ substring( FileNameString, ".", _ ),   % no extension
        default_extension( ExtString ),           % provided by metainterpreter
        !,
        concat_strings( FileNameString, ExtString, FullFileName ).

ensure_extension( FileName, FileName ).       % extension present, or no default




%% process_term( + term, + variable dictionary ):
%% Process a term, which should be a directive, a query, a program clause or
%% end_of_file.
%% The variable dictionary is used for printing out the results of a query.
%%
%% NOTE: The superficial correctness of this term as a program item has already
%%       been verified by "verify_program_item/1".

:- mode process_term( +, + ).

process_term( end_of_file, _ ) :-  !.            % just ignore this

process_term( (:- [ H | T ]), _ ) :-             % include
        !,
        include_files( [ H | T ] ).

process_term( (:- op( P, F, Ops )), _ ) :-
        !,
        op( P, F, Ops ).

process_term( (:- Directive), _ ) :-
        !,
        process_directive( Directive ).

process_term( (?- Query), VarDict ) :-
        !,
        process_query( Query, VarDict ),
        !.                                            % no alternative solutions

process_term( Clause, _ ) :-
        get_clause_head( Clause, Head ),
        hook_predicate( Head ),              % metainterpreter's hook predicate
        !,
        check_not_builtin( Clause ),         % fatal error if redefining builtin
        asserta( Clause ).

process_term( Clause, _ ) :-
        check_not_builtin( Clause ),         % fatal error if redefining builtin
        ensure_dynamic( Clause ),
        assertz( Clause ) @ interpreted.


%% include_files( + list of file names ):
%% Process the files whose names are in the list.

:- mode include_files( + ).

include_files( List ) :-
        member( FileName, List ),
        process_file( FileName ),
        fail.

include_files( _ ).



%% process_directive( + directive ):
%% Process a directive.

:- mode process_directive( + ).

process_directive( Directive ) :-
        legal_directive( Directive ),            % provided by a metainterpreter
        !,
        execute_directive( Directive ).          % provided by a metainterpreter

process_directive( Directive ) :-                % unsupported directive
        % \+ legal_directive( Directive ),
        !,
        error( [ "*** Unknown directive: \"", (:- Directive), ".\" ***" ] ).



%% ensure_dynamic( + clause ):
%% Make sure the predicate of this clause is dynamic.
%% known/2 is used to avoid multiple declarations.

:- mode ensure_dynamic( + ).

ensure_dynamic( Clause ) :-
        get_clause_head( Clause, Head ),
        functor( Head, PredicateSymbol, Arity ),
        \+ known( PredicateSymbol, Arity ),
        assert( known( PredicateSymbol, Arity ) ),
        dynamic( PredicateSymbol / Arity ) @ interpreted,
        fail.

ensure_dynamic( _ ).


%% process_query( + query, + variable dictionary ):
%% Process a query, i.e., produce and display solutions until
%% no more can be found.

:- mode process_query( +, + ).

process_query( Query, VarDict ) :-
        write( output, '-- Query: ' ),
        write( output, Query ),
        writeln( output, '.  --' ),
        execute_query( Query, VarDict, Answer ),
        Answer = no.                             % i.e., backtrack if 'yes'.

%
:- mode execute_query( +, +, + ).

execute_query( Query, VarDict, yes ) :-
        query( Query ),                          % provided by a metainterpreter
        show_results( VarDict ),
        writeln( output, 'Yes' ).

execute_query( _, _, no ) :-
        writeln( output, 'No' ).


%% show_results( + variable dictionary ):
%% Use the variable dictionary to show the results of a query.

:- mode show_results( + ).

show_results( Dict ) :-
        member( [ Name | Var ], Dict ),
        write(   output, Name ),
        write(   output, ' = ' ),
        writeln( output, Var ),
        fail.

show_results( _ ).



%% check_not_builtin( + clause ):
%% Raise a fatal error if this clause attempts to redefine a built-in predicate.

check_not_builtin( Clause ) :-
        get_clause_head( Clause, Head ),
        builtin( Head ),
        !,
        error( [ "An attempt to redefine a built-in predicate:\n  ",
                 Clause,
                 ".\n"
               ]
             ).

check_not_builtin( _ ).



%% top:
%% Interactive mode.  Each term that is not a directive or a query is treated
%% as an abbreviated query.  After displaying the results of each query read
%% characters upt the nearest newline: if the first character is ";",
%% backtrack to find alternative solutions.
%% Exit upon encountering end of file.

top :-
        repeat,
        write( output, ': ' ),                                          % prompt
        flush( output ),
        readvar( input, Term, VarDict ),
        verify_program_item( Term ),
        interactive_term( Term, VarDict ),
        ( Term = end_of_file ; Term = quit ),
        !.


%% interactive_term( + term, + variable dictionary ):
%% Process a term in interactive mode.
%% The variable dictionary is used for printing out the results of a query.

:- mode interactive_term( +, + ).

interactive_term( end_of_file, _ ) :-  !.              % just ignore this

interactive_term( quit       , _ ) :-  !.              % just ignore this

interactive_term( (:- [ H | T ]), _ ) :-               % include
        !,
        include_files( [ H | T ] ).

interactive_term( (:- Directive), _ ) :-               % directive
        !,
        process_directive( Directive ).

interactive_term( (?- Query), VarDict ) :-             % query
        !,
        execute_query( Query, VarDict, Ans ),
        continue_query( Ans ),
        !.

interactive_term( Other, VarDict ) :-                  % other: treat as a query
        % Other \= end_of_file,
        % Other \= (:- _),
        % Other \= (?- _),
        % Other \= quit
        interactive_term( (?- Other), VarDict ).


%% continue_query( + answer ):
%% Give the user a chance to type ";" if the answer is "yes".

:- mode continue_query( + ).

continue_query( yes ) :-
        user_accepts,
        !.

continue_query( no ).


%% user_accepts:
%% Read input upto the nearest newline.
%% If the first character is a semicolon, fail.

user_accepts :-
        getline( Line ),
        Line \= [ ";" | _ ].             % i.e., fail if 1st char is a semicolon

%-------------------------------------------------------------------------------
