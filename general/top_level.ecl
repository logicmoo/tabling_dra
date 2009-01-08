%%%                                                                      %%%
%%%  A general top level for metainterpreters.                           %%%
%%%  Written by Feliks Kluzniak at UTD.                                  %%%
%%%                                                                      %%%

%%% NOTE:
%%%    1. To load a new program use the query:
%%%           ?- prog( filename ).
%%%       If the filename has no extension, the default extension will be
%%%       used if provided (see the description of "default_extension" below).
%%%
%%%       To include files use the usual Prolog syntax:
%%%           :- [ file1, file2, ... ].
%%%
%%%    2. The metainterpreter should provide the following predicates
%%%       ("hooks" that will be called by the top level:
%%%
%%%          - default_extension/1:
%%%                 This predicate is optional.  If present, its argument
%%%                 should be a string that describes the extension to be
%%%                 added to file names that do not already have an extension.
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
%%%          - process_directive/1:
%%%                 Whenever the top level encounters a legal directive
%%%                 ":- D" (see above), it invokes "process_directive( D )"
%%%                 to give the interpreter a chance to act upon the
%%%                 directive.
%%%
%%%          - query/1:
%%%                 This would be the main entry point of the metainterpreter.
%%%                 Whenever the top level encounters a query (of the form
%%%                 "?- Q."), it will display the query and then call
%%%                 "query( Q )".  Depending on the result, it will then
%%%                 display "No", or "Yes" (preceded by a display of bindings
%%%                 acquired by the variables occurring in "Q").
%%%                 Please note that a query read in from a file will not be
%%%                 be retried for alternative solutions.
%%%



%% prog( + file name ):
%% Initialise, then load a program from this file, processing directives and
%% queries.

prog( FileName ) :-
        initialise,                              % provided by a metainterpreter
        process_file( FileName ).


%% process_file( + file name ):
%% Load a program from this file, processing directives and queries.

process_file( FileName ) :-
        \+ atom( FileName ),
        !,
        write(   error, "*** Illegal file name \"" ),
        write(   error, FileName ),
        writeln( error, "\" (not an atom) will be ignored. ***" ).

process_file( FileName ) :-
        atom( FileName ),
        ensure_extension( FileName, FullFileName ),
        open( FullFileName, read, ProgStream ),

        repeat,
        readvar( ProgStream, Term, VarDict ),
        % write( '<processing \"' ),  write( Term ),  writeln( '\">' ),
        process_term( Term, VarDict ),
        Term = end_of_file,
        !,

        close( ProgStream ).


%% ensure_extension( + file name, - ditto possibly extended ):
%% If the file name has no extension, add the default extension, if any

ensure_extension( FileName, FullFileName ) :-
        atom_string( FileName, FileNameString ),
        \+ substring( FileNameString, ".", _ ),   % no extension
        default_extension( ExtString ),           % provided by metainterpreter?
        !,
        concat_strings( FileNameString, ExtString, FullFileName ).

ensure_extension( FileName, FileName ).       % extension present, or no default



%% process_term( + term, + variable dictionary ):
%% Process a term, which should be a directive, a query, a program clause or
%% end_of_file.
%% The variable dictionary is used for printing out the results of a query.

process_term( end_of_file, _ ) :-  !.            % just ignore this

process_term( (:- [ H | T ]), _ ) :-             % include
        !,
        include_files( [ H | T ] ).

process_term( (:- Directive), _ ) :-
        legal_directive( Directive ),            % provided by a metainterpreter
        !,
        process_directive( Directive ).          % provided by a metainterpreter

process_term( (:- Directive), _ ) :-             % unsupported directive
        \+ legal_directive( Directive ),
        !,
        write(   error, '+++ Unknown directive: \"' ),
        write(   error, (:- Directive) ),
        writeln( error, '.\" +++' ).

process_term( (?- Query), VarDict ) :-
        !,
        process_query( Query, VarDict ).

process_term( Clause, _ ) :-
        Clause \= end_of_file, Clause \= (:- _), Clause \= (?- _),
        ( good_head( Clause ) ; Clause = (H :- _), good_head( H ) ),
        !,
        ensure_dynamic( Clause ),
        assertz( Clause ).

process_term( Clause, _ ) :-
        Clause \= end_of_file, Clause \= (:- _), Clause \= (?- _),
        \+ ( good_head( Clause ) ; Clause = (H :- _), good_head( H ) ),
        !,
        write(   error, 'Erroneous clause: \"' ),
        write(   error, Clause ),
        writeln( error, '\"' ).


%% include_files( + list of file names ):
%% Process the files whose names are in the list.

include_files( List ) :-
        member( FileName, List ),
        process_file( FileName ),
        fail.

include_files( _ ).


%% good_head( + term ):
%% Is this term a good head of a clause?

good_head( Hd ) :-
        atom( Hd ),
        !.

good_head( Hd ) :-
        compound( Hd ),
        \+ is_list( Hd ).


%% process_query( + query, + variable dictionary ):
%% Process a query, i.e., produce and display solutions until
%% no more can be found.

process_query( Query, VarDict ) :-
        write( '-- Query: ' ),  write( Query ), writeln( '.  --' ),
        execute_query( Query, VarDict ),
        !.                                            % no alternative solutions

%
execute_query( Query, VarDict ) :-
        query( Query ),                          % provided by a metainterpreter
        show_results( VarDict ),
        writeln( 'Yes' ).

execute_query( _, _ ) :-
        writeln( 'No' ).


%% show_results( + variable dictionary ):
%% Use the variable dictionary to show the results of a query.

show_results( Dict ) :-
        member( [ Name | Var ], Dict ),
        write( Name ), write( ' = ' ),  writeln( Var ),
        fail.
show_results( _ ).

%-------------------------------------------------------------------------------
