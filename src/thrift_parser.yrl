Header
"%% Copyright 2017 Pinterest, Inc."
"%%"
"%% Licensed under the Apache License, Version 2.0 (the \"License\");"
"%% you may not use this file except in compliance with the License."
"%% You may obtain a copy of the License at"
"%%"
"%%    http://www.apache.org/licenses/LICENSE-2.0"
"%%"
"%% Unless required by applicable law or agreed to in writing, software"
"%% distributed under the License is distributed on an \"AS IS\" BASIS,"
"%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."
"%% See the License for the specific language governing permissions and"
"%% limitations under the License.".

Nonterminals
    Schema File
    Headers Header
    Include Namespace
    Definitions Definition
    Typedef Struct Union Exception
    Const ConstValue ConstList ConstMap
    Enum EnumList EnumValue
    Service Extends
    FunctionList Function Oneway ReturnType Throws
    FieldList Field FieldIdentifier FieldRequired FieldDefault
    FieldType BaseType MapType SetType ListType
    Separator.

Terminals
    '*' '{' '}' '[' ']' '(' ')' '=' '>' '<' ',' ':' ';'
    file
    include namespace
    int ident
    bool byte i8 i16 i32 i64 double string binary list map set
    true false
    typedef const enum struct union service exception
    void oneway required optional extends throws.

Rootsymbol Schema.

% Schema

Schema -> Headers Definitions File:
    build_model('Schema', ['$3', '$1', '$2']).

File -> '$empty': nil.
File -> file string: 'Elixir.List':to_string(unwrap('$2')).

% Headers

Headers -> '$empty': [].
Headers -> Header Headers: ['$1'|'$2'].

Header -> Include: '$1'.
Header -> Namespace: '$1'.

Include -> include string:
    build_model('Include', line('$1'), [unwrap('$2')]).

Namespace -> namespace '*' ident:
    build_model('Namespace', line('$1'), ["*", unwrap('$3')]).
Namespace -> namespace ident ident:
    build_model('Namespace', line('$1'), [unwrap('$2'), unwrap('$3')]).

% Definitions

Definitions -> '$empty': [].
Definitions -> Definition Definitions: ['$1'|'$2'].

Definition -> Const: '$1'.
Definition -> Typedef: '$1'.
Definition -> Enum: '$1'.
Definition -> Struct: '$1'.
Definition -> Union: '$1'.
Definition -> Exception: '$1'.
Definition -> Service: '$1'.

% Constants

Const -> const FieldType ident '=' ConstValue Separator:
    build_model('Constant', line('$1'), [unwrap('$3'), '$5', '$2']).

ConstValue -> ident: build_model('ValueRef', line('$1'), [unwrap('$1')]).
ConstValue -> true: unwrap('$1').
ConstValue -> false: unwrap('$1').
ConstValue -> int: unwrap('$1').
ConstValue -> double: unwrap('$1').
ConstValue -> string: unwrap('$1').

ConstValue -> '{' ConstMap '}': '$2'.
ConstValue -> '[' ConstList ']': '$2'.

ConstMap -> '$empty': [].
ConstMap -> ConstValue ':' ConstValue Separator ConstMap: [{'$1','$3'}|'$5'].

ConstList -> '$empty': [].
ConstList -> ConstValue Separator ConstList: ['$1'|'$3'].

% Typedef

Typedef -> typedef FieldType ident Separator:
    {typedef, '$2', unwrap('$3')}.

% Enum

Enum -> enum ident '{' EnumList '}':
    build_model('TEnum', line('$1'), [unwrap('$2'), '$4']).

EnumList -> EnumValue Separator: ['$1'].
EnumList -> EnumValue Separator EnumList: ['$1'|'$3'].

EnumValue -> ident '=' int: {unwrap('$1'), unwrap('$3')}.
EnumValue -> ident: unwrap('$1').

% Struct

Struct -> struct ident '{' FieldList '}':
    build_model('Struct', line('$1'), [unwrap('$2'), '$4']).

% Union

Union -> union ident '{' FieldList '}':
    build_model('Union', line('$1'), [unwrap('$2'), '$4']).

% Exception

Exception -> exception ident '{' FieldList '}':
    build_model('Exception', line('$1'), [unwrap('$2'), '$4']).

% Service

Service -> service ident Extends '{' FunctionList '}':
    build_model('Service', line('$1'), [unwrap('$2'), '$5', '$3']).

Extends -> extends ident: unwrap('$2').
Extends -> '$empty': nil.

% Functions

FunctionList -> '$empty': [].
FunctionList -> Function FunctionList: ['$1'|'$2'].

Function -> Oneway ReturnType ident '(' FieldList ')' Throws Separator:
    build_model('Function', line('$3'), ['$1', '$2', unwrap('$3'), '$5', '$7']).

Oneway -> '$empty': false.
Oneway -> oneway: true.

ReturnType -> void: void.
ReturnType -> FieldType: '$1'.

Throws -> '$empty': [].
Throws -> throws '(' FieldList ')': '$3'.

% Fields

FieldList -> '$empty': [].
FieldList -> Field FieldList: ['$1'|'$2'].

Field -> FieldIdentifier FieldRequired FieldType ident FieldDefault Separator:
    warn_field_identifier('$1', unwrap('$4'), line('$4')),
    build_model('Field', line('$4'), ['$1', '$2', '$3', unwrap('$4'), '$5']).

FieldIdentifier -> int ':': unwrap('$1').
FieldIdentifier -> '$empty': nil.

FieldRequired -> required: true.
FieldRequired -> optional: false.
FieldRequired -> '$empty': default.

FieldDefault -> '$empty': nil.
FieldDefault -> '=' ConstValue: '$2'.

% Types

FieldType -> ident: build_model('TypeRef', line('$1'), [unwrap('$1')]).
FieldType -> BaseType: '$1'.
FieldType -> MapType: {map, '$1'}.
FieldType -> SetType: {set, '$1'}.
FieldType -> ListType: {list, '$1'}.

BaseType -> bool: bool.
BaseType -> byte: i8.
BaseType -> i8: i8.
BaseType -> i16: i16.
BaseType -> i32: i32.
BaseType -> i64: i64.
BaseType -> double: double.
BaseType -> string: string.
BaseType -> binary: binary.

MapType -> map '<' FieldType ',' FieldType '>': {'$3', '$5'}.
SetType -> set '<' FieldType '>': '$3'.
ListType -> list '<' FieldType '>': '$3'.

% Separator

Separator -> ','.
Separator -> ';'.
Separator -> '$empty'.

Erlang code.

% Construct a new model of the requested Type. Args are passed to the model's
% `new` function and, if provided, line number information is assigned to the
% resulting model.
build_model(Type, Args) when is_list(Args) ->
    Module = list_to_atom("Elixir.Thrift.Parser.Models." ++ atom_to_list(Type)),
    apply(Module, 'new', Args).
build_model(Type, Line, Args) when is_integer(Line) and is_list(Args) ->
    Model = build_model(Type, Args),
    maps:put(line, Line, Model).

% Extract the line number from the lexer's expression tuple.
line({_Token, Line}) -> Line;
line({_Token, Line, _Value}) -> Line;
line(_) -> nil.

% Return either the atom from a 2-tuple lexer expression or the processed
% value from a 3-tuple lexer expression.
unwrap({V, _}) when is_atom(V) -> V;
unwrap({_,_,V}) -> V.

% Warn when the field identifier isn't explicit.
warn_field_identifier(nil, Name, Line) ->
    io:format('~p: ~p is missing an explicit field identifier~n', [Line, Name]);
warn_field_identifier(_, _, _) -> nil.
