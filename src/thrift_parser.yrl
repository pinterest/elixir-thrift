
Nonterminals
schema
headers header
definitions definition
namespace_def constant_def include_def
type base_type literal set_type map_type list_type literal_list

mapping mappings

enum_def enum_body enum_value
exception_def
fields field_spec field_sep field_required field_id field_default
typedef_def
ns_name
struct_def union_def service_def
functions function
service_extends
oneway_marker
return_type
throws_clause
comments comment_t.

Terminals
'*' '{' '}' '[' ']' '(' ')' '=' '>' '<' ',' ':'
namespace
include
ident
int
double
const
bool
byte
i8
i16
i32
i64
u8
u16
u32
u64
string
binary
list
map
set
enum
exception
required
optional
typedef
struct
union
service
extends
oneway
void
throws
comment.


Rootsymbol schema.

schema ->
    headers definitions:
        'Elixir.Thrift.Parser.Models.Schema':new('$1', '$2').

headers -> '$empty': [].
headers -> header headers: ['$1'] ++ '$2'.

header -> namespace_def: '$1'.
header -> include_def: '$1'.

definitions -> '$empty': [].
definitions -> definition definitions: ['$1'] ++ '$2'.

definition -> constant_def: '$1'.
definition -> enum_def: '$1'.
definition -> exception_def: '$1'.
definition -> typedef_def: '$1'.
definition -> struct_def: '$1'.
definition -> union_def: '$1'.
definition -> service_def: '$1'.

type -> base_type: '$1'.
type -> set_type: {set, '$1'}.
type -> map_type: {map, '$1'}.
type -> list_type: {list, '$1'}.
type -> ident: 'Elixir.Thrift.Parser.Models.StructRef':new(unwrap('$1')).

base_type -> bool: bool.
base_type -> byte: i8.
base_type -> i8: i8.
base_type -> i16: i16.
base_type -> i32: i32.
base_type -> i64: i64.
base_type -> u8: u8.
base_type -> u16: u16.
base_type -> u32: u32.
base_type -> u64: u64.
base_type -> double: double.
base_type -> string: string.
base_type -> binary: binary.

set_type -> set '<' type '>': '$3'.
map_type -> map '<' type ',' type '>': {'$3', '$5'}.
list_type -> list '<' type '>': '$3'.

include_def ->
    include string:
        'Elixir.Thrift.Parser.Models.Include':new(unwrap('$2')).

namespace_def ->
    namespace ns_name ident:
        'Elixir.Thrift.Parser.Models.Namespace':new('$2', unwrap('$3')).

constant_def ->
    comments const type ident '=' literal:
        'Elixir.Thrift.Parser.Models.Constant':new('$1', unwrap('$4'), '$6', '$3').

ns_name -> '*': "*".
ns_name -> ident: unwrap('$1').


%% JS Style mapping "foo": 32
mapping -> literal ':' literal: {'$1', '$3'}.
mappings -> mapping: ['$1'].
mappings -> mapping ',' mappings: ['$1'] ++ '$3'.

% A list of literals ["hi", "bye", 3, 4]
literal_list -> literal: ['$1'].
literal_list -> literal ',' literal_list: ['$1'] ++ '$3'.

literal -> int: unwrap('$1').
literal -> double: unwrap('$1').
literal -> string: unwrap('$1').
literal -> '{' literal_list '}': '$2'.
literal -> '{' mappings '}': '$2'.
literal -> '[' literal_list ']': '$2'.

field_sep -> '$empty': nil.
field_sep -> ',': nil.

enum_def ->
    comments enum ident '{' enum_body '}':
        'Elixir.Thrift.Parser.Models.TEnum':new('$1', unwrap('$3'), '$5').
enum_body -> enum_value field_sep: ['$1'].
enum_body -> enum_value field_sep enum_body: ['$1'] ++ '$3'.

enum_value -> comments ident: {unwrap('$2'), '$1'}.
enum_value -> comments ident '=' int: {unwrap('$2'), unwrap('$4'), '$1'}.

exception_def ->
    comments exception ident '{' fields '}':
        'Elixir.Thrift.Parser.Models.Exception':new('$1', unwrap('$3'), '$5').

fields -> '$empty': [].
fields -> field_spec field_sep fields: ['$1'] ++ '$3'.

field_id -> int ':': unwrap('$1').
field_id -> '$empty': nil.

field_spec ->
    comments field_id field_required type ident field_default:
        'Elixir.Thrift.Parser.Models.Field':new('$1', '$2', '$3', '$4', unwrap('$5'), '$6').

field_required -> required: true.
field_required -> optional: false.
field_required -> '$empty': default.

field_default -> '$empty': nil.
field_default -> '=' literal: '$2'.

typedef_def ->
    comments typedef type ident:
        {typedef, '$3', unwrap('$4'), '$1'}.

struct_def ->
    comments struct ident '{' fields '}':
        'Elixir.Thrift.Parser.Models.Struct':new('$1', unwrap('$3'), '$5').

service_def ->
    comments service ident service_extends '{' functions '}':
        'Elixir.Thrift.Parser.Models.Service':new('$1', unwrap('$3'), '$6', '$4').

union_def ->
    comments union ident '{' fields '}':
        'Elixir.Thrift.Parser.Models.Union':new('$1', unwrap('$3'), '$5').

service_extends -> '(' extends ident ')': unwrap('$3').
service_extends -> extends ident: unwrap('$2').
service_extends -> '$empty': nil.


functions -> '$empty': [].
functions -> function functions: ['$1'] ++ '$2'.

function ->
    comments oneway_marker return_type ident '(' fields ')' throws_clause field_sep:
        'Elixir.Thrift.Parser.Models.Function':new('$1', '$2', '$3', unwrap('$4'), '$6', '$8').

oneway_marker -> '$empty': false.
oneway_marker -> oneway: true.

return_type -> void: void.
return_type -> type: '$1'.

throws_clause -> '$empty': [].
throws_clause ->
    throws '(' fields ')':
        '$3'.

comments -> '$empty': [].
comments -> comment_t comments: ['$1'] ++ '$2'.

comment_t -> comment: 'Elixir.Thrift.Parser.Models.Comment':new(unwrap('$1')).

Erlang code.

unwrap({V, _}) -> V;
unwrap({_,_,V}) -> V.
