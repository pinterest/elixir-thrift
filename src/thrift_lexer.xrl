% Thrift Lexer

Definitions.

INT             = [+-]?[0-9]+
HEX             = [+-]?0x[0-9A-Fa-f]+
DOUBLE          = [+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?
IDENTIFIER      = [a-zA-Z_](\.[a-zA-Z_0-9]|[a-zA-Z_0-9])*
ST_IDENTIFIER   = [a-zA-Z-](\.[a-zA-Z_0-9-]|[a-zA-Z_0-9-])*
SEMI            = [;]
WHITESPACE      = [\s\t\r\n]+
COMMENT         = //[^\n]*
CCOMMENT        = /\\*/*([^*/]|[^*]/|\\*[^/])*\\**\\*/
UNIXCOMMENT     = #[^\n]*
STRING1         = '(\\\^.|\\.|[^'])*'
STRING2         = "(\\\^.|\\.|[^"])*"
OPEN_CURLY      = [\{]
CLOSE_CURLY     = [\}]
LT              = [<]
GT              = [>]
COMMA           = [,]
COLON           = [:]
OPEN_BRACKET    = [\[]
CLOSE_BRACKET   = [\]]
OPEN_PAREN      = [\(]
CLOSE_PAREN     = [\)]
EQUALS          = [=]
ASTERISK        = [*]

Rules.

{WHITESPACE}    : skip_token.
{COMMENT}       : skip_token.
{CCOMMENT}      : skip_token.
{UNIXCOMMENT}   : skip_token.
{SEMI}          : skip_token.

{ASTERISK}      : {token, {'*', TokenLine}}.
{OPEN_CURLY}    : {token, {'{', TokenLine}}.
{CLOSE_CURLY}   : {token, {'}', TokenLine}}.
{OPEN_BRACKET}  : {token, {'[', TokenLine}}.
{CLOSE_BRACKET} : {token, {']', TokenLine}}.
{OPEN_PAREN}    : {token, {'(', TokenLine}}.
{CLOSE_PAREN}   : {token, {')', TokenLine}}.
{EQUALS}        : {token, {'=', TokenLine}}.
{GT}            : {token, {'>', TokenLine}}.
{LT}            : {token, {'<', TokenLine}}.
{COMMA}         : {token, {',', TokenLine}}.
{COLON}         : {token, {':', TokenLine}}.
namespace       : {token, {namespace, TokenLine}}.
include         : {token, {include, TokenLine}}.

void            : {token, {void, TokenLine}}.
bool            : {token, {bool, TokenLine}}.
byte            : {token, {byte, TokenLine}}.
i8              : {token, {i8, TokenLine}}.
i16             : {token, {i16, TokenLine}}.
i32             : {token, {i32, TokenLine}}.
i64             : {token, {i64, TokenLine}}.
u8              : {token, {u8, TokenLine}}.
u16             : {token, {u16, TokenLine}}.
u32             : {token, {u32, TokenLine}}.
u64             : {token, {u64, TokenLine}}.
double          : {token, {double, TokenLine}}.
string          : {token, {string, TokenLine}}.
binary          : {token, {binary, TokenLine}}.

list            : {token, {list, TokenLine}}.
map             : {token, {map, TokenLine}}.
set             : {token, {set, TokenLine}}.

typedef         : {token, {typedef, TokenLine}}.
enum            : {token, {enum, TokenLine}}.
struct          : {token, {struct, TokenLine}}.
union           : {token, {union, TokenLine}}.
exception       : {token, {exception, TokenLine}}.

const           : {token, {const, TokenLine}}.
oneway          : {token, {oneway, TokenLine}}.
extends         : {token, {extends, TokenLine}}.
throws          : {token, {throws, TokenLine}}.
service         : {token, {service, TokenLine}}.
required        : {token, {required, TokenLine}}.
optional        : {token, {optional, TokenLine}}.

false           : {token, {false, TokenLine}}.
true            : {token, {true, TokenLine}}.

{INT}           : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{HEX}           : {token, {int, TokenLine, hex_to_integer(TokenChars)}}.
{DOUBLE}        : {token, {double, TokenLine, list_to_float(TokenChars)}}.

{IDENTIFIER}    : {token, {ident, TokenLine, TokenChars}}.
{ST_IDENTIFIER} : {token, {st_ident, TokenLine, TokenChars}}.

{STRING1}       : {token, {string, TokenLine, dequote(TokenChars, TokenLen)}}.
{STRING2}       : {token, {string, TokenLine, dequote(TokenChars, TokenLen)}}.

Erlang code.

dequote(S,Len) -> lists:sublist(S, 2, Len - 2).

hex_to_integer([$+|Chars]) ->  hex_to_integer(Chars);
hex_to_integer([$-|Chars]) -> -hex_to_integer(Chars);
hex_to_integer([$0,$x|Chars]) -> list_to_integer(Chars, 16).
