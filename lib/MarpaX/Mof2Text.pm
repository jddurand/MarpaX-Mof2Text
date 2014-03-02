use strict;
use warnings FATAL => 'all';

package MarpaX::Mof2Text;

# ABSTRACT: MOF2TEXT 1.0 using Marpa

use Log::Any qw/$log/;
use Carp qw/croak/;
use Marpa::R2 2.082000;

# VERSION

=head1 DESCRIPTION

This module is an implementation of MOF Model To Text Transformation Language (MOFM2T), 1.0, as per OMG standard, also known as Mof2Text.

=cut

#
# We load the grammar once
#
our $G = Marpa::R2::Scanless::G->new({source => \do {local $/; <DATA>}});

=head1 SEE ALSO

L<MOF Model To Text Transformation Language (MOFM2T), 1.0|http://www.omg.org/spec/MOFM2T/1.0/>

=cut

sub new {
    bless {}, shift;
}

sub parse {
    my ($self, $inputp) = @_;

    my $R = Marpa::R2::Scanless::R->new( { grammar => $G, trace_terminals => 1, trace_actions => 1 } );
    my $max = length($$inputp);

    my $pos = undef;
    do {
	foreach (@{$R->events()}) {
	    my ($name) = @{$_};
	    print STDERR "==> $name\n";
	}
	print STDERR $R->show_progress();
	$pos = defined($pos) ? $R->resume($pos) : $R->read($inputp);
    } while ($pos < $max);

    my $nvalue = 0;
    my $value;
    while (defined($value = $R->value())) {
	++$nvalue;
	print STDERR "Got value No $nvalue\n";
    }

    if (! $nvalue) {
	print STDERR $R->show_progress();
    }
}

1;

__DATA__
################################################################################################################
# MOFM2T 1.0 (MOF2TEXT) as per OMG, MOF Model To Text Transformation Language (MOFM2T), 1.0
#
# http://www.omg.org/spec/MOFM2T/1.0/
################################################################################################################
:default ::= action => [name,values] # bless => ::lhs
lexeme default = action => [start,length,value] forgiving => 1

:start ::= module

#
# Note: the spec does NOT specify '[/module]' nor '/module'. But the examples are using them...
#       so we assume these keywords are optional
#
<module>       ::= <module_decl> <import_decl_many> <queries_section> <templates_section> <macros_section> <end_module_maybe>

<end_module_maybe> ::= LBRACKET SLASH MODULE RBRACKET
<end_module_maybe> ::=

<module_decl>  ::= LBRACKET MODULE <PathNameCS> LPAREN <PathNameCS> RPAREN <extends_decl_maybe> SLASH RBRACKET
                 | MODULE <PathNameCS> <extends_decl_maybe>

<extends_decl> ::= EXTENDS <PathNameCSlist>

<import_decl>  ::= LBRACKET IMPORT <PathNameCS> SLASH RBRACKET
                 | IMPORT <PathNameCS>

<queries_section> ::= <queries_section_unit>*

<query_defn> ::= LBRACKET QUERY <visibility> <PathNameCS> LPAREN <arglist> RPAREN COLON <typeCS> EQUAL <OclExpressionCS> SLASH RBRACKET 

<query_defn_code> ::= QUERY <visibility> <PathNameCS> LPAREN <arglist> RPAREN COLON <typeCS> EQUAL <OclExpressionCS> 

<visibility> ::= PUBLIC | PRIVATE

<arglist> ::= <arg_decl>* separator => COMMA
 
<arg_decl> ::= <SimpleNameCS> COLON <typeCS> 

<actualarglist> ::= <OclExpressionCS>+ separator => COMMA

<macros_section> ::= <macros_section_unit>*

<templates_section> ::= <templates_section_unit>*

<template_defn> ::= <template_defn_text>
                  | <template_defn_code>

<template_defn_text> ::= LBRACKET TEMPLATE <signature> RBRACKET <production> LBRACKET SLASH TEMPLATE RBRACKET

<template_defn_code> ::= TEMPLATE <signature> <production_code> SLASH TEMPLATE

<signature> ::= <visibility> <PathNameCS> LPAREN <arglist> RPAREN <overrides_maybe> <guard_maybe> <init_maybe>

<macro_defn> ::= LBRACKET MACRO <PathNameCS> LPAREN <arglist> RPAREN RBRACKET <production> LBRACKET SLASH MACRO RBRACKET

<macro_defn_code> ::= MACRO <PathNameCS> LPAREN <arglist> RPAREN <production_code> SLASH MACRO

<overrides> ::= OVERRIDES <PathNameCSlist>

<production> ::= <production_unit>*

<production_code> ::= <production_code_unit>*

<guard> ::= QUESTION_MARK LPAREN <OclExpressionCS> RPAREN

<init> ::= LCURLY <VariableDeclarationCS_unit_many> RCURLY

<filecmd> ::= LBRACKET FILE LPAREN <OclExpressionCSOrStdout> <OclExpressionCS_next_maybe> <OclExpressionCS_next_maybe> RPAREN RBRACKET <production> LBRACKET SLASH FILE RBRACKET

<filecmd_code> ::= FILE LPAREN <OclExpressionCSOrStdout> <OclExpressionCS_next_maybe> <OclExpressionCS_next_maybe> RPAREN <production_code> SLASH FILE

<protected> ::= LBRACKET PROTECTED LPAREN <OclExpressionCS> RPAREN RBRACKET <production> LBRACKET SLASH PROTECTED RBRACKET

<protected_code> ::= PROTECTED LPAREN <OclExpressionCS> RPAREN <production_code> SLASH PROTECTED

<tracecmd> ::= LBRACKET TRACE LPAREN <OclExpressionCS> RPAREN RBRACKET <production> LBRACKET SLASH TRACE RBRACKET 

<tracecmd_code> ::= TRACE LPAREN <OclExpressionCS> RPAREN <production_code> SLASH TRACE

<OclExpressionCS> ::= <PropertyCallExpCS>
                    | <VariableExpCS>
                    | <LiteralExpCS>
                    | <LetExpCS>
                    | <OclMessageExpCS>
                    | <ifExp>
                    | <invocation>

<ifExp> ::= <OclExpressionCS> QUESTION_MARK <OclExpressionCS> COLON <OclExpressionCS>

<invocation> ::= <invocation_first_argument> <before_maybe> <separator_maybe> <after_maybe>

<before> ::= BEFORE LPAREN <OclExpressionCS> RPAREN

<separator> ::= SEPARATOR LPAREN <OclExpressionCS> RPAREN

<after> ::= AFTER LPAREN <OclExpressionCS> RPAREN

<forcmd> ::= LBRACKET FOR LPAREN <arg_decl> PIPE <OclExpressionCS> RPAREN <before_maybe> <separator_maybe> <after_maybe> <guard_maybe> <init_maybe> RBRACKET <production> LBRACKET SLASH FOR RBRACKET

<forcmd_code> ::= FOR LPAREN <arg_decl> PIPE <OclExpressionCS> RPAREN <before_maybe> <separator_maybe> <after_maybe> <guard_maybe> <init_maybe> <production_code> SLASH FOR

<ifcmd> ::= LBRACKET IF LPAREN <OclExpressionCS> RPAREN RBRACKET <production> <elseif_many> <else_maybe> LBRACKET SLASH IF RBRACKET

<ifcmd_code> ::= IF LPAREN <OclExpressionCS> RPAREN <production_code> <elseif_code> <else_code_maybe> SLASH IF

<elseif> ::= LBRACKET ELSEIF LPAREN <OclExpressionCS> RPAREN RBRACKET <production>
           | LBRACKET ELSEIF LPAREN <OclExpressionCS> RPAREN RBRACKET <production> LBRACKET SLASH ELSEIF RBRACKET

<elseif_code> ::= ELSEIF LPAREN <OclExpressionCS> RPAREN <production_code>
                | ELSEIF LPAREN <OclExpressionCS> RPAREN <production_code> SLASH ELSEIF

<else> ::= LBRACKET ELSE RBRACKET <production>
         | LBRACKET ELSE RBRACKET <production> LBRACKET SLASH ELSE RBRACKET

<else_code> ::= ELSE <production_code> SLASH ELSE

<letcmd> ::= LBRACKET LET <VariableDeclarationCS> RBRACKET <production> <elselet_many> <else_maybe> LBRACKET SLASH LET RBRACKET

<letcmd_code> ::= LET <VariableDeclarationCS> <production_code> <elselet_code_many> <else_code_maybe> SLASH LET

<elselet> ::= LBRACKET ELSELET RBRACKET <VariableDeclarationCS> <production> LBRACKET SLASH ELSELET RBRACKET

<elselet_code> ::= ELSELET <VariableDeclarationCS> <production_code> SLASH ELSELET

<mode> ::= AT TEXT_EXPLICIT
         | AT CODE_EXPLICIT

LITERAL   ~ [\w]+
<literal> ~ LITERAL
<literal_code> ~ ['] LITERAL [']
COMMA     ~ ','

#
# Helpers
#
<import_decl_many> ::= <import_decl>*

<extends_decl_maybe> ::= <extends_decl>
<extends_decl_maybe> ::=

<PathNameCSlist>    ::= <PathNameCS>+ separator => COMMA

<queries_section_unit> ::= <query_defn>
                         | <query_defn_code>

<macros_section_unit> ::= <macro_defn>
                        | <macro_defn_code>

<templates_section_unit> ::=        <template_defn>
                           | <mode> <template_defn>

<production_unit> ::= <filecmd>
                    | <literal>
                    | <protected>
                    | <tracecmd>
                    | <forcmd>
                    | <ifcmd>
                    | <letcmd>
                    | LBRACKET <OclExpressionCS> SLASH RBRACKET
                    | ANYTHING

<production_code_unit> ::= <filecmd_code>
                         | <literal_code>
                         | <protected_code>
                         | <tracecmd_code>
                         | <forcmd_code>
                         | <ifcmd_code>
                         | <letcmd_code>
                         | <OclExpressionCS>
                         | ANYTHING

<VariableDeclarationCS_unit_many> ::= <VariableDeclarationCS_unit>+

<VariableDeclarationCS_unit> ::= <VariableDeclarationCS> SEMICOLON

<OclExpressionCSOrStdout> ::= <OclExpressionCS>
                            | STDOUT

<OclExpressionCS_next_maybe> ::= COMMA <OclExpressionCS>
<OclExpressionCS_next_maybe> ::=

<invocation_first_argument> ::= <PathNameCS> LPAREN <actualarglist> RPAREN
                              | SUPER


<before_maybe> ::= <before>
<before_maybe> ::=

<separator_maybe> ::= <separator>
<separator_maybe> ::=

<after_maybe> ::= <after>
<after_maybe> ::=

<guard_maybe> ::= <guard>
<guard_maybe> ::=

<init_maybe> ::= <init>
<init_maybe> ::=

<else_maybe> ::= <else>
<else_maybe> ::=

<else_code_maybe> ::= <else_code>
<else_code_maybe> ::=

<elseif_many> ::= <elseif>*

<elselet_many> ::= <elselet>*

<elselet_code_many> ::= <elselet_code>*

<overrides_maybe> ::= <overrides>
<overrides_maybe> ::=

################################################################################################################
# OCL 2.3.1 as per OMG, Object Constraint Language
#
# http://www.omg.org/spec/OCL/2.3.1/
#
# Note: MOFM2T overwrites some productions of OCL, only the subset of OCL needed for MOFM2T is putted here
################################################################################################################

LetExpCS ::= LET VariableDeclarationCS LetExpSubCS

typeCS_maybe ::= COLON typeCS
typeCS_maybe ::=

OclExpressionCS_maybe ::= EQUAL OclExpressionCS
OclExpressionCS_maybe ::=

VariableDeclarationCS ::= SimpleNameCS typeCS_maybe OclExpressionCS_maybe

VariableExpCS ::= SimpleNameCS
                | SELF

OclMessageArgumentsCS_maybe ::= OclMessageArgumentsCS
OclMessageArgumentsCS_maybe ::=

OclMessageExpCS ::= OclExpressionCS CARET CARET SimpleNameCS LPAREN OclMessageArgumentsCS_maybe RPAREN
                  | OclExpressionCS CARET SimpleNameCS LPAREN OclMessageArgumentsCS_maybe RPAREN

LiteralExpCS ::= EnumLiteralExpCS
               | CollectionLiteralExpCS
               | TupleLiteralExpCS
               | PrimitiveLiteralExpCS
               | TypeLiteralExpCS

OclMessageArgumentsCS ::= OclMessageArgCS
                        | OclMessageArgumentsCS COMMA OclMessageArgCS

EnumLiteralExpCS ::= PathNameCS TWOCOLONS SimpleNameCS

typeCS ::= PathNameCS
         | CollectionTypeCS
         | TupleTypeCS
         | PrimitiveTypeCS
         | OclTypeCS

LetExpSubCS ::= IN OclExpressionCS
              | COMMA VariableDeclarationCS LetExpSubCS

_NameChar_any ~ _NameChar*
_StringChar_any ~ _StringChar*
_WhiteSpaceChar_any ~ _WhiteSpaceChar*

_SimpleNameCS ~ _NameStartChar _NameChar_any
              | '_' [\x{27}] _StringChar_any [\x{27}]
#             | _SimpleNameCS _WhiteSpaceChar_any [\x{27}] _StringChar_any [\x{27}]
              | _SimpleNameCS                     [\x{27}] _StringChar_any [\x{27}]

SimpleNameCS ~ _SimpleNameCS

_NameStartChar ~ [A-Z]
               | '_'
               | '$'
               | [a-z]
               | [\x{C0}-\x{D6}] | [\x{D8}-\x{F6}] | [\x{F8}-\x{2FF}]
               | [\x{370}-\x{37D}] | [\x{37F}-\x{1FFF}]
               | [\x{200C}-\x{200D}] | [\x{2070}-\x{218F}] | [\x{2C00}-\x{2FEF}]
               | [\x{3001}-\x{D7FF}] | [\x{F900}-\x{FDCF}] | [\x{FDF0}-\x{FFFD}]
               | [\x{10000}-\x{EFFFF}]

TupleLiteralExpCS ::= TUPLE LCURLY VariableDeclarationListCS RCURLY

PathNameCS ::= SimpleNameCS
             | PathNameCS TWOCOLONS UnreservedSimpleNameCS

IsMarkedPreCS_maybe ::= IsMarkedPreCS
IsMarkedPreCS_maybe ::=

PropertyCallExpCS ::= OclExpressionCS DOT SimpleNameCS IsMarkedPreCS_maybe
                    | SimpleNameCS IsMarkedPreCS_maybe
                    | PathNameCS
                    | OclExpressionCS DOT PathNameCS TWOCOLONS SimpleNameCS IsMarkedPreCS_maybe

CollectionTypeCS ::= CollectionTypeIdentifierCS LPAREN typeCS RPAREN

VariableDeclarationListCS_maybe ::= VariableDeclarationListCS
VariableDeclarationListCS_maybe ::=


TupleTypeCS ::= TUPLE LPAREN VariableDeclarationListCS_maybe RPAREN

UnreservedSimpleNameCS ::= SimpleNameCS
                         | RestrictedKeywordCS

PrimitiveLiteralExpCS ::= IntegerLiteralExpCS
                        | RealLiteralExpCS
                        | StringLiteralExpCS
                        | BooleanLiteralExpCS
                        | UnlimitedNaturalLiteralExpCS
                        | NullLiteralExpCS
                        | InvalidLiteralExpCS

OclMessageArgCS ::= QUESTION_MARK typeCS_maybe
                  | OclExpressionCS

VariableDeclarationListCS ::= VariableDeclarationCS
                            | VariableDeclarationListCS COMMA VariableDeclarationCS

TypeLiteralExpCS ::= typeCS

_WhiteSpaceChar ~ [\x{09}\x{0a}\x{0c}\x{0d}\x{20}]

InvalidLiteralExpCS ::= INVALID

UnlimitedNaturalLiteralExpCS ::= <Integer Lexical Representation> 
                               | '*'

PrimitiveTypeCS ::= BOOLEAN
                  | INTEGER
                  | REAL
                  | STRING
                  | UNLIMITEDNATURAL

_StringLiteralExpCS ~ [\x{27}] _StringChar_any [\x{27}]
                    | _StringLiteralExpCS _WhiteSpaceChar_any [\x{27}] _StringChar_any [\x{27}]

StringLiteralExpCS ~ _StringLiteralExpCS

OclTypeCS ::= OCLANY
            | OCLINVALID
            | OCLMESSAGE
            | OCLVOID

CollectionLiteralPartsCS_maybe ::= CollectionLiteralPartsCS
CollectionLiteralPartsCS_maybe ::=

CollectionLiteralExpCS ::= CollectionTypeIdentifierCS LCURLY CollectionLiteralPartsCS_maybe RCURLY

_NameChar ~ _NameStartChar
          | [0-9]

CollectionLiteralPartsCS ::= CollectionLiteralPartCS
                           | CollectionLiteralPartsCS COMMA CollectionLiteralPartCS

IntegerLiteralExpCS ::= <Integer Lexical Representation>

# The lexical representation of an integer is a sequence of at least one of the 
# decimal digit characters, without a leading zero; except that a single leading zero character is required for the zero value.

DECIMALDIGIT_ANY ~ [0-9]*
DECIMALDIGIT_MANY ~ [0-9]+
<Integer Lexical Representation> ~ '0'
                                 | [1-9] DECIMALDIGIT_ANY

_StringChar ~ _Char
            | _EscapeSequence

_Char ~ [\x{20}-\x{26}]
      | [\x{28}-\x{5B}]
      | [\x{5D}-\x{D7FF}]
      | [\x{E000}-\x{FFFD}]
      | [\x{10000}-\x{10FFFF}] 

_EscapeSequence ~ '\b' #x08: backspace BS
                | '\t' #x09: horizontal tab HT
                | '\n' #x0a: linefeed LF
                | '\f' #x0c: form feed FF 78 Object Constraint Language, v2.3.1
                | '\r' #x0d: carriage return CR
                | '\"' #x22: double quote " 
                | '\' ['] #x27: single quote '
                | '\\' #x5c: backslash \
                | '\x' _Hex _Hex #x00 to #xFF
                | '\u' _Hex _Hex _Hex _Hex #x0000 to #xFFFF

_Hex ~ [0-9A-Fa-f]

CollectionTypeIdentifierCS ::= SET
                             | BAG
                             | SEQUENCE
                             | COLLECTION
                             | ORDEREDSET

RealLiteralExpCS ::= <Real Lexical Representation>

# A real literal consists of an integer part, a fractional part and an exponent 
# part. The exponent part consists of either the letter 'e' or 'E', followed optionally by a '+' or '-' letter followed by an 
# exponent integer part. Each integer part consists of a sequence of at least one of the decimal digit characters. The 
# fractional part consists of the letter DOT followed by a sequence of at least one of the decimal digit characters. Either the 
# fraction part or the exponent part may be missing but not both.

<Real Lexical Representation> ::= <Real Lexical Representation Integer Part> <Real Lexical Representation Fractional Part> <Real Lexical Representation Exponent Part>
                                | <Real Lexical Representation Integer Part>                                               <Real Lexical Representation Exponent Part>
                                | <Real Lexical Representation Integer Part> <Real Lexical Representation Fractional Part>

<Real Lexical Representation Exponent Part> ::= [eE] [-+] DECIMALDIGIT_MANY
                                              | [eE] DECIMALDIGIT_MANY

<Real Lexical Representation Fractional Part> ::= DOT DECIMALDIGIT_MANY

<Real Lexical Representation Integer Part> ::= DECIMALDIGIT_MANY

BooleanLiteralExpCS ::= TRUE
                      | FALSE

RestrictedKeywordCS ::= CollectionTypeIdentifierCS
                      | PrimitiveTypeCS
                      | OclTypeCS
                      | TUPLE

IsMarkedPreCS ::= AT PRE

CollectionLiteralPartCS ::= CollectionRangeCS
                          | OclExpressionCS

NullLiteralExpCS ::= NULL

CollectionRangeCS ::= OclExpressionCS DOT DOT OclExpressionCS

WS ~ [\s]
:discard ~ WS

# Lexeme helpers
# --------------
LPAREN ~ '('
RPAREN ~ ')'
COLON  ~ ':'
TWOCOLONS ~ '::'
LBRACKET ~ '['
RBRACKET ~ ']'
SLASH ~ '/'
PIPE ~ '|'
DOT ~ '.'
EQUAL ~ '='
LCURLY ~ '{'
RCURLY ~ '}'

MODULE ~ 'module'
EXTENDS ~ 'extends'
IMPORT ~ 'import'
QUERY ~ 'query'
PUBLIC ~ 'public'
PRIVATE ~ 'private'
TEMPLATE ~ 'template'
MACRO ~ 'macro'
OVERRIDES ~ 'overrides'
QUESTION_MARK ~ '?'
FILE ~ 'file'
PROTECTED ~ 'protected'
TRACE ~ 'trace'
BEFORE ~ 'before'
SEPARATOR ~ 'separator'
AFTER ~ 'after'
FOR ~ 'for'
IF ~ 'if'
ELSEIF ~ 'elseif'
ELSE ~ 'else'
LET ~ 'let'
ELSELET ~ 'elselet'
AT ~ '@'
TEXT_EXPLICIT ~ 'text-explicit'
CODE_EXPLICIT ~ 'code-explicit'
SEMICOLON ~ ';'
STDOUT ~ 'stdout'
SUPER ~ 'super'
SELF ~ 'self'
CARET ~ '^'
IN ~ 'in'
TUPLE ~ 'Tuple'
INVALID ~ 'invalid'
BOOLEAN ~ 'Boolean'
INTEGER ~ 'integer'
REAL ~ 'Real'
STRING ~ 'String'
UNLIMITEDNATURAL ~ 'UnlimitedNatural'
OCLANY ~ 'OclAny'
OCLINVALID ~ 'OclInvalid'
OCLMESSAGE ~ 'OclMessage'
OCLVOID ~ 'OclVoid'
SET ~ 'Set'
BAG ~ 'Bag'
SEQUENCE ~ 'Sequence'
COLLECTION ~ 'Collection'
ORDEREDSET ~ 'OrderedSet'
TRUE ~ 'true'
FALSE ~ 'false'
PRE ~ 'pre'
NULL ~ 'null'

:lexeme ~ <ANYTHING> priority => -1
ANYTHING ~ [\s\S]
