%token character_literal integer_literal floating_literal string_literal
%token identifier typedef_name enum_name CLASS_NAME template_name
%token original_namespace_name namespace_alias

%token ASM AUTO BOOL BREAK CASE CATCH CHAR CLASS CONST_CAST
%token CONST CONTINUE DEFAULT DELETE DOUBLE DO DYNAMIC_CAST ELSE
%token ENUM EXPLICIT EXPORT EXTERN FALSE FLOAT FOR FRIEND
%token GOTO IF INLINE INT LONG MUTABLE NAMESPACE NEW OPERATOR
%token PRIVATE PROTECTED PUBLIC REGISTER REINTERPRET_CAST
%token RETURN SHORT SIGNED SIZEOF STATIC_CAST STATIC STRUCT
%token SWITCH TEMPLATE THIS THROW TRUE TRY TYPEDEF TYPEID
%token TYPENAME UNION UNSIGNED USING VIRTUAL VOID VOLATILE
%token WCHAR_T WHILE

%token ADD_ASSIGN ANDAND AND_ASSIGN ARROWASTER ARROW COLONCOLON
%token DIV_ASSIGN DOTASTER DOTS EQUAL GREATEREQ LESSEQ LSH_ASSIGN
%token LSH MINUSMINUS MOD_ASSIGN MUL_ASSIGN NOTEQ OROR OR_ASSIGN
%token PLUSPLUS RSH_ASSIGN RSH SUB_ASSIGN XOR_ASSIGN

%%

/* A.3 Basic concepts */
translation_unit
  : declaration_seq
  |
  ;

/* A.4 Expressions */
primary_expression
  : literal
  | THIS
  | '(' expression ')'
  | id_expression
  ;

id_expression
  : unqualified_id
  | qualified_id
  ;

unqualified_id
  : identifier
  | operator_function_id
  | conversion_function_id
  | '~' class_name
  | template_id
  ;

qualified_id
  : COLONCOLON nested_name_specifier TEMPLATE unqualified_id
  | COLONCOLON nested_name_specifier          unqualified_id
  |            nested_name_specifier TEMPLATE unqualified_id
  |            nested_name_specifier          unqualified_id
  | COLONCOLON identifier
  | COLONCOLON operator_function_id
  | COLONCOLON template_id
  ;

nested_name_specifier
  : class_or_namespace_name COLONCOLON nested_name_specifier
  | class_or_namespace_name COLONCOLON
  | class_or_namespace_name COLONCOLON TEMPLATE nested_name_specifier
  ;

class_or_namespace_name
  : class_name
  | namespace_name
  ;

postfix_expression
  : primary_expression
  | postfix_expression '[' expression ']'
  | postfix_expression '(' expression_list ')'
  | postfix_expression '('                 ')'
  | simple_type_specifier '(' expression_list ')'
  | simple_type_specifier '('                 ')'
  | TYPENAME COLONCOLON nested_name_specifier identifier '(' expression_list ')'
  | TYPENAME COLONCOLON nested_name_specifier identifier '('                 ')'
  | TYPENAME            nested_name_specifier identifier '(' expression_list ')'
  | TYPENAME            nested_name_specifier identifier '('                 ')'
  | TYPENAME COLONCOLON nested_name_specifier TEMPLATE template_id '(' expression_list ')'
  | TYPENAME COLONCOLON nested_name_specifier TEMPLATE template_id '('                 ')'
  | TYPENAME COLONCOLON nested_name_specifier          template_id '(' expression_list ')'
  | TYPENAME COLONCOLON nested_name_specifier          template_id '('                 ')'
  | TYPENAME            nested_name_specifier TEMPLATE template_id '(' expression_list ')'
  | TYPENAME            nested_name_specifier TEMPLATE template_id '('                 ')'
  | TYPENAME            nested_name_specifier          template_id '(' expression_list ')'
  | TYPENAME            nested_name_specifier          template_id '('                 ')'
  | postfix_expression '.' TEMPLATE id_expression
  | postfix_expression '.'          id_expression
  | postfix_expression ARROW TEMPLATE id_expression
  | postfix_expression ARROW          id_expression
  | postfix_expression '.' pseudo_destructor_name
  | postfix_expression ARROW pseudo_destructor_name
  | postfix_expression PLUSPLUS
  | postfix_expression MINUSMINUS
  | DYNAMIC_CAST     '<' type_id '>' '(' expression ')'
  | STATIC_CAST      '<' type_id '>' '(' expression ')'
  | REINTERPRET_CAST '<' type_id '>' '(' expression ')'
  | CONST_CAST       '<' type_id '>' '(' expression ')'
  | TYPEID '(' expression ')'
  | TYPEID '(' type_id ')'
  ;

expression_list
  : assignment_expression
  | expression_list ',' assignment_expression
  ;

pseudo_destructor_name
  : COLONCOLON nested_name_specifier type_name COLONCOLON '~' type_name
  | COLONCOLON                       type_name COLONCOLON '~' type_name
  |            nested_name_specifier type_name COLONCOLON '~' type_name
  |                                  type_name COLONCOLON '~' type_name
  | COLONCOLON nested_name_specifier TEMPLATE template_id COLONCOLON '~' type_name
  |            nested_name_specifier TEMPLATE template_id COLONCOLON '~' type_name
  | COLONCOLON nested_name_specifier '~' type_name
  | COLONCOLON                       '~' type_name
  |            nested_name_specifier '~' type_name
  |                                  '~' type_name
  ;

unary_expression
  : postfix_expression
  | PLUSPLUS cast_expression
  | MINUSMINUS cast_expression
  | unary_operator cast_expression
  | SIZEOF unary_expression
  | SIZEOF '(' type_id ')'
  | new_expression
  | delete_expression
  ;

unary_operator
  : '*'
  | '&'
  | '+'
  | '-'
  | '!'
  | '~'
  ;

new_expression
  : COLONCOLON NEW new_placement new_type_id new_initializer
  | COLONCOLON NEW new_placement new_type_id
  | COLONCOLON NEW               new_type_id new_initializer
  | COLONCOLON NEW               new_type_id
  |            NEW new_placement new_type_id new_initializer
  |            NEW new_placement new_type_id
  |            NEW               new_type_id new_initializer
  |            NEW               new_type_id
  | COLONCOLON NEW new_placement '(' type_id ')' new_initializer
  | COLONCOLON NEW new_placement '(' type_id ')'
  | COLONCOLON NEW               '(' type_id ')' new_initializer
  | COLONCOLON NEW               '(' type_id ')'
  |            NEW new_placement '(' type_id ')' new_initializer
  |            NEW new_placement '(' type_id ')'
  |            NEW '(' type_id ')' new_initializer
  |            NEW '(' type_id ')'
  ;

new_placement
  : '(' expression_list ')'
  ;

new_type_id
  : type_specifier_seq new_declarator
  | type_specifier_seq
  ;

new_declarator
  : ptr_operator new_declarator
  | ptr_operator
  | direct_new_declarator
  ;

direct_new_declarator
  : '[' expression ']'
  | direct_new_declarator '[' constant_expression ']'
  ;

new_initializer
  : '(' expression_list ')'
  | '('                 ')'
  ;

delete_expression
  : COLONCOLON DELETE         cast_expression
  |            DELETE         cast_expression
  | COLONCOLON DELETE '[' ']' cast_expression
  |            DELETE '[' ']' cast_expression
  ;

cast_expression
  : unary_expression
  | '(' type_id ')' cast_expression
  ;

pm_expression
  : cast_expression
  | pm_expression DOTASTER cast_expression
  | pm_expression ARROWASTER cast_expression
  ;

multiplicative_expression
  : pm_expression
  | multiplicative_expression '*' pm_expression
  | multiplicative_expression '/' pm_expression
  | multiplicative_expression '%' pm_expression
  ;

additive_expression
  : multiplicative_expression
  | additive_expression '+' multiplicative_expression
  | additive_expression '-' multiplicative_expression
  ;

shift_expression
  : additive_expression
  | shift_expression LSH additive_expression
  | shift_expression RSH additive_expression
  ;

relational_expression
  : shift_expression
  | relational_expression '<' shift_expression
  | relational_expression '>' shift_expression
  | relational_expression LESSEQ shift_expression
  | relational_expression GREATEREQ shift_expression
  ;

equality_expression
  : relational_expression
  | equality_expression EQUAL relational_expression
  | equality_expression NOTEQ relational_expression
  ;

and_expression
  : equality_expression
  | and_expression '&' equality_expression
  ;

exclusive_or_expression
  : and_expression
  | exclusive_or_expression '^' and_expression
  ;

inclusive_or_expression
  : exclusive_or_expression
  | inclusive_or_expression '|' exclusive_or_expression
  ;

logical_and_expression
  : inclusive_or_expression
  | logical_and_expression ANDAND inclusive_or_expression
  ;

logical_or_expression
  : logical_and_expression
  | logical_or_expression OROR logical_and_expression
  ;

conditional_expression
  : logical_or_expression
  | logical_or_expression '?' expression ':' assignment_expression
  ;

assignment_expression
  : conditional_expression
  | logical_or_expression assignment_operator assignment_expression
  | throw_expression
  ;

assignment_operator
  : '='
  | MUL_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | ADD_ASSIGN
  | SUB_ASSIGN
  | RSH_ASSIGN
  | LSH_ASSIGN
  | AND_ASSIGN
  | XOR_ASSIGN
  |  OR_ASSIGN
  ;

expression
  : assignment_expression
  | expression ',' assignment_expression
  ;

constant_expression
  : conditional_expression
  ;

/* A.5 Statements */
statement
  : labeled_statement
  | expression_statement
  | compound_statement
  | selection_statement
  | iteration_statement
  | jump_statement
  | declaration_statement
  | try_block
  ;

labeled_statement
  : identifier ':' statement
  | CASE constant_expression ':' statement
  | DEFAULT ':' statement
  ;

expression_statement
  : expression ';'
  |            ';'
  ;

compound_statement
  : '{' statement_seq '}'
  | '{'               '}'
  ;

statement_seq
  : statement
  | statement_seq statement
  ;

selection_statement
  : IF '(' condition ')' statement
  | IF '(' condition ')' statement ELSE statement
  | SWITCH '(' condition ')' statement
  ;

condition
  : expression
  | type_specifier_seq declarator '=' assignment_expression
  ;

iteration_statement
  : WHILE '(' condition ')' statement
  | DO statement WHILE '(' expression ')' ';'
  | FOR '(' for_init_statement condition ';' expression ')' statement
  | FOR '(' for_init_statement condition ';'            ')' statement
  | FOR '(' for_init_statement           ';' expression ')' statement
  | FOR '(' for_init_statement           ';'            ')' statement
  ;

for_init_statement
  : expression_statement
  | simple_declaration
  ;

jump_statement
  : BREAK ';'
  | CONTINUE ';'
  | RETURN expression ';'
  | RETURN            ';'
  | GOTO identifier ';'
  ;

declaration_statement
  : block_declaration
  ;

/* A.6 Declarations */
declaration_seq
  : declaration
  | declaration_seq declaration
  ;

declaration
  : block_declaration
  | function_definition
  | template_declaration
  | explicit_instantiation
  | explicit_specialization
  | linkage_specification
  | namespace_definition
  ;

block_declaration
  : simple_declaration
  | asm_definition
  | namespace_alias_definition
  | using_declaration
  | using_directive
  ;

simple_declaration
  : decl_specifier_seq init_declarator_list ';'
  |                    init_declarator_list ';'
  | decl_specifier_seq                      ';'
  |                                         ';'
  ;

decl_specifier
  : storage_class_specifier
  | type_specifier
  | function_specifier
  | FRIEND
  | TYPEDEF
  ;

decl_specifier_seq
  : decl_specifier_seq decl_specifier
  |                    decl_specifier
  ;

storage_class_specifier
  : AUTO
  | REGISTER
  | STATIC
  | EXTERN
  | MUTABLE
  ;

function_specifier
  : INLINE
  | VIRTUAL
  | EXPLICIT
  ;

type_specifier
  : simple_type_specifier
  | class_specifier
  | enum_specifier
  | elaborated_type_specifier
  | cv_qualifier
  ;

simple_type_specifier
  : COLONCOLON nested_name_specifier type_name
  | COLONCOLON                       type_name
  |            nested_name_specifier type_name
  |                                  type_name
  | COLONCOLON nested_name_specifier TEMPLATE template_id
  |            nested_name_specifier TEMPLATE template_id
  | CHAR
  | WCHAR_T
  | BOOL
  | SHORT
  | INT
  | LONG
  | SIGNED
  | UNSIGNED
  | FLOAT
  | DOUBLE
  | VOID
  ;

type_name
  : class_name
  | enum_name
  | typedef_name
  ;

elaborated_type_specifier
  : class_key COLONCOLON nested_name_specifier identifier
  | class_key            nested_name_specifier identifier
  | class_key COLONCOLON                       identifier
  | class_key                                  identifier
  | class_key COLONCOLON nested_name_specifier TEMPLATE template_id
  | class_key            nested_name_specifier TEMPLATE template_id
  | class_key COLONCOLON                       TEMPLATE template_id
  | class_key COLONCOLON nested_name_specifier          template_id
  | class_key                                  TEMPLATE template_id
  | class_key COLONCOLON                                template_id
  | class_key            nested_name_specifier          template_id
  | class_key                                           template_id
  | ENUM COLONCOLON nested_name_specifier identifier
  | ENUM            nested_name_specifier identifier
  | ENUM COLONCOLON                       identifier
  | ENUM                                  identifier
  | TYPENAME COLONCOLON nested_name_specifier identifier
  | TYPENAME            nested_name_specifier identifier
  | TYPENAME COLONCOLON nested_name_specifier TEMPLATE template_id
  | TYPENAME            nested_name_specifier TEMPLATE template_id
  | TYPENAME COLONCOLON nested_name_specifier          template_id
  | TYPENAME            nested_name_specifier          template_id
  ;

enum_specifier
  : ENUM identifier '{' enumerator_list '}'
  | ENUM identifier '{'                 '}'
  | ENUM            '{' enumerator_list '}'
  | ENUM            '{'                 '}'
  ;

enumerator_list
  : enumerator_definition
  | enumerator_list ',' enumerator_definition
  ;

enumerator_definition
  : enumerator
  | enumerator '='  constant_expression
  ;

enumerator
  : identifier
  ;

namespace_name
  : original_namespace_name
  | namespace_alias
  ;

namespace_definition
  : named_namespace_definition
  | unnamed_namespace_definition
  ;

named_namespace_definition
  : original_namespace_definition
  | extension_namespace_definition
  ;

original_namespace_definition
  : NAMESPACE identifier '{' namespace_body '}'
  ;

extension_namespace_definition
  : NAMESPACE original_namespace_name '{' namespace_body '}'
  ;

unnamed_namespace_definition
  : NAMESPACE '{' namespace_body '}'
  ;

namespace_body
  : declaration_seq
  |
  ;

namespace_alias_definition
  : NAMESPACE identifier '=' qualified_namespace_specifier ';'
  ;

qualified_namespace_specifier
  : COLONCOLON nested_name_specifier namespace_name
  |            nested_name_specifier namespace_name
  | COLONCOLON                       namespace_name
  |                                  namespace_name
  ;

using_declaration
  : USING TYPENAME COLONCOLON nested_name_specifier unqualified_id ';'
  | USING          COLONCOLON nested_name_specifier unqualified_id ';'
  | USING TYPENAME            nested_name_specifier unqualified_id ';'
  | USING                     nested_name_specifier unqualified_id ';'
  | USING          COLONCOLON                       unqualified_id ';'
  ;

using_directive
  : USING NAMESPACE COLONCOLON nested_name_specifier namespace_name ';'
  | USING NAMESPACE            nested_name_specifier namespace_name ';'
  | USING NAMESPACE COLONCOLON                       namespace_name ';'
  | USING NAMESPACE                                  namespace_name ';'
  ;

asm_definition
  : ASM '(' string_literal ')' ';'
  ;

linkage_specification
  : EXTERN string_literal '{' declaration_seq '}'
  | EXTERN string_literal '{'                 '}'
  | EXTERN string_literal declaration
  ;

/* A.7 Declarators */
init_declarator_list
  : init_declarator
  | init_declarator_list ',' init_declarator
  ;

init_declarator
  : declarator initializer
  | declarator
  ;

declarator
  : direct_declarator
  | ptr_operator declarator
  ;

direct_declarator
  : declarator_id
  | direct_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq exception_specification
  | direct_declarator '(' parameter_declaration_clause ')'                   exception_specification
  | direct_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq
  | direct_declarator '(' parameter_declaration_clause ')'
  | direct_declarator '[' constant_expression ']'
  | direct_declarator '['                     ']'
  | '(' declarator ')'
  ;

ptr_operator
  : '*' cv_qualifier_seq
  | '*'
  | '&'
  | COLONCOLON nested_name_specifier '*' cv_qualifier_seq
  |            nested_name_specifier '*' cv_qualifier_seq
  | COLONCOLON nested_name_specifier '*'
  |            nested_name_specifier '*'
  ;

cv_qualifier_seq
  : cv_qualifier cv_qualifier_seq
  | cv_qualifier
  ;

cv_qualifier
  : CONST
  | VOLATILE
  ;

declarator_id
  : id_expression
  | COLONCOLON nested_name_specifier type_name
  |            nested_name_specifier type_name
  | COLONCOLON                       type_name
  |                                  type_name
  ;

type_id
  : type_specifier_seq abstract_declarator
  | type_specifier_seq
  ;

type_specifier_seq
  : type_specifier type_specifier_seq
  | type_specifier
  ;

abstract_declarator
  : ptr_operator abstract_declarator
  | ptr_operator
  | direct_abstract_declarator
  ;

direct_abstract_declarator
  : direct_abstract_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq exception_specification
  | direct_abstract_declarator '(' parameter_declaration_clause ')'                   exception_specification
  | direct_abstract_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq
  | direct_abstract_declarator '(' parameter_declaration_clause ')'
  |                            '(' parameter_declaration_clause ')' cv_qualifier_seq exception_specification
  |                            '(' parameter_declaration_clause ')'                   exception_specification
  |                            '(' parameter_declaration_clause ')' cv_qualifier_seq
  |                            '(' parameter_declaration_clause ')'
  | direct_abstract_declarator '[' constant_expression ']'
  |                            '[' constant_expression ']'
  | direct_abstract_declarator '['                     ']'
  |                            '['                     ']'
  | '(' abstract_declarator ')'
  ;

parameter_declaration_clause
  : parameter_declaration_list DOTS
  |                            DOTS
  | parameter_declaration_list
  |
  | parameter_declaration_list ',' DOTS
  ;

parameter_declaration_list
  : parameter_declaration
  | parameter_declaration_list ',' parameter_declaration
  ;

parameter_declaration
  : decl_specifier_seq declarator
  | decl_specifier_seq declarator '=' assignment_expression
  | decl_specifier_seq abstract_declarator
  | decl_specifier_seq
  | decl_specifier_seq abstract_declarator '=' assignment_expression
  | decl_specifier_seq  '=' assignment_expression
  ;

function_definition
  :                    declarator                  function_body
  |                    declarator ctor_initializer function_body
  | decl_specifier_seq declarator                  function_body
  | decl_specifier_seq declarator ctor_initializer function_body
  |                    declarator function_try_block
  | decl_specifier_seq declarator function_try_block
  ;

function_body
  : compound_statement
  ;

initializer
  : '=' initializer_clause
  | '(' expression_list ')'
  ;

initializer_clause
  : assignment_expression
  | '{' initializer_list ',' '}'
  | '{' initializer_list     '}'
  | '{'                      '}'
  ;

initializer_list
  :                      initializer_clause
  | initializer_list ',' initializer_clause
  ;

/* A.8 Classes */
class_name
  : CLASS_NAME
  | template_id
  ;

class_specifier
  : class_head '{' member_specification '}'
  | class_head '{'                      '}'
  ;

class_head
  : class_key identifier base_clause
  | class_key                base_clause
  | class_key
  | class_key nested_name_specifier identifier base_clause
  | class_key nested_name_specifier class_name
  | class_key nested_name_specifier template_id base_clause
  | class_key                       template_id base_clause
  ;

class_key
  : CLASS
  | STRUCT
  | UNION
  ;

member_specification
  : member_declaration member_specification
  | member_declaration
  | access_specifier ':' member_specification
  | access_specifier ':'
  ;

member_declaration
  : decl_specifier_seq member_declarator_list ';'
  | decl_specifier_seq                        ';'
  |                    member_declarator_list ';'
  |                                           ';'
  | function_definition ';'
  | function_definition
  | COLONCOLON nested_name_specifier TEMPLATE unqualified_id ';'
  | COLONCOLON nested_name_specifier          unqualified_id ';'
  |            nested_name_specifier TEMPLATE unqualified_id ';'
  |            nested_name_specifier          unqualified_id ';'
  | using_declaration
  | template_declaration
  ;

member_declarator_list
  : member_declarator
  | member_declarator_list ',' member_declarator
  ;

member_declarator
  : declarator
  | declarator constant_initializer
  | identifier ':' constant_expression
  |            ':' constant_expression
  ;

constant_initializer
  : '=' constant_expression
  ;

/* A.9 Dervied classes */
base_clause
  : ':' base_specifier_list
  ;

base_specifier_list
  : base_specifier
  | base_specifier_list ',' base_specifier
  ;

base_specifier
  : COLONCOLON nested_name_specifier class_name
  | COLONCOLON                       class_name
  |            nested_name_specifier class_name
  |                                  class_name
  | VIRTUAL access_specifier COLONCOLON nested_name_specifier class_name
  | VIRTUAL access_specifier COLONCOLON                       class_name
  | VIRTUAL access_specifier            nested_name_specifier class_name
  | VIRTUAL access_specifier                                  class_name
  | VIRTUAL                  COLONCOLON nested_name_specifier class_name
  | VIRTUAL                  COLONCOLON                       class_name
  | VIRTUAL                             nested_name_specifier class_name
  | VIRTUAL                                                   class_name
  | access_specifier VIRTUAL COLONCOLON nested_name_specifier class_name
  | access_specifier VIRTUAL COLONCOLON                       class_name
  | access_specifier VIRTUAL            nested_name_specifier class_name
  | access_specifier VIRTUAL                                  class_name
  | access_specifier         COLONCOLON nested_name_specifier class_name
  | access_specifier         COLONCOLON                       class_name
  | access_specifier                    nested_name_specifier class_name
  | access_specifier                                          class_name
  ;

access_specifier
  : PRIVATE  { $$ = PRIVATE; }
  | PROTECTED { $$ = PROTECTED; }
  | PUBLIC { $$ = PUBLIC; }
  ;


/* A.10 Special member functions */
conversion_function_id
  : OPERATOR conversion_type_id
  ;

conversion_type_id
  : type_specifier_seq conversion_declarator
  | type_specifier_seq
  ;

conversion_declarator
  : ptr_operator conversion_declarator
  | ptr_operator
  ;

ctor_initializer
  : ':' mem_initializer_list
  ;

mem_initializer_list
  : mem_initializer
  | mem_initializer ',' mem_initializer_list
  ;

mem_initializer
  : mem_initializer_id '(' expression_list ')'
  | mem_initializer_id '('                 ')'
  ;

mem_initializer_id
  : COLONCOLON nested_name_specifier class_name
  | COLONCOLON                       class_name
  |            nested_name_specifier class_name
  |                                  class_name
  | identifier
  ;

/* A.11 Overloading */
operator_function_id
  : OPERATOR operator
  | OPERATOR operator '<' template_argument_list '>'
  | OPERATOR operator '<'                        '>'
  ;

operator
  : NEW
  | DELETE
  | NEW '[' ']'
  | DELETE '[' ']'
  | '+'
  | '-'
  | '*'
  | '/'
  | '%'
  | '^'
  | '&'
  | '|'
  | '~'
  | '!'
  | '='
  | '<'
  | '>'
  | ADD_ASSIGN
  | SUB_ASSIGN
  | MUL_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | XOR_ASSIGN
  | AND_ASSIGN
  | OR_ASSIGN
  | LSH
  | RSH
  | LSH_ASSIGN
  | RSH_ASSIGN
  | EQUAL
  | NOTEQ
  | LESSEQ
  | GREATEREQ
  | ANDAND
  | OROR
  | PLUSPLUS
  | MINUSMINUS
  | ','
  | ARROWASTER
  | ARROW
  | '(' ')'
  | '[' ']'
  ;

/* A.12 Templates */
template_declaration
  : EXPORT TEMPLATE '<' template_parameter_list '>' declaration
  |        TEMPLATE '<' template_parameter_list '>' declaration
  ;

template_parameter_list
  : template_parameter
  | template_parameter_list ',' template_parameter
  ;

template_parameter
  : type_parameter
  | parameter_declaration
  ;

type_parameter
  : CLASS identifier
  | CLASS
  | CLASS identifier '=' type_id
  | CLASS            '=' type_id
  | TYPENAME identifier
  | TYPENAME
  | TYPENAME identifier '=' type_id
  | TYPENAME            '=' type_id
  | TEMPLATE '<' template_parameter_list '>' CLASS identifier
  | TEMPLATE '<' template_parameter_list '>' CLASS
  | TEMPLATE '<' template_parameter_list '>' CLASS identifier '=' id_expression
  | TEMPLATE '<' template_parameter_list '>' CLASS            '=' id_expression
  ;

template_id
  : template_name '<' template_argument_list '>'
  | template_name '<'                        '>'
  ;

template_argument_list
  : template_argument
  | template_argument_list ',' template_argument
  ;

template_argument
  : assignment_expression
  | type_id
  | id_expression
  ;

explicit_instantiation
  : TEMPLATE declaration
  ;

explicit_specialization
  : TEMPLATE '<' '>' declaration
  ;

/* A.13 Exception handling */
try_block
  : TRY compound_statement handler_seq
  ;

function_try_block
  : TRY ctor_initializer function_body handler_seq
  | TRY                  function_body handler_seq
  ;

handler_seq
  : handler handler_seq
  | handler
  ;

handler
  : CATCH '(' exception_declaration ')' compound_statement
  ;

exception_declaration
  : type_specifier_seq declarator
  | type_specifier_seq abstract_declarator
  | type_specifier_seq
  | DOTS
  ;

throw_expression
  : THROW assignment_expression
  | THROW
  ;

exception_specification
  : THROW '(' type_id_list ')'
  | THROW '('              ')'
  ;

type_id_list
  : type_id
  | type_id_list ',' type_id
  ;

/* (a part of) A.2 Lexical conventions */
literal
  : integer_literal
  | character_literal
  | floating_literal
  | string_literal
  | boolean_literal
  ;

boolean_literal
  : FALSE
  | TRUE
  ;

%%
