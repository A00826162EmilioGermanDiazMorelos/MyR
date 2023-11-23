
import ply.lex as lex
import ply.yacc as yacc

################ LEXER ################
# Lista de tokens y palabras reservadas
reserved = {
    'Program': 'PROGRAM',
    'main': 'MAIN',
    'int': 'INT',
    'flot': 'FLOT',
    'char': 'CHAR',
    'void': 'VOID',
    'vars': 'VARS',
    'function': 'FUNCTION',
    'return': 'RETURN',
    'read': 'READ',
    'write': 'WRITE',
    'If': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'while': 'WHILE',
    'for': 'FOR',
    'to': 'TO',
    'do': 'DO'
}

tokens = ['PLUS', 'MINUS', 'MULT', 'DIV', 'EQUALS', 'AND', 'OR', 'LESS_THAN', 'MORE_THAN',
          'EQUAL_TO', 'NUMBER', 'ID', 'LEFTPAREN', 'RIGHTPAREN', 'LEFTCURLY', 'RIGHTCURLY',
          'LEFTSQUARE', 'RIGHTSQUARE', 'PUNT_COMA', 'COMA'] + list(reserved.values())

# Ignorar espacios en blanco y saltos de línea
t_ignore = ' \t'

# Expresiones regulares para operaciones aritméticas
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULT = r'\*'
t_DIV = r'/'
t_EQUALS = r'='

# Expresiones regulares para operaciones lógicas
t_AND = r'&'
t_OR = r'\|'
t_LESS_THAN = r'<'
t_MORE_THAN = r'>'
t_EQUAL_TO = r'=='

# Expresiones regulares para abrir y cerrar paréntesis, corchetes y llaves, y signos de puntuación
t_LEFTPAREN = r'\('
t_RIGHTPAREN = r'\)'
t_LEFTCURLY = r'\{'
t_RIGHTCURLY = r'\}'
t_LEFTSQUARE = r'\['
t_RIGHTSQUARE = r'\]'
t_PUNT_COMA = r';'
t_COMA = r','

# Expresión regular para reconocer números enteros
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')  # Checa palabras reservadas
    return t

# Manejo de errores
def t_error(t):
    print(f"Caracter ilegal: {t.value[0]}")
    t.lexer.skip(1)

# Construir el lexer
lexer = lex.lex()

################ Parser ################

precedence = (
    ('left', 'OR'),             # Asociativo a la izquierda
    ('left', 'AND'),            # Asociativo a la izquierda
    ('nonassoc', 'LESS_THAN', 'MORE_THAN', 'EQUAL_TO'),  # No asociativo
    ('left', 'PLUS', 'MINUS'),  # Asociativo a la izquierda
    ('left', 'MULT', 'DIV'),    # Asociativo a la izquierda
    ('right', 'UMINUS'),        # Asociativo a la derecha para el unario negativo, si existe
)
# Estructura general del programa

class ProgramNode:
    def __init__(self, name, vars, functions, main):
        self.name = name
        self.vars = vars
        self.functions = functions
        self.main = main

def p_program(p):
    '''program : PROGRAM ID PUNT_COMA vars function MAIN block'''
    # p[0] = ('program', p[2], p[4], p[5], p[7])
    p[0] = ProgramNode(p[2], p[4], p[5], p[7])

# Estructura para las variables
class VarDeclarationNode:
    def __init__(self, var_type, var_name):
        self.var_type = var_type
        self.var_name = var_name

class ArrayDeclarationNode:
    def __init__(self, var_type, var_name, array_size):
        self.var_type = var_type
        self.var_name = var_name
        self.array_size = array_size

class VarListNode:
    def __init__(self):
        self.variables = []

    def add_variable(self, var):
        self.variables.append(var)

def p_vars(p):
    '''vars : VARS var_list
            | empty'''
    # p[0] = ('vars', p[2])
    p[0] = VarListNode() if p[1] != 'empty' else p[2]

def p_var_list(p):
    '''var_list : var_list COMA var
                | var PUNT_COMA'''
    if len(p) == 4:
        # Lista de variables, separadas por comas
        # p[0] = p[1] + [p[3]]
        p[1].add_variable(p[3])
        p[0] = p[1]
    else:
        # Última variable de la lista, terminando con punto y coma
        # p[0] = [p[1]]
        var_list = VarListNode()
        var_list.add_variable(p[1])
        p[0] = var_list

def p_var(p):
    '''var : type ID
           | type LEFTSQUARE NUMBER RIGHTSQUARE ID'''
    if len(p) == 3:
        # Declaración de una variable simple
        # p[0] = ('var_declaration', p[1], p[2])
        p[0] = VarDeclarationNode(p[1], p[2])
    else:
        # Declaración de un arreglo
        # p[0] = ('array_declaration', p[1], p[5], p[3])
        p[0] = ArrayDeclarationNode(p[1], p[5], p[3])

def p_type(p):
    '''type : INT
            | FLOT
            | CHAR
            | VOID'''
    p[0] = p[1]

def p_empty(p):
    'empty :'
    pass

# Estructura para las funciones

def p_function(p):
    '''function : FUNCTION type ID LEFTPAREN param_list RIGHTPAREN vars block'''
    p[0] = ('function_declaration', p[2], p[3], p[5], p[7])

def p_param_list(p):
    '''param_list : param_list COMA param
                  | param
                  | empty'''
    if len(p) == 4:
        # Si hay más de un parámetro, se concatena el nuevo parámetro a la lista existente
        p[0] = p[1] + [p[3]]
    elif len(p) == 2:
        # Si solo hay un parámetro, se inicia una nueva lista con ese parámetro
        p[0] = [p[1]]
    else:
        # Si no hay parámetros (caso 'empty'), se devuelve una lista vacía
        p[0] = []

def p_param(p):
    '''param : INT ID
             | FLOT ID
             | CHAR ID'''
    p[0] = ('param', p[1], p[2])


# Estructura para el main y lo que va dentro de las funciones o ciclos

def p_block(p):
    '''block : LEFTCURLY statement_list RIGHTCURLY'''
    p[0] = ('block', p[2])

def p_statement_list(p):
    '''statement_list : statement_list statement
                      | statement'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_statement(p):
    '''statement : assignment
                 | function_call
                 | return_statement
                 | read_statement
                 | write_statement
                 | decision_statement
                 | repetition_statement
                 | vars'''
    p[0] = p[1]

# Estructura para el estatuto de asignación

def p_assignment(p):
    '''assignment : ID EQUALS expr PUNT_COMA
                  | ID LEFTSQUARE expr RIGHTSQUARE EQUALS expr PUNT_COMA'''
    if len(p) == 5:
        # Asignación a una variable escalar
        p[0] = ('assignment', 'scalar', p[1], p[3])
    else:
        # Asignación a un elemento de un arreglo
        p[0] = ('assignment', 'array_element', p[1], p[3], p[6])

# Estructura para el estatuto de llamar a una función

def p_function_call(p):
    '''function_call : ID LEFTPAREN arg_list RIGHTPAREN PUNT_COMA
                     | ID LEFTPAREN RIGHTPAREN PUNT_COMA'''
    if len(p) == 6:
        # Llamada a función con argumentos
        p[0] = ('function_call', p[1], p[3])
    else:
        # Llamada a función sin argumentos
        p[0] = ('function_call', p[1], [])

def p_arg_list(p):
    '''arg_list : arg_list COMA expr
                | expr'''
    if len(p) == 4:
        # Lista de argumentos con más de un argumento
        p[0] = p[1] + [p[3]]
    else:
        # Un solo argumento
        p[0] = [p[1]]

# Estructura para el estatuto de retorno

def p_return_statement(p):
    '''return_statement : RETURN expr PUNT_COMA
                        | RETURN PUNT_COMA'''
    if len(p) == 4:
        # Instrucción de retorno con una expresión
        p[0] = ('return_statement', p[2])
    else:
        # Instrucción de retorno sin expresión (retorna vacío o void)
        p[0] = ('return_statement', None)

# Estructura para el estatuto de lectura

def p_read_statement(p):
    '''read_statement : READ LEFTPAREN ID RIGHTPAREN PUNT_COMA'''
    p[0] = ('read_statement', p[3])

# Estructura para el estatuto de escritura

def p_write_statement(p):
    '''write_statement : WRITE LEFTPAREN expr RIGHTPAREN PUNT_COMA
                       | WRITE LEFTPAREN RIGHTPAREN PUNT_COMA'''
    if len(p) == 6:
        # Instrucción de escritura con una expresión
        p[0] = ('write_statement', p[3])
    else:
        # Instrucción de escritura sin expresión (escribe una línea en blanco o un mensaje predeterminado)
        p[0] = ('write_statement', None)

# Estructura para el estatuto de decisión (If)

def p_decision_statement(p):
    '''decision_statement : IF LEFTPAREN expr RIGHTPAREN THEN block
                          | IF LEFTPAREN expr RIGHTPAREN THEN block ELSE block'''
    if len(p) == 7:
        # Instrucción IF-THEN sin ELSE
        p[0] = ('if_then', p[3], p[6])
    else:
        # Instrucción IF-THEN-ELSE
        p[0] = ('if_then_else', p[3], p[6], p[8])

# Estructura para el estatuto de repetición (while o for)

def p_repetition_statement(p):
    '''repetition_statement : FOR ID EQUALS expr TO expr DO block
                            | WHILE LEFTPAREN expr RIGHTPAREN DO block'''
    if p[1] == "for":
        # Bucle FOR
        p[0] = ('for_loop', p[2], p[4], p[6], p[8])
    else:
        # Bucle WHILE
        p[0] = ('while_loop', p[3], p[6])

def p_expr(p):
    '''expr : expr PLUS expr
            | expr MINUS expr
            | expr MULT expr
            | expr DIV expr
            | expr AND expr
            | expr OR expr
            | expr LESS_THAN expr
            | expr MORE_THAN expr
            | expr EQUAL_TO expr
            | LEFTPAREN expr RIGHTPAREN
            | MINUS expr %prec UMINUS
            | NUMBER
            | ID'''
    if len(p) == 4:
        if p[1] == '(':
            # Expresión entre paréntesis
            p[0] = p[2]
        else:
            # Operación binaria
            p[0] = ('binary_operation', p[2], p[1], p[3])
    elif len(p) == 3:
        # Operación unaria (menos unario)
        p[0] = ('unary_operation', p[1], p[2])
    else:
        # Número literal o identificador
        p[0] = p[1]

def p_error(p):
    if p:
        print(f"Syntax error at token {p.type} - Line: {p.lineno}, Position: {p.lexpos}")
    else:
        print("Syntax error at EOF")


parser = yacc.yacc()

def parse(program):
    return parser.parse(program, lexer=lexer)

