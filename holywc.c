#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <time.h>

// For strdup on some platforms
#define _POSIX_C_SOURCE 200809L

/*
char* strdup(const char* s) {
    size_t len = strlen(s) + 1;
    char* new = malloc(len);
    if (new == NULL) return NULL;
    return (char*)memcpy(new, s, len);
}
*/

// Token types
typedef enum {
    TOKEN_IDENTIFIER,
    TOKEN_NUMBER,
    TOKEN_STRING,
    TOKEN_OPERATOR,
    TOKEN_KEYWORD,
    TOKEN_ARROW,      // ->
    TOKEN_LBRACE,     // {
    TOKEN_RBRACE,     // }
    TOKEN_LPAREN,     // (
    TOKEN_RPAREN,     // )
    TOKEN_COMMA,      // ,
    TOKEN_SEMICOLON,  // ;
    TOKEN_COLON,      // :
    TOKEN_EOF,
} TokenType;

// Token structure
typedef struct {
    TokenType type;
    char* value;
    int line;
    int column;
} Token;

// AST node types
typedef enum {
    NODE_PROGRAM,
    NODE_FUNCTION,
    NODE_PARAM,
    NODE_TYPE,
    NODE_BLOCK,
    NODE_RETURN,
    NODE_BINARY_EXPR,
    NODE_IDENTIFIER,
    NODE_NUMBER,
    // Implement an import
    NODE_CALL,
} NodeType;

// AST node structure
typedef struct ASTNode {
    NodeType type;
    char* value;
    struct ASTNode** children;
    int childCount;
    int childCapacity;
    
    // Additional fields for specific node types
    char* returnType;
    char* paramType;
} ASTNode;

// Lexer state
typedef struct {
    char* source;
    size_t sourceLength;
    size_t position;
    int line;
    int column;
} Lexer;

// Parser state
typedef struct {
    Lexer* lexer;
    Token currentToken;
} Parser;

// Symbol table for name mangling/optimization
typedef struct SymbolEntry {
    char* originalName;
    char* mangledName;
    char* type;
    struct SymbolEntry* next;
} SymbolEntry;

typedef struct {
    SymbolEntry* entries;
    int count;
    int nextId;
} SymbolTable;

// Function prototypes
Lexer* createLexer(char* source);
void freeLexer(Lexer* lexer);
Token getNextToken(Lexer* lexer);

Parser* createParser(Lexer* lexer);
void freeParser(Parser* parser);
ASTNode* parse(Parser* parser);
ASTNode* parseFunction(Parser* parser);
ASTNode* parseBlock(Parser* parser);
ASTNode* parseStatement(Parser* parser);
ASTNode* parseExpression(Parser* parser);
ASTNode* parseTerm(Parser* parser);
ASTNode* parseFactor(Parser* parser);

SymbolTable* createSymbolTable();
void freeSymbolTable(SymbolTable* table);
char* getOptimizedName(SymbolTable* table, char* originalName, char* type, const char* context);

void generateWat(ASTNode* ast, SymbolTable* symbolTable, FILE* output);
void generateWatFunction(ASTNode* funcNode, SymbolTable* symbolTable, FILE* output);
void generateWatExpression(ASTNode* exprNode, SymbolTable* symbolTable, const char* currentFunction, FILE* output);

void freeAST(ASTNode* node) {
    if (!node) return;
    
    free(node->value);
    free(node->returnType);
    free(node->paramType);
    
    for (int i = 0; i < node->childCount; i++) {
        freeAST(node->children[i]);
    }
    
    free(node->children);
    free(node);
}

// Main function
int main(int argc, char** argv) {
    clock_t start = clock();
    const double timeout = 5.0; // 5 second timeout

    if (argc < 3) {
        printf("Usage: %s <input_file> <output_file>\n", argv[0]);
        return 1;
    }

    // Read input file
    FILE* inputFile = fopen(argv[1], "r");
    if (!inputFile) {
        printf("Error: Could not open input file %s\n", argv[1]);
        return 1;
    }

    fseek(inputFile, 0, SEEK_END);
    size_t fileSize = ftell(inputFile);
    fseek(inputFile, 0, SEEK_SET);

    char* source = (char*)malloc(fileSize + 1);
    if (!source) {
        printf("Error: Memory allocation failed\n");
        fclose(inputFile);
        return 1;
    }

    size_t bytesRead = fread(source, 1, fileSize, inputFile);
    source[bytesRead] = '\0';
    fclose(inputFile);

    // Create lexer and parser
    Lexer* lexer = createLexer(source);
    Parser* parser = createParser(lexer);
    
    // Parse the source code
    ASTNode* ast = NULL;
    while (parser->currentToken.type != TOKEN_EOF) {
        if ((double)(clock() - start) / CLOCKS_PER_SEC > timeout) {
            printf("Error: Parser timeout\n");
            break;
        }
        ast = parse(parser);
    }

    if (!ast) {
        printf("Error: Failed to parse input\n");
        freeParser(parser);
        freeLexer(lexer);
        free(source);
        return 1;
    }

    printf("AST has %d top-level nodes\n", ast->childCount);
    for (int i = 0; i < ast->childCount; i++) {
        printf("Node %d: type=%d, value=%s\n", i, ast->children[i]->type, 
               ast->children[i]->value ? ast->children[i]->value : "NULL");
    }
    
    // Create symbol table for optimization
    SymbolTable* symbolTable = createSymbolTable();
    
    // Generate WAT code
    FILE* outputFile = fopen(argv[2], "w");
    if (!outputFile) {
        printf("Error: Could not open output file %s\n", argv[2]);
        freeParser(parser);
        freeLexer(lexer);
        free(source);
        return 1;
    }
    
    generateWat(ast, symbolTable, outputFile);

    if (fflush(outputFile) != 0) {
        printf("Error: Failed to write output\n");
    }

    if (ast) {
        freeAST(ast);
    }
    
    // Clean up
    freeSymbolTable(symbolTable);
    freeParser(parser);
    freeLexer(lexer);
    free(source);
    
    fclose(outputFile);
    printf("Translation completed.\n");
    return 0;
}

// Lexer implementation
Lexer* createLexer(char* source) {
    Lexer* lexer = (Lexer*)malloc(sizeof(Lexer));
    lexer->source = source;
    lexer->sourceLength = strlen(source);
    lexer->position = 0;
    lexer->line = 1;
    lexer->column = 1;
    return lexer;
}

void freeLexer(Lexer* lexer) {
    free(lexer);
}

// Check if a string is a keyword
bool isKeyword(const char* str) {
    if (str == NULL) return false;
    
    return strcmp(str, "fn") == 0 ||
           strcmp(str, "return") == 0 ||
           strcmp(str, "if") == 0 ||
           strcmp(str, "else") == 0 ||
           strcmp(str, "while") == 0 ||
           strcmp(str, "for") == 0 ||
           strcmp(str, "let") == 0 ||
           strcmp(str, "const") == 0 ||
           strcmp(str, "import") == 0 ||
           strcmp(str, "export") == 0 ||
           strcmp(str, "i32") == 0 ||
           strcmp(str, "i64") == 0 ||
           strcmp(str, "f32") == 0 ||
           strcmp(str, "f64") == 0;
}

Token getNextToken(Lexer* lexer) {
    Token token;
    token.line = lexer->line;
    token.column = lexer->column;
    token.value = NULL;
    
    // Skip whitespace and comments
    while (lexer->position < lexer->sourceLength) {
        // Skip whitespace
        if (isspace(lexer->source[lexer->position])) {
            if (lexer->source[lexer->position] == '\n') {
                lexer->line++;
                lexer->column = 1;
            } else {
                lexer->column++;
            }
            lexer->position++;
            continue;
        }
        
        // Skip single-line comments
        if (lexer->position + 1 < lexer->sourceLength && 
            lexer->source[lexer->position] == '/' && 
            lexer->source[lexer->position + 1] == '/') {
            lexer->position += 2;
            lexer->column += 2;
            
            while (lexer->position < lexer->sourceLength && 
                   lexer->source[lexer->position] != '\n') {
                lexer->position++;
                lexer->column++;
            }
            continue;
        }
        
        // Skip multi-line comments
        if (lexer->position + 1 < lexer->sourceLength && 
            lexer->source[lexer->position] == '/' && 
            lexer->source[lexer->position + 1] == '*') {
            lexer->position += 2;
            lexer->column += 2;
            
            while (lexer->position + 1 < lexer->sourceLength && 
                   !(lexer->source[lexer->position] == '*' && 
                     lexer->source[lexer->position + 1] == '/')) {
                if (lexer->source[lexer->position] == '\n') {
                    lexer->line++;
                    lexer->column = 1;
                } else {
                    lexer->column++;
                }
                lexer->position++;
            }
            
            if (lexer->position + 1 < lexer->sourceLength) {
                lexer->position += 2;
                lexer->column += 2;
            }
            continue;
        }
        
        break;
    }
    
    // Check for EOF
    if (lexer->position >= lexer->sourceLength) {
        token.type = TOKEN_EOF;
        printf("Token: type=%d, value='%s', line=%d, col=%d\n", 
               token.type, token.value ? token.value : "NULL", token.line, token.column);
        return token;
    }
    
    // Check for identifiers and keywords
    if (isalpha(lexer->source[lexer->position]) || lexer->source[lexer->position] == '_') {
        size_t start = lexer->position;
        while (lexer->position < lexer->sourceLength && 
               (isalnum(lexer->source[lexer->position]) || lexer->source[lexer->position] == '_')) {
            lexer->position++;
            lexer->column++;
        }
        
        size_t length = lexer->position - start;
        token.value = (char*)malloc(length + 1);
        strncpy(token.value, &lexer->source[start], length);
        token.value[length] = '\0';
        
        // Check if it's a keyword
        if (isKeyword(token.value)) {
            token.type = TOKEN_KEYWORD;
        } else {
            token.type = TOKEN_IDENTIFIER;
        }
        
        printf("Token: type=%d, value='%s', line=%d, col=%d\n", 
               token.type, token.value ? token.value : "NULL", token.line, token.column);
        return token;
    }
    
    // Check for numbers
    if (isdigit(lexer->source[lexer->position])) {
        size_t start = lexer->position;
        while (lexer->position < lexer->sourceLength && isdigit(lexer->source[lexer->position])) {
            lexer->position++;
            lexer->column++;
        }
        
        // Handle decimal point
        if (lexer->position < lexer->sourceLength && lexer->source[lexer->position] == '.') {
            lexer->position++;
            lexer->column++;
            
            while (lexer->position < lexer->sourceLength && isdigit(lexer->source[lexer->position])) {
                lexer->position++;
                lexer->column++;
            }
        }
        
        size_t length = lexer->position - start;
        token.value = (char*)malloc(length + 1);
        strncpy(token.value, &lexer->source[start], length);
        token.value[length] = '\0';
        token.type = TOKEN_NUMBER;
        
        printf("Token: type=%d, value='%s', line=%d, col=%d\n", 
               token.type, token.value ? token.value : "NULL", token.line, token.column);
        return token;
    }
    
    // Check for string literals
    if (lexer->source[lexer->position] == '"') {
        lexer->position++;
        lexer->column++;
        
        size_t start = lexer->position;
        while (lexer->position < lexer->sourceLength && 
               lexer->source[lexer->position] != '"') {
            // Handle escape sequences
            if (lexer->source[lexer->position] == '\\' && 
                lexer->position + 1 < lexer->sourceLength) {
                lexer->position += 2;
                lexer->column += 2;
            } else {
                if (lexer->source[lexer->position] == '\n') {
                    lexer->line++;
                    lexer->column = 1;
                } else {
                    lexer->column++;
                }
                lexer->position++;
            }
        }
        
        size_t length = lexer->position - start;
        token.value = (char*)malloc(length + 1);
        strncpy(token.value, &lexer->source[start], length);
        token.value[length] = '\0';
        token.type = TOKEN_STRING;
        
        // Skip closing quote
        if (lexer->position < lexer->sourceLength) {
            lexer->position++;
            lexer->column++;
        }
        
        printf("Token: type=%d, value='%s', line=%d, col=%d\n", 
               token.type, token.value ? token.value : "NULL", token.line, token.column);
        return token;
    }
    
    // Check for arrow ->
    if (lexer->position + 1 < lexer->sourceLength && 
        lexer->source[lexer->position] == '-' && 
        lexer->source[lexer->position + 1] == '>') {
        token.type = TOKEN_ARROW;
        token.value = strdup("->");
        lexer->position += 2;
        lexer->column += 2;
        
        printf("Token: type=%d, value='%s', line=%d, col=%d\n", 
               token.type, token.value ? token.value : "NULL", token.line, token.column);
        return token;
    }
    
    // Check for special characters
    switch (lexer->source[lexer->position]) {
        case '{':
            token.type = TOKEN_LBRACE;
            token.value = strdup("{");
            lexer->position++;
            lexer->column++;
            break;
        case '}':
            token.type = TOKEN_RBRACE;
            token.value = strdup("}");
            lexer->position++;
            lexer->column++;
            break;
        case '(':
            token.type = TOKEN_LPAREN;
            token.value = strdup("(");
            lexer->position++;
            lexer->column++;
            break;
        case ')':
            token.type = TOKEN_RPAREN;
            token.value = strdup(")");
            lexer->position++;
            lexer->column++;
            break;
        case ',':
            token.type = TOKEN_COMMA;
            token.value = strdup(",");
            lexer->position++;
            lexer->column++;
            break;
        case ';':
            token.type = TOKEN_SEMICOLON;
            token.value = strdup(";");
            lexer->position++;
            lexer->column++;
            break;
        case ':':
            token.type = TOKEN_COLON;
            token.value = strdup(":");
            lexer->position++;
            lexer->column++;
            break;
        default:
            // Handle operators
            if (strchr("+-*/=<>!&|^%", lexer->source[lexer->position])) {
                token.type = TOKEN_OPERATOR;
                token.value = (char*)malloc(2);
                token.value[0] = lexer->source[lexer->position];
                token.value[1] = '\0';
                
                lexer->position++;
                lexer->column++;
                
                // Handle two-character operators
                if (lexer->position < lexer->sourceLength) {
                    char nextChar = lexer->source[lexer->position];
                    if ((token.value[0] == '=' && nextChar == '=') ||
                        (token.value[0] == '!' && nextChar == '=') ||
                        (token.value[0] == '<' && nextChar == '=') ||
                        (token.value[0] == '>' && nextChar == '=') ||
                        (token.value[0] == '&' && nextChar == '&') ||
                        (token.value[0] == '|' && nextChar == '|')) {
                        
                        free(token.value);
                        token.value = (char*)malloc(3);
                        token.value[0] = lexer->source[lexer->position - 1];
                        token.value[1] = nextChar;
                        token.value[2] = '\0';
                        
                        lexer->position++;
                        lexer->column++;
                    }
                }
                
                printf("Token: type=%d, value='%s', line=%d, col=%d\n", 
                       token.type, token.value ? token.value : "NULL", token.line, token.column);
                return token;
            } else {
                // Unknown character
                token.type = TOKEN_OPERATOR;
                token.value = (char*)malloc(2);
                token.value[0] = lexer->source[lexer->position];
                token.value[1] = '\0';
                
                lexer->position++;
                lexer->column++;
            }
            break;
    }
    
    printf("Token: type=%d, value='%s', line=%d, col=%d\n", 
           token.type, token.value ? token.value : "NULL", token.line, token.column);
    return token;
}

// Symbol table implementation for name optimization
SymbolTable* createSymbolTable() {
    SymbolTable* table = (SymbolTable*)malloc(sizeof(SymbolTable));
    table->entries = NULL;
    table->count = 0;
    table->nextId = 0;
    return table;
}

void freeSymbolTable(SymbolTable* table) {
    SymbolEntry* current = table->entries;
    while (current != NULL) {
        SymbolEntry* next = current->next;
        free(current->originalName);
        free(current->mangledName);
        free(current->type);
        free(current);
        current = next;
    }
    free(table);
}

// Generate short, optimized names
char* getOptimizedName(SymbolTable* table, char* originalName, char* type, const char* context) {
    if (!table || !originalName) {
        return strdup("error");
    }

    // Create unique key by combining context and name
    char uniqueKey[256];
    if (context) {
        snprintf(uniqueKey, sizeof(uniqueKey), "%s.%s", context, originalName);
        originalName = uniqueKey;
    }

    // Check for duplicate function names
    if (context == NULL) { // This is a function declaration
        SymbolEntry* current = table->entries;
        while (current != NULL) {
            if (current->originalName && strcmp(current->originalName, originalName) == 0 && 
                current->type && strcmp(current->type, "function") == 0) {
                fprintf(stderr, "Error: Duplicate function name '%s'\n", originalName);
                return strdup("error");
            }
            current = current->next;
        }
    }

    // Check if name already exists in table
    SymbolEntry* current = table->entries;
    while (current != NULL) {
        if (current->originalName && strcmp(current->originalName, originalName) == 0) {
            return strdup(current->mangledName);
        }
        current = current->next;
    }

    // Generate a new name according to pattern: a-z, a0-z0, a1-z1, etc.
    char buffer[16];
    int id = table->nextId++;
    
    if (id < 26) {
        snprintf(buffer, sizeof(buffer), "%c", 'a' + id);
    } else {
        int letter_index = (id - 26) % 26;
        int number = (id - 26) / 26;
        snprintf(buffer, sizeof(buffer), "%c%d", 'a' + letter_index, number);
    }

    // Add to symbol table
    SymbolEntry* entry = (SymbolEntry*)malloc(sizeof(SymbolEntry));
    if (!entry) return strdup("error");

    entry->originalName = strdup(originalName);
    entry->mangledName = strdup(buffer);
    entry->type = type ? strdup(type) : (context ? strdup("param") : strdup("function"));
    entry->next = table->entries;
    
    if (!entry->originalName || !entry->mangledName || !entry->type) {
        free(entry->originalName);
        free(entry->mangledName);
        free(entry->type);
        free(entry);
        return strdup("error");
    }

    table->entries = entry;
    table->count++;

    return strdup(entry->mangledName);
}

// Parser implementation
Parser* createParser(Lexer* lexer) {
    Parser* parser = (Parser*)malloc(sizeof(Parser));
    parser->lexer = lexer;
    parser->currentToken = getNextToken(lexer);
    return parser;
}

void freeParser(Parser* parser) {
    free(parser->currentToken.value);
    free(parser);
}

// Helper function to create AST nodes
ASTNode* createNode(NodeType type, char* value) {
    ASTNode* node = (ASTNode*)malloc(sizeof(ASTNode));
    if (!node) return NULL;

    node->type = type;
    node->value = value ? strdup(value) : NULL;
    node->children = NULL;
    node->childCount = 0;
    node->childCapacity = 0;
    node->returnType = NULL;
    node->paramType = NULL;

    if (value && !node->value) {
        free(node);
        return NULL;
    }

    return node;
}

// Helper function to add a child to an AST node
void addChild(ASTNode* parent, ASTNode* child) {
    if (parent->childCount >= parent->childCapacity) {
        parent->childCapacity = parent->childCapacity == 0 ? 4 : parent->childCapacity * 2;
        parent->children = (ASTNode**)realloc(parent->children, parent->childCapacity * sizeof(ASTNode*));
    }
    parent->children[parent->childCount++] = child;
}

// Helper function to consume a token and advance
void consume(Parser* parser) {
    free(parser->currentToken.value);
    parser->currentToken = getNextToken(parser->lexer);
}

// Helper function to expect a specific token type
bool expect(Parser* parser, TokenType type) {
    if (parser->currentToken.type != type) {
        printf("Error: Expected token type %d, got %d at line %d, column %d\n",
               type, parser->currentToken.type, parser->currentToken.line, parser->currentToken.column);
        return false;
    }
    return true;
}

// Helper function to consume a token if it matches the expected type
bool match(Parser* parser, TokenType type) {
    if (parser->currentToken.type == type) {
        consume(parser);
        return true;
    }
    return false;
}

// Parse the entire program
ASTNode* parse(Parser* parser) {
    printf("Parsing program\n");
    ASTNode* program = createNode(NODE_PROGRAM, NULL);
    
    while (parser->currentToken.type != TOKEN_EOF) {
        printf("Top-level token: type=%d, value='%s'\n", 
               parser->currentToken.type, 
               parser->currentToken.value ? parser->currentToken.value : "NULL");
        
        // Check for function declarations
        if (parser->currentToken.type == TOKEN_KEYWORD && 
            parser->currentToken.value != NULL &&
            strcmp(parser->currentToken.value, "fn") == 0) {
            printf("Found function declaration\n");
            ASTNode* funcNode = parseFunction(parser);
            if (funcNode) {
                addChild(program, funcNode);
            } else {
                printf("Failed to parse function\n");
                // Skip to next function declaration or EOF
                while (parser->currentToken.type != TOKEN_EOF) {
                    if (parser->currentToken.type == TOKEN_KEYWORD && 
                        parser->currentToken.value != NULL &&
                        strcmp(parser->currentToken.value, "fn") == 0) {
                        break;
                    }
                    consume(parser);
                }
            }
        } else {
            printf("Error: Unexpected token at line %d, column %d. Expected 'fn' but got '%s'\n",
                   parser->currentToken.line, parser->currentToken.column,
                   parser->currentToken.value ? parser->currentToken.value : "unknown");
            consume(parser);
        }
    }
    
    return program;
}

// Parse a function declaration
ASTNode* parseFunction(Parser* parser) {
    printf("Parsing function, current token: type=%d, value='%s'\n", 
           parser->currentToken.type, 
           parser->currentToken.value ? parser->currentToken.value : "NULL");
    
    // Expect 'fn' keyword
    if (parser->currentToken.type != TOKEN_KEYWORD || 
        parser->currentToken.value == NULL || 
        strcmp(parser->currentToken.value, "fn") != 0) {
        printf("Error: Expected 'fn' keyword at line %d, column %d\n",
               parser->currentToken.line, parser->currentToken.column);
        return NULL;
    }
    consume(parser);
    
    // Expect function name
    if (parser->currentToken.type != TOKEN_IDENTIFIER) {
        printf("Error: Expected function name at line %d, column %d\n",
               parser->currentToken.line, parser->currentToken.column);
        return NULL;
    }
    
    printf("Function name: %s\n", parser->currentToken.value);
    ASTNode* funcNode = createNode(NODE_FUNCTION, parser->currentToken.value);
    consume(parser);
    
    // Expect opening parenthesis
    if (parser->currentToken.type != TOKEN_LPAREN) {
        printf("Error: Expected '(' after function name at line %d, column %d\n",
               parser->currentToken.line, parser->currentToken.column);
        return NULL;
    }
    consume(parser); // Consume the '('
    
    // Parse parameters
    if (parser->currentToken.type != TOKEN_RPAREN) {
        do {
            // Check for parameter name
            if (parser->currentToken.type == TOKEN_IDENTIFIER) {
                ASTNode* paramNode = createNode(NODE_PARAM, parser->currentToken.value);
                consume(parser); // Consume parameter name
                
                // Check for type annotation (not required in your example)
                if (parser->currentToken.type == TOKEN_COLON) {
                    consume(parser); // Consume ':'
                    if (parser->currentToken.type == TOKEN_KEYWORD) {
                        paramNode->paramType = strdup(parser->currentToken.value);
                        consume(parser); // Consume type
                    }
                } else {
                    // Default to i32 if no type is specified
                    paramNode->paramType = strdup("i32");
                }
                
                addChild(funcNode, paramNode);
            } else {
                printf("Error: Expected parameter name at line %d, column %d\n",
                       parser->currentToken.line, parser->currentToken.column);
                return NULL;
            }
            
            // Check for comma or closing parenthesis
            if (parser->currentToken.type == TOKEN_COMMA) {
                consume(parser); // Consume ','
                // After a comma, we must have another parameter
                if (parser->currentToken.type != TOKEN_IDENTIFIER) {
                    printf("Error: Expected parameter name after ',' at line %d, column %d\n",
                           parser->currentToken.line, parser->currentToken.column);
                    return NULL;
                }
            } else if (parser->currentToken.type != TOKEN_RPAREN) {
                printf("Error: Expected ',' or ')' at line %d, column %d\n",
                       parser->currentToken.line, parser->currentToken.column);
                return NULL;
            }
        } while (parser->currentToken.type != TOKEN_RPAREN);
    }
    
    // Consume closing parenthesis
    if (parser->currentToken.type != TOKEN_RPAREN) {
        printf("Error: Expected ')' at line %d, column %d\n",
               parser->currentToken.line, parser->currentToken.column);
        return NULL;
    }
    consume(parser); // Consume the ')'
    
    // Check for return type
    if (parser->currentToken.type == TOKEN_ARROW) {
        consume(parser); // Consume '->'
        if (parser->currentToken.type == TOKEN_KEYWORD) {
            funcNode->returnType = strdup(parser->currentToken.value);
            consume(parser); // Consume return type
        } else {
            printf("Error: Expected return type after '->' at line %d, column %d\n",
                   parser->currentToken.line, parser->currentToken.column);
            return NULL;
        }
    } else {
        // Default to no return value
        funcNode->returnType = NULL;
    }
    
    // Parse function body
    if (parser->currentToken.type != TOKEN_LBRACE) {
        printf("Error: Expected '{' at line %d, column %d\n",
               parser->currentToken.line, parser->currentToken.column);
        return NULL;
    }
    
    ASTNode* blockNode = parseBlock(parser);
    if (blockNode) {
        addChild(funcNode, blockNode);
    } else {
        printf("Error: Failed to parse function body\n");
        return NULL;
    }
    
    return funcNode;
}

// Parse a block of statements
ASTNode* parseBlock(Parser* parser) {
    // Expect opening brace
    if (parser->currentToken.type != TOKEN_LBRACE) {
        printf("Error: Expected '{' at line %d, column %d\n",
               parser->currentToken.line, parser->currentToken.column);
        return NULL;
    }
    consume(parser); // Consume '{'
    
    ASTNode* blockNode = createNode(NODE_BLOCK, NULL);
    
    // Parse statements until closing brace
    while (parser->currentToken.type != TOKEN_RBRACE && 
           parser->currentToken.type != TOKEN_EOF) {
        ASTNode* stmtNode = parseStatement(parser);
        if (stmtNode) {
            addChild(blockNode, stmtNode);
        } else {
            printf("Error: Failed to parse statement at line %d, column %d\n",
                   parser->currentToken.line, parser->currentToken.column);
            // Skip to next statement or closing brace
            while (parser->currentToken.type != TOKEN_SEMICOLON && 
                   parser->currentToken.type != TOKEN_RBRACE && 
                   parser->currentToken.type != TOKEN_EOF) {
                consume(parser);
            }
            if (parser->currentToken.type == TOKEN_SEMICOLON) {
                consume(parser); // Consume ';'
            }
        }
    }
    
    // Expect closing brace
    if (parser->currentToken.type != TOKEN_RBRACE) {
        printf("Error: Expected '}' at line %d, column %d\n",
               parser->currentToken.line, parser->currentToken.column);
        return NULL;
    }
    consume(parser); // Consume '}'
    
    return blockNode;
}

// Parse a statement
ASTNode* parseStatement(Parser* parser) {
    // Return statement
    if (parser->currentToken.type == TOKEN_KEYWORD && 
        strcmp(parser->currentToken.value, "return") == 0) {
        consume(parser);
        
        ASTNode* returnNode = createNode(NODE_RETURN, NULL);
        
        // Parse return expression (if any)
        if (parser->currentToken.type != TOKEN_SEMICOLON) {
            ASTNode* exprNode = parseExpression(parser);
            if (exprNode) {
                addChild(returnNode, exprNode);
            } else {
                printf("Error: Failed to parse return expression at line %d, column %d\n",
                       parser->currentToken.line, parser->currentToken.column);
                return NULL;
            }
        }
        
        // Expect semicolon
        if (parser->currentToken.type != TOKEN_SEMICOLON) {
            printf("Error: Expected ';' after return statement at line %d, column %d\n",
                   parser->currentToken.line, parser->currentToken.column);
            return NULL;
        }
        consume(parser);
        
        return returnNode;
    }
    
    // Other statement types would go here
    
    // Default to expression statement
    ASTNode* exprNode = parseExpression(parser);
    
    // Expect semicolon
    if (!expect(parser, TOKEN_SEMICOLON)) {
        return NULL;
    }
    consume(parser);
    
    return exprNode;
}

// Parse an expression
ASTNode* parseExpression(Parser* parser) {
    return parseTerm(parser);
}

// Parse a term (addition/subtraction)
ASTNode* parseTerm(Parser* parser) {
    ASTNode* left = parseFactor(parser);
    
    while (parser->currentToken.type == TOKEN_OPERATOR && 
           (parser->currentToken.value[0] == '+' || 
            parser->currentToken.value[0] == '-')) {
        char* op = strdup(parser->currentToken.value);
        consume(parser);
        
        ASTNode* right = parseFactor(parser);
        
        ASTNode* binaryNode = createNode(NODE_BINARY_EXPR, op);
        free(op);
        
        addChild(binaryNode, left);
        addChild(binaryNode, right);
        
        left = binaryNode;
    }
    
    return left;
}

// Parse a factor (multiplication/division)
ASTNode* parseFactor(Parser* parser) {
    ASTNode* left = NULL;
    
    // Parse primary expression
    if (parser->currentToken.type == TOKEN_NUMBER) {
        left = createNode(NODE_NUMBER, parser->currentToken.value);
        consume(parser);
    } else if (parser->currentToken.type == TOKEN_IDENTIFIER) {
        char* identName = strdup(parser->currentToken.value);
        consume(parser);
        
        // Check if this is a function call
        if (parser->currentToken.type == TOKEN_LPAREN) {
            // This is a function call
            ASTNode* callNode = createNode(NODE_CALL, identName);
            free(identName); // Free the duplicated string
            
            consume(parser); // Consume '('
            
            // Parse arguments
            if (parser->currentToken.type != TOKEN_RPAREN) {
                do {
                    ASTNode* argExpr = parseExpression(parser);
                    if (argExpr) {
                        addChild(callNode, argExpr);
                    } else {
                        printf("Error: Failed to parse function argument at line %d, column %d\n",
                               parser->currentToken.line, parser->currentToken.column);
                        return NULL;
                    }
                    
                    // Expect comma or closing parenthesis
                    if (parser->currentToken.type == TOKEN_COMMA) {
                        consume(parser); // Consume ','
                    } else if (parser->currentToken.type != TOKEN_RPAREN) {
                        printf("Error: Expected ',' or ')' at line %d, column %d\n",
                               parser->currentToken.line, parser->currentToken.column);
                        return NULL;
                    }
                } while (parser->currentToken.type != TOKEN_RPAREN);
            }
            
            // Consume closing parenthesis
            if (parser->currentToken.type != TOKEN_RPAREN) {
                printf("Error: Expected ')' at line %d, column %d\n",
                       parser->currentToken.line, parser->currentToken.column);
                return NULL;
            }
            consume(parser); // Consume ')'
            
            left = callNode;
        } else {
            // This is a simple variable reference
            left = createNode(NODE_IDENTIFIER, identName);
            free(identName); // Free the duplicated string
        }
    } else if (parser->currentToken.type == TOKEN_LPAREN) {
        consume(parser);
        left = parseExpression(parser);
        if (!expect(parser, TOKEN_RPAREN)) {
            return NULL;
        }
        consume(parser);
    } else {
        printf("Error: Unexpected token in expression at line %d, column %d\n",
               parser->currentToken.line, parser->currentToken.column);
        consume(parser);
        return createNode(NODE_NUMBER, "0"); // Error recovery
    }
    
    return left;
}

// WAT code generation
void generateWat(ASTNode* ast, SymbolTable* symbolTable, FILE* output) {
    // Write WAT module header
    fprintf(output, "(module\n");

    // TODO: Implemet a generate code for each "import"
    
    // Generate code for each function
    for (int i = 0; i < ast->childCount; i++) {
        if (ast->children[i]->type == NODE_FUNCTION) {
            generateWatFunction(ast->children[i], symbolTable, output);
        }
    }
    
    // Write WAT module footer
    fprintf(output, ")\n");
}

// Generate WAT code for a function
void generateWatFunction(ASTNode* funcNode, SymbolTable* symbolTable, FILE* output) {
    if (!funcNode || !symbolTable || !output || !funcNode->value) return;

    char* optimizedFuncName = getOptimizedName(symbolTable, funcNode->value, funcNode->returnType, NULL);
    if (!optimizedFuncName || strcmp(optimizedFuncName, "error") == 0) {
        if (optimizedFuncName) free(optimizedFuncName);
        return;
    }

    fprintf(output, "  (func (export \"%s\")", optimizedFuncName);
    free(optimizedFuncName);

    // Parameters
    for (int i = 0; i < funcNode->childCount - 1; i++) {
        ASTNode* param = funcNode->children[i];
        if (param && param->type == NODE_PARAM && param->value) {
            char* optimizedParamName = getOptimizedName(symbolTable, param->value, param->paramType, funcNode->value);
            if (!optimizedParamName || strcmp(optimizedParamName, "error") == 0) {
                if (optimizedParamName) free(optimizedParamName);
                continue;
            }
            fprintf(output, " (param $%s i32)", optimizedParamName);
            free(optimizedParamName);
        }
    }

    // Return type
    if (funcNode->returnType) {
        fprintf(output, " (result i32)");
    }

    fprintf(output, "\n");

    // Function body
    if (funcNode->childCount > 0) {
        ASTNode* body = funcNode->children[funcNode->childCount - 1];
        if (body && body->type == NODE_BLOCK) {
            for (int i = 0; i < body->childCount; i++) {
                ASTNode* stmt = body->children[i];
                if (!stmt) continue;

                if (stmt->type == NODE_RETURN) {
                    if (stmt->childCount > 0) {
                        generateWatExpression(stmt->children[0], symbolTable, funcNode->value, output);
                    }
                    fprintf(output, "    return\n");
                }
            }
        }
    }

    fprintf(output, "  )\n");
}

// Generate WAT code for an expression
void generateWatExpression(ASTNode* exprNode, SymbolTable* symbolTable, const char* currentFunction, FILE* output) {
    if (!exprNode || !output) {
        fprintf(stderr, "Error: Null node or output file in generateWatExpression\n");
        return;
    }

    printf("Generating expression of type %d\n", exprNode->type);

    switch (exprNode->type) {
        case NODE_NUMBER:
            printf("Generating number: %s\n", exprNode->value);
            fprintf(output, "    i32.const %s\n", exprNode->value);
            break;
            
        case NODE_IDENTIFIER: {
            char* optimizedName = getOptimizedName(symbolTable, exprNode->value, NULL, currentFunction);
            if (optimizedName) {
                fprintf(output, "    local.get $%s\n", optimizedName);
                free(optimizedName);
            }
            break;
        }
            
        case NODE_BINARY_EXPR: {
            printf("Generating binary expression: %s\n", exprNode->value);
            if (exprNode->childCount < 2) {
                fprintf(stderr, "Error: Binary expression needs 2 operands\n");
                break;
            }
            
            generateWatExpression(exprNode->children[0], symbolTable, currentFunction, output);
            generateWatExpression(exprNode->children[1], symbolTable, currentFunction, output);
            
            if (strcmp(exprNode->value, "+") == 0) {
                fprintf(output, "    i32.add\n");
            } else if (strcmp(exprNode->value, "-") == 0) {
                fprintf(output, "    i32.sub\n");
            } else if (strcmp(exprNode->value, "*") == 0) {
                fprintf(output, "    i32.mul\n");
            } else if (strcmp(exprNode->value, "/") == 0) {
                fprintf(output, "    i32.div_s\n");
            } else {
                fprintf(output, "    ;; Unsupported operator: %s\n", exprNode->value);
            }
            break;
        }
            
        case NODE_CALL: {
            printf("Generating function call: %s\n", exprNode->value);
            char* optimizedFuncName = getOptimizedName(symbolTable, exprNode->value, NULL, NULL);
            if (!optimizedFuncName) break;
            
            // Generate arguments
            for (int i = 0; i < exprNode->childCount; i++) {
                generateWatExpression(exprNode->children[i], symbolTable, currentFunction, output);
            }
            
            fprintf(output, "    call $%s\n", optimizedFuncName);
            free(optimizedFuncName);
            break;
        }
            
        default:
            fprintf(stderr, "Error: Unsupported node type %d in generateWatExpression\n", exprNode->type);
            fprintf(output, "    ;; Unsupported node type %d\n", exprNode->type);
            break;
    }
    
    fflush(output);
}