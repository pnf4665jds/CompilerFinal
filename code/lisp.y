%code requires {
#include <stdio.h>
#include <string.h>

extern int yyleng;

// struct for variables
struct Var {
	char name[30];
	int value;
};

// stack for store declared variables
struct Var vars[100];
int varStackTop;	// top of stack
int varNameLen;

// struct for run-time parameters
struct Para {
	int level;		// level of hierarchy
	char name[30];
	int value;
};

// stack for storing parameters at run-time
struct Para runParas[100];
int currentLevel;	// current level of hierarchy
int runParaTop;		// top of stack

/***************************/
// tree node declaration
/***************************/
// enum the node type
enum NodeType { If_Node, Num_Op_Node, Fun_Node, Num_Node, Var_Node, Exps_Node };

// struct for tree node
struct TreeNode {
	int nodeType;				// record the type of node
	int value;					// record value if this is Num_Node
	char str[30];				// record variable if this is Var_Node or operator if this is Num_Op_Node
	struct Var paras[30];		// record parameter if this is Fun_Node
	int paraNum;				// record number of parameters
	struct TreeNode* childA;	// pointer to child node
	struct TreeNode* childB;
	struct TreeNode* childC;
};

typedef struct TreeNode treeNode;
typedef treeNode* nodePtr;


// struct for function
struct Fun {
	char name[30];
	nodePtr entry;		// point to function body's root
};

// stack for store declared functions
struct Fun funs[100];
int funStackTop;	// top of stack
int funNameLen;

// stack for function parameters, use for temporary storing
struct Var paras[30];
int paraStackTop;	// top of stack without assigned value
int paraValueTop;	// top of stack with assigned value

// declare functions
void yyerror(const char* message);
nodePtr createNumNode(int num);
nodePtr createVarNode(char* name);
nodePtr createFunNode(nodePtr ptrA);
nodePtr createNumOpNode(char op, nodePtr ptrA, nodePtr ptrB, nodePtr ptrC);
nodePtr createExpsNode(nodePtr ptrA, nodePtr ptrB);
nodePtr createIfNode(nodePtr ptrA, nodePtr ptrB, nodePtr ptrC);
int preOrder(nodePtr current, char op);
void createVar(char* name, int value);
void createFun(char* name, nodePtr entry);
nodePtr assignParaValue(nodePtr ptrA);
nodePtr getFun(char* name);
int getVar(char* name);
int getPara(char* name);
}

%error-verbose
%union {
	nodePtr nodePtr;
	int ival;			// for integer token
	char* str;			// for string token
	union ExpType {		// for token "exp"
		int ival;
		char* str;
	}expType;
}

%token<ival> BOOL_VAL
%token<ival> NUMBER
%token<str> ID
%token MOD AND OR NOT DEFINE FUN IF PRINT_NUM PRINT_BOOL

%type<nodePtr> exp
%type<nodePtr> exps
%type<nodePtr> num_op
%type<nodePtr> logical_op
%type<nodePtr> if_exp
%type<nodePtr> test_exp
%type<nodePtr> then_exp
%type<nodePtr> else_exp
%type<nodePtr> fun_body
%type<nodePtr> fun_exp
%type<nodePtr> fun_call
%type<str> variable
%type<str> fun_name

%%

program: program stmt
	| stmt
	;

stmt: exp
	| print_stmt
	| def_stmt
	;


print_stmt: '(' PRINT_NUM exp ')'		{ printf("%d\n", preOrder($3, ' ')); }
| '(' PRINT_BOOL exp ')'				{ printf("#%c\n", (preOrder($3, ' ') ? 't' : 'f')); }
;

exp: BOOL_VAL		{ $$ = createNumNode($1); }
| NUMBER			{ $$ = createNumNode($1); }
| num_op			{ $$ = $1; }
| logical_op		{ $$ = $1; }
| if_exp			{ $$ = $1; }
| variable			{ $$ = createVarNode($1); }
//| fun_exp{  }
| fun_call			{ $$ = $1; }
;

num_op: '(' '+' exp exp exps ')'	{ $$ = createNumOpNode('+', $3, $4, $5); }
| '(' '-' exp exp ')'				{ $$ = createNumOpNode('-', $3, $4, NULL); }
| '(' '*' exp exp exps ')'			{ $$ = createNumOpNode('*', $3, $4, $5); }
| '(' '/' exp exp ')'				{ $$ = createNumOpNode('/', $3, $4, NULL); }
| '(' MOD exp exp ')'				{ $$ = createNumOpNode('%', $3, $4, NULL); }
| '(' '>' exp exp ')'				{ $$ = createNumOpNode('>', $3, $4, NULL); }
| '(' '<' exp exp ')'				{ $$ = createNumOpNode('<', $3, $4, NULL); }
| '(' '=' exp exp exps ')'			{ $$ = createNumOpNode('=', $3, $4, $5); }
;

exps: exp exps		{ $$ = createExpsNode($1, $2); }
| /* lambda */		{ $$ = NULL; }
;

logical_op: '(' AND exp exp exps ')'		{ $$ = createNumOpNode('&', $3, $4, $5); }
| '(' OR exp exp exps ')'					{ $$ = createNumOpNode('|', $3, $4, $5); }
| '(' NOT exp ')'							{ $$ = createNumOpNode('!', $3, NULL, NULL); }
;

if_exp: '(' IF test_exp then_exp else_exp ')'	{ $$ = createIfNode($3, $4, $5); }
;

test_exp: exp	{ $$ = $1; }
;

then_exp: exp	{ $$ = $1; }
;

else_exp: exp	{ $$ = $1; }
;

def_stmt: '(' DEFINE variable exp ')'	{ createVar($3, preOrder($4, ' ')); }
| '(' DEFINE variable fun_exp ')'	{createFun($3, $4); }
;

variable: ID	{ 
					varNameLen = yyleng;
					strncpy($$, $1, varNameLen); 
				}
;

fun_exp: '(' FUN fun_ids fun_body ')'	{ $$ = createFunNode($4); paraStackTop = 0; }
;

fun_ids: '(' ids ')'
;

ids: ids ID		{ 
					varNameLen = yyleng;
					strncpy(paras[paraStackTop].name, $2, varNameLen); 
					paraStackTop++;
				}
| /* lambda */
;

fun_body: exp		{ $$ = $1; }
;

fun_call: '(' fun_exp param ')'		{ $$ = assignParaValue($2); paraValueTop = 0; }
| '(' fun_name param')'				{ $$ = assignParaValue(getFun($2)); paraValueTop = 0; }
;

fun_name: ID
;

param: param exp	{
						paras[paraValueTop].value = preOrder($2, ' ');
						paraValueTop++;
					}
| /* lambda */
;

%%
// create node for number
nodePtr createNumNode(int num) {
	//printf("X1\n");
	nodePtr temp = (nodePtr)malloc(sizeof(treeNode));
	temp->nodeType = Num_Node;
	temp->value = num;
	//printf("X1\n");
	return temp;
}

// create node for variable
nodePtr createVarNode(char* name) {
	//printf("X2\n");
	nodePtr temp = (nodePtr)malloc(sizeof(treeNode));
	temp->nodeType = Var_Node;
	strncpy(temp->str, name, strlen(name) + 1);
	//printf("X2\n");
	return temp;
}

// create node for num-op
nodePtr createNumOpNode(char op, nodePtr ptrA, nodePtr ptrB, nodePtr ptrC) {
	//printf("X3\n");
	nodePtr temp = (nodePtr)malloc(sizeof(treeNode));
	temp->nodeType = Num_Op_Node;
	(temp->str)[0] = op;
	temp->childA = ptrA;
	temp->childB = ptrB;
	temp->childC = ptrC;
	//printf("X3\n");
	return temp;
}

// create node for exps
nodePtr createExpsNode(nodePtr ptrA, nodePtr ptrB) {
	//printf("X4\n");
	nodePtr temp = (nodePtr)malloc(sizeof(treeNode));
	temp->nodeType = Exps_Node;
	temp->childA = ptrA;
	temp->childB = ptrB;
	//printf("X4\n");
	return temp;
}

// create node for if
nodePtr createIfNode(nodePtr ptrA, nodePtr ptrB, nodePtr ptrC) {
	//printf("X5\n");
	nodePtr temp = (nodePtr)malloc(sizeof(treeNode));
	temp->nodeType = If_Node;
	temp->childA = ptrA;
	temp->childB = ptrB;
	temp->childC = ptrC;
	return temp;
}

// create node for function
nodePtr createFunNode(nodePtr ptrA) {
	//printf("X6\n");
	nodePtr temp = (nodePtr)malloc(sizeof(treeNode));
	temp->nodeType = Fun_Node;
	temp->childA = ptrA;
	int i;
	for (i = 0; i < paraStackTop; i++) {
		strncpy((temp->paras)[i].name, paras[i].name, strlen(paras[i].name) + 1);
		//printf("para: %s\n", paras[i].name);
		(temp->paraNum)++;
	}
	return temp;
}

//use pre-order to traverse tree
int preOrder(nodePtr current, char op) {
	if (current != NULL) {
		if (current->nodeType == Num_Node) {
			//printf("@%d\n", current->value);
			return current->value;
		}
		else if (current->nodeType == Exps_Node) {
			if (current->childB != NULL) {
				switch (op) {
					case '+':
						return preOrder(current->childA, ' ') + preOrder(current->childB, '+');
					case '*':
						return preOrder(current->childA, ' ') * preOrder(current->childB, '*');
					case '=':
						return preOrder(current->childA, ' ') == preOrder(current->childB, '=');
					case '&':
						return preOrder(current->childA, ' ') && preOrder(current->childB, '&');
					case '|':
						return preOrder(current->childA, ' ') || preOrder(current->childB, '|');
				}
			}
			else {
				//printf("@@%d\n", preOrder(current->childA, ' '));
				return preOrder(current->childA, ' ');
			}
		}
		else if (current->nodeType == Num_Op_Node) {
			int n1 = preOrder(current->childA, ' '), n2 = 0;
			if (current->childB != NULL)
				n2 = preOrder(current->childB, ' ');
			switch ((current->str)[0]) {
				case '+':
					if (current->childC != NULL) {
						//printf("@@@%d\n", n1 + n2 + preOrder(current->childC, '+'));
						return n1 + n2 + preOrder(current->childC, '+');
					}
					else {
						return n1 + n2;
					}
				case '-':
					return n1 - n2;
				case '*':
					if (current->childC != NULL) {
						return n1 * n2 * preOrder(current->childC, '*');
					}
					else {
						return n1 * n2;
					}
				case '/':
					return n1 / n2;
				case '%':
					return n1 % n2;
				case '>':
					return n1 > n2;
				case '<':
					return n1 < n2;
				case '=':
					if (current->childC != NULL) {
						return (n1 == n2 && n1 == preOrder(current->childC, '='));
					}
					else {
						return n1 == n2;
					}
				case '&':
					if (current->childC != NULL) {
						return n1 && n2 && preOrder(current->childC, '&');
					}
					else {
						return n1 && n2;
					}
				case '|':
					if (current->childC != NULL) {
						return n1 || n2 || preOrder(current->childC, '|');
					}
					else {
						return n1 || n2;
					}
				case '!':
					return !n1;
			}
		}
		else if (current->nodeType == If_Node) {
			if (preOrder(current->childA, ' ')) {
				return preOrder(current->childB, ' ');
			}
			else {
				return preOrder(current->childC, ' ');
			}
		}
		else if (current->nodeType == Var_Node) {
			int pos1 = getVar(current->str), pos2 = getPara(current->str);
			if (pos1 == -1 && pos2 == -1)
				printf("Variable not exist: %s\n", current->str);
			else if (pos2 != -1)
				return runParas[pos2].value;
			else if(pos1 != -1)
				return vars[pos1].value;
		}
		else if (current->nodeType == Fun_Node) {
			currentLevel++;
			int i, value;
			for (i = 0; i < current->paraNum; i++) {		// add parameters of this function node to run time stack
				int len = strlen((current->paras)[i].name);
				strncpy(runParas[runParaTop].name, (current->paras)[i].name, len + 1);
				runParas[runParaTop].value = (current->paras)[i].value;
				runParas[runParaTop].level = currentLevel;
				//printf("run time: %s: %d: %d\n", runParas[runParaTop].name, runParas[runParaTop].value, runParas[runParaTop].level);
				runParaTop++;
			}
			value = preOrder(current->childA, ' ');			// remove parameter sof this function node from run time stack
			for (i = runParaTop - 1; i > 0; i--) {
				if (runParas[runParaTop].level == currentLevel) {
					runParaTop--;
				}
			}
			currentLevel--;
			return value;
		}
	}
}

// create a new variable
void createVar(char* name, int value) {
	if (getVar(name) == -1) {		// if variable not exist
		strncpy(vars[varStackTop].name, name, strlen(name) + 1);
		vars[varStackTop].value = value;
		varStackTop++;
	}
}

// get var position in array with name, return -1 if not exist
int getVar(char* name) {
	int i;
	for (i = 0; i < 100 && i < varStackTop; i++) {
		if (strcmp(vars[i].name, name) == 0)
			return i;
	}
	return -1;
}

// get parameters in run time stack with name, return -1 if not exist
int getPara(char* name) {
	int i;
	for (i = runParaTop - 1; i >= 0; i--) {
		if (strcmp(runParas[i].name, name) == 0 && runParas[i].level <= currentLevel)
			return i;
	}
	return -1;
}

// create a new function
void createFun(char* name, nodePtr entry) {
	if (getFun(name) == NULL) {		// if function not exist
		strncpy(funs[funStackTop].name, name, strlen(name) + 1);
		funs[funStackTop].entry = entry;
		funStackTop++;
	}
}

// get function position in array with name
nodePtr getFun(char* name) {
	int i;
	for (i = 0; i < 100 && i < funStackTop; i++) {
		if (strcmp(funs[i].name, name) == 0)
			return funs[i].entry;
	}
	return NULL;
}

// assign value of each parameter of function
nodePtr assignParaValue(nodePtr ptrA) {
	int i;
	for (i = 0; i < paraValueTop; i++) {
		(ptrA->paras)[i].value = paras[i].value;
		//printf("value: %d\n", paras[i].value);
	}
	return ptrA;
}

void yyerror(const char* message) {
	printf("%s\n", message);
}

int main(int argc, char* argv[]) {
	yyparse();
	return(0);
}