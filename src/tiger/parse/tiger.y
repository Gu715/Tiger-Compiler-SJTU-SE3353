%filenames parser
%scanner tiger/lex/scanner.h
%baseclass-preinclude tiger/absyn/absyn.h

 /*
  * Please don't modify the lines above.
  */

%union {
  int ival;
  std::string* sval;
  sym::Symbol *sym;
  absyn::Exp *exp;
  absyn::ExpList *explist;
  absyn::Var *var;
  absyn::DecList *declist;
  absyn::Dec *dec;
  absyn::EFieldList *efieldlist;
  absyn::EField *efield;
  absyn::NameAndTyList *tydeclist;
  absyn::NameAndTy *tydec;
  absyn::FieldList *fieldlist;
  absyn::Field *field;
  absyn::FunDecList *fundeclist;
  absyn::FunDec *fundec;
  absyn::Ty *ty;
  }

%token <sym> ID
%token <sval> STRING
%token <ival> INT

%token
  COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK
  LBRACE RBRACE DOT
  ARRAY IF THEN ELSE WHILE FOR TO DO LET IN END OF
  BREAK NIL
  FUNCTION VAR TYPE

 /* token priority */
 /* TODO: Put your lab3 code here */

%right ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%right UMINUS

%type <exp> exp expseq arrayexp
%type <explist> actuals nonemptyactuals sequencing sequencing_exps
%type <var> lvalue one oneormore
%type <declist> decs decs_nonempty
%type <dec> decs_nonempty_s vardec
%type <efieldlist> rec rec_nonempty
%type <efield> rec_one
%type <tydeclist> tydec
%type <tydec> tydec_one
%type <fieldlist> tyfields tyfields_nonempty
%type <field> tyfield
%type <ty> ty
%type <fundeclist> fundec
%type <fundec> fundec_one

%start program

%%
program:  exp  {absyn_tree_ = std::make_unique<absyn::AbsynTree>($1);};

/* Declaration List & Declarations */
decs:   {$$ = new absyn::DecList();}
  |  decs_nonempty  {$$ = $1;}
  ;

decs_nonempty:  decs_nonempty_s  {$$ = new absyn::DecList($1);}
  |  decs_nonempty_s decs_nonempty  {$$ = $2->Prepend($1);}
  ;

decs_nonempty_s:  fundec  {$$ = new absyn::FunctionDec(scanner_.GetTokPos(), $1);}
  |  vardec  {$$ = $1;}
  |  tydec  {$$ = new absyn::TypeDec(scanner_.GetTokPos(), $1);}
  ;

/* Variable Declaration */
vardec:  VAR ID ASSIGN exp  {$$ = new absyn::VarDec(scanner_.GetTokPos(), $2, nullptr, $4);}
  |  VAR ID COLON ID ASSIGN exp  {$$ = new absyn::VarDec(scanner_.GetTokPos(), $2, $4, $6);}
  ;

/* Type Declaration */
tydec:  tydec_one  {$$ = new absyn::NameAndTyList($1);}
  |  tydec_one tydec  {$$ = $2->Prepend($1);}
  ;

tydec_one:  TYPE ID EQ ty  {$$ = new absyn::NameAndTy($2, $4);};

ty:  ID  {$$ = new absyn::NameTy(scanner_.GetTokPos(), $1);}
  |  LBRACE tyfields RBRACE  {$$ = new absyn::RecordTy(scanner_.GetTokPos(), $2);}
  |  ARRAY OF ID  {$$ = new absyn::ArrayTy(scanner_.GetTokPos(), $3);}
  ;

tyfields:  {$$ = new absyn::FieldList();}
  |  tyfields_nonempty  {$$ = $1;}
  ;

tyfields_nonempty:  tyfield  {$$ = new absyn::FieldList($1);}
  |  tyfield COMMA tyfields_nonempty  {$$ = $3->Prepend($1);}
  ;

tyfield:  ID COLON ID  {$$ = new absyn::Field(scanner_.GetTokPos(), $1, $3);};

/* Function Declaration */
fundec:  fundec_one  {$$ = new absyn::FunDecList($1);}
  |  fundec_one fundec  {$$ = $2->Prepend($1);}
  ;

fundec_one:  
     FUNCTION ID LPAREN tyfields RPAREN EQ exp
     {
       $$ = new absyn::FunDec(scanner_.GetTokPos(), $2, $4, nullptr, $7);
     }
  |  FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp
     {
       $$ = new absyn::FunDec(scanner_.GetTokPos(), $2, $4, $7, $9);
     }
  ;


/* Left-Values */
lvalue:  ID  {$$ = new absyn::SimpleVar(scanner_.GetTokPos(), $1);}
  |  lvalue DOT ID  {$$ = new absyn::FieldVar(scanner_.GetTokPos(), $1, $3);}
  |  ID LBRACK exp RBRACK /* defined to avoid prefix conflict with ArrayExp when parsing lvalue like "d[3]" */
    {
      $$ = new absyn::SubscriptVar(scanner_.GetTokPos(), 
             new absyn::SimpleVar(scanner_.GetTokPos(), $1), $3);
    }
  |  lvalue LBRACK exp RBRACK  {$$ = new absyn::SubscriptVar(scanner_.GetTokPos(), $1, $3);}
  ;

/* Expressions */
exp:
     lvalue  {$$ = new absyn::VarExp(scanner_.GetTokPos(), $1);}
  |  NIL  {$$ = new absyn::NilExp(scanner_.GetTokPos());}
  |  INT  {$$ = new absyn::IntExp(scanner_.GetTokPos(), $1);}
  |  STRING  {$$ = new absyn::StringExp(scanner_.GetTokPos(), $1);}
  |  ID LPAREN actuals RPAREN  {$$ = new absyn::CallExp(scanner_.GetTokPos(), $1, $3);}
  |  exp AND exp      {$$ = new absyn::OpExp(scanner_.GetTokPos(), absyn::AND_OP, $1, $3);}
  |  exp OR exp       {$$ = new absyn::OpExp(scanner_.GetTokPos(), absyn::OR_OP, $1, $3);}
  |  exp PLUS exp     {$$ = new absyn::OpExp(scanner_.GetTokPos(), absyn::PLUS_OP, $1, $3);}
  |  exp MINUS exp    {$$ = new absyn::OpExp(scanner_.GetTokPos(), absyn::MINUS_OP, $1, $3);}
  |  MINUS exp %prec UMINUS
    {
      $$ = new absyn::OpExp(
      scanner_.GetTokPos(), absyn::MINUS_OP, new absyn::IntExp(scanner_.GetTokPos(), 0), $2);
    }
  |  exp TIMES exp    {$$ = new absyn::OpExp(scanner_.GetTokPos(), absyn::TIMES_OP, $1, $3);}
  |  exp DIVIDE exp   {$$ = new absyn::OpExp(scanner_.GetTokPos(), absyn::DIVIDE_OP, $1, $3);}
  |  exp EQ exp       {$$ = new absyn::OpExp(scanner_.GetTokPos(), absyn::EQ_OP, $1, $3);}
  |  exp NEQ exp      {$$ = new absyn::OpExp(scanner_.GetTokPos(), absyn::NEQ_OP, $1, $3);}
  |  exp LT exp       {$$ = new absyn::OpExp(scanner_.GetTokPos(), absyn::LT_OP, $1, $3);}
  |  exp LE exp       {$$ = new absyn::OpExp(scanner_.GetTokPos(), absyn::LE_OP, $1, $3);}
  |  exp GT exp       {$$ = new absyn::OpExp(scanner_.GetTokPos(), absyn::GT_OP, $1, $3);}
  |  exp GE exp       {$$ = new absyn::OpExp(scanner_.GetTokPos(), absyn::GE_OP, $1, $3);}
  |  ID LBRACE rec RBRACE  {$$ = new absyn::RecordExp(scanner_.GetTokPos(), $1, $3);}
  |  LPAREN sequencing RPAREN  {$$ = new absyn::SeqExp(scanner_.GetTokPos(), $2);}
  |  lvalue ASSIGN exp  {$$ = new absyn::AssignExp(scanner_.GetTokPos(), $1, $3);}
  |  IF exp THEN exp  {$$ = new absyn::IfExp(scanner_.GetTokPos(), $2, $4, nullptr);}
  |  IF exp THEN exp ELSE exp  {$$ = new absyn::IfExp(scanner_.GetTokPos(), $2, $4, $6);}
  |  WHILE exp DO exp  {$$ = new absyn::WhileExp(scanner_.GetTokPos(), $2, $4);}
  |  FOR ID ASSIGN exp TO exp DO exp  {$$ = new absyn::ForExp(scanner_.GetTokPos(), $2, $4, $6, $8);}
  |  BREAK  {$$ = new absyn::BreakExp(scanner_.GetTokPos());}
  |  LET decs IN expseq END  {$$ = new absyn::LetExp(scanner_.GetTokPos(), $2, $4);}
  |  ID LBRACK exp RBRACK OF exp  {$$ = new absyn::ArrayExp(scanner_.GetTokPos(), $1, $3, $6);}
  |  LPAREN RPAREN {$$ = new absyn::VoidExp(scanner_.GetTokPos());}
  |  LPAREN exp RPAREN  {$$ = $2;}
  ;

expseq:  sequencing_exps  {$$ = new absyn::SeqExp(scanner_.GetTokPos(), $1);};


/* Sequence of Expressions */
sequencing:       /* A sequence of two or more expressions */
     exp SEMICOLON exp  {$$ = new absyn::ExpList($3); $$ = $$->Prepend($1);}  
  |  exp SEMICOLON sequencing  {$$ = $3->Prepend($1);}
  ;

sequencing_exps:  /* A sequence of zero or more expressions */
     {$$ = new absyn::ExpList();}
  |  exp  {$$ = new absyn::ExpList($1);}
  |  exp SEMICOLON sequencing_exps  {$$ = $3->Prepend($1);}
  ;

actuals:  {$$ = new absyn::ExpList();}
  | nonemptyactuals  {$$ = $1;}
  ;

nonemptyactuals:  exp  {$$ = new absyn::ExpList($1);}
  |  exp COMMA nonemptyactuals  {$$ = $3->Prepend($1);}
  ;

/* Record Field */
rec:  {$$ = new absyn::EFieldList();}
  |  rec_nonempty  {$$ = $1;}
  ;

rec_nonempty:  rec_one  {$$ = new absyn::EFieldList($1);}
  |  rec_one COMMA rec_nonempty  {$$ = $3->Prepend($1);}
  ;

rec_one:  ID EQ exp  {$$ = new absyn::EField($1, $3);};