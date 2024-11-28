%filenames = "scanner"

 /*
  * Please don't modify the lines above.
  */

 /* You can add lex definitions here. */
 /* TODO: Put your lab2 code here */

%x COMMENT STR IGNORE

%%

 /*
  * Below is examples, which you can wipe out
  * and write regular expressions and actions of your own.
  *
  * All the tokens:
  *   Parser::ID
  *   Parser::STRING
  *   Parser::INT
  *   Parser::COMMA
  *   Parser::COLON
  *   Parser::SEMICOLON
  *   Parser::LPAREN
  *   Parser::RPAREN
  *   Parser::LBRACK
  *   Parser::RBRACK
  *   Parser::LBRACE
  *   Parser::RBRACE
  *   Parser::DOT
  *   Parser::PLUS
  *   Parser::MINUS
  *   Parser::TIMES
  *   Parser::DIVIDE
  *   Parser::EQ
  *   Parser::NEQ
  *   Parser::LT
  *   Parser::LE
  *   Parser::GT
  *   Parser::GE
  *   Parser::AND
  *   Parser::OR
  *   Parser::ASSIGN
  *   Parser::ARRAY
  *   Parser::IF
  *   Parser::THEN
  *   Parser::ELSE
  *   Parser::WHILE
  *   Parser::FOR
  *   Parser::TO
  *   Parser::DO
  *   Parser::LET
  *   Parser::IN
  *   Parser::END
  *   Parser::OF
  *   Parser::BREAK
  *   Parser::NIL
  *   Parser::FUNCTION
  *   Parser::VAR
  *   Parser::TYPE
  */

 /* comments */
\/\* {adjust(); comment_level_ = 1; begin(StartCondition_::COMMENT);}
<COMMENT>{
  /* start of nested comment */
  \/\* {adjustStr(); comment_level_++;}

  /* end of (nested) comment */
  \*\/ {
    adjustStr();
    comment_level_--;
    if (comment_level_ == 0) {  // end of comment if comment level is 0
      begin(StartCondition_::INITIAL);
    }
  }

  /* handle newline */
  \n {adjustStr(); errormsg_->Newline();}

  /* ignore other characters */
  . {adjustStr();}

  /* unterminated comment */
  <<EOF>> {errormsg_->Error(errormsg_->tok_pos_, "unterminated comment"); return 0;}
}

 /* punctuation symbols */
","  {adjust(); return Parser::COMMA;}
":"  {adjust(); return Parser::COLON;}
";"  {adjust(); return Parser::SEMICOLON;}
"("  {adjust(); return Parser::LPAREN;}
")"  {adjust(); return Parser::RPAREN;}
"["  {adjust(); return Parser::LBRACK;}
"]"  {adjust(); return Parser::RBRACK;}
"{"  {adjust(); return Parser::LBRACE;}
"}"  {adjust(); return Parser::RBRACE;}
"."  {adjust(); return Parser::DOT;}
"+"  {adjust(); return Parser::PLUS;}
"-"  {adjust(); return Parser::MINUS;}
"*"  {adjust(); return Parser::TIMES;}
"/"  {adjust(); return Parser::DIVIDE;}
"="  {adjust(); return Parser::EQ;}
"<>" {adjust(); return Parser::NEQ;}
"<"  {adjust(); return Parser::LT;}
"<=" {adjust(); return Parser::LE;}
">"  {adjust(); return Parser::GT;}
">=" {adjust(); return Parser::GE;}
"&"  {adjust(); return Parser::AND;}
"|"  {adjust(); return Parser::OR;}
":=" {adjust(); return Parser::ASSIGN;}

 /* reserved words */
"array"    {adjust(); return Parser::ARRAY;}
"if"       {adjust(); return Parser::IF;}
"then"     {adjust(); return Parser::THEN;}
"else"     {adjust(); return Parser::ELSE;}
"while"    {adjust(); return Parser::WHILE;}
"for"      {adjust(); return Parser::FOR;}
"to"       {adjust(); return Parser::TO;}
"do"       {adjust(); return Parser::DO;}
"let"      {adjust(); return Parser::LET;}
"in"       {adjust(); return Parser::IN;}
"end"      {adjust(); return Parser::END;}
"of"       {adjust(); return Parser::OF;}
"break"    {adjust(); return Parser::BREAK;}
"nil"      {adjust(); return Parser::NIL;}
"function" {adjust(); return Parser::FUNCTION;}
"var"      {adjust(); return Parser::VAR;}
"type"     {adjust(); return Parser::TYPE;}

 /* identifier */
[a-zA-Z][a-zA-Z0-9_]* {adjust(); return Parser::ID;}

 /* integer literal */
[0-9]+ {adjust(); return Parser::INT;}

 /* string literal */
\" {adjust(); begin(StartCondition_::STR);}
<STR>{
  /* newline */
  \\n {adjustStr(); string_buf_ += "\n";}

  /* tab */
  \\t {adjustStr(); string_buf_ += "\t";}

  /* control characters (\^c) */
  \\\^[A-Z] {adjustStr(); string_buf_ += std::string(1, matched()[2] - 64);}
  \\\^@     {adjustStr(); string_buf_ += std::string(1, 0);}
  \\\^\[    {adjustStr(); string_buf_ += std::string(1, 27);}
  \\\^\\    {adjustStr(); string_buf_ += std::string(1, 28);}
  \\\^\]    {adjustStr(); string_buf_ += std::string(1, 29);}
  \\\^\^    {adjustStr(); string_buf_ += std::string(1, 30);}
  \\\^_     {adjustStr(); string_buf_ += std::string(1, 31);}

  /* 3 decimal digits ascii code (\ddd) */
  \\[0-9]{3} {
    adjustStr();
    int ascii = std::stoi(matched().substr(1, 3));
    if (ascii >= 0 && ascii <= 255) {  // legal ascii (0~255)
      string_buf_ += std::string(1, ascii);
    } else {  // illegal ascii
      errormsg_->Error(errormsg_->tok_pos_, "illegal ascii code");
    }
  }

  /* double-quote character (") */
  \\\" {adjustStr(); string_buf_ += "\"";}

  /* backslash character (\) */
  \\\\ {adjustStr(); string_buf_ += "\\";}

  /* ignored sequence in long strings
   * sequence like \f___f\
   * where f___f stands for a sequence of one or more formatting characters (space, tab, newline, formfeed, etc.)
   */
  
  \\[ \n\t\f]+\\ {
    adjustStr();
    for (char c : matched()) {
      if (c == '\n') {  // handle newline
        errormsg_->Newline();
      }
    }
  }

  /* illegal use of \ */
  \\. {adjustStr(); errormsg_->Error(errormsg_->tok_pos_, "illegal use of \\");}

  /* end of string */
  \" {
    adjustStr(); 
    begin(StartCondition_::INITIAL);
    setMatched(string_buf_);
    string_buf_.clear();
    return Parser::STRING;
  }

  /* place other characters to string_buf_ as they are */
  . {adjustStr(); string_buf_ += matched();}

  /* missing terminated " */
  <<EOF>> {errormsg_->Error(errormsg_->tok_pos_, "unterminated string"); return 0;}
}

 /*
  * skip white space chars.
  * space, tabs and LF
  */
[ \t]+ {adjust();}
\n {adjust(); errormsg_->Newline();}

 /* illegal input */
. {adjust(); errormsg_->Error(errormsg_->tok_pos_, "illegal token");}
