#include <fstream>
#include <iostream>
#include <optional>
#include <set>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <tuple>
#include <unistd.h>
#include <unordered_map>
#include <utility>
#include <vector>

#undef NDEBUG
#include <assert.h>

#define GIVE
#define TAKE
#define KEEP

static const int LONG_LATEX_BLOCK_SIZE = 30;
static const int LONG_CODE_BLOCK_SIZE = 60;

using namespace std;

using ll = long long;
static const ll MAX_CHARS = 1e9;

// indent for logging to tell which function is calling what.
struct Logger {
  static int G_LOG_INDENT;
  const int indent;
  Logger() : indent(G_LOG_INDENT) {
    G_LOG_INDENT++;
    if (indent > 40) {
      assert(false &&
             "indent more than 40 levels deep; you sure this is correct?");
    }
  }
  ~Logger() { G_LOG_INDENT--; }
  void print(std::ostream &o) {
    for (int i = 0; i < indent; ++i) {
      o << " ";
    }
  }
};
int Logger::G_LOG_INDENT = 1;

// pretty printing doc
struct Doc {
  enum class Kind {
    RAW,
    INDENTED,
  };
  const Kind kind;

  void print(ostream &o, int indent = 0) {
    if (kind == Kind::RAW) {
      for (char c : s) {
        o << c;
        if (c == '\n') {
          print_indent(o, indent);
        }
      }
    } else if (kind == Kind::INDENTED) {
      indent++;
      for (Doc *d : children) {
        d->print(o, indent);
      }
      indent--;
    } else {
      assert(false && "unknown kind");
    }
  }

  Doc(string s) : s(s), kind(Kind::RAW){};

private:
  vector<Doc *> children;
  string s;

  void print_indent(ostream &o, int indent) {
    for (int i = 0; i < indent; ++i) {
      o << ' ';
    }
  }
};

ll hashstr(const char *s, const ll len) {
  const ll p = 53;
  // closest prime below 2^62. found using b(2^62) on
  // https://www.alpertron.com.ar/ECM.HTM
  const ll mod = 1e9 + 9;
  ll h = 0;
  ll ppow = 1;
  for (int i = 0; i < len; ++i) {
    assert(s[i] != '\0');
    h += ((s[i] + 1) * ppow) % mod;
    ppow = (ppow * p) % mod;
  }
  return h;
}

// L for location
struct Loc {
  ll si, line, col;
  Loc(ll si, ll line, ll col) : si(si), line(line), col(col){};
  Loc nextline() const { return Loc(si + 1, line + 1, 1); }
  static Loc beginning_of_file() { return Loc(0, 1, 1); }
  Loc next(char c) const {
    if (c == '\n') {
      return nextline();
    } else {
      return nextcol();
    }
  }

  Loc next(const char *s) const {
    Loc l = *this;
    for (int i = 0; s[i] != 0; ++i) {
      l = l.next(s[i]);
    }
    return l;
  }

  Loc prev(char c) const {
    if (c == '\n') {
      assert(false && "don't know how to walk back newline");
    } else {
      return prevcol();
    }
  }

  Loc prev(const char *s) const {
    Loc l = *this;
    for (int i = strlen(s) - 1; i >= 0; --i) {
      l = l.prev(s[i]);
    }
    return l;
  }

  bool operator==(const Loc &other) const {
    return si == other.si && line == other.line && col == other.col;
  }

  bool operator!=(const Loc &other) const { return !(*this == other); }

private:
  Loc nextcol() const { return Loc(si + 1, line, col + 1); }
  Loc prevcol() const {
    assert(col - 1 >= 1);
    return Loc(si - 1, line, col - 1);
  }
};
const Loc LOC_FIRST = Loc(0, 1, 1);

std::ostream &operator<<(std::ostream &o, const Loc &l) {
  return cout << ":" << l.line << ":" << l.col;
}

// half open [...)
// substr := str[span.begin...span.end-1];
struct Span {
  Loc begin, end;
  Span(Loc begin, Loc end) : begin(begin), end(end) {
    assert(end.si >= begin.si);
  };
  ll nchars() const { return end.si - begin.si; }
};

std::ostream &operator<<(std::ostream &o, const Span &s) {
  return cout << s.begin << " - " << s.end;
}

const set<std::string> keywords = {
    "Assert", "Info",     "IsBound", "Quit", 
    "TryNextMethod", "Unbind", "and", "atomic",
    "break",    "continue", "do",     "elif",
    "else",    "end",  "false",    "fi",
    "for",    "function", "if",      "in",
    "local",    "mod", "not",    "od",
    "or",     "quit", "readonly", "readwrite",
    "rec",    "repeat",   "return",  "then",
    "true",     "until", "while"};

const std::string special = "\"`()*+,-#./:;<=>~[\\]^_{}";

const set<std::string> symbols = {"+",  "-",  "*",  "/",  "^", "~",  "!.",
                                  "=",  "<>", "<",  "<=", ">", ">=", "![",
                                  ":=", ".",  "..", "->", ",", ";",  "!{",
                                  "[",  "]",  "{",  "}",  "(", ")",  ":"};

bool is_special(char c) {
  return c == '"' || c == '`' || c == '(' || c == ')' || c == '*' || c == '+' ||
         c == ',' || c == '-' || c == '#' || c == '.' || c == '/' || c == ':' ||
         c == ';' || c == '<' || c == '=' || c == '>' || c == '~' || c == '[' ||
         c == '\\' || c == ']' || c == '^' || c == '_' || c == '{' ||
         c == '}' || c == '!';
}

bool is_whitespace(char c) { return c == ' ' || c == '\t' || c == '\n'; }

struct Token {
  enum class Kind { TOK_SYMBOL, TOK_KEYWORD, TOK_IDENTIFIER, TOK_STRING, TOK_EOF };

  Token(Span span, Token::Kind kind, std::string str)
      : span(span), kind(kind), str(str){};
  void print(ostream &o) const {
    if (kind == Kind::TOK_EOF) {
      o << "EOF";
    } else {
      o << str;
    }
  }

  const Span span;
  const Kind kind;
  const std::string str;

private:
};

struct Tokenizer {
  const char *data;
  int len;
  Loc loc;

  Tokenizer(int len, const char *data)
      : len(len), data(data), loc(Loc::beginning_of_file()) {}
  bool eof() const { return loc.si > len; }

  Token consume_symbol(string s) {
    assert(symbols.count(s));
    Token t = consume();
    assert(t.kind == Token::Kind::TOK_SYMBOL);
    assert(t.str == s);
    return t;
  }

  Token consume_keyword(string s) {
    assert(keywords.count(s));
    Token t = consume();

    if (t.kind == Token::Kind::TOK_KEYWORD && t.str == s) { return t; };

    this->print_span(t.span);
    cerr << "Expected keyword |" << s << "|\n";
    assert(t.kind == Token::Kind::TOK_KEYWORD);
    assert(t.str == s);
    assert(false && "did not find expected keyword.");
  }

  Token consume_identifier() {
    Token t = consume();
    if(t.kind == Token::Kind::TOK_IDENTIFIER) {
      return t;
    }
    this->print_span(t.span);
    cerr << "Expected identifer.\n";
    assert(false && "did not find expected identifier.");
  }

  Token consume_string() {
    Token t = consume();
    assert(t.kind == Token::Kind::TOK_STRING);
    return t;
  }

  bool peek_symbol(string s) {
    assert(symbols.count(s));
    Token t = peek_raw();
    return t.kind == Token::Kind::TOK_SYMBOL && t.str == s;
  }

  bool peek_keyword(string s) {
    assert(keywords.count(s));
    Token t = peek_raw();
    return t.kind == Token::Kind::TOK_KEYWORD && t.str == s;
  }

  Token peek_raw() {
    eat_whitespace();
    if (eof()) {
      return Token(Span(loc, loc), Token::Kind::TOK_EOF, "");
    }

    const char ccur = *this->peekc();
    cerr << "\n\tpeeking: " << ccur << "\n";
    if (ccur == '\"') {
      Loc lend = loc.next('\"');
      string s = "\"";
      while(1) {
        if (lend.si >= len) {
          this->print_span(Span(loc, lend));
          assert(false && "unterminated \"");
        }
        char c = data[lend.si];
        // TODO: string escape.
        s += data[lend.si];
        lend = lend.next(c);
        if (c == '\"') { break; }
      }
      const Span span(loc, lend);
      return Token(span, Token::Kind::TOK_STRING, s);

    } else if (!is_special(ccur)) {
      string s;
      Loc lend = loc;
      while (1) {
        if (lend.si >= len) {
          break;
        }
        char c = data[lend.si];
        if (is_whitespace(c) || is_special(c)) {
          break;
        }
        s += data[lend.si];
        lend = lend.next(c);
      }
      const Span span(loc, lend);
      if (keywords.count(s)) {
        return Token(span, Token::Kind::TOK_KEYWORD, s);
      } else {
        return Token(span, Token::Kind::TOK_IDENTIFIER, s);
      }
    } else {
      // we have a symbol
      string s;
      Loc lend = loc;
      while (1) {
        if (lend.si > len) {
          break;
        }
        char c = data[lend.si];
        s += c;
        lend = lend.next(c);
        // symbols are prefix free, so no symbol is a prefix of any other.
        if (symbols.count(s)) {
          return Token(Span(loc, lend), Token::Kind::TOK_SYMBOL, s);
        }
      }
      this->print_span(Span(loc, lend));
      assert(false && "uknown symbol");
    }
  }

  void print_span(Span span) const {
    // TODO: handle multiline span
    cerr << "===\n";
    int i = span.begin.si;
    for (; i >= 1 && data[i - 1] != '\n'; i--) {}

    cerr << "Source file [" << span.begin << ":" << span.end << "]\n";
    const int nlines = span.end.line - span.begin.line+1;
    if (span.begin.line == span.end.line) {
      string squiggle;
      printf("%4d>", span.begin.line);
      for (; data[i] != '\0' && data[i] != '\n'; ++i) {
        squiggle += i == span.begin.si ? '^' : i == span.end.si ? '^' : i >= span.begin.si && i <= span.end.si ? '~' : ' ';
        cerr << data[i];
      }
      printf("\n%4d>%s", span.begin.line, squiggle.c_str());
    } else if (nlines <= 4) {

      cerr << ">";
      for (i += 1; data[i] != '\0' && i <= span.end.si; ++i) {
        cerr << data[i];
        if (data[i] == '\n') {
          cerr << ">";
        }
      }
    } else {
      printf("%4d>", span.begin.line);
      string squiggle = "";
      for(; data[i] != '\0' && data[i] != '\n'; i++) {
        squiggle += i == span.begin.si ? '^' : i >= span.begin.si ? '~' : ' ';
        cerr << data[i];
      }
      printf("\n%4d>%s\n", span.begin.line, squiggle.c_str());
      int nlines = 0;
      for(i += 1; nlines < 2; i++) {
        if (data[i] == '\n') { nlines++; printf("%4d>", span.begin.line + nlines + 1); }
        cerr << data[i]; 
      }
      printf("%4d>%s\n", span.begin.line + 4, "  ...");


      nlines = 0;
      int i = span.end.si-1;
      for (; i >= 1 && nlines <= 3; i--) {
        if (data[i] == '\n') { nlines++; }
      }

      nlines = 0;
      for(i += 1; nlines <= 2; i++) {
        if (data[i] == '\n') {
          nlines++;
          printf("\n%4d>", span.end.line - (2 - nlines) - 1);
        } else {
          cerr << data[i];
        }
      }


      squiggle = "";
      printf("\n%4d>", span.end.line);
      for(; data[i] != '\0' && data[i] != '\n'; i++) {
        squiggle += i == span.end.si-1 ? '^' : i <= span.end.si ? '~' : ' ';
        cerr << data[i];
      }
      printf("\n%4d>%s\n", span.end.line, squiggle.c_str());

    }
    cerr << "\nSource file [" << span.begin << ":" << span.end << "]\n";
  }

  void print_loc(Loc l) const {
    cerr << "===\n";
    int i = loc.si;
    for (; i >= 1 && data[i - 1] != '\n'; i--)
      ;

    cerr << "Source file [" << loc << "]> ";
    string squiggle;
    for (; data[i] != '\0' && data[i] != '\n'; ++i) {
      squiggle += i == loc.si ? '^' : ' ';
      cerr << data[i];
    }
    cerr << "\n" << squiggle << "\n";
  }

private:
  optional<char> peekc() {
    if (loc.si > this->len) {
      return {};
    }
    return this->data[loc.si];
  }

  bool ispeekc(char c) const {
    if (loc.si >= len) {
      return false;
    }
    return data[loc.si] == c;
  }

  void consumec(char c) {
    assert(ispeekc(c));
    loc = loc.next(c);
  }

  void eat_whitespace() {
    while (1) {
      if (ispeekc('#')) {
        while (1) {
          std::optional<char> c = peekc();
          if (!c) {
            return;
          }
          consumec(*c);
          if (*c == '\n') {
            break;
          }
        }
      } else if (ispeekc(' ')) {
        consumec(' ');
      } else if (ispeekc('\t')) {
        consumec('\t');
      } else if (ispeekc('\n')) {
        consumec('\n');
      } else {
        return;
      }
    }
  }

  Token consume() {
    Token t = peek_raw();
    assert(t.span.begin == loc);
    loc = t.span.end;
    return t;
  }
};

// SYNTAX
// ======

class StxExpr;
class StxStmt;
class StxBlock;

class StxExpr {
public:
  virtual void print(std::ostream &o) const = 0;
};

class StxFnCall : public StxExpr {
public:
  StxFnCall(Token name, std::vector<StxExpr *> args) : name(name), args(args){};
  Token name;
  std::vector<StxExpr *> args;
  void print(std::ostream &o) const {
    name.print(o);
    o << "(";
    for (int i = 0; i < args.size(); ++i) {
      if (i > 0) {
        o << ",";
      }
      args[i]->print(o);
    }
    o << ")";
  }
};

class StxVar : public StxExpr {
public:
  StxVar(Token name) : name(name){};
  const Token name;

  void print(std::ostream &o) const { name.print(o); }
};

class StxStr : public StxExpr {
public:
  StxStr(Token str) : str(str){};
  const Token str;

  void print(std::ostream &o) const { str.print(o); }
};


class StxBinop : public StxExpr {
public:
  StxBinop(StxExpr *left, Token symbol, StxExpr *right)
      : left(left), symbol(symbol), right(right) {}
  StxExpr *left;
  StxExpr *right;
  Token symbol;

  void print(std::ostream &o) const {
    left->print(o);
    o << " ";
    symbol.print(o);
    o << " ";
    right->print(o);
  }
};

// l[ix]
class StxIndex : public StxExpr {
public:
  StxIndex(StxExpr *e, StxExpr *index) : e(e), index(index) {}
  StxExpr *e;
  StxExpr *index;

  void print(std::ostream &o) const {
    e->print(o);
    o << "[";
    index->print(o);
    o << "]";
  }
};



// [a, b, c]
class StxList : public StxExpr {
  public:
  StxList(vector<StxExpr *> args) : args(args) {};


  void print(std::ostream &o) const {
    o << "[";
    for(int i = 0; i < args.size(); ++i){
      if (i > 0) o << ", ";
      args[i]->print(o);
    }
    o << "]";
  }

  private:
  vector<StxExpr *> args;
};

class StxStmt {
public:
  virtual void print(std::ostream &o, int indent) const = 0;
};

class StxAssign : public StxStmt {
public:
  StxAssign(Token lhs, StxExpr *rhs) : lhs(lhs), rhs(rhs){};

  void print(std::ostream &o, int indent) const {
    lhs.print(o);
    o << " := ";
    rhs->print(o);
    o << "\n";
  }
  Token lhs;
  StxExpr *rhs;
};

class StxProcedureCall : public StxStmt {
public:
  StxProcedureCall(Token name, vector<StxExpr *> args)
      : name(name), args(args){};

  void print(std::ostream &o, int indent) const {
    name.print(o);
    o << "(";
    for (int i = 0; i < args.size(); ++i) {
      if (i > 0) {
        o << ", ";
      }
      args[i]->print(o);
    }
    o << ")";
  }
  Token name;
  vector<StxExpr *> args;
};


class StxFnDefn : public StxExpr {
  public:
    StxFnDefn(vector<Token> params, StxBlock *stmts) : params(params), stmts(stmts) {};

  void print(std::ostream &o) const {
    o << "function (";
    for(int i = 0; i < params.size(); ++i){
      if (i > 0) o << ", ";
      o << params[i].str;
    }
    o << ")\n";
    o << "TODO: print statements\n";
    o << "end\n";
  }
  private:
    vector<Token> params;
    StxBlock *stmts;
};


// expr -> expr_logical[and, or] -> expr_compare[>=, <=] -> expr_index["expr[index]"] -> expr_leaf
StxExpr *parse_expr_leaf(Tokenizer &t);
StxExpr *parse_expr_index(Tokenizer &t);
StxExpr *parse_expr(Tokenizer &t);
StxExpr *parse_expr_compare(Tokenizer &t);
StxStmt *parse_stmt(Tokenizer &t);
template<typename Token2Bool>
StxBlock *parse_stmts(Tokenizer &t, Token2Bool isEnd);

StxExpr *parse_expr_logical(Tokenizer &t) {
  StxExpr *l = parse_expr_compare(t);
  if (t.peek_keyword("and")) {
    Token and_ = t.consume_keyword("and");
    StxExpr *r = parse_expr(t);
    return new StxBinop(l, and_, r);
  } else {
    return l;
  }
};

// expressions (4.7)
StxExpr *parse_expr(Tokenizer &t) { 
  return parse_expr_logical(t);
}

// e[e]
StxExpr *parse_expr_index(Tokenizer &t) {
  StxExpr *l = parse_expr_leaf(t);
  if (t.peek_symbol("[")) {
    t.consume_symbol("[");
    // TODO: multi dimensional?
    StxExpr *ix = parse_expr(t);
    t.consume_symbol("]");
    return new StxIndex(l, ix);
  } else {
    return l;
  }
}

StxExpr *parse_expr_compare(Tokenizer &t) {
  StxExpr *l = parse_expr_index(t);
  if (t.peek_symbol("=")) {
    Token sym = t.consume_symbol("=");
    StxExpr *r = parse_expr(t);
    return new StxBinop(l, sym, r);
  } else {
    return l;
  }
};

// 4.11
// parse expressions delimited by sl, sr
vector<StxExpr *> parse_exprs_delimited(Tokenizer &t, string sl, string sr) {
  std::cerr << "\t" << __PRETTY_FUNCTION__ << "\n";
  // function call
  t.consume_symbol(sl);

  std::vector<StxExpr *> args;
  if (t.peek_symbol(sr)) {
    t.consume_symbol(sr);
    return args;
  }
  while (1) {
    args.push_back(parse_expr(t));
    if (t.peek_symbol(sr)) {
      t.consume_symbol(sr);
      break;
    } else if (t.peek_symbol(",")) {
      t.consume_symbol(",");
      continue;
    } else {
      t.print_span(t.peek_raw().span);
      cerr << "expected list of expressions of the form |" << sl << "expr, expr, ... " << sr << "|\n";
      assert(false && "unknown parse in expression sequence");
    }
  }
  return args;
}

StxExpr * parse_fn_defn(Tokenizer &t) {
  t.consume_keyword("function"); // consume "function"

  // consume function arguments
  t.consume_symbol("(");
  vector<Token> params;
  while(1) {
    Token param = t.consume_identifier();
    params.push_back(param);
    if (t.peek_symbol(")")) { 
        t.consume_symbol(")");
        break;
    }
    else if (t.peek_symbol(",")) {
      t.consume_symbol(",");
      continue;
    } else {
      t.print_span(t.peek_raw().span);
      cerr << "Expected , or ) in function definition argument list\n";
      assert(false && "unknown symbol in function definition argument list");
    }
  }

  // done parsng function params. now parse statements.
  StxBlock *stmts = parse_stmts(t, [](Token t) { return t.kind == Token::Kind::TOK_KEYWORD && t.str == "end"; });
  t.consume_keyword("end");
  return new StxFnDefn(params, stmts);
}

// variable (4.8) or function call (4.11) or string or function defn
StxExpr *parse_expr_leaf(Tokenizer &t) {
  std::cerr << "\t" << __PRETTY_FUNCTION__ << "\n";
  Token p = t.peek_raw();
  if (p.kind == Token::Kind::TOK_STRING) {
    t.consume_string();
    return new StxStr(p);
  } else if (p.kind == Token::Kind::TOK_IDENTIFIER) {
    t.consume_identifier();
    if (t.peek_symbol("(")) {
      std::vector<StxExpr *> args = parse_exprs_delimited(t, "(", ")");
      return new StxFnCall(p, args);
    } else {
      return new StxVar(p);
    }
  } else if (p.kind == Token::Kind::TOK_SYMBOL && p.str == "[") {
    t.print_span(p.span);
    vector<StxExpr *> args = parse_exprs_delimited(t, "[", "]");
    return new StxList(args);
  }  else if (p.kind == Token::Kind::TOK_KEYWORD && p.str == "function") {
    return parse_fn_defn(t);
  } else {
    t.print_span(p.span);
    assert(false && "unknown expression leaf token");
  }
}

StxStmt *parse_assgn_or_procedure_call(Tokenizer &t) {
  std::cerr << "\t" << __PRETTY_FUNCTION__ << "\n";
  Token name = t.consume_identifier(); // todo: generalize to lvalue.
  if (t.peek_symbol("(")) {
    std::vector<StxExpr *> args = parse_exprs_delimited(t, "(", ")");
    return new StxProcedureCall(name, args);
  } else if (t.peek_symbol(":=")) {
    StxExpr *rhs = parse_expr(t);
    return new StxAssign(name, rhs);
  }
  std::cerr << "unknown toplevel symbol for assign/procedure call:  |";
  t.peek_raw().print(std::cerr);
  assert(false && "unknown symbol at top level");
}

class StxBlock {
public:
  vector<StxStmt *> stmts;
  StxBlock(vector<StxStmt *> stmts) : stmts(stmts) {}
  void print(std::ostream &o, int indent) const {
    for (int i = 0; i < stmts.size(); ++i) {
      stmts[i]->print(o, indent);
      o << "\n";
    }
  }
};

StxStmt *parse_stmt(Tokenizer &t);

template <typename Token2Bool>
StxBlock *parse_stmts(Tokenizer &t, Token2Bool isEnd) {
  std::cerr << "\t" << __PRETTY_FUNCTION__ << "|" << __LINE__ << endl;
  vector<StxStmt *> stmts;
  while (1) {
    cerr << "parsing statements, currently at: ";
    t.print_span(t.peek_raw().span);
    // std::cerr << "\t" << __PRETTY_FUNCTION__ << "|" << __LINE__ << endl;
    if (isEnd(t.peek_raw())) {
      // std::cerr << "\t" << __PRETTY_FUNCTION__ << "|" << __LINE__ << endl;
      break;
    } else {
      // std::cerr << "\t" << __PRETTY_FUNCTION__ << "|" << __LINE__ << endl;
      stmts.push_back(parse_stmt(t));
      // TODO: does this belong to parse_stmt?
      t.consume_symbol(";");
    }
  }
  // std::cerr << "\t" << __PRETTY_FUNCTION__ << "|" << __LINE__ << endl;
  return new StxBlock(stmts);
};

class StxIf : public StxStmt {
public:
  StxExpr *cond;
  StxBlock *thenb;
  vector<pair<StxExpr *, StxBlock *>> elifs;
  optional<StxBlock *> elseb;

  StxIf(StxExpr *cond, StxBlock *thenb,
        vector<pair<StxExpr *, StxBlock *>> elifs, optional<StxBlock *> elseb)
      : cond(cond), thenb(thenb), elifs(elifs), elseb(elseb){};

  void print(std::ostream &o, int indent) const {}
};

class StxReturn : public StxStmt {
public:
  StxExpr *e;
  StxReturn(StxExpr *e) : e(e) {}

  void print(std::ostream &o, int indent) const {
    o << "return ";
    e->print(o);
    o << "\n";
  }
};

// Statement(4.14)
StxStmt *parse_stmt(Tokenizer &t) {
  std::cerr << "\t" << __PRETTY_FUNCTION__ << endl;
  const auto is_fi_or_elif_or_else = [](Token t) {
    return t.kind == Token::Kind::TOK_KEYWORD &&
           (t.str == "fi" || t.str == "else" || t.str == "elif");
  };
  if (t.peek_keyword("if")) {
    t.consume_keyword("if");
    StxExpr *cond = parse_expr(t);
    t.consume_keyword("then");
    StxBlock *thenb = parse_stmts(t, is_fi_or_elif_or_else);
    vector<pair<StxExpr *, StxBlock *>> elifs;
    optional<StxBlock *> elseb;
    if (t.peek_keyword("fi")) {
      t.consume_keyword("fi");
      return new StxIf(cond, thenb, elifs, elseb);
    } else {
      while (1) {
        if (t.peek_keyword("elif")) {
          t.consume_keyword("elif");
          StxExpr *e = parse_expr(t);
          t.consume_keyword("then");
          StxBlock *b = parse_stmts(t, is_fi_or_elif_or_else);
          elifs.push_back({e, b});
          continue;
        } else if (t.peek_keyword("else")) {
          elseb = parse_stmts(t, is_fi_or_elif_or_else);
          t.consume_keyword("fi");
          break;
        } else {
          assert(false && "expected elif/else after a then");
        }
      }
    }
    return new StxIf(cond, thenb, elifs, elseb);

  } 
  else if (t.peek_keyword("return")) {
    t.consume_keyword("return");
    StxExpr *e = parse_expr(t);
    return new StxReturn(e);
  } else {
    return parse_assgn_or_procedure_call(t);
  }
}

int main(int argc, char **argv) {
  // parse gap/grp/basic.gd
  assert(argc == 2);
  FILE *f = fopen(argv[1], "r");

  // Determine file size
  fseek(f, 0, SEEK_END);
  size_t len = ftell(f);

  char *where = new char[len];

  rewind(f);
  fread(where, sizeof(char), len, f);
  Tokenizer t(len, where);
  StxBlock *toplevel =
      parse_stmts(t, [](Token t) { return t.kind == Token::Kind::TOK_EOF; });
}
