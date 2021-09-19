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

// TODO: upgrade this to take a space, not just a location.
void vprintfspan(Span span, const char *raw_input, const char *fmt,
                 va_list args) {
  char *outstr = nullptr;
  vasprintf(&outstr, fmt, args);
  assert(outstr);
  cerr << "===\n";
  cerr << span.begin << ":" << span.end << "\n";

  cerr << "===\n";
  cerr << span << "\t" << outstr << "\n";
  for (ll i = span.begin.si; i < span.end.si; ++i) {
    cerr << raw_input[i];
  }
  cerr << "\n===\n";
}

void printfspan(Span span, const char *raw_input, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vprintfspan(span, raw_input, fmt, args);
  va_end(args);
}

void vprintferr(Loc loc, const char *raw_input, const char *fmt, va_list args) {
  char *outstr = nullptr;
  vasprintf(&outstr, fmt, args);
  assert(outstr);

  cerr << "===\n";
  cerr << loc << "  " << outstr << "\n";
  // find the previous newloc character.
  int i = loc.si;
  for (; i >= 1 && raw_input[i - 1] != '\n'; i--) {
  }

  cerr << "Source file [" << loc << "]> ";
  for (; raw_input[i] != '\0' && raw_input[i] != '\n'; ++i) {
    if (i == loc.si) {
      cerr << "⌷";
    }
    cerr << raw_input[i];
  }
  cerr << "\n===\n";
  free(outstr);
}

void printferr(Loc loc, const char *raw_input, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vprintferr(loc, raw_input, fmt, args);
  va_end(args);
}

const set<std::string> keywords = {
    "Assert", "Info",     "IsBound", "Quit", "break",    "continue",
    "do",     "elif",     "else",    "end",  "false",    "fi",
    "for",    "function", "if",      "in",   "local",    "mod",
    "not",    "od",       "or",      "quit", "readonly", "readwrite",
    "rec",    "repeat",   "return",  "then", "true",     "until",
    "while"};

const std::string special = "\"`()*+,-#./:;<=>~[\\]^_{}";

const set<std::string> symbols = {"+",  "-",  "*",  "/",  "^", "~",  "!.",
                                  "=",  "<>", "<",  "<=", ">", ">=", "![",
                                  ":=", ".",  "..", "->", ",", ";",  "!{",
                                  "[",  "]",  "{",  "}",  "(", ")",  ":"};

struct Token {
  enum class Kind { TOK_SYMBOL, TOK_KEYWORD, TOK_IDENTIFIER, TOK_EOF };

  Token(Span span, Token::Kind kind, std::string str)
      : span(span), kind(kind), str(str){};
  void print(ostream &o) const { o << str; }

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

  static bool is_special(char c) {
    return c == '"' || c == '`' || c == '(' || c == ')' || c == '*' ||
           c == '+' || c == ',' || c == '-' || c == '#' || c == '.' ||
           c == '/' || c == ':' || c == ';' || c == '<' || c == '=' ||
           c == '>' || c == '~' || c == '[' || c == '\\' || c == ']' ||
           c == '^' || c == '_' || c == '{' || c == '}' || c == '!';
  }

  static bool is_keyword(string s) { return keywords.count(s); }

  static bool is_whitespace(char c) {
    return c == ' ' || c == '\t' || c == '\n';
  }

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
    assert(t.kind == Token::Kind::TOK_KEYWORD);
    assert(t.str == s);
    return t;
  }

  Token consume_identifier() {
    Token t = consume();
    assert(t.kind == Token::Kind::TOK_IDENTIFIER);
    return t;
  }

  bool peek_symbol(string s) {
    assert(symbols.count(s));
    Token t = peek();
    return t.kind == Token::Kind::TOK_SYMBOL && t.str == s;
  }

  bool peek_keyword(string s) {
    assert(keywords.count(s));
    Token t = peek();
    return t.kind == Token::Kind::TOK_KEYWORD && t.str == s;
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
        std::optional<char> c = peekc();
        if (!c) {
          return;
        }
        if (*c != '\n') {
          consumec(*c);
        }
      }
      if (ispeekc(' ')) {
        consumec(' ');
      }
      if (ispeekc('\t')) {
        consumec('\t');
      } else if (ispeekc('\n')) {
        consumec('\n');
      } else {
        return;
      }
    }
  }

  Token peek() {
    eat_whitespace();
    if (eof()) {
      return Token(Span(loc, loc), Token::Kind::TOK_EOF, "");
    }
    Loc lend = loc;

    const char ccur = *this->peekc();
    if (is_special(ccur)) {
      string s;
      while (1) {
        if (loc.si > len) {
          break;
        }
        char c = data[loc.si];
        if (is_whitespace(c) || is_special(c)) {
          break;
        }
        s += data[loc.si];
        lend = lend.next(c);
      }
      const Span span(loc, lend);
      if (is_keyword(s)) {
        return Token(span, Token::Kind::TOK_KEYWORD, s);
      } else {
        return Token(span, Token::Kind::TOK_IDENTIFIER, s);
      }
    } else {
      // we have a symbol
      string s;
      while (1) {
        if (loc.si > len) {
          break;
        }
        char c = data[loc.si];
        s += c;
        lend = lend.next(c);

        if (symbols.count(s)) {
          return Token(Span(loc, lend), Token::Kind::TOK_SYMBOL, s);
        }
      }
      assert(false && "uknown symbol");
    }
  }

  Token consume() {
    Token t = peek();
    assert(t.span.begin == loc);
    loc = t.span.end;
    return t;
  }
};

// SYNTAX
// ======

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

class StxStmt {
    public:
        virtual void print(std::ostream &o, int indent) const = 0;
};

class StxAssign : public StxStmt {
    public:
    StxAssign(Token lhs, StxExpr *rhs) : lhs(lhs), rhs(rhs) {};

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
    StxProcedureCall(Token name, vector<StxExpr *> args) : name(name), args(args) {};

    void print(std::ostream &o, int indent) const {
        name.print(o);
        o << "(";
        for(int i = 0; i < args.size(); ++i) {
            if (i > 0) { o << ", "; }
            args[i]->print(o);
        }
        o << ")";
    }
    Token name;
    vector<StxExpr *> args;
};


StxExpr *parse_expr_leaf(Tokenizer &t);
StxExpr *parse_expr(Tokenizer &t);

// expressions (4.7)
StxExpr *parse_expr(Tokenizer &t) { 
    return parse_expr_leaf(t);
}

// variable (4.8) or function call (4.11);
StxExpr *parse_expr_leaf(Tokenizer &t) {
  Token name = t.consume_identifier();
  if (t.peek_symbol("(")) {
    // function call
    t.consume_symbol("(");

    std::vector<StxExpr *> args;
    if (t.peek_symbol(")")) {
      t.consume_symbol(")");
      return new StxFnCall(name, args);
    } else {
      while (1) {
        args.push_back(parse_expr(t));
        if (t.peek_symbol(")")) {
          t.consume_symbol(")");
          break;
        } else if (t.peek_symbol(",")) {
          t.consume_symbol(",");
          continue;
        } else {
          assert(false && "expected , or ) in function call");
        }
      }
      return new StxFnCall(name, args);
    }
  } else {
    return new StxVar(name);
  }
}

StxStmt *parse_assgn_or_procedure_call(Tokenizer &t) {
  Token name = t.consume_identifier(); // todo: generalize to lvalue.
  if (t.peek_symbol("(")) {
    assert(false && "TODO: parse procedure call");
  } else if (t.peek_symbol(":=")) {
      StxExpr *rhs = parse_expr(t);
      return new StxAssign(name, rhs);
  }
  assert(false && "unknown symbol at top level");
}

class StxBlock {
  vector<StxStmt *> stmts;
  void print(std::ostream &o, int indent) const {}
};

StxBlock *parse_stmts(Tokenizer &t) {
  assert(false && "unimplemented");
};

class StxIf : public StxStmt {
  public:
  StxExpr *cond;
  StxBlock *thenb;
  vector<pair<StxExpr *, StxBlock *>> elifs;
  optional<StxBlock *>elseb;

  StxIf(StxExpr *cond, StxBlock *thenb, 
      vector<pair<StxExpr *, StxBlock *>> elifs,
      optional<StxBlock *>elseb) : cond(cond), thenb(thenb), elifs(elifs), elseb(elseb) {};

  void print(std::ostream &o, int indent) const {}
};

// Statement(4.14)
StxStmt *parse_stmt(Tokenizer &t) {
  if (t.peek_keyword("if")) {
    StxExpr *cond = parse_expr(t);
    t.consume_keyword("then");
    StxBlock *thenb = parse_stmts(t);
    vector<pair<StxExpr *, StxBlock *>> elifs;
    optional<StxBlock *>elseb;
    if (t.peek_keyword("fi")) {
      t.consume_keyword("fi");
      return new StxIf(cond, thenb, elifs, elseb);
    }
    while(1) {
      if (t.peek_keyword("elif")) {
        StxExpr *e = parse_expr(t);
        t.consume_keyword("then");
        StxBlock *b = parse_stmts(t);
        elifs.push_back({e, b});
        continue;
      } else if (t.peek_keyword("else")) {
        elseb = parse_stmts(t);
        t.consume_keyword("fi");
        break;
      } else {
        assert(false && "expected elif/else after a then");
      }
    }
    return new StxIf(cond, thenb, elifs, elseb);


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
  Tokenizer p(len, where);
}
