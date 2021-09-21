#include <fstream>
#include <iostream>
#include <optional>
#include <set>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <strings.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <tuple>
#include <type_traits>
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
  static void newline(ostream &o, int indent) {
    o << "\n";
    for (int i = 0; i < indent; ++i) {
      o << " ";
    }
  }

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
    "Assert", "Info",     "IsBound",   "Quit",  "TryNextMethod",
    "Unbind", "and",      "atomic",    "break", "continue",
    "do",     "elif",     "else",      "end",   "false",
    "fi",     "for",      "function",  "if",    "in",
    "local",  "mod",      "not",       "od",    "or",
    "quit",   "readonly", "readwrite", "rec",   "repeat",
    "return", "then",     "true",      "until", "while"};

const std::string special = "\"`()*+,-#./:;<=>~[\\]^_{}";

// why is "..." not a symbol?
const set<std::string> symbols = {
    "+",  "-", "*",  "/",  "^",  "~", "!.", "=",  "<>", "<",
    "<=", ">", ">=", "![", ":=", ".", "..", "->", ",",  ";",
    "!{", "[", "]",  "{",  "}",  "(", ")",  ":",
    "..." // TODO: check if I misunderstood something.
};

bool is_special(char c) {
  return c == '"' || c == '`' || c == '(' || c == ')' || c == '*' || c == '+' ||
         c == ',' || c == '-' || c == '#' || c == '.' || c == '/' || c == ':' ||
         c == ';' || c == '<' || c == '=' || c == '>' || c == '~' || c == '[' ||
         c == '\\' || c == ']' || c == '^' || c == '_' || c == '{' ||
         c == '}' || c == '!';
}

bool is_whitespace(char c) { return c == ' ' || c == '\t' || c == '\n'; }

struct Token {
  enum class Kind {
    TOK_SYMBOL,
    TOK_KEYWORD,
    TOK_IDENTIFIER,
    TOK_STRING,
    TOK_EOF
  };

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
  bool peek_eof() {
    eat_whitespace();
    return loc.si >= len;
  }

  Token consume_symbol(string s) {
    assert(symbols.count(s));
    optional<Token> t = peek_symbol(s);
    if (t) {
      assert(this->loc == t->span.begin);
      this->loc = t->span.end;
      return *t;
    } else {
      this->print_current_loc();
      cerr << "Expected symbol |" << s << "|\n";
      assert(false && "expected symbol");
    }
  }

  Token consume_keyword(string s) {
    assert(keywords.count(s));
    optional<Token> t = peek_keyword(s);
    if (t) {
      assert(this->loc == t->span.begin);
      this->loc = t->span.end;
      return *t;
    } else {
      this->print_current_loc();
      cerr << "Expected keyword |" << s << "|\n";
      assert(false && "expected keyword");
    }
  }

  Token consume_identifier() {
    optional<Token> t = peek_identifier();
    if (t) {
      assert(this->loc == t->span.begin);
      this->loc = t->span.end;
      return *t;
    } else {
      this->print_current_loc();
      cerr << "Expected identifer at " << this->loc << ".\n";
      assert(false && "did not find identifier (perhaps it is a keyword?)");
    }
  }

  Token consume_string() {
    optional<Token> t = this->peek_string();
    if (t) {
      assert(this->loc == t->span.begin);
      this->loc = t->span.end;
      return *t;
    } else {
      this->print_current_loc();
      cerr << "Expected string.\n";
      assert(false && "did not find string.");
    }
  }

  optional<Token> peek_symbol(string sym) {
    assert(symbols.count(sym));
    eat_whitespace();
    if (peek_eof()) {
      return {};
    }
    const char ccur = *this->peekc();
    if (!is_special(ccur)) {
      return {};
    }
    // we have a symbol
    string s;
    Loc lend = loc;
    while (1) {
      if (lend.si >= len) {
        break;
      }
      const char c = data[lend.si];
      lend = lend.next(c);
      s += c;
      if (s.size() == sym.size()) {
        break;
      }
    }

    if (s == sym) {
      return Token(Span(loc, lend), Token::Kind::TOK_SYMBOL, s);
    } else {
      return {};
    }
  }

  optional<Token> peek_string() {
    eat_whitespace();
    if (peek_eof()) {
      return {};
    }

    const char ccur = *this->peekc();
    // cerr << "\n\tpeeking string: " << ccur << "\n";
    if (ccur != '\"') {
      return {};
    }
    string s = "\"";
    Loc lend = this->loc.next("\""); // is after the opening "
    while (1) {
      if (lend.si >= len) {
        this->print_span(Span(loc, lend));
        assert(false && "unterminated \"");
      }
      const char c = data[lend.si];
      // TODO: string escape.
      s += c;
      lend = lend.next(c);
      if (c == '\"') {
        break;
      }
    }
    return Token(Span(loc, lend), Token::Kind::TOK_STRING, s);
  }

  optional<Token> peek_identifier() {
    eat_whitespace();
    if (peek_eof()) {
      return {};
    }
    const char ccur = *this->peekc();
    if (is_special(ccur)) {
      return {};
    }
    string s;
    Loc lend = loc;
    while (1) {
      if (lend.si >= len) {
        break;
      }
      const char c = data[lend.si];
      if (is_whitespace(c) || (c != '_' && is_special(c))) {
        break;
      }
      s += data[lend.si];
      lend = lend.next(c);
    }
    if (keywords.count(s)) {
      return {};
    }
    return Token(Span(loc, lend), Token::Kind::TOK_IDENTIFIER, s);
  }

  optional<Token> peek_keyword(string kwd) {
    assert(keywords.count(kwd));
    eat_whitespace();
    if (peek_eof()) {
      return {};
    }
    const char ccur = *this->peekc();
    if (is_special(ccur)) {
      return {};
    }
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
    if (s == kwd) {
      return Token(Span(loc, lend), Token::Kind::TOK_KEYWORD, s);
    } else {
      return {};
    }
  }

  void print_span(Span span) const {
    // TODO: handle multiline span
    cerr << "===\n";
    int i = span.begin.si;
    for (; i >= 1 && data[i - 1] != '\n'; i--) {
    }

    cerr << "Source file [" << span.begin << ":" << span.end << "]\n";
    const int nlines = span.end.line - span.begin.line + 1;
    if (span.begin.line == span.end.line) {
      string squiggle;
      printf("%4d>", span.begin.line);
      for (; data[i] != '\0' && data[i] != '\n'; ++i) {
        squiggle += i == span.begin.si                       ? '^'
                    : i == span.end.si                       ? '^'
                    : i >= span.begin.si && i <= span.end.si ? '~'
                                                             : ' ';
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
      for (; data[i] != '\0' && data[i] != '\n'; i++) {
        squiggle += i == span.begin.si ? '^' : i >= span.begin.si ? '~' : ' ';
        cerr << data[i];
      }
      printf("\n%4d>%s\n", span.begin.line, squiggle.c_str());
      int nlines = 0;
      for (i += 1; nlines < 2; i++) {
        if (data[i] == '\n') {
          nlines++;
          printf("%4d>", span.begin.line + nlines + 1);
        }
        cerr << data[i];
      }
      printf("%4d>%s\n", span.begin.line + 4, "  ...");

      nlines = 0;
      int i = span.end.si - 1;
      for (; i >= 1 && nlines <= 3; i--) {
        if (data[i] == '\n') {
          nlines++;
        }
      }

      nlines = 0;
      for (i += 1; nlines <= 2; i++) {
        if (data[i] == '\n') {
          nlines++;
          printf("\n%4d>", span.end.line - (2 - nlines) - 1);
        } else {
          cerr << data[i];
        }
      }

      squiggle = "";
      printf("\n%4d>", span.end.line);
      for (; data[i] != '\0' && data[i] != '\n'; i++) {
        squiggle += i == span.end.si - 1 ? '^' : i <= span.end.si ? '~' : ' ';
        cerr << data[i];
      }
      printf("\n%4d>%s\n", span.end.line, squiggle.c_str());
    }
    cerr << "\nSource file [" << span.begin << ":" << span.end << "]\n";
  }

  void print_loc(Loc l) const {
    if (l.si >= this->len) {
      printf("\n%4d>EOF", l.line);
      return;
    }
    cerr << "\n===\n";
    int i = l.si;
    for (; i >= 1 && data[i - 1] != '\n'; i--) {
    }

    printf("\n%4d>", l.line);
    string squiggle;
    for (; data[i] != '\0' && data[i] != '\n'; ++i) {
      squiggle += i == l.si ? '^' : ' ';
      cerr << data[i];
    }
    printf("\n%4d>%s\n", l.line, squiggle.c_str());
  }

  void print_current_loc() { this->print_loc(this->loc); }

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
};

// SYNTAX
// ======

class StxExpr;
class StxStmt;
class StxBlock;

class StxExpr {
public:
  virtual void print(std::ostream &o, int indent) const = 0;
};

class StxPermutation : public StxExpr {
public:
  StxPermutation(vector<StxExpr *> es) : es(es){};
  void print(std::ostream &o, int indent) const {
    o << "(";
    for (int i = 0; i < es.size(); ++i) {
      if (i > 0) {
        o << ", ";
      }
      es[i]->print(o, indent);
    }
    o << ")";
  }
  vector<StxExpr *> es;
};

class StxBool : public StxExpr {
public:
  StxBool(bool value) : value(value) {}
  void print(std::ostream &o, int indent) const {
    o << (value ? "true" : "false");
  }

private:
  bool value;
};

class StxFnCall : public StxExpr {
public:
  StxFnCall(Token name, std::vector<StxExpr *> args) : name(name), args(args){};
  Token name;
  std::vector<StxExpr *> args;
  void print(std::ostream &o, int indent) const {
    name.print(o);
    o << "(";
    for (int i = 0; i < args.size(); ++i) {
      if (i > 0) {
        o << ",";
      }
      args[i]->print(o, indent);
    }
    o << ")";
  }
};

class StxVar : public StxExpr {
public:
  StxVar(Token name) : name(name){};
  const Token name;

  void print(std::ostream &o, int indent) const { name.print(o); }
};

class StxStr : public StxExpr {
public:
  StxStr(Token str) : str(str){};
  const Token str;

  void print(std::ostream &o, int indent) const { str.print(o); }
};

class StxNot : public StxExpr {
public:
  StxNot(StxExpr *e) : e(e) {}
  StxExpr *e;

  void print(std::ostream &o, int indent) const {
    o << "not ";
    e->print(o, indent);
  }
};

class StxBinop : public StxExpr {
public:
  StxBinop(StxExpr *left, Token symbol, StxExpr *right)
      : left(left), symbol(symbol), right(right) {}
  StxExpr *left;
  StxExpr *right;
  Token symbol;

  void print(std::ostream &o, int indent) const {
    left->print(o, indent);
    o << " ";
    symbol.print(o);
    o << " ";
    right->print(o, indent);
  }
};

// l[ix]
class StxIndex : public StxExpr {
public:
  StxIndex(StxExpr *e, StxExpr *index) : e(e), index(index) {}
  StxExpr *e;
  StxExpr *index;

  void print(std::ostream &o, int indent) const {
    e->print(o, indent);
    o << "[";
    index->print(o, indent);
    o << "]";
  }
};

// [a, b, c]
class StxList : public StxExpr {
public:
  StxList(vector<StxExpr *> args) : args(args){};

  void print(std::ostream &o, int indent) const {
    o << "[";
    for (int i = 0; i < args.size(); ++i) {
      if (i > 0)
        o << ", ";
      args[i]->print(o, indent);
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

// TODO, HACK: currently parse anything as the LHS, actually should be a subset
// of expressions called lvalue, perhaps?
class StxAssign : public StxStmt {
public:
  StxAssign(StxExpr *lhs, StxExpr *rhs) : lhs(lhs), rhs(rhs){};

  void print(std::ostream &o, int indent) const {
    lhs->print(o, indent);
    o << " := ";
    rhs->print(o, indent);
    o << "\n";
  }
  StxExpr *lhs;
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
      args[i]->print(o, indent);
    }
    o << ")";
  }
  Token name;
  vector<StxExpr *> args;
};

// TODO: consider removing this and directly storing vector of statements.
class StxBlock {
public:
  vector<StxStmt *> stmts;
  StxBlock(vector<StxStmt *> stmts) : stmts(stmts) {}
  void print(std::ostream &o, int indent) const {
    indent += 1;
    for (int i = 0; i < stmts.size(); ++i) {
      stmts[i]->print(o, indent);
      o << ";";
      Doc::newline(o, indent);
    }
  }
};

class StxFnDefn : public StxExpr {
public:
  StxFnDefn(vector<Token> params, bool vararg, vector<Token> locals,
            StxBlock *stmts)
      : params(params), vararg(vararg), locals(locals), stmts(stmts){};

  void print(std::ostream &o, int indent) const {
    o << "function (";
    for (int i = 0; i < params.size(); ++i) {
      if (i > 0)
        o << ", ";
      o << params[i].str;
    }
    if (vararg) {
      o << "...";
    }
    o << ")";
    indent += 1;

    Doc::newline(o, indent);
    if (locals.size() > 0) {
      o << "local ";
      for (int i = 0; i < locals.size(); ++i) {
        if (i > 0) {
          o << ", ";
        }
        o << locals[i].str;
      }
      o << ";";
      Doc::newline(o, indent);
    }

    stmts->print(o, indent);
    Doc::newline(o, indent);
    o << "end";
    indent -= 1;
    Doc::newline(o, indent);
  }

private:
  vector<Token> params;
  bool vararg;
  vector<Token> locals;
  StxBlock *stmts;
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

  void print(std::ostream &o, int indent) const {
    o << "if ";
    cond->print(o, indent + 1);
    o << " then";
    Doc::newline(o, indent + 1);
    thenb->print(o, indent + 1);
    for (int i = 0; i < elifs.size(); ++i) {
      Doc::newline(o, indent);
      o << " elif ";
      elifs[i].first->print(o, indent + 1);
      o << " then";
      Doc::newline(o, indent + 1);
      elifs[i].second->print(o, indent + 1);
    }
    if (elseb) {
      Doc::newline(o, indent);
      o << " else ";
      Doc::newline(o, indent + 1);
      (*elseb)->print(o, indent + 1);
    }
    Doc::newline(o, indent);
    o << " fi";
  }
};

class StxReturn : public StxStmt {
public:
  StxExpr *e;
  StxReturn(StxExpr *e) : e(e) {}

  void print(std::ostream &o, int indent) const {
    o << "return ";
    e->print(o, indent);
  }
};

class StxFor : public StxStmt {
public:
  Token lhs;
  StxExpr *rhs;
  StxBlock *body;

  StxFor(Token lhs, StxExpr *rhs, StxBlock *body)
      : lhs(lhs), rhs(rhs), body(body) {}
  void print(std::ostream &o, int indent) const {
    o << "for " << lhs.str << " in ";
    rhs->print(o, indent + 1);
    o << " do";
    Doc::newline(o, indent + 1);
    body->print(o, indent + 1);
    Doc::newline(o, indent);
    o << "od";
  }
};

// expr ->
// expr_logical[and, or] ->
// expr_compare[>=, <=] ->
// expr_arith[+, -] ->
// expr_index["expr[index]"] ->
// expr_leaf
StxExpr *parse_expr_leaf(Tokenizer &t);
StxExpr *parse_expr_index(Tokenizer &t);
StxExpr *parse_expr(Tokenizer &t);
StxExpr *parse_expr_compare(Tokenizer &t);
StxStmt *parse_stmt(Tokenizer &t);

// list-expr -> expr
StxExpr *parse_list_expr(Tokenizer &t);

template <typename Token2Bool>
StxBlock *parse_stmts(Tokenizer &t, Token2Bool isEnd);

StxExpr *parse_expr_logical(Tokenizer &t) {
  StxExpr *l = parse_expr_compare(t);
  if (t.peek_keyword("and")) {
    Token and_ = t.consume_keyword("and");
    StxExpr *r = parse_expr(t);
    return new StxBinop(l, and_, r);
  } else if (t.peek_keyword("or")) {
    Token or_ = t.consume_keyword("or");
    StxExpr *r = parse_expr(t);
    return new StxBinop(l, or_, r);
  } else {
    return l;
  }
};

// 21. ranges
// https://www.gap-system.org/Manuals/doc/ref/chap21.html#X79596BDE7CAF8491
StxExpr *parse_list_expr(Tokenizer &t) { return parse_expr(t); }

// expressions (4.7)
StxExpr *parse_expr(Tokenizer &t) { return parse_expr_logical(t); }

// e[e]
StxExpr *parse_expr_index(Tokenizer &t) {

  StxExpr *l = parse_expr_leaf(t);
  if (t.peek_symbol("[")) {
    t.consume_symbol("[");
    StxExpr *ix = parse_expr(t);
    t.consume_symbol("]");
    return new StxIndex(l, ix);
  } else {
    return l;
  }
}

StxExpr *parse_expr_arith(Tokenizer &t) {
  StxExpr *l = parse_expr_index(t);
  if (t.peek_symbol("+")) {
    Token sym = t.consume_symbol("+");
    StxExpr *r = parse_expr(t);
    return new StxBinop(l, sym, r);
  } else if (t.peek_symbol("-")) {
    Token sym = t.consume_symbol("-");
    StxExpr *r = parse_expr(t);
    return new StxBinop(l, sym, r);
  } else if (t.peek_keyword("mod")) {
    Token sym = t.consume_keyword("mod");
    StxExpr *r = parse_expr(t);
    return new StxBinop(l, sym, r);
  } else {
    return l;
  }
}

StxExpr *parse_expr_compare(Tokenizer &t) {
  StxExpr *l = parse_expr_arith(t);
  if (t.peek_symbol("=")) {
    Token sym = t.consume_symbol("=");
    StxExpr *r = parse_expr(t);
    return new StxBinop(l, sym, r);
  } else if (t.peek_symbol("<")) {
    Token sym = t.consume_symbol("<");
    StxExpr *r = parse_expr(t);
    return new StxBinop(l, sym, r);
  } else {
    return l;
  }
};

// 4.11
// parse expressions delimited by sl, sr
vector<StxExpr *> parse_exprs_delimited(Tokenizer &t, string sl, string sr) {
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
      t.print_current_loc();
      cerr << "expected list of expressions of the form |" << sl
           << "expr, expr, ... " << sr << "|\n";
      assert(false && "unknown parse in expression sequence");
    }
  }
  return args;
}

StxExpr *parse_fn_defn(Tokenizer &t) {
  t.consume_keyword("function"); // consume "function"

  t.consume_symbol("(");
  bool vararg = false;
  vector<Token> params;
  while (1) {
    Token param = t.consume_identifier();
    params.push_back(param);
    if (t.peek_symbol(")")) {
      t.consume_symbol(")");
      break;
    } else if (t.peek_symbol(",")) {
      t.consume_symbol(",");
      continue;
    } else if (t.peek_symbol("...")) {
      t.consume_symbol("...");
      t.consume_symbol(")");
      vararg = true;
      break;
    } else {
      t.print_current_loc();
      cerr << "Expected , or ) in function definition argument list\n";
      assert(false && "unknown symbol in function definition argument list");
    }
  }

  // now parse locals
  vector<Token> locals;
  if (t.peek_keyword("local")) {
    t.consume_keyword("local");
    while (1) {
      locals.push_back(t.consume_identifier());
      if (t.peek_symbol(";")) {
        t.consume_symbol(";");
        break;
      } else if (t.peek_symbol(",")) {
        t.consume_symbol(",");
        continue;
      } else {
        t.print_current_loc();
        cerr << "Expected ; or , in function definition argument list\n";
        assert(false && "expect ; or , in locals list");
      }
    }
  }

  // done parsng function params. now parse statements.
  StxBlock *stmts = parse_stmts(
      t, [](Tokenizer &t) -> bool { return bool(t.peek_keyword("end")); });
  t.consume_keyword("end");
  return new StxFnDefn(params, vararg, locals, stmts);
}

// variable (4.8) or function call (4.11) or string or function defn
// MUST make progress
StxExpr *parse_expr_leaf(Tokenizer &t) {
  if (t.peek_keyword("true")) {
    t.consume_keyword("true");
    return new StxBool(true);
  } else if (t.peek_keyword("false")) {
    t.consume_keyword("false");
    return new StxBool(false);
  } else if (t.peek_keyword("not")) {
    t.consume_keyword("not");
    StxExpr *e = parse_expr(t);
    return new StxNot(e);

  } else if (optional<Token> s = t.peek_string()) {
    t.consume_string();
    return new StxStr(*s);
  } else if (optional<Token> ident = t.peek_identifier()) {
    t.consume_identifier();
    // 4.23: function gap> Sum( List( [1..100], x -> x^2 ) );
    // we must parse x -> <expr> for anonymous function.
    // { arg-list } -> expr
    // this is sugar for
    //   function ( arg-list ) return expr; end.
    if (t.peek_symbol("->")) {
      t.consume_symbol("->");
      StxExpr *rhs = parse_expr(t);
      const vector<Token> params = {*ident};
      const vector<Token> locals;
      const bool vararg = false;
      return new StxFnDefn(params, vararg, locals,
                           new StxBlock({new StxReturn(rhs)}));
    } else if (t.peek_symbol("(")) {
      std::vector<StxExpr *> args = parse_exprs_delimited(t, "(", ")");
      return new StxFnCall(*ident, args);
    } else {
      return new StxVar(*ident);
    }
  } else if (t.peek_symbol("[")) {
    vector<StxExpr *> args = parse_exprs_delimited(t, "[", "]");
    return new StxList(args);
  } else if (t.peek_keyword("function")) {
    return parse_fn_defn(t);
  } else if (t.peek_symbol("(")) {
    t.consume_symbol("(");
    // empty permutation
    if (t.peek_symbol(")")) {
      t.consume_symbol(")");
      return new StxPermutation({});
    }

    // TODO: ambiguous? can be either (permutation) or (expr)?
    StxExpr *e = parse_expr(t);
    if (t.peek_symbol(")")) {
      t.consume_symbol(")");
      return e;
    } else if (t.peek_symbol(",")) {
      // okay, we have a permutation!
      vector<StxExpr *> es;
      es.push_back(e);

      t.consume_symbol(",");
      while (1) {
        es.push_back(parse_expr(t));
        if (t.peek_symbol(")")) {
          t.consume_symbol(")");
          return new StxPermutation(es);
        } else if (t.peek_symbol(",")) {
          t.consume_symbol(",");
          continue;
        } else {
          t.print_current_loc();
          cerr << "expected , or ) to continue permutation";
          assert(false);
        }
      }
    } else {
      t.print_current_loc();
      cerr << "expected ) to close bracketed expression or , to create "
              "permutation";
      assert(false);
    }
  } else {
    t.print_current_loc();
    assert(false && "unknown expression leaf token");
    exit(1);
  }
}

StxStmt *parse_assgn_or_procedure_call(Tokenizer &t) {
  Token name = [&]() -> Token {
    vector<string> allowedKeywords = {"Info", "TryNextMethod"};
    for (string kwd : allowedKeywords) {
      if (optional<Token> info = t.peek_keyword(kwd)) {
        t.consume_keyword(kwd);
        return *info;
      }
    }
    return t.consume_identifier();
  }();

  if (t.peek_symbol("(")) {
    std::vector<StxExpr *> args = parse_exprs_delimited(t, "(", ")");
    return new StxProcedureCall(name, args);
  } else if (t.peek_symbol("[")) {
    // <name>[ix] := rhs
    t.consume_symbol("[");
    StxExpr *ix = parse_expr(t);
    t.consume_symbol("]");
    t.consume_symbol(":=");
    StxExpr *rhs = parse_expr(t);
    return new StxAssign(new StxIndex(new StxVar(name), ix), rhs);
  } else if (t.peek_symbol(":=")) {
    t.consume_symbol(":=");
    StxExpr *rhs = parse_expr(t);
    return new StxAssign(new StxVar(name), rhs);
  }
  std::cerr << "unknown toplevel symbol for assign/procedure call:  |";
  t.print_current_loc();
  assert(false && "unknown symbol at top level");
}

StxStmt *parse_stmt(Tokenizer &t);

template <typename Token2Bool>
StxBlock *parse_stmts(Tokenizer &t, Token2Bool isEnd) {
  vector<StxStmt *> stmts;
  while (1) {
    // std::cerr << "\t" << __PRETTY_FUNCTION__ << "|" << __LINE__ << endl;
    if (isEnd(t)) {
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

// Statement(4.14)
StxStmt *parse_stmt(Tokenizer &t) {
  std::cerr << "\t" << __PRETTY_FUNCTION__ << endl;
  const auto is_fi_or_elif_or_else = [](Tokenizer &t) -> bool {
    return bool(t.peek_keyword("fi")) || bool(t.peek_keyword("else")) ||
           bool(t.peek_keyword("elif"));
  };

  if (t.peek_keyword("if")) {
    t.consume_keyword("if");
    StxExpr *cond = parse_expr(t);
    t.consume_keyword("then");
    StxBlock *thenb = parse_stmts(t, is_fi_or_elif_or_else);
    vector<pair<StxExpr *, StxBlock *>> elifs;
    optional<StxBlock *> elseb;
    while (1) {
      if (t.peek_keyword("elif")) {
        t.consume_keyword("elif");
        StxExpr *e = parse_expr(t);
        t.consume_keyword("then");
        StxBlock *b = parse_stmts(t, is_fi_or_elif_or_else);
        elifs.push_back({e, b});
        continue;
      } else if (t.peek_keyword("else")) {
        t.consume_keyword("else");
        elseb = parse_stmts(t, is_fi_or_elif_or_else);
        t.consume_keyword("fi");
        break;
      } else if (t.peek_keyword("fi")) {
        t.consume_keyword("fi");
        break;
      } else {
        t.print_current_loc();
        assert(false && "expected elif/else/fi after a then");
      }
    }
    return new StxIf(cond, thenb, elifs, elseb);
  } else if (t.peek_keyword("return")) {
    t.consume_keyword("return");
    StxExpr *e = parse_expr(t);
    return new StxReturn(e);
  } else if (t.peek_keyword("for")) {
    t.consume_keyword("for");
    // for simple-var in list-expr do statements od;
    Token var = t.consume_identifier();
    t.consume_keyword("in");
    StxExpr *e = parse_list_expr(t);
    t.consume_keyword("do");
    StxBlock *stmts = parse_stmts(
        t, [](Tokenizer &t) -> bool { return bool(t.peek_keyword("od")); });
    t.consume_keyword("od");
    return new StxFor(var, e, stmts);

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
      parse_stmts(t, [](Tokenizer &t) -> bool { return t.peek_eof(); });

  cerr << "\n###PARSED###\n";
  toplevel->print(cout, 0);
}
