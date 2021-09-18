#include <fstream>
#include <iostream>
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
#include <optional>
#include <vector>
#include <set>

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
  static Loc beginning_of_file() {
      return Loc(0, 1, 1);
  }
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
  Span(Loc begin, Loc end) : begin(begin), end(end) { assert(end.si >= begin.si); };
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
  "Assert", "Info", "IsBound", "Quit",
  "break", "continue", "do", "elif",
  "else", "end", "false", "fi",
  "for", "function", "if", "in",
  "local", "mod", "not", "od",
  "or", "quit", "readonly", "readwrite",
  "rec", "repeat", "return", "then",
  "true", "until", "while"
};


const std::string special =  "\"`()*+,-#./:;<=>~[\\]^_{}";

const set<std::string> symbols = {
  "+", "-", "*", "/", "^", "~", "!.",
  "=", "<>", "<", "<=", ">", ">=", "![",
  ":=", ".", "..", "->", ",", ";", "!{",
  "[", "]", "{", "}", "(",")", ":"
};



struct Token {
  enum class Kind {
    TOK_SYMBOL,
    TOK_KEYWORD,
    TOK_IDENTIFIER
  } kind;

  Token(Span span, Token::Kind kind, std::string str) : span(span), kind(kind), str(str) {};
  std::string str;
  Span span;
};

struct Tokenizer {
  const char* data;
  int len;
  Loc loc;


  Tokenizer(int len, const char *data) : len(len), data(data), loc(Loc::beginning_of_file()) {}

  static bool is_special(char c) {
    return c == '"' ||
      c == '`'    || c == '(' || c == ')' || c == '*' || c == '+' || c == ',' || c == '-' || c == '#'
       || c == '.' || c == '/' || c == ':' || c == ';' || c == '<' || c == '=' || c == '>' || c == '~' ||
        c == '[' || c == '\\' || c == ']' || c == '^' || c == '_' || c == '{' || c == '}' || c == '!';
  }

  static bool is_keyword(string s) {
    return keywords.count(s); 
  }

  static bool is_whitespace(char c) {
    return c == ' ' || c == '\t' || c == '\n';
  }

  bool ispeek(char c) const {
    if (loc.si > len) { return false; }
    return data[loc.si] == c;

  }

  std::optional<char> peek() const {
    if (loc.si > len) { return {}; }
    return data[loc.si];
  }

  bool eof() const { 
    return loc.si > len;
  }

  void eat_whitespace() {
    while(1) {
      if (ispeek('#')) {
          std::optional<char> c = peek();
        if (!c) { return; }
        if (*c != '\n') {
            consume(*c);
        }
      }
      if (ispeek(' ')) { consume (' '); }
      if (ispeek('\t')) { consume ('\t'); }
      else if (ispeek('\n')) { consume ('\n'); }
      else { return; }
    }
  }

  std::optional<Token> token() {
    eat_whitespace();
    if (eof()) { return {}; }
    const Loc lbegin = loc;

    const char peekc = *this->peek(); 
    if (is_special(peekc)) {
      string s;
      while(1) {
        if (loc.si > len) { break; }
        char c = data[loc.si];
        if (is_whitespace(c) || is_special(c)) { break;}
        s += data[loc.si];
        loc = loc.next(c);
      }
      const Span span(lbegin, loc);
      if (is_keyword(s)) {
        return Token(span, Token::Kind::TOK_KEYWORD, s);
      } else {
        return Token(span, Token::Kind::TOK_IDENTIFIER, s);
      }
    } else {
      // we have a symbol
      string s;
      while(1) {
        if (loc.si > len) { break; }
        char c = data[loc.si];
        s += c;
        loc = loc.next(c);

        if (symbols.count(s)) {
          return Token(Span(lbegin, loc), Token::Kind::TOK_SYMBOL, s);
        }
      }
      assert(false && "uknown symbol");
    }
  }

  void consume(char c) {
    assert(ispeek(c));
    loc = loc.next(c);

  }

private:

};


int main(int argc, char **argv) {
  assert(argc == 2);
  FILE* f = fopen(argv[1], "r");

 // Determine file size
 fseek(f, 0, SEEK_END);
 size_t len = ftell(f);

 char* where = new char[len];

 rewind(f);
 fread(where, sizeof(char), len, f);
 Tokenizer p(len, where);


}
