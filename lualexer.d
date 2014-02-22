module lualexer;

import std.array;
import std.file;
import std.algorithm;
import std.stdio;
import std.string;

import stdx.lexer;

/**
    Static tokens:

    A listing of the tokens whose exact value never changes and which cannot
    possibly be a token handled by the default token lexing function.
    The most common example of this kind of token is an operator such as
    "*", or "-" in a programming language.
*/
enum TokOperators =
[
    "+", "-", "*", "/", "%", "^", "#", "==", "~=", "<=", ">=", "<", ">", "=",
    "(", ")", "{", "}", "[", "]", "::", ":", ";", ",", ".", "..", "..."
];

/**
    Dynamic tokens:

    A listing of tokens whose value is variable, such as whitespace
    (e.g. one or more whitespace), identifiers, number literals
    (one or more numbers), and string literals.
*/
enum TokDynamic =
[
    "stringLiteral", "comment", "identifier", "numberLiteral", "whitespace"
];

/**
    Possible default tokens:

    A listing of tokens that could posibly be one of the tokens handled
    by the default token handling function. A common example of this is a
    keyword such as "for", which looks like the beginning of the identifier
    "fortunate". isSeparating is called to determine if the character after
    the 'r' separates the identifier, indicating that the token is "for",
    or if lexing should be turned over to the defaultTokenFunction.
*/
enum TokKeywords =
[
    "and", "break", "do", "else", "elseif", "end", "false", "for", "function",
    "goto", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then",
    "true", "until", "while"
];

/// A collection of all Lua tokens, for code reuse. Note: The order of these is important.
alias LuaTokens = TypeTuple!(TokOperators, TokDynamic, TokKeywords);

/**
    Token handlers:

    A mapping of prefixes to custom token handling function names.
    The generated lexer will search for the even-index elements of this
    array (0, 2, 4.., meaning the left column), and then call the function
    whose name is the element immedately after the even-indexed element.
    This is used for lexing complex tokens whose prefix is fixed.
*/
enum tokenHandlers =
[
    "[",  "lexBracket",
    "\"", "lexStringLiteral",
    "'",  "lexStringLiteral",
    "--", "lexComment",
    "0",  "lexNumber",
    "1",  "lexNumber",
    "2",  "lexNumber",
    "3",  "lexNumber",
    "4",  "lexNumber",
    "5",  "lexNumber",
    "6",  "lexNumber",
    "7",  "lexNumber",
    "8",  "lexNumber",
    "9",  "lexNumber",
    " ",  "lexWhitespace",
    "\t", "lexWhitespace",
    "\r", "lexWhitespace",
    "\n", "lexWhitespace",
];

/// The smallest unsigned integral type that can hold all these tokens.
alias TokID = TokenIdType!LuaTokens;

/// It's a ubyte, could have used 'alias TokID = ubyte;' but above version is safer.
static assert(is(TokID == ubyte));

/// toString function, takes a 'Token' structure and outputs a string representation of its
/// current value.
alias tokToString = tokenStringRepresentation!(TokID, LuaTokens);

/// Example
static assert(tokToString(tok!"and") == "and");

/// Maps a string representation into a TokID identifier. (range: 0 .. token.length)
/// Note: Must be named `tok`, the Lexer mixin template expects to implicitly find it.
alias tok(string symbol) = TokenId!(TokID, LuaTokens, symbol);

/// Examples
static assert(tok!"and" == 33);
static assert(tok!"+" == 1);
static assert(tok!"" == 0);  // "" is special, returns 0
static assert(tok!"\0" == 55);  // \0 is special, returns max valid token ID (55 in this case, with "" counted)
static assert(TokOperators.length + TokDynamic.length + TokKeywords.length == 54);  // special "" not counted

/// The actual Token structure, it will hold the TokenID instance with a
/// value in range [0 .. tokenCount].
/// The last argument is used to mixin some code or fields if necessary.
alias Token = TokenStructure!(TokID,
q{
    /// Better string representation
    void toString(scope void delegate(const(char)[]) sink) const
    {
        import std.conv;
        import %s;

        sink("(");
        sink(this.line.to!string);
        sink(":");
        sink(this.column.to!string);
        sink(")");
        sink(": ");
        sink(this.text ? this.text : tokToString(this.type));
    }
}.format(__MODULE__)
);

/// Example usage
enum Token _tokMin = Token(tok!"-");

///
static assert(_tokMin.type == tok!"-");
static assert(_tokMin.type == tok!(tokToString(tok!"-")));

/**
    Once Lexer!() is mixed-in, we need to define:

    - popFront():
        It should call the mixin's popFront first,
        but can afterwards be used to skip comments or tokens.

    - A function that serves as the default token lexing function.
      For most languages this will be the lexing function for identifiers
      -> lexIdentifier below.

    - A function that is able to determine if an identifier/keyword has come to an end.
      This function must retorn bool and take a single size_t argument representing the
      number of bytes to skip over before looking for a separating character.
      -> isSeparating below.

    - Any functions referred to in the tokenHandlers template parameter.
      These functions must be marked pure nothrow, take no arguments, and return a token.
      -> lexBracket, lexStringLiteral, lexComment, lexNumber, lexWhitespace

    - A constructor that initializes the range field, and calls popFront() exactly once
      to initialize the front field.
*/
struct LuaLexer
{
    /+
    mixin Lexer(IDType, Token, alias defaultTokenFunction,
        alias tokenSeparatingFunction, alias staticTokens, alias dynamicTokens,
        alias tokenHandlers, alias possibleDefaultTokens)
    +/

    mixin Lexer!(Token, lexIdentifier, isSeparating, tokenHandlers, LuaTokens);

    this(ubyte[] source, StringCache* cache)
    {
        this.range = source;
        this.cache = cache;
        popFront();
    }

    /// No additional work here, call Lexer's popFront.
    void popFront() pure
    {
        _popFront();
    }

    /// Token handler
    version (none)  // see below version which tracks line numbers.
    Token lexWhitespace() pure nothrow @safe
    {
        import std.ascii;
        mixin (tokenStart); // line, column, index, mark
        while (!range.empty && isWhite(range.front))
            range.popFront();
        string text = cache.cacheGet(range.slice(mark));
        return Token(tok!"whitespace", text, line, column, index);
    }

    /// Token handler: This one is taken directly from Dscanner's D lexer,
    /// with the major difference that it tracks the line numbers.
    Token lexWhitespace() pure nothrow
    {
        mixin (tokenStart);
        loop: do
        {
            switch (range.front)
            {
            case '\r':
                range.popFront();
                if (!range.empty && range.front == '\n')
                    range.popFront();
                range.incrementLine();
                break;
            case '\n':
                range.popFront();
                range.incrementLine();
                break;
            case ' ':
            case '\t':
                range.popFront();
                break;
            case 0xe2:
                auto lookahead = range.peek(3);
                if (lookahead.length != 3)
                    break loop;
                if (lookahead[1] != 0x80)
                    break loop;
                if (lookahead[2] == 0xa8 || lookahead[2] == 0xa9)
                {
                    range.popFront();
                    range.popFront();
                    range.popFront();
                    range.incrementLine();
                    break;
                }
                break loop;
            default:
                break loop;
            }
        } while (!range.empty);
        string text = cache.cacheGet(range.slice(mark));
        return Token(tok!"whitespace", text, line, column, index);
    }

    /// Token handler
    Token lexIdentifier() pure nothrow @safe
    {
        import std.ascii;
        mixin (tokenStart); // line, column, index, mark
        while (!range.empty && isAlphaNum(range.front) || range.front == '_')
            range.popFront();

        if (range.slice(mark).length == 0)
            return Token(tok!"");

        string text = cache.cacheGet(range.slice(mark));
        return Token(tok!"identifier", text, line, column, index);
    }

    /// Token handler
    Token lexComment() pure nothrow @safe
    {
        mixin (tokenStart); // line, column, index, mark
        range.popFrontN(2);
        if (!range.empty && range.front == '[')
            return lexLongComment(mark, line, column, index);
        else
            return lexShortComment(mark, line, column, index);
    }

    private Token lexLongComment(size_t mark, size_t line, size_t column, size_t index) pure nothrow @safe
    {
        range.popFront();
        TokID t = lexDoubleBrackets();
        if (t == tok!"")
            return Token(tok!"", null, 0, 0, 0);
        return Token(tok!"comment", cache.cacheGet(range.slice(mark)), line, column, index);
    }

    private Token lexShortComment(size_t mark, size_t line, size_t column, size_t index) pure nothrow @safe
    {
        while (!range.empty && range.front != '\r' && range.front != '\n')
            range.popFront();
        return Token(tok!"comment", cache.cacheGet(range.slice(mark)), line, column, index);
    }

    /// Token handler
    Token lexStringLiteral() pure nothrow @safe
    {
        mixin (tokenStart); // line, column, index, mark
        ubyte quote = range.front;
        range.popFront();
        while (true)
        {
            if (range.empty)
                return Token(tok!"", null, 0, 0, 0);
            if (range.front == '\\')
            {
                range.popFront();
                if (range.empty)
                    return Token(tok!"", null, 0, 0, 0);
                range.popFront();
            }
            else if (range.front == quote)
            {
                range.popFront();
                break;
            }
            else
                range.popFront();
        }
        return Token(tok!"stringLiteral", cache.cacheGet(range.slice(mark)), line, column, index);
    }

    /// Token handler
    Token lexNumber() pure nothrow
    {
        mixin (tokenStart); // line, column, index, mark
        if (range.canPeek(1) && range.front == '0')
        {
            auto ahead = range.peek(1)[1];
            switch (ahead)
            {
            case 'x':
            case 'X':
                range.popFront();
                range.popFront();
                return lexHex(mark, line, column, index);
            default:
                return lexDecimal(mark, line, column, index);
            }
        }
        else
            return lexDecimal(mark, line, column, index);
    }

    private Token lexHex(size_t mark, size_t line, size_t column, size_t index) pure nothrow
    {
        bool foundDot;
        hexLoop: while (!range.empty)
        {
            switch (range.front)
            {
            case 'a': .. case 'f':
            case 'A': .. case 'F':
            case '0': .. case '9':
                range.popFront();
                break;
            case 'p':
            case 'P':
                lexExponent();
                break hexLoop;
            case '.':
                if (foundDot)
                    break hexLoop;
                if (range.peek(1).length && range.peek(1)[0] == '.')
                    break hexLoop;
                range.popFront();
                foundDot = true;
                break;
            default:
                break hexLoop;
            }
        }
        return Token(tok!"numberLiteral", cache.cacheGet(range.slice(mark)), line, column, index);
    }

    private Token lexDecimal(size_t mark, size_t line, size_t column, size_t index) pure nothrow
    {
        bool foundDot = range.front == '.';
        if (foundDot)
            range.popFront();
        decimalLoop: while (!range.empty)
        {
            switch (range.front)
            {
            case '0': .. case '9':
                range.popFront();
                break;
            case 'e':
            case 'E':
                lexExponent();
                break decimalLoop;
            case '.':
                if (foundDot || !range.canPeek(1) || range.peekAt(1) == '.')
                    break decimalLoop;
                else
                {
                    // The following bit of silliness tries to tell the
                    // difference between "int dot identifier" and
                    // "double identifier".
                    if (range.canPeek(1))
                    {
                        switch (range.peekAt(1))
                        {
                        case '0': .. case '9':
                            goto doubleLiteral;
                        default:
                            break decimalLoop;
                        }
                    }
                    else
                    {
                    doubleLiteral:
                        range.popFront();
                        foundDot = true;
                    }
                }
                break;
            default:
                break decimalLoop;
            }
        }
        return Token(tok!"numberLiteral", cache.cacheGet(range.slice(mark)), line, column, index);
    }

    private void lexExponent() pure nothrow @safe
    {
        range.popFront();
        bool foundSign = false;
        bool foundDigit = false;
        while (!range.empty)
        {
            switch (range.front)
            {
            case '-':
            case '+':
                if (foundSign)
                    return;
                foundSign = true;
                range.popFront();
                break;
            case '0': .. case '9':
                foundDigit = true;
                range.popFront();
                break;
            default:
                return;
            }
        }
    }

    /// Token handler
    Token lexBracket() pure nothrow @safe
    {
        mixin (tokenStart); // line, column, index, mark
        range.popFront();
        if (range.empty || (range.front != '[' && range.front != '='))
            return Token(tok!"[", null, line, column, index);
        TokID t = lexDoubleBrackets();
        if (t == tok!"")
            return Token(tok!"", null, 0, 0, 0);
        return Token(tok!"stringLiteral", cache.cacheGet(range.slice(mark)), line, column, index);
    }

    private TokID lexDoubleBrackets() pure nothrow @safe
    {
        int equalCount = 0;
        while (true)
        {
            if (range.empty)
                return tok!"";
            if (range.front == '=')
            {
                range.popFront();
                equalCount++;
            }
            else if (range.front == '[')
            {
                range.popFront();
                break;
            }
        }
        string equalsSigns;
        foreach (i; 0 .. equalCount)
            equalsSigns ~= "=";
        string ending = "]" ~ equalsSigns ~ "]";
        while (true)
        {
            if (!range.canPeek(ending.length))
                return tok!"";
            if (range.peek(ending.length - 1) == ending)
            {
                range.popFrontN(ending.length);
                break;
            }
            else
                range.popFront();
        }
        return tok!"\0";
    }

    /// Check whether this is a keyword separated by some mark that we
    /// understand. In Lua we check whether we're at EOL, start of a comment,
    /// etc.
    bool isSeparating(size_t offset) pure nothrow @safe
    {
        if (!range.canPeek(offset)) return true;
        auto c = range.peekAt(offset);
        if (c >= 'A' && c <= 'Z') return false;
        if (c >= 'a' && c <= 'z') return false;
        if (c <= 0x2f) return true;
        if (c >= ':' && c <= '@') return true;
        if (c >= '[' && c <= '^') return true;
        if (c >= '{' && c <= '~') return true;
        if (c == '`') return true;
        return true;
    }

    /// Get all the current info for: line, column, index, mark.
    /// Later we can slice the byte range after N popFront's
    /// by providing the old position (mark) from which to slice
    /// up to the current position.
    enum tokenStart = q{
        size_t line = range.line;
        size_t column = range.column;
        size_t index = range.index;
        auto mark = range.mark();
    };

    StringCache* cache;
}

ubyte[] readFile(string fileName)
{
    import std.array;
    import std.conv;
    if (!exists(fileName))
    {
        stderr.writefln("%s does not exist", fileName);
        return [];
    }
    File f = File(fileName);
    ubyte[] sourceCode = uninitializedArray!(ubyte[])(to!size_t(f.size));
    f.rawRead(sourceCode);
    return sourceCode;
}

void main(string[] args)
{
    if (args.length == 1)
    {
        writeln("Error: Must pass a file path.");
        return;
    }

    ubyte[] source = readFile(args[1]);
    StringCache* cache = new StringCache(StringCache.defaultBucketCount);
    auto lexer = LuaLexer(source, cache);

    while (!lexer.empty)
    {
        auto token = lexer.front;
        writeln(token);
        lexer.popFront();
    }
}
