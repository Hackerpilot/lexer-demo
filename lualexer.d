import std.file;
import std.algorithm;
import stdx.lexer;
import std.stdio;

private enum operators = [
	"+", "-", "*", "/", "%", "^", "#", "==", "~=", "<=", ">=", "<", ">", "=",
	"(", ")", "{", "}", "[", "]", "::", ":", ";", ",", ".", "..", "..."
];

private enum keywords = [
	"and", "break", "do", "else", "elseif", "end", "false", "for", "function",
	"goto", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then",
	"true", "until", "while"
];

private enum dynamicTokens = [
	"stringLiteral", "comment", "identifier", "numberLiteral", "whitespace"
];

private enum tokenHandlers = [
	"[", "lexBracket",
	"\"", "lexStringLiteral",
	"'", "lexStringLiteral",
	"--", "lexComment",
	"0", "lexNumber",
	"1", "lexNumber",
	"2", "lexNumber",
	"3", "lexNumber",
	"4", "lexNumber",
	"5", "lexNumber",
	"6", "lexNumber",
	"7", "lexNumber",
	"8", "lexNumber",
	"9", "lexNumber",
	" ", "lexWhitespace",
	"\t", "lexWhitespace",
	"\r", "lexWhitespace",
	"\n", "lexWhitespace",
];

alias IdType = TokenIdType!(operators, dynamicTokens, keywords);

public alias str = tokenStringRepresentation!(IdType, operators, dynamicTokens, keywords);

template tok(string token)
{
	alias tok = TokenId!(IdType, operators, dynamicTokens, keywords, token);
}

enum extraFields = "";

alias Token = TokenStructure!(IdType, extraFields);

struct LuaLexer
{
	mixin Lexer!(Token, lexIdentifier, isSeparating, operators,
		dynamicTokens, keywords, tokenHandlers);

	this(ubyte[] source, StringCache* cache)
	{
		this.range = LexerRange(source);
		this.cache = cache;
		popFront();
	}

	void popFront() pure
	{
		_popFront();
	}

	Token lexWhitespace() pure nothrow @safe
	{
		import std.ascii;
		mixin (tokenStart);
		while (!range.empty && isWhite(range.front))
			range.popFront();
		string text = cache.cacheGet(range.slice(mark));
		return Token(tok!"whitespace", text, line, column, index);
	}

	Token lexIdentifier() pure nothrow @safe
	{
		import std.ascii;
		mixin (tokenStart);
		while (!range.empty && isAlphaNum(range.front) || range.front == '_')
		{
			range.popFront();
		}
		if (range.slice(mark).length == 0)
			return Token(tok!"");
		string text = cache.cacheGet(range.slice(mark));
		return Token(tok!"identifier", text, line, column, index);
	}

	Token lexComment() pure nothrow @safe
	{
		mixin (tokenStart);
		range.popFrontN(2);
		if (!range.empty && range.front == '[')
			return lexLongComment(mark, line, column, index);
		else
			return lexShortComment(mark, line, column, index);
	}

	Token lexShortComment(size_t mark, size_t line, size_t column, size_t index) pure nothrow @safe
	{
		while (!range.empty && range.front != '\r' && range.front != '\n')
			range.popFront();
		return Token(tok!"comment", cache.cacheGet(range.slice(mark)), line,
			column, index);
	}

	Token lexLongComment(size_t mark, size_t line, size_t column, size_t index) pure nothrow @safe
	{
		range.popFront();
		IdType t = lexDoubleBrackets();
		if (t == tok!"")
			return Token(tok!"", null, 0, 0, 0);
		return Token(tok!"comment", cache.cacheGet(range.slice(mark)), line,
			column, index);
	}

	Token lexStringLiteral() pure nothrow @safe
	{
		mixin (tokenStart);
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
		return Token(tok!"stringLiteral", cache.cacheGet(range.slice(mark)), line,
			column, index);
	}

	Token lexNumber() pure nothrow
	{
		mixin (tokenStart);
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

	Token lexHex(size_t mark, size_t line, size_t column, size_t index) pure nothrow
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
		return Token(tok!"numberLiteral", cache.cacheGet(range.slice(mark)), line, column,
			index);
	}

	Token lexDecimal(size_t mark, size_t line, size_t column, size_t index) pure nothrow
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
		return Token(tok!"numberLiteral", cache.cacheGet(range.slice(mark)),
			line, column, index);
	}

	void lexExponent() pure nothrow @safe
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

	Token lexBracket() pure nothrow @safe
	{
		mixin (tokenStart);
		range.popFront();
		if (range.empty || (range.front != '[' && range.front != '='))
			return Token(tok!"[", null, line, column, index);
		IdType t = lexDoubleBrackets();
		if (t == tok!"")
			return Token(tok!"", null, 0, 0, 0);
		return Token(tok!"stringLiteral", cache.cacheGet(range.slice(mark)),
			line, column, index);
	}

	IdType lexDoubleBrackets() pure nothrow @safe
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

	enum tokenStart = q{
		size_t line = range.line;
		size_t column = range.column;
		size_t index = range.index;
		auto mark = range.mark();
	};

	StringCache* cache;
}

void main(string[] args)
{
	import std.array;
	ubyte[] source = readFile(args[1]);
	StringCache* cache = new StringCache(StringCache.defaultBucketCount);
	auto lexer = LuaLexer(source, cache);
	while (!lexer.empty)
	{
		auto token = lexer.front();
		writeln("<<", token.text is null ? str(token.type) : token.text, ">>");
		lexer.popFront();
	}
	writeln("done");
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
