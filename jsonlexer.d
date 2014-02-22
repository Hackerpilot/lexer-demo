/**
 * JSON lexer.
 * Authors: Brian Schott
 * Standards: $(LINK2 http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf, ECMA-404)
 */

module jsonlexer;

import std.file;
import std.algorithm;
import stdx.lexer;
import std.stdio;

// JSON is pretty simple. These are the only fixed-representation tokens.
// Normally things like "true", and "false" would be possible default tokens,
// but JSON has no identifiers that they could be confused with.
private enum fixedTokens = [
	"{", "}", "[", "]", ",", ":", "true", "false", "null"
];

// Empty. JSON has no keywords that could be confused with any sort of dynamic
// token.
private enum keywords = [];

// Very simple
private enum dynamicTokens = [
	"string", "number", "whitespace"
];

// Map the beginning of the dynamic tokens to their handling functions
private enum tokenHandlers = [
	"\"", "lexStringLiteral",
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
	"\n", "lexWhitespace"
];

alias IdType = TokenIdType!(fixedTokens, dynamicTokens, keywords);

public alias str = tokenStringRepresentation!(IdType, fixedTokens, dynamicTokens, keywords);

template tok(string token)
{
	alias tok = TokenId!(IdType, fixedTokens, dynamicTokens, keywords, token);
}

enum extraFields = "";

alias Token = TokenStructure!(IdType, extraFields);

struct JSONLexer
{
	mixin Lexer!(Token, lexError, isSeparating, fixedTokens,
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

private:

	Token lexWhitespace() pure nothrow @safe
	{
		import std.ascii;
		mixin (tokenStart);
		while (!range.empty && isWhite(range.front))
			range.popFront();
		string text = cache.cacheGet(range.slice(mark));
		return Token(tok!"whitespace", text, line, column, index);
	}

	Token lexError() pure nothrow @safe
	{
		range.popFront();
		return Token(tok!"", null, 0, 0, 0);
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
		return Token(tok!"string", cache.cacheGet(range.slice(mark)), line,
			column, index);
	}

	Token lexNumber() pure nothrow
	{
		mixin (tokenStart);
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
		return Token(tok!"number", cache.cacheGet(range.slice(mark)),
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

	// JSON has no valid default token such as an identifier, so the JSON lexer
	// will never need to determine if it is at the end of an identifier.
	// Therefore, this always returns false.
	bool isSeparating(size_t offset) pure nothrow @safe
	{
		return true;
	}

	private enum tokenStart = q{
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
	auto lexer = JSONLexer(source, cache);
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
