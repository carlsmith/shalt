### shlat.js | https://github.com/carlsmith/shalt

    Shalt JS | Custom JavaScript Dialects
    Carl Smith 2016 GPL3 | CoffeeScript 1.10

###

# Generic Helper Functions...

put = (args...) -> console.log args...

init = (amount) -> undefined for n in [1..amount]

factory = (mutator) -> (args...) ->
    mutator (self = Object.create null), args...
    return self

upgrade = (mutator) -> (self, args...) ->
    mutator self, args...
    return self

# The Parser API Constructor...

Parser = factory (self) ->

    # Configuration State...

    keywords = []
    constants = {}
    namedOperators = []
    symbolicOperators = {}
    prefixHandlers = {}
    infixHandlers = {}
    bindingPowers = {}

    # Internal Helper Functions...

    self.handlePrefix = (token) ->
        prefixHandlers[token.type] token

    self.handleInfix = (token, node) ->
        infixHandlers[token.type] token, node

    # The High Level API Functions...

    self.createSymbolicPrefix = (name, symbol, lbp=0, handler=undefined) ->
        symbolicOperators[symbol] = name
        prefixHandlers[name] = handler or upgrade (token) ->
            token.right = token.subparse lbp
            token.updateEndPosition token, token.right

    self.createGoofySymbolicPrefix = (name, symbol, lbp=0, handler=undefined) ->
        symbolicOperators[symbol] = name
        prefixHandlers[name] = handler or upgrade (token) ->
            token.right = token.subparse lbp - 1
            token.updateEndPosition token, token.right

    self.createSymbolicInfix = (name, symbol, lbp=0, handler=undefined) ->
        symbolicOperators[symbol] = name
        bindingPowers[name] = lbp
        infixHandlers[name] = handler or upgrade (token, left) ->
            token.left = left
            token.right = token.subparse token.lbp
            token.updateStartPosition token, left
            token.updateEndPosition token, token.right

    self.createGoofySymbolicInfix = (name, symbol, lbp=0, handler=undefined) ->
        symbolicOperators[symbol] = name
        bindingPowers[name] = lbp
        infixHandlers[name] = handler or upgrade (token, left) ->
            token.left = left
            token.right = token.subparse token.lbp - 1
            token.updateStartPosition token, left
            token.updateEndPosition token, token.right

    self.createNamedPrefix = (name, lbp=0, handler=undefined) ->
        namedOperators.push name
        prefixHandlers[name] = handler or upgrade (token) ->
            token.right = token.subparse lbp
            token.updateEndPosition token, token.right

    self.createGoofyNamedPrefix = (name, lbp=0, handler=undefined) ->
        namedOperators.push name
        prefixHandlers[name] = handler or upgrade (token) ->
            token.right = token.subparse lbp - 1
            token.updateEndPosition token, token.right

    self.createNamedInfix = (name, lbp=0, handler=undefined) ->
        namedOperators.push name
        bindingPowers[name] = lbp
        infixHandlers[name] = handler or upgrade (token, left) ->
            token.left = left
            token.right = token.subparse token.lbp
            token.updateStartPosition token, left
            token.updateEndPosition token, token.right

    self.createSymbolicSuffix = (name, symbol, lbp=0, handler=undefined) ->
        symbolicOperators[symbol] = name
        bindingPowers[name] = lbp
        infixHandlers[name] = handler or upgrade (token, left) ->
            token.left = left
            token.updateStartPosition token, left

    self.createTerminal = (terminal) ->
        self.createNamedPrefix terminal, 0, upgrade ->

    self.createConstant = (name, type) ->
        prefixHandlers[type] = upgrade ->
        constants[name] = type

    self.createNamedOperatorToken = (name) ->
        self.createNamedPrefix name

    self.createPredicatedBlock = (name, lbp) ->
        self.createNamedPrefix name, lbp, upgrade (token) ->
            token.block = []
            token.gatherPredicatedBlock token, token.block

    # The Low Level API Functions...

    self.updateStartPosition = upgrade (self, openingToken) ->
        self.start = openingToken.start
        self.loc.start.line = openingToken.loc.start.line
        self.loc.start.column = openingToken.loc.start.column

    self.updateEndPosition = upgrade (self, closingToken) ->
        self.end = closingToken.end
        self.loc.end.line = closingToken.loc.end.line
        self.loc.end.column = closingToken.loc.end.column

    self.gatherSequence = (token, target, closer) ->
        token.ignore "endStatement"
        until token.peek().type is closer
            target.push token.subparse()
            token.ignore "endStatement"
        closingToken = (token.advance closer)[0]
        token.updateEndPosition token, closingToken

    self.gatherGroup = (token, target) ->
        token.gatherSequence token, target, "closeParen"

    self.gatherArray = (token, target) ->
        token.gatherSequence token, target, "closeBracket"

    self.gatherBlock = (token, target) ->
        token.gatherSequence token, target, "closeBrace"

    self.gatherPredicatedBlock = (token, target) ->
        token.test = token.subparse token.lbp
        token.advance "openBrace"
        token.gatherBlock token, target

    # The Lexer Function...

    self.lex = (source, filename=null) ->

        # Lexer state...

        lastToken = null
        source = source.trimRight()
        [word, char, line, index, offset] = ["", "", 1, 0, 1]
        [tokenStartLine, tokenStartColumn, tokenStartIndex] = [1, 0, 0]

        # Helpful constants...

        [singlequote, doublequote] = ["'", '"']
        [space, dot, comma, hash] = [" ", ".", ",", "#"]
        [escape, newline, tab, emptyString] = ["\\", "\n", "\t", ""]

        quotes = ["'", '"']
        digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

        symbols = [
            "<", ">", "=", ".", "`", "#", "~", "@", "?", "!", "$", "_", "|"
            "&", "+", "*", "^", "-", "/", "%", ",", ";", ":"
            ]

        escapees =
            "s" : space
            "n" : newline
            "t" : tab
            "\\": escape
            "'" : singlequote
            '"' : doublequote

        openers = "(": "openParen", "[": "openBracket", "{": "openBrace"
        closers = ")": "closeParen", "]": "closeBracket", "}": "closeBrace"

        operatorHints = new Set(name[0] for name of symbolicOperators)

        startNameChars = "$_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        laterNameChars = startNameChars + "0123456789"

        # Helper functions...

        Token = (type, value=word) ->
            word = emptyString
            token = Object.create self
            token.type = type
            token.value = value
            token.lbp = bindingPowers[type]
            token.start = tokenStartIndex
            token.end = index - 1
            token.loc =
                start: line: tokenStartLine, column: tokenStartColumn
                end: line: line, column: index - offset
            return lastToken = token

        updatePosition = ->
            tokenStartLine = line
            tokenStartColumn = index - offset
            tokenStartIndex = index - 1

        gatherOne = ->
            updatePosition()
            word += char
            advance()

        gatherWhile = (check) ->
            updatePosition()
            loop
                return char unless char and check char
                word += char
                advance()

        peek = -> source[index]

        do advance = -> char = source[index++]

        # The Main Loop...

        while char

            if char is space then advance()

            else if char is comma

                gatherOne()
                yield Token "endStatement", word

            else if char in startNameChars

                gatherWhile -> char in laterNameChars
                yield Token(
                    if word in keywords then word
                    else if word in namedOperators then word
                    else if word of constants then constants[word]
                    else "identifier"
                    )

            else if char in digits

                gatherWhile ->
                    if char in digits then true
                    else if char is dot and dot not in word
                        peek() in digits
                    else false
                yield Token "numberLiteral"

            else if char is hash

                gatherWhile -> char and char isnt newline
                word = emptyString

            else if char of openers

                gatherOne()
                yield Token openers[word], word

            else if char of closers

                gatherOne()
                yield Token closers[word], word

            else if operatorHints.has char

                gatherWhile -> char in symbols
                yield Token symbolicOperators[word], word

            else if char is newline

                advance()
                updatePosition()
                unless lastToken.type is "continuation"
                    yield Token "endStatement"
                offset = index
                line++

            else if char is escape

                advance()
                Token "continuation"

            else if char in quotes

                delimiter = char
                escaping = false
                updatePosition()
                advance()
                until (char is undefined) or (char is delimiter and not escaping)
                    if escaping
                        word += escapees[char]
                        escaping = false
                    else if char is escape then escaping = true
                    else word += char
                    advance()
                yield Token "stringLiteral"
                advance()

            else

                updatePosition()
                console.log "invalid: " + char
                return

        updatePosition()
        yield Token "endStatement"
        yield Token "endFile"

    # The Parser Function...
    # Note: Internal parser functions are part of the external API.

    self.parse = (tokens)  ->

        # Shared Internal State...

        [token, lastToken] = init 2

        # Helper Functions...

        chuck = (checks, token) -> throw """
            expected one of #{Array.from checks}
            got #{token.type} #{token.value}
            """

        self.peek = -> token

        self.ignore = ->
            for arg in arguments
                if arg is token.type then return self.advance arg

        self.advance = ->
            if arguments.length and token.type not in arguments
                chuck(arguments, token)
            lastToken = token
            token = tokens.next().value
            return [lastToken, token]

        # The Pratt Parsing Function...

        self.subparse = (rbp=0) ->
            self.advance()
            node = self.handlePrefix lastToken
            while rbp < token.lbp
                self.advance()
                node = self.handleInfix lastToken, node
            return node

        # The Main Loop...

        self.advance()

        while (token is undefined) or (token.type isnt "endFile")
            yield statement = self.subparse()
            unless statement.type is "if"
                self.advance "endStatement", "endFile"

    # API Wrapper Functions...

    self.compile = (source) -> self.parse self.lex source

# Define Core Grammar...

api = Parser()

# Define Generic Constructs...

api.createConstant "undefined", "undefined"

api.createConstant "null", "null"
api.createConstant "true", "bool"
api.createConstant "false", "bool"

api.createTerminal "identifier"
api.createTerminal "numberLiteral"
api.createTerminal "stringLiteral"

api.createNamedInfix "or",         50
api.createNamedInfix "and",        60
api.createNamedInfix "like",       100
api.createNamedInfix "unlike",     100
api.createNamedInfix "in",         110
api.createNamedInfix "instanceof", 110

api.createPredicatedBlock "while", Infinity

api.createSymbolicPrefix "spread", "...", 10
api.createSymbolicPrefix "not",    "!",   150

api.createGoofyNamedPrefix "void",   150
api.createGoofyNamedPrefix "typeof", 150
api.createGoofyNamedPrefix "delete", 150

api.createSymbolicInfix "apply",         "|",   10
api.createSymbolicInfix "more",          ">",   110
api.createSymbolicInfix "less",          "<",   110
api.createSymbolicInfix "notMore",       "<!",  110
api.createSymbolicInfix "notLess",       "!>",  110
api.createSymbolicInfix "equals",        "==",  100
api.createSymbolicInfix "unequals",      "!=",  100
api.createSymbolicInfix "plus",          "+",   130
api.createSymbolicInfix "minus",         "-",   130
api.createSymbolicInfix "times",         "*",   140
api.createSymbolicInfix "divide",        "/",   140
api.createSymbolicInfix "modulo",        "%",   140
api.createSymbolicInfix "dot",           ".",   180

api.createGoofySymbolicInfix "assign",        "=",   30
api.createGoofySymbolicInfix "plusAssign",    "+=",  30
api.createGoofySymbolicInfix "minusAssign",   "-=",  30
api.createGoofySymbolicInfix "timesAssign",   "*=",  30
api.createGoofySymbolicInfix "divideAssign",  "/=",  30
api.createGoofySymbolicInfix "moduloAssign",  "%=",  30
api.createGoofySymbolicInfix "raiseAssign",   "**=", 30
api.createGoofySymbolicInfix "moduloAssign",  "%=",  30
api.createGoofySymbolicInfix "raise",         "**",  140
api.createGoofySymbolicInfix "not",           "!",   150

api.createGoofySymbolicPrefix "increment", "++", 150
api.createGoofySymbolicPrefix "decrement", "--", 150

api.createSymbolicSuffix "increment", "++", 160
api.createSymbolicSuffix "decrement", "--", 160

# Define Unique Constructs...

api.createGoofySymbolicPrefix "plus", "+", 150, upgrade (self) ->
    self.type = "positive"
    self.right = self.subparse Infinity #self.lbp - 1
    self.updateEndPosition self, self.right

api.createGoofySymbolicPrefix "minus", "-", 150, upgrade (self) ->
    self.type = "negative"
    self.right = self.subparse Infinity #self.lbp -1
    self.updateEndPosition self, self.right

api.createNamedPrefix "for", 0, upgrade (self) ->
    predicate = self.subparse self.lbp
    self.left = predicate.left
    self.right = predicate.right
    self.block = []
    self.advance "openBrace"
    self.gatherBlock self, self.block

api.createNamedPrefix "if", 0, upgrade (self) ->
    self.block = []
    self.gatherPredicatedBlock self, self.block
    self.ignore "endStatement"
    return unless self.peek().type is "else"
    self.type = "either"
    self.advance "else"
    statement = (self.advance "openBrace", "if")[0]
    if statement.type is "if"
        self.elseBlock = [self.handlePrefix statement]
    else if statement.type is "openBrace"
        self.elseBlock = []
        self.gatherBlock self, self.elseBlock
    else throw "ifelse error"

api.createNamedOperatorToken "else"

api.createNamedPrefix "yield", 2, upgrade (self) ->
    if self.peek().type is "times"
        self.type = "yieldAll"
        self.value = "yield *"
        self.advance "times"
    self.right = self.subparse self.lbp - 1
    self.updateEndPosition self, self.right

api.createSymbolicPrefix "lambda", "->", 0, upgrade (self) ->
    self.right = self.subparse 0
    self.updateEndPosition self, self.right

api.createSymbolicInfix "lambda", "->", 0, upgrade (self, left) ->
    if left.type is "group" then self.args = left.args
    else if left.type is "identifier" then self.args = [left]
    else throw "invalid args"
    self.right = self.subparse 0
    self.updateStartPosition self, left
    self.updateEndPosition self, self.right

api.createNamedPrefix "function", 0, upgrade (self) ->
    self.params = []
    self.block = []
    unless self.peek().type is "openBrace" then loop
        self.params.push self.subparse()
        nextToken = self.peek()
        if nextToken.value is "," then self.advance "endStatement" else break
    self.advance "openBrace"
    self.gatherBlock self, self.block

api.createSymbolicPrefix "openParen", "(", 190, upgrade (self) ->
    self.type = "group"
    self.block = []
    self.gatherGroup self, self.block

api.createSymbolicInfix "openParen", "(", 170, upgrade (self, left) ->
    self.type = "invocation"
    self.left = left
    self.args = []
    self.gatherGroup self, self.args

api.createSymbolicPrefix "openBracket", "[", 0, upgrade (self) ->
    self.type = "arrayLiteral"
    self.args = []
    self.gatherArray self, self.args

api.createSymbolicInfix "openBracket", "[", 180, upgrade (self, left) ->
    self.type = "index"
    self.left = left
    self.args = []
    self.gatherArray self, self.args

api.createSymbolicPrefix "openBrace", "{", 0, upgrade (self) ->
    self.type = "scopeLiteral"
    self.args = []
    self.gatherBlock self, self.args

api.createNamedInfix "not", 110, upgrade (self, left) ->
    self.advance "in"
    self.type = "notIn"
    self.left = left
    self.right = self.subparse self.lbp
    self.updateStartPosition self, left
    self.updateEndPosition self, self.right
