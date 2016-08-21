### shalt.js | https://github.com/carlsmith/shalt

    shaltJS | Custom JavaScript Dialects
    Carl Smith 2016 GPL3 | CoffeeScript 1.10

###

# Generic Local Helper Functions: These are shared across all files in the
# library, but this is currently the only file in the library, so...

put = (args...) -> console.log args...

init = (amount) -> undefined for n in [1..amount]

inspect = do ->
    util = require "util"
    return (arg) -> console.log util.inspect arg, false, null

factory = (mutator) -> (args...) ->
    mutator (self = Object.create null), args...
    return self

upgrade = (mutator) -> (self, args...) ->
    mutator self, args...
    return self

# The Library Constructor: Apart from the helpers above, the whole library is
# defined by this function. The function takes an empty object (`self`), that
# the `factory` decorator passes to it, so users call `ShaltJS` with no args.
# The API is bound to `self`, which `factory` also makes the return value.

ShaltJS = factory (self) ->

    # Configuration State. This is shared across the lexer and parser, and
    # across repeated calls to `lex` and `parse`. Each parser instance can
    # parse any number of source strings once configured, but the instance
    # can only parse the single dialect it has been configured for.

    keywords = []
    namedLiterals = {}
    namedOperators = []
    symbolicOperators = {}
    prefixHandlers = {}
    infixHandlers = {}
    bindingPowers = {}

    # Internal Helper Functions: These functions are only used internally.
    # They use the exposed API functions (which are defined just beneath).

    handleUnaryPrefix = (goofy, lbp, type) -> upgrade (token) ->
        token.type = type or "UnaryExpression"
        token.operator = token.value
        token.prefix = true
        self.handlePotentialGroup token, token.subparse lbp - goofy
        token.updateEndPosition token, token.argument

    handleBinaryExpression = (goofy, lbp, type) -> upgrade (token, left) ->
        token.type = type or "BinaryExpression"
        token.operator = token.value
        token.left = left
        token.right = token.subparse lbp - goofy
        self.updateBoundaryPositions token, left, token.right

    createSymbolicPrefix = (goofy, type=undefined) ->
        (name, symbol, lbp=0, handler=undefined) ->
            symbolicOperators[symbol] = name
            prefixHandlers[name] = handler or \
                handleUnaryPrefix goofy, lbp, type

    createNamedPrefix = (goofy, type=undefined) ->
        (name, lbp=0, handler=undefined) ->
            namedOperators.push name
            prefixHandlers[name] = handler or \
                handleUnaryPrefix goofy, lbp, type

    createSymbolicInfix = (goofy, type=undefined) ->
        (name, symbol, lbp=0, handler=undefined) ->
            symbolicOperators[symbol] = name
            bindingPowers[name] = lbp
            infixHandlers[name] = handler or \
                handleBinaryExpression goofy, lbp, type

    createNamedInfix = (goofy, type=undefined) ->
        (name, lbp=0, handler=undefined) ->
            namedOperators.push name
            bindingPowers[name] = lbp
            infixHandlers[name] = handler or \
                handleBinaryExpression goofy, lbp, type

    # The High Level API Functions: These functions can be called with a
    # few arguments to create new operators that resuse typical grammar,
    # for example, a new infix operator or a new predicated block. Every
    # one of these functions accepts an optional callback, always as the
    # last argument, that will be used by the parser to parse any tokens
    # of the new type (instead of the default handler for that method).

    self.createSymbolicTerminal = (name, symbol, handler=undefined) ->
        symbolicOperators[symbol] = name
        prefixHandlers[name] = handler or upgrade ->

    self.createNamedTerminal = (name, handler=undefined) ->
        namedOperators.push name
        prefixHandlers[name] = handler or upgrade ->

    self.createSymbolicPrefix = createSymbolicPrefix false
    self.createGoofySymbolicPrefix = createSymbolicPrefix true

    self.createNamedPrefix = createNamedPrefix false
    self.createGoofyNamedPrefix = createNamedPrefix true

    self.createSymbolicInfix = createSymbolicInfix false
    self.createGoofySymbolicInfix = createSymbolicInfix true

    self.createNamedInfix = createNamedInfix false
    self.createGoofyNamedInfix = createNamedInfix true

    self.createAssignmentOperator = \
        createSymbolicInfix true, "AssignmentExpression"

    self.createPredicatedBlock = (name, type, handler=undefined) ->
        self.createNamedPrefix name, 0, handler or upgrade (token) ->
            token.type = type
            token.complete = true
            token.body = type: "BlockStatement", body: []
            token.gatherPredicatedBlock token, token.body.body

    self.createSymbolicSuffix = (name, symbol, lbp=0, handler=undefined) ->
        symbolicOperators[symbol] = name
        bindingPowers[name] = lbp
        infixHandlers[name] = handler or upgrade (token, left) ->
            token.left = left
            token.updateStartPosition token, left

    self.createNamedOperatorToken = (name) ->
        self.createNamedPrefix name

    # The Low Level API Functions: These are used inside the functions
    # that define custom parser actions. The parser internals are also
    # exposed by functions defined within the `parse` method, and with
    # these functions, they form the Low Level API.

    self.updateStartPosition = upgrade (token, startToken) ->
        token.start = startToken.start
        token.loc.start.line = startToken.loc.start.line
        token.loc.start.column = startToken.loc.start.column

    self.updateEndPosition = upgrade (token, endToken) ->
        token.end = endToken.end
        token.loc.end.line = endToken.loc.end.line
        token.loc.end.column = endToken.loc.end.column

    self.updateBoundaryPositions = (token, startToken, endToken) ->
        self.updateStartPosition token, startToken
        self.updateEndPosition token, endToken

    self.buildDeclaration = (kind, params) ->
        type: "VariableDeclaration", kind: kind, declarations: [
            type: "VariableDeclarator", id:
                if params.length is 1 then params[0]
                else type: "ArrayPattern", elements: params
        ]

    self.buildLetDeclaration = (params) -> self.buildDeclaration "let", params

    self.buildVarDeclaration = (params) -> self.buildDeclaration "var", params

    self.handlePrefix = (token) ->
        prefixHandlers[token.type] token

    self.handleInfix = (token, node) ->
        infixHandlers[token.type] token, node

    self.gatherNames = (token, target) ->
        while token.peek().type in ["Identifier"]
            target.push identifier = (token.advance "Identifier")[0]
            identifier.name = identifier.value
            if token.peek().value is "," then token.advance "endStatement"
            else break
        token.updateEndPosition token, identifier

    self.gatherParams = (token, target) ->
        while token.peek().type in ["Identifier", "RestElement"]
            target.push param = token.subparse 0
            if token.peek().value is "," then token.advance "endStatement"
            else break
        token.updateEndPosition token, param

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

    self.handlePotentialGroup = (token, argument) ->
        updateToken = (argument, parenthesized) ->
            token.argument = argument
            token.extras = parenthesizedArgument: parenthesized
        isSequence = argument.type is "SequenceExpression"
        if isSequence and argument.expressions.length is 1
            updateToken argument.expressions[0], true
        else updateToken argument, false

    # Common Grammar: This section defines the literals that are common to
    # all shaltJS dialects. Along with the whitespace rules for tokenising
    # source code, these grammar rules should never change.

    prefixHandlers["Identifier"] = upgrade (self) ->
        self.name = self.value

    namedLiterals["undefined"] = "undefinedLiteral"
    prefixHandlers["undefinedLiteral"] = upgrade (self) ->
        self.type = "Identifier"
        self.name = self.value

    namedLiterals["null"] = "NullLiteral"
    prefixHandlers["NullLiteral"] = upgrade ->

    namedLiterals["true"] = "TrueLiteral"
    prefixHandlers["TrueLiteral"] = upgrade (self) ->
        self.type = "BooleanLiteral"
        self.value = true

    namedLiterals["false"] = "FalseLiteral"
    prefixHandlers["FalseLiteral"] = upgrade (self) ->
        self.type = "BooleanLiteral"
        self.value = false

    prefixHandlers["NumericLiteral"] = upgrade (self) ->
        self.value = Number self.value

    prefixHandlers["StringLiteral"] = upgrade ->

    # The Lexer Function...

    self.lex = (source) ->

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

        # Some useful constants for checking that names are legal...

        alphas = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        startNameChars = alphas + "$_"
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
                    if word in keywords or word in namedOperators then word
                    else if word of namedLiterals then namedLiterals[word]
                    else "Identifier"
                    )

            else if char in digits

                gatherWhile ->
                    if char in digits then true
                    else if char is dot and dot not in word
                        peek() in digits
                    else false
                token = Token "NumericLiteral"
                token.extra = raw: token.value, rawValue: Number token.value
                token.value = Number token.value
                yield token

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
                raw = char
                escaping = false
                updatePosition()
                advance()
                until (not char) or (char is delimiter and not escaping)
                    raw += char
                    if escaping
                        word += escapees[char]
                        escaping = false
                    else if char is escape then escaping = true
                    else word += char
                    advance()
                token = Token "StringLiteral"
                token.extra = raw: raw + delimiter, rawValue: token.value
                advance()
                yield token

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

        Statement = factory (statement, expression) ->
            statement.type = "ExpressionStatement"
            statement.start = expression.start
            statement.end = expression.end
            statement.loc = expression.loc
            statement.expression = expression

        # Low Level API Functions...

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
            statement = self.subparse()
            if statement.complete then yield statement
            else yield Statement statement
            continue if statement.terminated
            self.advance "endStatement", "endFile"

    # API Wrapper Functions...

    self.compile = (source) ->

        body = Array.from self.parse self.lex source

        type: "File"
        start: body[0].start, end: body[0].end
        loc: start: body[0].loc.start, end: body[body.length - 1].loc.end
        program:
            type: "Program"
            start: body[0].start, end: body[0].end
            loc: start: body[0].loc.start, end: body[body.length - 1].loc.end
            sourceType: "script"
            body: body

# Define Core Grammar...

api = ShaltJS()

api.createPredicatedBlock "while", "WhileStatement"

api.createNamedPrefix "void", 150
api.createNamedPrefix "typeof", 150
api.createNamedPrefix "delete", 150

api.createSymbolicPrefix "not", "!", 150
api.createSymbolicPrefix "plus", "+", 150
api.createSymbolicPrefix "minus", "-", 150

api.createNamedInfix "instanceof", 110
api.createNamedInfix "in", 110

api.createSymbolicInfix "more", ">", 110
api.createSymbolicInfix "less", "<", 110
api.createSymbolicInfix "notMore", "<=", 110
api.createSymbolicInfix "notLess", ">=", 110
api.createSymbolicInfix "equals", "==", 100
api.createSymbolicInfix "unequals", "!=", 100
api.createSymbolicInfix "plus", "+", 130
api.createSymbolicInfix "minus", "-", 130
api.createSymbolicInfix "times", "*", 140
api.createSymbolicInfix "divide", "/", 140
api.createSymbolicInfix "modulo", "%", 140
api.createSymbolicInfix "dot", ".", 180

api.createGoofySymbolicInfix "raise", "**", 140

api.createAssignmentOperator "assign", "=", 30
api.createAssignmentOperator "plusAssign", "+=", 30
api.createAssignmentOperator "minusAssign", "-=", 30
api.createAssignmentOperator "timesAssign", "*=", 30
api.createAssignmentOperator "divideAssign", "/=", 30
api.createAssignmentOperator "moduloAssign", "%=", 30
api.createAssignmentOperator "raiseAssign", "**=", 30
api.createAssignmentOperator "moduloAssign", "%=", 30

api.createSymbolicPrefix "RestElement", "...", 0, (self) ->
    self.argument = self.subparse self.lbp
    self.updateEndPosition self, self.argument

api.createNamedInfix "or", 50, upgrade (self, left) ->
    self.type = "LogicalExpression"
    self.operator = "||"
    self.left = left
    self.right = self.subparse self.lbp
    self.updateBoundaryPositions self, left, self.right

api.createNamedInfix "and", 60, upgrade (self, left) ->
    self.type = "LogicalExpression"
    self.operator = "&&"
    self.left = left
    self.right = self.subparse self.lbp
    self.updateBoundaryPositions self, left, self.right

api.createNamedPrefix "for", 0, upgrade (self) ->
    self.complete = true
    self.gatherNames self, names = []
    self.left = self.buildLetDeclaration names
    nextValue = (self.advance "in", "of")[0].value
    self.type = (if nextValue is "in" then "ForIn" else "ForOf") + "Statement"
    self.right = self.subparse 0
    self.advance "openBrace"
    self.body = type: "BlockStatement", body: []
    self.gatherBlock self, self.body.body

api.createNamedOperatorToken "of"

api.createNamedPrefix "if", 0, upgrade (self) ->
    self.type = "IfStatement"
    self.terminated = true
    self.consequent = type: "BlockStatement", body: []
    self.gatherPredicatedBlock self, self.consequent.body
    self.ignore "endStatement"
    return unless self.peek().type is "else"
    self.advance "else"
    statement = (self.advance "openBrace", "if")[0]
    if statement.type is "if"
        body = [self.handlePrefix statement]
        self.alternate = type: "BlockStatement", body: body
    else if statement.type is "openBrace"
        self.alternate = type: "BlockStatement", body: []
        self.gatherBlock self, self.alternate.body
        self.advance "endStatement"
    else throw "branch error"

api.createNamedOperatorToken "else"

api.createNamedPrefix "function", 0, upgrade (self) ->
    self.type = "FunctionExpression"
    self.generator = false
    self.expression = false
    self.async = false
    self.gatherParams self, self.params = []
    self.body = type: "BlockStatement", body: []
    self.advance "openBrace"
    self.gatherBlock self, self.body.body

api.createNamedPrefix "generator", 0, upgrade (self) ->
    self.type = "FunctionExpression"
    self.generator = true
    self.expression = false
    self.async = false
    self.gatherParams self, self.params = []
    self.body = type: "BlockStatement", body: []
    self.advance "openBrace"
    self.gatherBlock self, self.body.body

api.createNamedPrefix "return", Infinity, upgrade (self) ->
    self.type = "ReturnStatement"
    self.complete = true
    self.argument = self.subparse 0
    self.updateEndPosition self, self.argument

api.createNamedPrefix "yield", 20, upgrade (self) ->
    self.type = "YieldExpression"
    if self.peek().value is "from"
        self.delegate = true
        self.value = "yield *"
        self.advance()
    else self.delegate = false
    self.argument = self.subparse self.lbp - 1
    self.updateEndPosition self, self.argument

api.createNamedOperatorToken "from"

api.createSymbolicInfix "colon", ":", 5, upgrade (self, left) ->
    self.type = "ObjectProperty"
    self.key = left
    self.value = self.subparse 0
    self.method = false
    self.shorthand = false
    self.computed = false
    self.updateBoundaryPositions self, left, self.value

api.createSymbolicPrefix "lambda", "->", Infinity, upgrade (self) ->
    self.type = "ArrowFunctionExpression"
    self.generator = false
    self.expression = true
    self.async = false
    self.params = []
    self.body = self.subparse 0
    self.updateEndPosition self, self.body

api.createSymbolicInfix "lambda", "->", Infinity, upgrade (self, left) ->
    self.type = "ArrowFunctionExpression"
    self.generator = false
    self.expression = true
    self.async = false
    if left.type is "SequenceExpression"
        self.params = left.expressions
    else self.params = [left]
    self.body = self.subparse 0
    self.updateBoundaryPositions self, left, self.body

api.createSymbolicTerminal "openParen", "(", upgrade (self) ->
    self.type = "SequenceExpression"
    self.extra = parenthesized: true, parenStart: 0
    self.expressions = []
    self.gatherGroup self, self.expressions

api.createSymbolicInfix "openParen", "(", 170, upgrade (self, left) ->
    self.type = "CallExpression"
    self.callee = left
    self.arguments = []
    self.gatherGroup self, self.arguments
    self.updateStartPosition self, left

api.createSymbolicTerminal "openBracket", "[", upgrade (self) ->
    self.type = "ArrayExpression"
    self.elements = []
    self.gatherArray self, self.elements

api.createSymbolicInfix "openBracket", "[", 180, upgrade (self, left) ->
    self.type = "MemberExpression"
    self.computed = true
    self.object = left
    self.property = self.subparse 0
    self.advance "closeBracket"
    self.updateBoundaryPositions self, left, self.property

api.createSymbolicTerminal "openBrace", "{", upgrade (self) ->
    self.type = "ObjectExpression"
    self.properties = []
    self.gatherBlock self, self.properties

handlePrefixUpdateOperator = upgrade (self) ->
    self.type = "UpdateExpression"
    self.operator = self.value
    self.prefix = true
    self.handlePotentialGroup self, self.subparse self.lbp - 1

handleSuffixUpdateOperator = upgrade (self, left) ->
    self.type = "UpdateExpression"
    self.operator = self.value
    self.prefix = false
    self.handlePotentialGroup self, left

api.createSymbolicPrefix "increment", "++", 150, handlePrefixUpdateOperator
api.createSymbolicPrefix "decrement", "--", 150, handlePrefixUpdateOperator
api.createSymbolicInfix "increment", "++", 160, handleSuffixUpdateOperator
api.createSymbolicInfix "decrement", "--", 160, handleSuffixUpdateOperator

# Test Code...

source = """
"""
fs = require "fs"
babel = require "babel-core"
inspect ast = api.compile source
put (babel.transformFromAst ast).code
