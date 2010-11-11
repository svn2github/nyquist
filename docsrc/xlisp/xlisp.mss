@define(codef, FaceCode T, size 11)
@section(Introduction)
        XLISP is an experimental programming language combining some of
        the features of Common Lisp with an object-oriented extension
        capability.  It was implemented to allow experimentation with
        object-oriented programming on small computers.

        Implementations of XLISP run on virtually every operating system.
        XLISP is completely written in the programming language
        C and is easily extended with user written built-in functions
        and classes.  It is available in source form to non-commercial
        users.

        Many Common Lisp functions are built into XLISP.  In addition,
        XLISP defines the objects Object and Class as primitives.
        Object is the only class that has no superclass and hence is
        the root of the class hierarchy tree.  Class is the class of
        which all classes are instances (it is the only object that is
        an instance of itself).

        This document is a brief description of XLISP.  It assumes some
        knowledge of LISP and some understanding of the concepts of
        object-oriented programming.

        I recommend the book @i(Lisp) by Winston and Horn and published by
        Addison Wesley for learning Lisp.  The first edition of this
        book is based on MacLisp and the second edition is based on
        Common Lisp. 

        You will probably also need a copy of @i(Common Lisp: The
        Language) by Guy L. Steele, Jr., published by Digital Press to
        use as a reference for some of the Common Lisp functions that
        are described only briefly in this document.

        @section(A Note From The Author)

        If you have any problems with XLISP, feel free to contact me [me being David Betz - RBD] for
        help or advice.  Please remember that since XLISP is available
        in source form in a high level language, many users [e.g. that Dannenberg fellow - RBD] have been
        making versions available on a variety of machines.  If you call
        to report a problem with a specific version, I may not be able
        to help you if that version runs on a machine to which I don't
        have access.  Please have the version number of the version that
        you are running readily accessible before calling me.

        If you find a bug in XLISP, first try to fix the bug yourself
        using the source code provided.  If you are successful in fixing
        the bug, send the bug report along with the fix to me.  If you
        don't have access to a C compiler or are unable to fix a bug,
        please send the bug report to me and I'll try to fix it.

        Any suggestions for improvements will be welcomed.  Feel free to
        extend the language in whatever way suits your needs.  However,
        PLEASE DO NOT RELEASE ENHANCED VERSIONS WITHOUT CHECKING WITH ME
        FIRST!!  I would like to be the clearing house for new features
        added to XLISP.  If you want to add features for your own
        personal use, go ahead.  But, if you want to distribute your
        enhanced version, contact me first.  Please remember that the
        goal of XLISP is to provide a language to learn and experiment
        with LISP and object-oriented programming on small computers.  I
        don't want it to get so big that it requires megabytes of memory
        to run.


        @section(XLISP Command Loop)@index(XLISP Command Loop)@index(Command Loop)

        When XLISP is started, it first tries to load the workspace
        @code(xlisp.wks) from the current directory.  If that file doesn't
        exist, XLISP builds an initial workspace, empty except for the
        built-in functions and symbols.

        Then XLISP attempts to load @code(init.lsp) from the current
        directory.  It then loads any files named as parameters on the
        command line (after appending @code(.lsp) to their names).

        XLISP then issues the following prompt:
@begin(example)
        >
@end(example)
        This indicates that XLISP is waiting for an expression to be
        typed.

        When a complete expression has been entered, XLISP attempts to
        evaluate that expression.  If the expression evaluates
        successfully, XLISP prints the result and then returns to the
        initial prompt waiting for another expression to be typed.

        @section(Special Characters)@index(control characters, XLISP)

 When XLISP is running from a console, some control characters invoke operations:
@begin(itemize)
Backspace and Delete characters erase the previous character on the input line (if any).

Control-U erases the entire input line. 

Control-C executes the TOP-LEVEL function.

Control-G executes the CLEAN-UP function.

Control-P executes the CONTINUE function.

Control-B stops execution and enters the break command loop. Execution can be continued by typing Control-P or (CONTINUE).

Control-E turns on character echoing (Linux and Mac OS X only).

Control-F turns off character echoing (Linux and Mac OS X only).

Control-T evaluates the INFO function.
@end(itemize)

        @section(Break Command Loop)@index(break)

        When XLISP encounters an error while evaluating an expression,
        it attempts to handle the error in the following way:

        If the symbol @code(*breakenable*@index(*breakenable*)) is true, the message corresponding
        to the error is printed.  If the error is correctable, the
        correction message is printed.

        If the symbol @code(*tracenable*@index(*tracenable*)) is true, a trace back is printed.
        The number of entries printed depends on the value of the symbol
        @code(*tracelimit*@index(*tracelimit*)).  If this symbol is set to something other than a
        number, the entire trace back stack is printed.

        XLISP then enters a read/eval/print loop to allow the user to
        examine the state of the interpreter in the context of the
        error.  This loop differs from the normal top-level
        read/eval/print loop in that if the user invokes the function
        @code(continue), XLISP will continue from a correctable error.  If
        the user invokes the function @code(clean-up), XLISP will abort the
        break loop and return to the top level or the next lower
        numbered break loop.  When in a break loop, XLISP prefixes the
        break level to the normal prompt.

        If the symbol @code(*breakenable*@index(*breakenable*)) is @code(nil), XLISP looks for a
        surrounding errset function.  If one is found, XLISP examines
        the value of the print flag.  If this flag is true, the error
        message is printed.  In any case, XLISP causes the errset
        function call to return @code(nil).

        If there is no surrounding errset function, XLISP prints the
        error message and returns to the top level.

        @section(Data Types)@index(XLISP Data Types)@index(Data Types)

        There are several different data types available to XLISP
        programmers.

@begin(itemize)
lists

symbols

strings

integers

characters

floats

objects

arrays

streams

subrs (built-in functions)

fsubrs (special forms)

closures (user defined functions)
@end(itemize)




@section(The Evaluator)@index(evaluator)@index(XLISP evaluator)

        The process of evaluation in XLISP:
@begin(itemize)
        Strings, integers, characters, floats, objects, arrays, streams,
        subrs, fsubrs and closures evaluate to themselves.

        Symbols act as variables and are evaluated by retrieving the
        value associated with their current binding.

        Lists are evaluated by examining the first element of the list
        and then taking one of the following actions:
@begin(itemize)
            If it is a symbol, the functional binding of the symbol is
            retrieved.

            If it is a lambda expression, a closure is constructed for
            the function described by the lambda expression.

            If it is a subr, fsubr or closure, it stands for itself.

            Any other value is an error.
@end(itemize)
        Then, the value produced by the previous step is examined:
@begin(itemize)
            If it is a subr or closure, the remaining list elements are
            evaluated and the subr or closure is called with these
            evaluated expressions as arguments.

            If it is an fsubr, the fsubr is called using the remaining
            list elements as arguments (unevaluated).

            If it is a macro, the macro is expanded using the remaining
            list elements as arguments (unevaluated).  The macro
            expansion is then evaluated in place of the original macro
            call.
@end(itemize)
@end(itemize)

@section(Lexical Conventions)@index(Lexical conventions)@index(XLISP Lexical Conventions)

        The following conventions must be followed when entering XLISP
        programs:

        Comments in XLISP code begin with a semi-colon character and
        continue to the end of the line.

        Symbol names in XLISP can consist of any sequence of non-blank
        printable characters except the following:
@begin(example)
                ( ) ' ` , " ;
@end(example)
        Uppercase and lowercase characters are not distinguished within
        symbol names.  All lowercase characters are mapped to uppercase
        on input.

        Integer literals consist of a sequence of digits optionally
        beginning with a @code(+) or @code(-).  The range of values an integer can
        represent is limited by the size of a C @code(long) on the machine on
        which XLISP is running.

        Floating point literals consist of a sequence of digits
        optionally beginning with a @code(+) or @code(-) and including an embedded
        decimal point.  The range of values a floating point number can
        represent is limited by the size of a C @code(float) (@code(double) on
        machines with 32 bit addresses) on the machine on which XLISP is
        running.

        Literal strings are sequences of characters surrounded by double
        quotes.  Within quoted strings the ``@code(\)'' character is used to
        allow non-printable characters to be included.  The codes
        recognized are:
@begin(itemize)
@code(\\)        means the character ``@code(\)''

@code(\n)      means newline

@code(\t)       means tab

@code(\r)       means return

@code(\f)       means form feed

@code(\nnn)    means the character whose octal code is nnn
@end(itemize)

@section(Readtables)@index(Readtables)

        The behavior of the reader is controlled by a data structure
        called a @i(readtable).  The reader uses the symbol @code(*readtable*@index(*readtable*)) to
        locate the current readtable.  This table controls the
        interpretation of input characters.  It is an array with 128
        entries, one for each of the ASCII character codes.  Each entry
        contains one of the following things:
@begin(itemize)
                @code(NIL) @itemsep          Indicating an invalid character

                @code(:CONSTITUENT) @itemsep   Indicating a symbol constituent

                @code(:WHITE-SPACE)  @itemsep  Indicating a whitespace character

                @code[(:TMACRO . @i(fun))] @itemsep Terminating readmacro

                @code[(:NMACRO . @i(fun))] @itemsep Non-terminating readmacro

                @code(:SESCAPE) @itemsep       Single escape character ('\')

                @code(:MESCAPE) @itemsep       Multiple escape character ('|')
@end(itemize)

        In the case of @code(:TMACRO) and @code(:NMACRO), the @i(fun) component is a
        function.  This can either be a built-in readmacro function or a
        lambda expression.  The function should take two parameters.
        The first is the input stream and the second is the character
        that caused the invocation of the readmacro.  The readmacro
        function should return @code(NIL) to indicate that the character should
        be treated as white space or a value consed with @code(NIL) to indicate
        that the readmacro should be treated as an occurence of the
        specified value.  Of course, the readmacro code is free to read
        additional characters from the input stream.

        XLISP defines several useful read macros@index(read macros):
@begin(itemize)
                '@i[<expr>]         == (quote @i[<expr>])

                #'@i[<expr>]        == (function @i[<expr>])

                #(@i[<expr>]...)    == an array of the specified expressions

                #x@i[<hdigits>]     == a hexadecimal number (0-9,A-F)

                #o@i[<odigits>]     == an octal number (0-7)

                #b@i[<bdigits>]     == a binary number (0-1)

                #\@i[<char>] == the ASCII code of the character

                #| ... |#       == a comment

                #:@i[<symbol>]      == an uninterned symbol

                `@i[<expr>]         == (backquote @i[<expr>])

                ,@i[<expr>]         == (comma @i[<expr>])

                ,@@@i[<expr>]        == (comma-at @i[<expr>])

@end(itemize)
@section(Lambda Lists)@index(Lambda Lists)

        There are several forms in XLISP that require that a ``lambda
        list'' be specified.  A lambda list is a definition of the
        arguments accepted by a function.  There are four different
        types of arguments.

        The lambda list starts with required arguments.  Required
        arguments must be specified in every call to the function.

        The required arguments are followed by the &optional arguments.
        Optional arguments may be provided or omitted in a call.  An
        initialization expression may be specified to provide a default
        value for an &optional argument if it is omitted from a call.
        If no initialization expression is specified, an omitted
        argument is initialized to @code(NIL).  It is also possible to provide
        the name of a @code(supplied-p) variable that can be used to
        determine if a call provided a value for the argument or if the
        initialization expression was used.  If specified, the supplied-
        p variable will be bound to T if a value was specified in the
        call and @code(NIL) if the default value was used.

        The &optional arguments are followed by the &rest argument.  The
        &rest argument gets bound to the remainder of the argument list
        after the required and &optional arguments have been removed.

        The &rest argument is followed by the &key arguments.  When a
        keyword argument is passed to a function, a pair of values
        appears in the argument list.  The first expression in the pair
        should evaluate to a keyword symbol (a symbol that begins with a
        ``@code(:)'').  The value of the second expression is the value of the
        keyword argument.  Like &optional arguments, &key arguments can
        have initialization expressions and supplied-p variables.  In
        addition, it is possible to specify the keyword to be used in a
        function call.  If no keyword is specified, the keyword obtained
        by adding a ``@code(:)'' to the beginning of the keyword argument symbol
        is used.  In other words, if the keyword argument symbol is
        @code(foo), the keyword will be @code(':foo).

        The &key arguments are followed by the &aux variables.  These
        are local variables that are bound during the evaluation of the
        function body.  It is possible to have initialization
        expressions for the &aux variables.

    Here is the complete syntax for lambda lists:
@begin(display)
                (@i<rarg>...
                 [&optional [@i<oarg> | (@i<oarg> [@i<init> [@i<svar>]])]...]
                 [&rest @i<rarg>]
                 [&key
                   [@i<karg> | ([@i<karg> | (@i<key> @i<karg>)] [@i<init> [@i<svar>]])]...
                   &allow-other-keys]
                 [&aux
                   [@i<aux> | (@i<aux> [@i<init>])]...])

            where:

                @i<rarg> is a required argument symbol
                @i<oarg> is an &optional argument symbol
                @i<rarg> is the &rest argument symbol
                @i<karg> is a &key argument symbol
                @i<key> is a keyword symbol
                @i<aux> is an auxiliary variable symbol
                @i<init> is an initialization expression
                @i<svar> is a supplied-p variable symbol
@end(display)


@section(Objects)@index(Objects)@label(objects-sec)

        Definitions:
@begin(itemize)
selector @itemsep a symbol used to select an appropriate method

message @itemsep a selector and a list of actual arguments

method @itemsep the code that implements a message
@end(itemize)
        Since XLISP was created to provide a simple basis for
        experimenting with object-oriented programming, one of the
        primitive data types included is @i(object).  In XLISP, an object
        consists of a data structure containing a pointer to the
        object's class as well as an array containing the values of the
        object's instance variables.

        Officially, there is no way to see inside an object (look at the
        values of its instance variables).  The only way to communicate
        with an object is by sending it a message.

        You can send a message to an object using the @code(send) function.
        This function takes the object as its first argument, the
        message selector as its second argument (which must be a symbol)
        and the message arguments as its remaining arguments.

        The @code(send) function determines the class of the receiving object
        and attempts to find a method corresponding to the message
        selector in the set of messages defined for that class.  If the
        message is not found in the object's class and the class has a
        super-class, the search continues by looking at the messages
        defined for the super-class.  This process continues from one
        super-class to the next until a method for the message is found.
        If no method is found, an error occurs.

@begin(comment)
THIS IS WRONG -- I DON'T KNOW IF IT WAS CORRECT IN THE ORIGINAL XLISP. -RBD
        A message can also be sent from the body of a method by using
        the current object, but the method lookup starts with the
        object's superclass rather than its class.  This allows a
        subclass to invoke a standard method in its parent class even
        though it overrides that method with its own specialized
        version.
@end(comment)

        When a method is found, the evaluator binds the receiving object
        to the symbol @code(self) and evaluates the method using the
        remaining elements of the original list as arguments to the
        method.  These arguments are always evaluated prior to being
        bound to their corresponding formal arguments.  The result of
        evaluating the method becomes the result of the expression.

        Within the body of a method, a message can be sent to the current
        object by calling the @code[(send self ...)]. The method lookup
        starts with the object's class regardless of the class containing
        the current method.

        Sometimes it is desirable to invoke a general method in a superclass
        even when it is overridden by a more specific method in a subclass.
        This can be accomplished by calling @code(send-super), which begins
        the method lookup in the superclass of the class defining the current
        method rather than in the class of the current object.

        The @code(send-super) function takes a selector as its first argument
        (which must be a symbol) and the message arguments as its remaining
        arguments. Notice that @code(send-super) can only be sent from within
        a method, and the target of the message is always the current object
        (@code(self)). @code[(send-super ...)] is similar to 
        @code[(send self ...)] except that method lookup begins in the 
        superclass of the class containing the current method
        rather than the class of the current object.

@section(The ``Object'' Class)@index(Object Class)

@code(Object)@index(Object) @itemsep the top of the class hierarchy.

Messages:
@begin(fdescription)
@code(:show@index(:show)) @itemsep show an object's instance variables.
@begin(pdescription)
returns @itemsep  the object
@end(pdescription)
@blankspace(1)

@code{:class@index(:class)} @itemsep return the class of an object
@begin(pdescription)
returns @itemsep  the class of the object
@end(pdescription)
@blankspace(1)

@code{:isa(:isa)} @i(class) @itemsep test if object inherits from class
@begin(pdescription)
returns @itemsep @code(t) if object is an instance of @i(class) or a subclass of @i(class), otherwise @code(nil)
@end(pdescription)
@blankspace(1)

@code(:isnew@index(:isnew)) @itemsep the default object initialization routine
@begin(pdescription)
returns @itemsep  the object
@end(pdescription)
@end(fdescription)

@section(The ``Class'' Class)@index(Class class)

@code(Class@index(Class)) @itemsep class of all object classes (including itself)

            Messages:

@begin(fdescription)
                :new@index(:new) @itemsep create a new instance of a class
@begin(pdescription)
                    returns @itemsep    the new class object
@end(pdescription)
@blankspace(1)

                :isnew@index(:isnew) @i<ivars> [@i<cvars> [@i<super>]] @itemsep initialize a new class
@begin(pdescription)
                    @i<ivars> @itemsep    the list of instance variable symbols

                    @i<cvars> @itemsep    the list of class variable symbols

                    @i<super> @itemsep    the superclass (default is object)

                    returns @itemsep    the new class object
@end(pdescription)
@blankspace(1)

                :answer@index(:answer) @i<msg> @i<fargs> @i<code> @itemsep add a message to a class
@begin(pdescription)
                    @i<msg> @itemsep      the message symbol

                @i<fargs> @itemsep    the formal argument list (lambda list)

                    @i<code> @itemsep     a list of executable expressions

                    returns @itemsep    the object
@end(pdescription)
@blankspace(1)
@end(fdescription)

        When a new instance of a class is created by sending the message
        @code(:new) to an existing class, the message @code(:isnew) followed by
        whatever parameters were passed to the @code(:new) message is sent to
        the newly created object.

        When a new class is created by sending the @code(:new) message to the
        object @code(Class), an optional parameter may be specified
        indicating the superclass of the new class.  If this parameter
        is omitted, the new class will be a subclass of @code(Object).  A
        class inherits all instance variables, class variables, and
        methods from its super-class.

@section(Profiling)@index(profiling)
The Xlisp 2.0 release has been extended with a profiling facility, which counts how many times and where @code(eval) is executed.  A separate count is maintained for each named function, closure, or macro, and a count indicates an @code(eval) in the immediately (lexically) enclosing named function, closure, or macro.  Thus, the count gives an indication of the amount of time spent in a function, not counting nested function calls.  The list of all functions executed is maintained on the global @code(*profile*) variable.  These functions in turn have @code(*profile*) properties, which maintain the counts.  The profile system merely increments counters and puts symbols on the @code(*profile*) list.  It is up to the user to initialize data and gather results.  Profiling is turned on or off with the @code(profile) function.  Unfortunately, methods cannot be profiled with this facility.

@label(symbols-sec)
@section(Symbols)@index(symbols)
@begin(itemize)
@codef(self)@pragma(defn)@index(self) @dash the current object (within a method context)

@codef(*obarray*@pragma(defn)@index(*obarray*)) @dash the object hash table

@codef(*standard-input*@pragma(defn)@index(*standard-input*)) @dash the standard input stream

@codef(*standard-output*@pragma(defn)@index(*standard-output*)) @dash the standard output stream

@codef(*error-output*@pragma(defn)@index(*error-output*)) @dash the error output stream

@codef(*trace-output*@pragma(defn)@index(*trace-output*)) @dash the trace output stream

@codef(*debug-io*@pragma(defn)@index(*debug-io*)) @dash the debug i/o stream

@codef(*breakenable*@pragma(defn)@index(*breakenable*)) @dash flag controlling entering break loop on errors

@codef(*tracelist*@pragma(defn)@index(*tracelist*)) @dash list of names of functions to trace

@codef(*tracenable*@pragma(defn)@index(*tracenable*)) @dash enable trace back printout on errors

@codef(*tracelimit*@pragma(defn)@index(*tracelimit*)) @dash number of levels of trace back information

@codef(*evalhook*@pragma(defn)@index(*evalhook*)) @dash user substitute for the evaluator function

@codef(*applyhook*@pragma(defn)@index(*applyhook*)) @dash (not yet implemented)

@codef(*readtable*@pragma(defn)@index(*readtable*)) @dash the current readtable

@codef(*unbound*@pragma(defn)@index(*unbound*)) @dash indicator for unbound symbols

@codef(*gc-flag*@pragma(defn)@index(*gc-flag*)) @dash controls the printing of gc messages

@codef(*gc-hook*@pragma(defn)@index(*gc-hook*)) @dash function to call after garbage collection

@codef(*integer-format*@pragma(defn)@index(*integer-format*)) @dash format for printing integers (``%d'' or ``%ld'')

@codef(*float-format*@pragma(defn)@index(*float-format*)) @dash format for printing floats (``%g'')

@codef(*print-case*@pragma(defn)@index(*print-case*)) @dash symbol output case (:upcase or :downcase)
@end(itemize)

        There are several symbols maintained by the read/eval/print
        loop.  The symbols @code(+), @code(++), and @code(+++) are bound to the most
        recent three input expressions.  The symbols @code(*), @code(**) and @code(***)
        are bound to the most recent three results.  The symbol @code(-) is
        bound to the expression currently being evaluated.  It becomes
        the value of @code(+) at the end of the evaluation.
@section(Evaluation Functions)@index(evaluation functions)
@begin(fdescription)
        (eval@pragma(defn)@index(eval) @i<expr>) @itemsep evaluate an xlisp expression
@begin(pdescription)
            @i<expr> @itemsep     the expression to be evaluated

            returns @itemsep     the result of evaluating the expression
@end(pdescription)
@blankspace(1)

        (apply@pragma(defn)@index(apply) @i<fun> @i<args>) @itemsep apply a function to a list of arguments
@begin(pdescription)
            @i<fun> @itemsep      the function to apply (or function symbol)

            @i<args> @itemsep     the argument list

            returns @itemsep    the result of applying the function to the arguments
@end(pdescription)
@blankspace(1)

        (funcall@pragma(defn)@index(funcall) @i<fun> @i<arg>...) @itemsep call a function with arguments
@begin(pdescription)
            @i<fun> @itemsep      the function to call (or function symbol)

            @i<arg> @itemsep      arguments to pass to the function

            returns @itemsep    the result of calling the function with the arguments
@end(pdescription)
@blankspace(1)

        (quote@pragma(defn)@index(quote) @i<expr>) @itemsep  return an expression unevaluated
@begin(pdescription)
            @i<expr>  @itemsep    the expression to be quoted (quoted)

            returns   @itemsep  @i<expr> unevaluated
@end(pdescription)
@blankspace(1)

        (function@pragma(defn)@index(function) @i<expr>) @itemsep  get the functional interpretation

@begin(pdescription)
            @i<expr> @itemsep     the symbol or lambda expression (quoted)

            returns  @itemsep   the functional interpretation

@end(pdescription)
@blankspace(1)

        (backquote@pragma(defn)@index(backquote) @i<expr>) @itemsep fill in a template

@begin(pdescription)
            @i<expr> @itemsep     the template

            returns  @itemsep   a copy of the template with comma and comma-at

             expressions expanded
@end(pdescription)
@blankspace(1)

        (lambda@pragma(defn)@index(lambda) @i<args> @i<expr>...) @itemsep make a function closure

@begin(pdescription)
            @i<args> @itemsep     formal argument list (lambda list) (quoted)

            @i<expr> @itemsep     expressions of the function body

            returns  @itemsep   the function closure

@end(pdescription)
@blankspace(1)

        (get-lambda-expression@pragma(defn)@index(get-lambda-expression) @i<closure>) @itemsep get the lambda expression

@begin(pdescription)
            @i<closure> @itemsep  the closure

            returns  @itemsep   the original lambda expression

@end(pdescription)
@blankspace(1)

        (macroexpand@pragma(defn)@index(macroexpand) @i<form>) @itemsep recursively expand macro calls

@begin(pdescription)
            @i<form> @itemsep     the form to expand

            returns  @itemsep   the macro expansion

@end(pdescription)
@blankspace(1)

        (macroexpand-1@pragma(defn)@index(macroexpand-1) @i<form>) @itemsep expand a macro call

@begin(pdescription)
            @i<form> @itemsep     the macro call form

            returns  @itemsep   the macro expansion

@end(pdescription)
@blankspace(1)
@end(fdescription)


@section(Symbol Functions)@index(Symbol Functions)
@begin(fdescription)
        (set@pragma(defn)@index(set) @i<sym> @i<expr>) @itemsep  set the value of a symbol
@begin(pdescription)
            @i<sym>  @itemsep     the symbol being set

            @i<expr> @itemsep     the new value

            returns  @itemsep   the new value

@end(pdescription)
@blankspace(1)

        (setq@pragma(defn)@index(setq) [@i<sym> @i<expr>]...) @itemsep  set the value of a symbol
@begin(pdescription)
            @i<sym>  @itemsep     the symbol being set (quoted)

            @i<expr> @itemsep     the new value

            returns  @itemsep   the new value

@end(pdescription)
@blankspace(1)

        (psetq@pragma(defn)@index(psetq) [@i<sym> @i<expr>]...)  @itemsep parallel version of setq
@begin(pdescription)
            @i<sym>  @itemsep     the symbol being set (quoted)

            @i<expr> @itemsep     the new value

            returns  @itemsep   the new value

@end(pdescription)
@blankspace(1)

        (setf@pragma(defn)@index(setf) [@i<place> @i<expr>]...)  @itemsep set the value of a field
@begin(pdescription)
            @i<place> @itemsep     the field specifier (quoted):

@begin(pdescription)
  @i<sym> @itemsep                  set value of a symbol

  (car @i<expr>)  @itemsep          set car of a cons node

  (cdr @i<expr>)  @itemsep          set cdr of a cons node

  (nth @i<n> @i<expr>) @itemsep       set nth car of a list

  (aref @i<expr> @i<n>) @itemsep       set nth element of an array

  (get @i<sym> @i<prop>) @itemsep      set value of a property

  (symbol-value @i<sym>) @itemsep   set value of a symbol

  (symbol-function @i<sym>) @itemsep set functional value of a symbol

  (symbol-plist @i<sym>) @itemsep    set property list of a symbol

@end(pdescription)@pragma(stopcodef)
        @i<expr> @itemsep     the new value

        returns  @itemsep   the new value

@end(pdescription)
@blankspace(1)
@begin(fgroup)
        (defun@pragma(defn)@index(defun) @i<sym> @i<fargs> @i<expr>...)  @itemsep define a function

@pragma(startcodef)
        (defmacro@pragma(defn)@index(defmacro) @i<sym> @i<fargs> @i<expr>...) @itemsep  define a macro
@end(fgroup)
@begin(pdescription)
            @i<sym> @itemsep      symbol being defined (quoted)

            @i<fargs> @itemsep     formal argument list (lambda list) (quoted)

            @i<expr>  @itemsep    expressions constituting the body of the

                        function (quoted)
            returns   @itemsep  the function symbol

@end(pdescription)
@blankspace(1)

        (gensym@pragma(defn)@index(gensym) [@i<tag>])  @itemsep generate a symbol
@begin(pdescription)
            @i<tag>   @itemsep    string or number

            returns   @itemsep  the new symbol

@end(pdescription)
@blankspace(1)

(intern@pragma(defn)@index(intern) @i<pname>)  @itemsep make an interned symbol
@begin(pdescription)
            @i<pname> @itemsep    the symbol's print name string

            returns   @itemsep  the new symbol

@end(pdescription)
@blankspace(1)

        (make-symbol@pragma(defn)@index(make-symbol) @i<pname>)  @itemsep make an uninterned symbol
@begin(pdescription)
            @i<pname> @itemsep    the symbol's print name string

            returns   @itemsep  the new symbol

@end(pdescription)
@blankspace(1)

        (symbol-name@pragma(defn)@index(symbol-name) @i<sym>)  @itemsep get the print name of a symbol
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            returns   @itemsep  the symbol's print name

@end(pdescription)
@blankspace(1)

    (symbol-value@pragma(defn)@index(symbol-value) @i<sym>)  @itemsep get the value of a symbol
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            returns   @itemsep  the symbol's value

@end(pdescription)
@blankspace(1)

        (symbol-function@pragma(defn)@index(symbol-function) @i<sym>)  @itemsep get the functional value of a symbol
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            returns   @itemsep  the symbol's functional value

@end(pdescription)
@blankspace(1)

        (symbol-plist@pragma(defn)@index(symbol-plist) @i<sym>)  @itemsep get the property list of a symbol
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            returns   @itemsep  the symbol's property list

@end(pdescription)
@blankspace(1)

        (hash@pragma(defn)@index(hash) @i<sym> @i<n>)  @itemsep compute the hash index for a symbol
@begin(pdescription)
            @i<sym>   @itemsep    the symbol or string

            @i<n>     @itemsep    the table size (integer)

            returns   @itemsep  the hash index (integer)

@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Property List Functions)@index(Property List Functions)
@begin(fdescription)
        (get@pragma(defn)@index(get) @i<sym> @i<prop>)  @itemsep get the value of a property
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            @i<prop>  @itemsep    the property symbol

            returns   @itemsep  the property value or @code(nil)

@end(pdescription)
@blankspace(1)

        (putprop@pragma(defn)@index(putprop) @i<sym> @i<val> @i<prop>)  @itemsep put a property onto a property list
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            @i<val>   @itemsep    the property value

            @i<prop>  @itemsep    the property symbol

            returns   @itemsep  the property value

@end(pdescription)
@blankspace(1)

        (remprop@pragma(defn)@index(remprop) @i<sym> @i<prop>)  @itemsep remove a property
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            @i<prop>  @itemsep    the property symbol

            returns   @itemsep  @code(nil)

@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Array Functions)@index(Array Functions)
@begin(fdescription)
        (aref@pragma(defn)@index(aref) @i<array> @i<n>)  @itemsep get the nth element of an array
@begin(pdescription)
            @i<array> @itemsep    the array

            @i<n>     @itemsep    the array index (integer)

            returns   @itemsep  the value of the array element

@end(pdescription)
@blankspace(1)

        (make-array@pragma(defn)@index(make-array) @i<size>)  @itemsep make a new array
@begin(pdescription)
            @i<size>  @itemsep    the size of the new array (integer)

            returns   @itemsep  the new array

@end(pdescription)
@blankspace(1)

        (vector@pragma(defn)@index(vector) @i<expr>...)  @itemsep make an initialized vector
@begin(pdescription)
            @i<expr>  @itemsep    the vector elements

            returns   @itemsep  the new vector

@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(List Functions)@index(List Functions)
@begin(fdescription)
        (car@pragma(defn)@index(car) @i<expr>) @itemsep  return the car of a list node
@begin(pdescription)
            @i<expr>  @itemsep    the list node

            returns   @itemsep  the car of the list node

@end(pdescription)
@blankspace(1)

        (cdr@pragma(defn)@index(cdr) @i<expr>)  @itemsep return the cdr of a list node
@begin(pdescription)
            @i<expr>  @itemsep    the list node

            returns   @itemsep  the cdr of the list node

@end(pdescription)
@blankspace(1)

        (c@i(xx)r@index(cxxr) @i<expr>)  @itemsep all c@i(xx)r combinations
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        (c@i(xxx)r@index(cxxxr) @i<expr>)  @itemsep all c@i(xxx)r combinations
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        (c@i(xxxx)r@index(cxxxxr) @i<expr>)  @itemsep all c@i(xxxx)r combinations
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        (first@pragma(defn)@index(first) @i<expr>)  @itemsep  a synonym for car
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        (second@pragma(defn)@index(second) @i<expr>)  @itemsep a synonym for cadr
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        (third@pragma(defn)@index(third) @i<expr>)  @itemsep  a synonym for caddr
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        (fourth@pragma(defn)@index(fourth) @i<expr>)  @itemsep a synonym for cadddr
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        (rest@pragma(defn)@index(rest) @i<expr>)  @itemsep   a synonym for cdr
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        (cons@pragma(defn)@index(cons) @i<expr1> @i<expr2>)  @itemsep construct a new list node
@begin(pdescription)
            @i<expr1> @itemsep    the car of the new list node

            @i<expr2> @itemsep    the cdr of the new list node

            returns   @itemsep  the new list node

@end(pdescription)
@blankspace(1)

        (list@pragma(defn)@index(list) @i<expr>...)  @itemsep create a list of values
@begin(pdescription)
            @i<expr>  @itemsep    expressions to be combined into a list

            returns   @itemsep  the new list

@end(pdescription)
@blankspace(1)

        (append@pragma(defn)@index(append) @i<expr>...)  @itemsep append lists
@begin(pdescription)
            @i<expr>  @itemsep    lists whose elements are to be appended

            returns   @itemsep  the new list

@end(pdescription)
@blankspace(1)

        (reverse@pragma(defn)@index(reverse) @i<expr>)  @itemsep reverse a list
@begin(pdescription)
            @i<expr>  @itemsep    the list to reverse

            returns   @itemsep  a new list in the reverse order

@end(pdescription)
@blankspace(1)

        (last@pragma(defn)@index(last) @i<list>)  @itemsep return the last list node of a list
@begin(pdescription)
            @i<list>  @itemsep    the list

            returns   @itemsep  the last list node in the list

@end(pdescription)
@blankspace(1)

        (member@pragma(defn)@index(member) @i<expr> @i<list> &key :test :test-not)  @itemsep find an expression in a list
@begin(pdescription)
            @i<expr>  @itemsep    the expression to find

            @i<list>  @itemsep    the list to search

            :test     @itemsep  the test function (defaults to eql)

            :test-not @itemsep  the test function (sense inverted)      

            returns   @itemsep  the remainder of the list starting with the expression

@end(pdescription)
@blankspace(1)

        (assoc@pragma(defn)@index(assoc) @i<expr> @i<alist> &key :test :test-not)  @itemsep find an expression in an a-list
@begin(pdescription)
            @i<expr>  @itemsep    the expression to find

            @i<alist> @itemsep    the association list

            :test     @itemsep  the test function (defaults to eql)

            :test-not @itemsep  the test function (sense inverted)      

            returns   @itemsep  the alist entry or @code(nil)

@end(pdescription)
@blankspace(1)

        (remove@pragma(defn)@index(remove) @i<expr> @i<list> &key :test :test-not)  @itemsep remove elements from a list
@begin(pdescription)
            @i<expr>  @itemsep    the element to remove

            @i<list>  @itemsep    the list

            :test     @itemsep  the test function (defaults to eql)

            :test-not @itemsep  the test function (sense inverted)      

            returns   @itemsep  copy of list with matching expressions removed

@end(pdescription)
@blankspace(1)

        (remove-if@pragma(defn)@index(remove-if) @i<test> @i<list>)  @itemsep remove elements that pass test
@begin(pdescription)
            @i<test>  @itemsep    the test predicate

            @i<list>  @itemsep    the list

            returns   @itemsep  copy of list with matching elements removed

@end(pdescription)
@blankspace(1)

        (remove-if-not@pragma(defn)@index(remove-if-not) @i<test> @i<list>)  @itemsep remove elements that fail test
@begin(pdescription)
            @i<test>  @itemsep    the test predicate

            @i<list>  @itemsep    the list

            returns   @itemsep  copy of list with non-matching elements removed

@end(pdescription)
@blankspace(1)

        (length@pragma(defn)@index(length) @i<expr>)  @itemsep find the length of a list, vector or string
@begin(pdescription)
            @i<expr>  @itemsep    the list, vector or string

            returns   @itemsep  the length of the list, vector or string

@end(pdescription)
@blankspace(1)

        (nth@pragma(defn)@index(nth) @i<n> @i<list>)  @itemsep return the nth element of a list
@begin(pdescription)
            @i<n>     @itemsep    the number of the element to return (zero origin)

            @i<list>  @itemsep    the list

            returns   @itemsep  the nth element or @code(nil) if the list isn't that long

@end(pdescription)
@blankspace(1)

        (nthcdr@pragma(defn)@index(nthcdr) @i<n> @i<list>)  @itemsep return the nth cdr of a list
@begin(pdescription)
            @i<n>     @itemsep    the number of the element to return (zero origin)

            @i<list>  @itemsep    the list

            returns   @itemsep  the nth cdr or @code(nil) if the list isn't that long

@end(pdescription)
@blankspace(1)

        (mapc@pragma(defn)@index(mapc) @i<fcn> @i<list1> @i<list>...)  @itemsep apply function to successive cars
@begin(pdescription)
            @i<fcn>   @itemsep    the function or function name

            @i<listn> @itemsep    a list for each argument of the function

            returns   @itemsep  the first list of arguments

@end(pdescription)
@blankspace(1)

        (mapcar@pragma(defn)@index(mapcar) @i<fcn> @i<list1> @i<list>...)  @itemsep apply function to successive cars
@begin(pdescription)
            @i<fcn>   @itemsep    the function or function name

            @i<listn> @itemsep    a list for each argument of the function

            returns   @itemsep  a list of the values returned

@end(pdescription)
@blankspace(1)

        (mapl@pragma(defn)@index(mapl) @i<fcn> @i<list1> @i<list>...)  @itemsep apply function to successive cdrs
@begin(pdescription)
            @i<fcn>   @itemsep    the function or function name

            @i<listn> @itemsep    a list for each argument of the function

            returns   @itemsep  the first list of arguments

@end(pdescription)
@blankspace(1)

        (maplist@pragma(defn)@index(maplist) @i<fcn> @i<list1> @i<list>...)  @itemsep apply function to successive cdrs
@begin(pdescription)
            @i<fcn>   @itemsep    the function or function name

            @i<listn> @itemsep    a list for each argument of the function

            returns   @itemsep  a list of the values returned

@end(pdescription)
@blankspace(1)

       (subst@pragma(defn)@index(subst) @i<to> @i<from> @i<expr> &key :test :test-not)  @itemsep substitute expressions
@begin(pdescription)
            @i<to>    @itemsep    the new expression

            @i<from>  @itemsep    the old expression

            @i<expr>  @itemsep    the expression in which to do the substitutions

            :test     @itemsep  the test function (defaults to eql)

            :test-not @itemsep  the test function (sense inverted)      

            returns   @itemsep  the expression with substitutions

@end(pdescription)
@blankspace(1)

        (sublis@pragma(defn)@index(sublis) @i<alist> @i<expr> &key :test :test-not)  @itemsep substitute with an a-list
@begin(pdescription)
            @i<alist> @itemsep    the association list

            @i<expr>  @itemsep    the expression in which to do the substitutions

            :test     @itemsep  the test function (defaults to eql)

            :test-not @itemsep  the test function (sense inverted)      

            returns   @itemsep  the expression with substitutions

@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Destructive List Functions)@index(Destructive List Functions)
@begin(fdescription)
        (rplaca@pragma(defn)@index(rplaca) @i<list> @i<expr>)  @itemsep replace the car of a list node
@begin(pdescription)
            @i<list> @itemsep     the list node

            @i<expr> @itemsep     the new value for the car of the list node

            returns  @itemsep   the list node after updating the car

@end(pdescription)
@blankspace(1)

        (rplacd@pragma(defn)@index(rplacd) @i<list> @i<expr>)  @itemsep replace the cdr of a list node
@begin(pdescription)
            @i<list> @itemsep     the list node

            @i<expr> @itemsep     the new value for the cdr of the list node

            returns  @itemsep   the list node after updating the cdr

@end(pdescription)
@blankspace(1)

        (nconc@pragma(defn)@index(nconc) @i<list>...)  @itemsep destructively concatenate lists
@begin(pdescription)
            @i<list> @itemsep     lists to concatenate

            returns  @itemsep   the result of concatenating the lists

@end(pdescription)
@blankspace(1)

        (delete@pragma(defn)@index(delete) @i<expr> &key :test :test-not)  @itemsep delete elements from a list
@begin(pdescription)
            @i<expr> @itemsep     the element to delete

            @i<list> @itemsep     the list

            :test    @itemsep   the test function (defaults to eql)

            :test-not @itemsep   the test function (sense inverted)      

            returns   @itemsep  the list with the matching expressions deleted

@end(pdescription)
@blankspace(1)

        (delete-if@pragma(defn)@index(delete-if) @i<test> @i<list>)  @itemsep delete elements that pass test
@begin(pdescription)
            @i<test>  @itemsep    the test predicate

            @i<list>  @itemsep    the list

            returns   @itemsep  the list with matching elements deleted

@end(pdescription)
@blankspace(1)

        (delete-if-not@pragma(defn)@index(delete-if-not) @i<test> @i<list>)  @itemsep delete elements that fail test
@begin(pdescription)
            @i<test>  @itemsep    the test predicate

            @i<list>  @itemsep    the list

            returns   @itemsep  the list with non-matching elements deleted

@end(pdescription)
@blankspace(1)

        (sort@pragma(defn)@index(sort) @i<list> @i<test>)  @itemsep sort a list
@begin(pdescription)
            @i<list>  @itemsep    the list to sort

            @i<test>  @itemsep    the comparison function

            returns   @itemsep  the sorted list

@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Predicate Functions)@index(Predicate Functions)
@begin(fdescription)
	(atom@pragma(defn)@index(atom) @i<expr>)  @itemsep is this an atom?
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @code(t) if the value is an atom, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (symbolp@pragma(defn)@index(symbolp) @i<expr>)  @itemsep is this a symbol?
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @code(t) if the expression is a symbol, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (numberp@pragma(defn)@index(numberp) @i<expr>)  @itemsep is this a number?
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @code(t) if the expression is a number, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (null@pragma(defn)@index(null) @i<expr>)  @itemsep is this an empty list?
@begin(pdescription)
            @i<expr>  @itemsep    the list to check

            returns   @itemsep @code(t) if the list is empty, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (not@pragma(defn)@index(not) @i<expr>)  @itemsep is this false?
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            return    @itemsep @code(t) if the value is @code(nil), @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (listp@pragma(defn)@index(listp) @i<expr>)  @itemsep is this a list?
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @code(t) if the value is a cons or @code(nil), @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (endp@pragma(defn)@index(endp) @i<list>)  @itemsep is this the end of a list
@begin(pdescription)
            @i<list>  @itemsep    the list

            returns   @itemsep @code(t) if the value is @code(nil), @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (consp@pragma(defn)@index(consp) @i<expr>)  @itemsep is this a non-empty list?
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @code(t) if the value is a cons, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (integerp@pragma(defn)@index(integerp) @i<expr>)  @itemsep is this an integer?
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @code(t) if the value is an integer, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (floatp@pragma(defn)@index(floatp) @i<expr>)  @itemsep is this a float?
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @code(t) if the value is a float, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (stringp@pragma(defn)@index(stringp) @i<expr>)  @itemsep is this a string?
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @code(t) if the value is a string, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (characterp@pragma(defn)@index(characterp) @i<expr>)  @itemsep is this a character?
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @code(t) if the value is a character, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (arrayp@pragma(defn)@index(arrayp) @i<expr>)  @itemsep is this an array?
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @code(t) if the value is an array, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (streamp@pragma(defn)@index(streamp) @i<expr>)  @itemsep is this a stream?
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @code(t) if the value is a stream, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (objectp@pragma(defn)@index(objectp) @i<expr>)  @itemsep is this an object?
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @code(t) if the value is an object, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (filep@pragma(defn)@index(filep) @i<expr>)@foot(This is not part of standard XLISP nor is it built-in. Nyquist defines it though.)  @itemsep is this a file? 
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @code(t) if the value is an object, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (boundp@pragma(defn)@index(boundp) @i<sym>)  @itemsep is a value bound to this symbol?
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            returns   @itemsep @code(t) if a value is bound to the symbol, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (fboundp@pragma(defn)@index(fboundp) @i<sym>)  @itemsep is a functional value bound to this symbol?
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            returns   @itemsep @code(t) if a functional value is bound to the symbol,

                        @code(nil) otherwise
@end(pdescription)
@blankspace(1)

        (minusp@pragma(defn)@index(minusp) @i<expr>)  @itemsep is this number negative?
@begin(pdescription)
            @i<expr>  @itemsep    the number to test

            returns   @itemsep @code(t) if the number is negative, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (zerop@pragma(defn)@index(zerop) @i<expr>)  @itemsep is this number zero?
@begin(pdescription)
            @i<expr>  @itemsep    the number to test

            returns   @itemsep @code(t) if the number is zero, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (plusp@pragma(defn)@index(plusp) @i<expr>)  @itemsep is this number positive?
@begin(pdescription)
            @i<expr>  @itemsep    the number to test

            returns   @itemsep @code(t) if the number is positive, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (evenp@pragma(defn)@index(evenp) @i<expr>)  @itemsep is this integer even?
@begin(pdescription)
            @i<expr>  @itemsep    the integer to test

            returns   @itemsep @code(t) if the integer is even, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (oddp@pragma(defn)@index(oddp) @i<expr>)  @itemsep is this integer odd?
@begin(pdescription)
            @i<expr>  @itemsep    the integer to test

            returns   @itemsep @code(t) if the integer is odd, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (eq@pragma(defn)@index(eq) @i<expr1> @i<expr2>)  @itemsep are the expressions identical?
@begin(pdescription)
            @i<expr1> @itemsep    the first expression

            @i<expr2> @itemsep    the second expression

            returns   @itemsep @code(t) if they are equal, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

(eql@pragma(defn)@index(eql) @i<expr1> @i<expr2>)  @itemsep are the expressions
identical? (works with all numbers)
@begin(pdescription)
            @i<expr1> @itemsep    the first expression

            @i<expr2> @itemsep    the second expression

            returns   @itemsep @code(t) if they are equal, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (equal@pragma(defn)@index(equal) @i<expr1> @i<expr2>)  @itemsep are the expressions equal?
@begin(pdescription)
            @i<expr1> @itemsep    the first expression

            @i<expr2> @itemsep    the second expression

            returns   @itemsep @code(t) if they are equal, @code(nil) otherwise

@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Control Constructs)@index(Control Constructs)
@begin(fdescription)
        (cond@pragma(defn)@index(cond) @i<pair>...)  @itemsep evaluate conditionally
@begin(pdescription)
            @i<pair>  @itemsep    pair consisting of:

@begin(pdescription)
                            (@i<pred> @i<expr>...)
@end(pdescription)@pragma(stopcodef)
                          where:
@begin(pdescription)
                            @i<pred> @itemsep     is a predicate expression

                            @i<expr> @itemsep     evaluated if the predicate
 is not @code(nil)
@end(pdescription)@pragma(stopcodef)
returns  @itemsep   the value of the first expression whose predicate is not
@code(nil)
@end(pdescription)
@blankspace(1)

        (and@pragma(defn)@index(and) @i<expr>...)  @itemsep the logical and of a list of expressions
@begin(pdescription)
            @i<expr> @itemsep     the expressions to be anded

            returns  @itemsep   @code(nil) if any expression evaluates to @code(nil),
                        otherwise the value of the last expression
                        (evaluation of expressions stops after the first
                         expression that evaluates to @code(nil))
@end(pdescription)
@blankspace(1)

        (or@pragma(defn)@index(or) @i<expr>...)  @itemsep the logical or of a list of expressions
@begin(pdescription)
            @i<expr> @itemsep     the expressions to be ored

            returns  @itemsep   @code(nil) if all expressions evaluate to @code(nil),
                  otherwise the value of the first non-@code(nil) expression
                        (evaluation of expressions stops after the first
                         expression that does not evaluate to @code(nil))
@end(pdescription)
@blankspace(1)

        (if@pragma(defn)@index(if) @i<texpr> @i<expr1> [@i<expr2>])  @itemsep evaluate expressions conditionally
@begin(pdescription)
            @i<texpr> @itemsep    the test expression

            @i<expr1> @itemsep    the expression to be evaluated if texpr is non-@code(nil)

            @i<expr2> @itemsep    the expression to be evaluated if texpr is @code(nil)

            returns   @itemsep  the value of the selected expression

@end(pdescription)
@blankspace(1)

        (when@pragma(defn)@index(when) @i<texpr> @i<expr>...)  @itemsep evaluate only when a condition is true
@begin(pdescription)
            @i<texpr> @itemsep    the test expression

            @i<expr>  @itemsep    the expression(s) to be evaluated if texpr is non-@code(nil)

            returns @itemsep the value of the last expression or @code(nil)
@end(pdescription)
@blankspace(1)

        (unless@pragma(defn)@index(unless) @i<texpr> @i<expr>...)  @itemsep evaluate only when a condition is false
@begin(pdescription)
            @i<texpr> @itemsep    the test expression

            @i<expr>  @itemsep    the expression(s) to be evaluated if texpr is @code(nil)

            returns   @itemsep  the value of the last expression or @code(nil)

@end(pdescription)
@blankspace(1)

          (case@pragma(defn)@index(case) @i<expr> @i<case>...)  @itemsep select by case
@begin(pdescription)
            @i<expr>  @itemsep    the selection expression

            @i<case>  @itemsep    pair consisting of:

@begin(pdescription)
                            (@i<value> @i<expr>...)
@end(pdescription)@pragma(stopcodef)
                          where:
@begin(pdescription)
                            @i<value> @itemsep     is a single expression or a list of
                                        expressions (unevaluated)

                            @i<expr>  @itemsep    are expressions to execute if the
                                        case matches
@end(pdescription)@pragma(stopcodef)
            returns @itemsep     the value of the last expression of the matching case

@end(pdescription)
@blankspace(1)
@begin(fgroup)
       (let@pragma(defn)@index(let) (@i<binding>...) @i<expr>...)  @itemsep create local bindings

@pragma(startcodef)
        (let*@pragma(defn)@index(let*) (@i<binding>...) @i<expr>...)  @itemsep let with sequential binding
@end(fgroup)
@begin(pdescription)
            @i<binding> @itemsep   the variable bindings each of which is either:

@begin(pdescription)
                        1)  a symbol (which is initialized to @code(nil))

                        2)  a list whose car is a symbol and whose cadr
                                is an initialization expression
@end(pdescription)@pragma(stopcodef)
            @i<expr> @itemsep     the expressions to be evaluated

            returns  @itemsep   the value of the last expression

@end(pdescription)
@blankspace(1)
@begin(fgroup)
        (flet@pragma(defn)@index(flet) (@i<binding>...) @i<expr>...)  @itemsep create local functions

@pragma(startcodef)
        (labels@pragma(defn)@index(labels) (@i<binding>...) @i<expr>...) @itemsep  flet with recursive functions

@pragma(startcodef)
        (macrolet@pragma(defn)@index(macrolet) (@i<binding>...) @i<expr>...) @itemsep  create local macros
@end(fgroup)
@begin(pdescription)
            @i<binding> @itemsep  the function bindings each of which is:

@begin(pdescription)
                          (@i<sym> @i<fargs> @i<expr>...)
@end(pdescription)@pragma(stopcodef)
                        where:
@begin(pdescription)
                            @i<sym> @itemsep      the function/macro name

                            @i<fargs> @itemsep     formal argument list (lambda list)

                            @i<expr>  @itemsep    expressions constituting the body of
                                        the function/macro
@end(pdescription)@pragma(stopcodef)
            @i<expr> @itemsep     the expressions to be evaluated

            returns  @itemsep   the value of the last expression
@end(pdescription)
@blankspace(1)

        (catch@pragma(defn)@index(catch) @i<sym> @i<expr>...)  @itemsep evaluate expressions and catch throws
@begin(pdescription)
            @i<sym>  @itemsep     the catch tag

            @i<expr> @itemsep     expressions to evaluate

            returns  @itemsep   the value of the last expression the throw expression

@end(pdescription)
@blankspace(1)

        (throw@pragma(defn)@index(throw) @i<sym> [@i<expr>])  @itemsep throw to a catch
@begin(pdescription)
            @i<sym>  @itemsep     the catch tag

            @i<expr> @itemsep     the value for the catch to return (defaults to @code(nil))

            returns  @itemsep   never returns

@end(pdescription)
@blankspace(1)

        (unwind-protect@pragma(defn)@index(unwind-protect) @i<expr> @i<cexpr>...)  @itemsep protect evaluation of an expression
@begin(pdescription)
            @i<expr> @itemsep     the expression to protect

            @i<cexpr> @itemsep     the cleanup expressions

            returns   @itemsep  the value of the expression@*

          Note:  unwind-protect guarantees to execute the cleanup expressions
                 even if a non-local exit terminates the evaluation of the
                 protected expression
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Looping Constructs)@index(Looping Constructs)
@begin(fdescription)
        (loop@pragma(defn)@index(loop) @i<expr>...)  @itemsep basic looping form
@begin(pdescription)
            @i<expr> @itemsep     the body of the loop

            returns  @itemsep   never returns (must use non-local exit)

@end(pdescription)
@blankspace(1)
@begin(fgroup)
        (do@pragma(defn)@index(do) (@i<binding>...) (@i<texpr> @i<rexpr>...) @i<expr>...)
@pragma(endcodef)
        (do*@pragma(defn)@index(do*) (@i<binding>...) (@i<texpr> @i<rexpr>...) @i<expr>...)
@end(fgroup)
@begin(pdescription)
            @i<binding> @itemsep  the variable bindings each of which is either:

@begin(pdescription)
                        1)  a symbol (which is initialized to @code(nil))

                        2)  a list of the form: (@i<sym> @i<init> [@i<step>])
                            where:
@begin(pdescription)
                                @i<sym> @itemsep is the symbol to bind

                                @i<init> @itemsep is the initial value of the symbol

                                @i<step> @itemsep is a step expression

@end(pdescription)
@end(pdescription)@pragma(stopcodef)
            @i<texpr> @itemsep    the termination test expression

            @i<rexpr> @itemsep    result expressions (the default is @code(nil))

            @i<expr>  @itemsep    the body of the loop (treated like an implicit prog)

            returns   @itemsep  the value of the last result expression

@end(pdescription)
@blankspace(1)

    (dolist@pragma(defn)@index(dolist) (@i<sym> @i<expr> [@i<rexpr>]) @i<expr>...)  @itemsep loop through a list
@begin(pdescription)
            @i<sym>   @itemsep    the symbol to bind to each list element

            @i<expr>  @itemsep    the list expression

            @i<rexpr> @itemsep    the result expression (the default is @code(nil))

            @i<expr>  @itemsep    the body of the loop (treated like an implicit prog)

@end(pdescription)
@blankspace(1)

        (dotimes@pragma(defn)@index(dotimes) (@i<sym> @i<expr> [@i<rexpr>]) @i<expr>...)  @itemsep loop from zero to n-1
@begin(pdescription)
            @i<sym>   @itemsep    the symbol to bind to each value from 0 to n-1

            @i<expr>  @itemsep    the number of times to loop

            @i<rexpr> @itemsep    the result expression (the default is @code(nil))

            @i<expr>  @itemsep    the body of the loop (treated like an implicit prog)

@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(The Program Feature)@index(The Program Feature)
@begin(fdescription)
@begin(fgroup)
(prog@pragma(defn)@index(prog) (@i<binding>...) @i<expr>...)  @itemsep the program feature

@pragma(startcodef)
(prog*@pragma(defn)@index(prog*) (@i<binding>...) @i<expr>...)  @itemsep prog with sequential binding
@end(fgroup)
@begin(pdescription)
            @i<binding> @itemsep  the variable bindings each of which is either:

@begin(pdescription)
                        1)  a symbol (which is initialized to @code(nil))

                        2)  a list whose car is a symbol and whose cadr
                                is an initialization expression
@end(pdescription)@pragma(stopcodef)
            @i<expr>  @itemsep    expressions to evaluate or tags (symbols)

            returns   @itemsep  @code(nil) or the argument passed to the return function

@end(pdescription)
@blankspace(1)

        (block@pragma(defn)@index(block) @i<name> @i<expr>...)  @itemsep named block
@begin(pdescription)
            @i<name>  @itemsep    the block name (symbol)

            @i<expr>  @itemsep    the block body

            returns   @itemsep  the value of the last expression

@end(pdescription)
@blankspace(1)

        (return@pragma(defn)@index(return) [@i<expr>])  @itemsep cause a prog construct to return a value
@begin(pdescription)
            @i<expr>  @itemsep    the value (defaults to @code(nil))

            returns   @itemsep  never returns

@end(pdescription)
@blankspace(1)

        (return-from@pragma(defn)@index(return-from) @i<name> [@i<value>])  @itemsep return from a named block
@begin(pdescription)
            @i<name>  @itemsep    the block name (symbol)

            @i<value> @itemsep    the value to return (defaults to @code(nil))

            returns   @itemsep  never returns

@end(pdescription)
@blankspace(1)

        (tagbody@pragma(defn)@index(tagbody) @i<expr>...)  @itemsep block with labels
@begin(pdescription)
            @i<expr>  @itemsep    expression(s) to evaluate or tags (symbols)

            returns   @itemsep  @code(nil)

@end(pdescription)
@blankspace(1)

        (go@pragma(defn)@index(go) @i<sym>)  @itemsep go to a tag within a tagbody or prog
@begin(pdescription)
            @i<sym>   @itemsep    the tag (quoted)

            returns   @itemsep  never returns

@end(pdescription)
@blankspace(1)

        (progv@pragma(defn)@index(progv) @i<slist> @i<vlist> @i<expr>...)  @itemsep dynamically bind symbols
@begin(pdescription)
            @i<slist> @itemsep    list of symbols

            @i<vlist> @itemsep    list of values to bind to the symbols

            @i<expr>  @itemsep    expression(s) to evaluate

            returns   @itemsep  the value of the last expression

@end(pdescription)
@blankspace(1)

        (prog1@pragma(defn)@index(prog1) @i<expr1> @i<expr>...)  @itemsep execute expressions sequentially
@begin(pdescription)
            @i<expr1> @itemsep    the first expression to evaluate

            @i<expr>  @itemsep    the remaining expressions to evaluate

            returns   @itemsep  the value of the first expression

@end(pdescription)
@blankspace(1)

        (prog2@pragma(defn)@index(prog2) @i<expr1> @i<expr2> @i<expr>...)  @itemsep execute expressions sequentially
@begin(pdescription)
            @i<expr1> @itemsep    the first expression to evaluate

            @i<expr2> @itemsep    the second expression to evaluate

            @i<expr>  @itemsep    the remaining expressions to evaluate

            returns   @itemsep  the value of the second expression

@end(pdescription)
@blankspace(1)

        (progn@pragma(defn)@index(progn) @i<expr>...)  @itemsep execute expressions sequentially
@begin(pdescription)
            @i<expr>  @itemsep    the expressions to evaluate

            returns   @itemsep  the value of the last expression (or @code(nil))

@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Debugging and Error Handling)@index(Debugging)@index(Error Handling)
@begin(fdescription)
        (trace@pragma(defn)@index(trace) @i<sym>)  @itemsep add a function to the trace list
@begin(pdescription)
            @i<sym>   @itemsep    the function to add (quoted)

            returns   @itemsep  the trace list

@end(pdescription)
@blankspace(1)

        (untrace@pragma(defn)@index(untrace) @i<sym>)  @itemsep remove a function from the trace list
@begin(pdescription)
            @i<sym>   @itemsep    the function to remove (quoted)

            returns   @itemsep  the trace list

@end(pdescription)
@blankspace(1)

        (error@pragma(defn)@index(error) @i<emsg> [@i<arg>])  @itemsep signal a non-correctable error
@begin(pdescription)
            @i<emsg>  @itemsep    the error message string

            @i<arg>   @itemsep    the argument expression (printed after the message)

            returns   @itemsep  never returns

@end(pdescription)
@blankspace(1)

        (cerror@pragma(defn)@index(cerror) @i<cmsg> @i<emsg> [@i<arg>])  @itemsep signal a correctable error
@begin(pdescription)
            @i<cmsg>  @itemsep    the continue message string

            @i<emsg>  @itemsep    the error message string

            @i<arg>   @itemsep    the argument expression (printed after the message)

            returns   @itemsep  @code(nil) when continued from the break loop

@end(pdescription)
@blankspace(1)

        (break@pragma(defn)@index(break) [@i<bmsg> [@i<arg>]])  @itemsep enter a break loop
@begin(pdescription)
            @i<bmsg>  @itemsep    the break message string (defaults to @code(**break**))

            @i<arg>   @itemsep    the argument expression (printed after the message)

            returns   @itemsep  @code(nil) when continued from the break loop

@end(pdescription)
@blankspace(1)

        (clean-up@pragma(defn)@index(clean-up))  @itemsep clean-up after an error
@begin(pdescription)
            returns   @itemsep  never returns

@end(pdescription)
@blankspace(1)

        (top-level@pragma(defn)@index(top-level))  @itemsep clean-up after an error and return to the top level
@begin(pdescription)
            returns   @itemsep  never returns

@end(pdescription)
@blankspace(1)

        (continue@pragma(defn)@index(continue))  @itemsep continue from a correctable error
@begin(pdescription)
            returns   @itemsep  never returns

@end(pdescription)
@blankspace(1)

        (errset@pragma(defn)@index(errset) @i<expr> [@i<pflag>])  @itemsep trap errors
@begin(pdescription)
            @i<expr>  @itemsep    the expression to execute

            @i<pflag> @itemsep    flag to control printing of the error message

            returns   @itemsep  the value of the last expression consed with @code(nil)

                        or @code(nil) on error
@end(pdescription)
@blankspace(1)

        (baktrace@pragma(defn)@index(baktrace)@index(debugging)@index(stack trace) [@i<n>])  @itemsep print n levels of trace back information
@begin(pdescription)
            @i<n>     @itemsep    the number of levels (defaults to all levels)

            returns   @itemsep  @code(nil)

@end(pdescription)
@blankspace(1)

        (evalhook@pragma(defn)@index(evalhook) @i<expr> @i<ehook> @i<ahook> [@i<env>])  @itemsep evaluate with hooks
@begin(pdescription)
            @i<expr>  @itemsep    the expression to evaluate

            @i<ehook> @itemsep    the value for @code(*evalhook*)

            @i<ahook> @itemsep    the value for @code(*applyhook*)

            @i<env>   @itemsep    the environment (default is @code(nil))

            returns   @itemsep  the result of evaluating the expression

@end(pdescription)
@blankspace(1)

        (profile@pragma(defn)@index(profile) @i(flag))@foot(This is not a standard XLISP 2.0 function.)  @itemsep turn profiling on or off.
@begin(pdescription)
            @i<flag>   @itemsep    @code(nil) turns profiling off, otherwise on

            returns   @itemsep  the previous state of profiling.

@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Arithmetic Functions)@index(Arithmetic Functions)
@begin(fdescription)
        (truncate@pragma(defn)@index(truncate) @i<expr>)  @itemsep truncates a floating point number to an integer
@begin(pdescription)
            @i<expr>  @itemsep    the number

            returns   @itemsep  the result of truncating the number

@end(pdescription)
@blankspace(1)

        (float@pragma(defn)@index(float) @i<expr>)  @itemsep converts an integer to a floating point number
@begin(pdescription)
            @i<expr>  @itemsep    the number

            returns   @itemsep  the result of floating the integer

@end(pdescription)
@blankspace(1)

        (+@pragma(defn)@index(+) @i<expr>...)  @itemsep add a list of numbers
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the addition

@end(pdescription)
@blankspace(1)

        (-@pragma(defn)@index(-) @i<expr>...)  @itemsep subtract a list of numbers or negate a single number
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the subtraction

@end(pdescription)
@blankspace(1)

        (*@pragma(defn)@index(*) @i<expr>...)  @itemsep multiply a list of numbers
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the multiplication

@end(pdescription)
@blankspace(1)

        (/@pragma(defn)@index(/) @i<expr>...)  @itemsep divide a list of numbers
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the division

@end(pdescription)
@blankspace(1)

        (1+@pragma(defn)@index(1+) @i<expr>)  @itemsep add one to a number
@begin(pdescription)
            @i<expr>  @itemsep    the number

            returns   @itemsep  the number plus one

@end(pdescription)
@blankspace(1)

        (1-@pragma(defn)@index(1-) @i<expr>)  @itemsep subtract one from a number
@begin(pdescription)
            @i<expr>  @itemsep    the number

            returns   @itemsep  the number minus one

@end(pdescription)
@blankspace(1)

        (rem@pragma(defn)@index(rem)@index(remainder)@index[modulo (rem) function] @i<expr>...)  @itemsep remainder of a list of numbers
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the remainder operation

@end(pdescription)
@blankspace(1)

        (min@pragma(defn)@index(min)@index(minimum) @i<expr>...)  @itemsep the smallest of a list of numbers
@begin(pdescription)
            @i<expr>  @itemsep    the expressions to be checked

            returns   @itemsep  the smallest number in the list

@end(pdescription)
@blankspace(1)

        (max@pragma(defn)@index(max)@index(maximum) @i<expr>...)  @itemsep the largest of a list of numbers
@begin(pdescription)
            @i<expr>  @itemsep    the expressions to be checked

            returns   @itemsep  the largest number in the list

@end(pdescription)
@blankspace(1)

        (abs@pragma(defn)@index(abs) @i<expr>)  @itemsep the absolute value of a number
@begin(pdescription)
            @i<expr>  @itemsep    the number

            returns   @itemsep  the absolute value of the number

@end(pdescription)
@blankspace(1)

        (gcd@pragma(defn)@index(gcd) @i<n1> @i<n2>...)  @itemsep compute the greatest common divisor
@begin(pdescription)
            @i<n1>    @itemsep    the first number (integer)

            @i<n2>    @itemsep    the second number(s) (integer)

            returns   @itemsep  the greatest common divisor

@end(pdescription)
@blankspace(1)

       (random@pragma(defn)@index(random) @i<n>)  @itemsep compute a random number between 0 and n-1 inclusive
@begin(pdescription)
            @i<n>     @itemsep    the upper bound (integer)

            returns   @itemsep  a random number

@end(pdescription)
@blankspace(1)

       (rrandom@pragma(defn)@index(rrandom)@index(uniform random))  @itemsep compute a random real number between 0 and 1 inclusive
@begin(pdescription)
            returns   @itemsep  a random floating point number

@end(pdescription)
@blankspace(1)

        (sin@pragma(defn)@index(sin) @i<expr>)  @itemsep compute the sine of a number
@begin(pdescription)
            @i<expr>  @itemsep    the floating point number

            returns   @itemsep  the sine of the number

@end(pdescription)
@blankspace(1)

        (cos@pragma(defn)@index(cos) @i<expr>)  @itemsep compute the cosine of a number
@begin(pdescription)
            @i<expr>  @itemsep    the floating point number

            returns   @itemsep  the cosine of the number

@end(pdescription)
@blankspace(1)

        (tan@pragma(defn)@index(tan) @i<expr>)  @itemsep compute the tangent of a number
@begin(pdescription)
            @i<expr>  @itemsep    the floating point number

            returns   @itemsep  the tangent of the number

@end(pdescription)
@blankspace(1)

        (atan@pragma(defn)@index(atan) @i<expr> [@i<expr2>])@foot(This is not a standard XLISP 2.0 function.)  @itemsep compute the arctangent
@begin(pdescription)
            @i<expr>  @itemsep    the value of @i(x)

            @i<expr2> @itemsep    the value of @i(y) (default value is 1.0)

            returns   @itemsep  the arctangent of @i(x)/@i(y)

@end(pdescription)
@blankspace(1)

        (expt@pragma(defn)@index(expt) @i<x-expr> @i<y-expr>)  @itemsep compute x to the y power
@begin(pdescription)
            @i<x-expr> @itemsep    the floating point number

            @i<y-expr> @itemsep   the floating point exponent

            returns    @itemsep x to the y power

@end(pdescription)
@blankspace(1)

        (exp@pragma(defn)@index(exp) @i<x-expr>)  @itemsep compute e to the x power
@begin(pdescription)
            @i<x-expr> @itemsep   the floating point number

            returns   @itemsep  e to the x power

@end(pdescription)
@blankspace(1)

        (sqrt@pragma(defn)@index(sqrt) @i<expr>)  @itemsep compute the square root of a number
@begin(pdescription)
            @i<expr>  @itemsep    the floating point number

            returns   @itemsep  the square root of the number

@end(pdescription)
@blankspace(1)
@begin(fgroup)
(<@pragma(defn)@index(<) @i<n1> @i<n2>...)  @itemsep test for less than

(<=@pragma(defn)@index(<=) @i<n1> @i<n2>...) @itemsep  test for less than or equal to

(=@pragma(defn)@index(=) @i<n1> @i<n2>...)  @itemsep test for equal to

(/=@pragma(defn)@index(/=) @i<n1> @i<n2>...) @itemsep  test for not equal to

(>=@pragma(defn)@index(>=) @i<n1> @i<n2>...) @itemsep   test for greater than or equal to

(>@pragma(defn)@index(>) @i<n1> @i<n2>...) @itemsep   test for greater than
@end(fgroup)
@begin(pdescription)
            @i<n1>    @itemsep    the first number to compare

            @i<n2>    @itemsep    the second number to compare

returns   @itemsep  @code(t) if the results of comparing @i<n1> with @i<n2>,
@i<n2> with @i<n3>, etc., are all true.

@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Bitwise Logical Functions)@index(Bitwise Logical Functions)
@begin(fdescription)
        (logand@pragma(defn)@index(logand) @i<expr>...) @itemsep  the bitwise and of a list of numbers
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the and operation

@end(pdescription)
@blankspace(1)

        (logior@pragma(defn)@index(logior) @i<expr>...)  @itemsep the bitwise inclusive or of a list of numbers
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the inclusive or operation

@end(pdescription)
@blankspace(1)

        (logxor@pragma(defn)@index(logxor) @i<expr>...)  @itemsep the bitwise exclusive or of a list of numbers
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the exclusive or operation

@end(pdescription)
@blankspace(1)

        (lognot@pragma(defn)@index(lognot) @i<expr>)  @itemsep the bitwise not of a number
@begin(pdescription)
            @i<expr>  @itemsep    the number

            returns   @itemsep  the bitwise inversion of number

@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(String Functions)@index(String Functions)
@begin(fdescription)
        (string@pragma(defn)@index(string) @i<expr>) @itemsep  make a string from a value
@begin(pdescription)
            @i<expr>  @itemsep    an integer (which is first converted into its ASCII character value), string, character, or symbol

            returns   @itemsep  the string representation of the argument

@end(pdescription)
@blankspace(1)

        (string-search@pragma(defn)@index(string-search)@index(find string) @i<pat> @i<str> &key :start :end)@foot(This is not a standard XLISP 2.0 function.)  @itemsep search for pattern in string
@begin(pdescription)
            @i<pat>   @itemsep    a string to search for

            @i<str>   @itemsep    the string to be searched

            :start    @itemsep  the starting offset in str

            :end      @itemsep  the ending offset + 1

            returns   @itemsep  index of pat in str or NIL if not found

@end(pdescription)
@blankspace(1)

        (string-trim@pragma(defn)@index(string-trim) @i<bag> @i<str>)  @itemsep trim both ends of a string
@begin(pdescription)
            @i<bag>   @itemsep    a string containing characters to trim

            @i<str>   @itemsep    the string to trim

            returns   @itemsep  a trimed copy of the string

@end(pdescription)
@blankspace(1)

        (string-left-trim@pragma(defn)@index(string-left-trim) @i<bag> @i<str>)  @itemsep trim the left end of a string
@begin(pdescription)
            @i<bag>   @itemsep    a string containing characters to trim

            @i<str>   @itemsep    the string to trim

            returns   @itemsep  a trimed copy of the string

@end(pdescription)
@blankspace(1)

        (string-right-trim@pragma(defn)@index(string-right-trim) @i<bag> @i<str>)  @itemsep trim the right end of a string
@begin(pdescription)
            @i<bag>   @itemsep    a string containing characters to trim

            @i<str>   @itemsep    the string to trim

            returns   @itemsep  a trimed copy of the string

@end(pdescription)
@blankspace(1)

        (string-upcase@pragma(defn)@index(string-upcase) @i<str> &key :start :end)  @itemsep convert to uppercase
@begin(pdescription)
            @i<str>   @itemsep    the string

            :start    @itemsep  the starting offset

            :end      @itemsep  the ending offset + 1

            returns   @itemsep  a converted copy of the string

@end(pdescription)
@blankspace(1)

        (string-downcase@pragma(defn)@index(string-downcase) @i<str> &key :start :end)  @itemsep convert to lowercase
@begin(pdescription)
            @i<str>   @itemsep    the string

            :start    @itemsep  the starting offset

            :end      @itemsep  the ending offset + 1

            returns   @itemsep  a converted copy of the string

@end(pdescription)
@blankspace(1)

        (nstring-upcase@pragma(defn)@index(nstring-upcase) @i<str> &key :start :end)  @itemsep convert to uppercase
@begin(pdescription)
            @i<str>   @itemsep    the string

            :start    @itemsep  the starting offset

            :end      @itemsep  the ending offset + 1

            returns   @itemsep  the converted string (not a copy)

@end(pdescription)
@blankspace(1)

        (nstring-downcase@pragma(defn)@index(nstring-downcase) @i<str> &key :start :end)  @itemsep convert to lowercase
@begin(pdescription)
            @i<str>   @itemsep    the string

            :start    @itemsep  the starting offset

            :end      @itemsep  the ending offset + 1

            returns  @itemsep   the converted string (not a copy)

@end(pdescription)
@blankspace(1)

        (strcat@pragma(defn)@index(strcat)@index(concatenate strings) @i<expr>...)  @itemsep concatenate strings
@begin(pdescription)
            @i<expr> @itemsep     the strings to concatenate

            returns  @itemsep   the result of concatenating the strings

@end(pdescription)
@blankspace(1)

        (subseq@pragma(defn)@index(subseq) @i<string> @i<start> [@i<end>])  @itemsep extract a substring
@begin(pdescription)
            @i<string> @itemsep   the string

            @i<start>  @itemsep   the starting position (zero origin)

            @i<end>    @itemsep   the ending position + 1 (defaults to end)

            returns    @itemsep substring between @i<start> and @i<end>

@end(pdescription)
@blankspace(1)
@begin(fgroup)
        (string<@pragma(defn)@index(string<) @i<str1> @i<str2> &key :start1 :end1 :start2 :end2)
@pragma(endcodef)
        (string<=@pragma(defn)@index(string<=) @i<str1> @i<str2> &key :start1 :end1 :start2 :end2)
@pragma(endcodef)

        (string=@pragma(defn)@index(string=) @i<str1> @i<str2> &key :start1 :end1 :start2 :end2)
@pragma(endcodef)

        (string/=@pragma(defn)@index(string/=) @i<str1> @i<str2> &key :start1 :end1 :start2 :end2)
@pragma(endcodef)

        (string>=@pragma(defn)@index(string>=) @i<str1> @i<str2> &key :start1 :end1 :start2 :end2)
@pragma(endcodef)

        (string>@pragma(defn)@index(string>) @i<str1> @i<str2> &key :start1 :end1 :start2 :end2)
@end(fgroup)
@begin(pdescription)
            @i<str1> @itemsep     the first string to compare

            @i<str2> @itemsep     the second string to compare

            :start1  @itemsep   first substring starting offset

            :end1    @itemsep   first substring ending offset + 1

            :start2  @itemsep   second substring starting offset

            :end2    @itemsep   second substring ending offset + 1

            returns  @itemsep   @code(t) if predicate is true, @code(nil) otherwise

          Note: case is significant with these comparison functions.
@end(pdescription)
@blankspace(1)
@begin(fgroup)
(string-lessp@pragma(defn)@index(string-lessp) @i<str1> @i<str2> &key :start1 :end1 :start2 :end2)
@pragma(endcodef)

(string-not-greaterp@pragma(defn)@index(string-not-greaterp) @i<str1> @i<str2> &key :start1 :end1 :start2 :end2)
@pragma(endcodef)

(string-equalp@pragma(defn)@index(string-equalp) @i<str1> @i<str2> &key :start1 :end1 :start2 :end2)
@pragma(endcodef)

(string-not-equalp@pragma(defn)@index(string-not-equalp) @i<str1> @i<str2> &key :start1 :end1 :start2 :end2)
@pragma(endcodef)

(string-not-lessp@pragma(defn)@index(string-not-lessp) @i<str1> @i<str2> &key :start1 :end1 :start2 :end2)
@pragma(endcodef)

(string-greaterp@pragma(defn)@index(string-greaterp) @i<str1> @i<str2> &key :start1 :end1 :start2 :end2)
@end(fgroup)
@begin(pdescription)
            @i<str1> @itemsep     the first string to compare

            @i<str2> @itemsep     the second string to compare

            :start1  @itemsep   first substring starting offset

            :end1    @itemsep   first substring ending offset + 1

            :start2  @itemsep   second substring starting offset

            :end2    @itemsep   second substring ending offset + 1

    returns  @itemsep   @code(t) if predicate is true, @code(nil) otherwise

          Note: case is not significant with these comparison functions.
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Character Functions)@index(Character Functions)
@begin(fdescription)
        (char@pragma(defn)@index(char) @i<string> @i<index>) @itemsep  extract a character from a string
@begin(pdescription)
            @i<string> @itemsep   the string

            @i<index>  @itemsep   the string index (zero relative)

            returns    @itemsep the ascii code of the character

@end(pdescription)
@blankspace(1)

        (upper-case-p@pragma(defn)@index(upper-case-p) @i<chr>)  @itemsep is this an upper case character?
@begin(pdescription)
            @i<chr> @itemsep      the character

            returns @itemsep    @code(t) if the character is upper case, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (lower-case-p@pragma(defn)@index(lower-case-p) @i<chr>)  @itemsep is this a lower case character?
@begin(pdescription)
            @i<chr> @itemsep      the character

            returns @itemsep    @code(t) if the character is lower case, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (both-case-p@pragma(defn)@index(both-case-p) @i<chr>)  @itemsep is this an alphabetic (either case) character?
@begin(pdescription)
            @i<chr> @itemsep      the character

            returns @itemsep    @code(t) if the character is alphabetic, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (digit-char-p@pragma(defn)@index(digit-char-p) @i<chr>)  @itemsep is this a digit character?
@begin(pdescription)
            @i<chr> @itemsep      the character

            returns @itemsep    the digit weight if character is a digit, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (char-code@pragma(defn)@index(char-code) @i<chr>)  @itemsep get the ascii code of a character
@begin(pdescription)
            @i<chr> @itemsep      the character

            returns @itemsep    the ascii character code (integer)

@end(pdescription)
@blankspace(1)

        (code-char@pragma(defn)@index(code-char) @i<code>)  @itemsep get the character with a specified ascii code
@begin(pdescription)
            @i<code> @itemsep     the ascii code (integer)

            returns  @itemsep   the character with that code or @code(nil)

@end(pdescription)
@blankspace(1)

        (char-upcase@pragma(defn)@index(char-upcase) @i<chr>)  @itemsep convert a character to upper case
@begin(pdescription)
            @i<chr>  @itemsep     the character

            returns  @itemsep   the upper case character

@end(pdescription)
@blankspace(1)

        (char-downcase@pragma(defn)@index(char-downcase) @i<chr>)  @itemsep convert a character to lower case
@begin(pdescription)
            @i<chr>  @itemsep     the character

            returns  @itemsep   the lower case character

@end(pdescription)
@blankspace(1)

        (digit-char@pragma(defn)@index(digit-char) @i<n>)  @itemsep convert a digit weight to a digit
@begin(pdescription)
            @i<n>    @itemsep     the digit weight (integer)

            returns  @itemsep   the digit character or @code(nil)

@end(pdescription)
@blankspace(1)

        (char-int@pragma(defn)@index(char-int) @i<chr>)  @itemsep convert a character to an integer
@begin(pdescription)
            @i<chr>  @itemsep     the character

            returns  @itemsep   the ascii character code

@end(pdescription)
@blankspace(1)

        (int-char@pragma(defn)@index(int-char) @i<int>)  @itemsep convert an integer to a character
@begin(pdescription)
            @i<int>  @itemsep     the ascii character code

            returns  @itemsep   the character with that code

@end(pdescription)
@blankspace(1)
@begin(fgroup)
       (char<@pragma(defn)@index(char<) @i<chr1> @i<chr2>...)
@pragma(endcodef)

        (char<=@pragma(defn)@index(char<=) @i<chr1> @i<chr2>...)
@pragma(endcodef)

        (char=@pragma(defn)@index(char=) @i<chr1> @i<chr2>...)
@pragma(endcodef)

        (char/=@pragma(defn)@index(char/=) @i<chr1> @i<chr2>...)
@pragma(endcodef)

        (char>=@pragma(defn)@index(char>=) @i<chr1> @i<chr2>...)
@pragma(endcodef)

        (char>@pragma(defn)@index(char>) @i<chr1> @i<chr2>...)
@end(fgroup)
@begin(pdescription)
            @i<chr1> @itemsep     the first character to compare

            @i<chr2> @itemsep     the second character(s) to compare

            returns  @itemsep   @code(t) if predicate is true, @code(nil) otherwise

          Note: case is significant with these comparison functions.
@end(pdescription)
@blankspace(1)
@begin(fgroup)
(char-lessp@pragma(defn)@index(char-lessp) @i<chr1> @i<chr2>...)
@pragma(endcodef)

(char-not-greaterp@pragma(defn)@index(char-not-greaterp) @i<chr1> @i<chr2>...)
@pragma(endcodef)

(char-equalp@pragma(defn)@index(char-equalp) @i<chr1> @i<chr2>...)
@pragma(endcodef)

(char-not-equalp@pragma(defn)@index(char-not-equalp) @i<chr1> @i<chr2>...)
@pragma(endcodef)

(char-not-lessp@pragma(defn)@index(char-not-lessp) @i<chr1> @i<chr2>...)
@pragma(endcodef)

(char-greaterp@pragma(defn)@index(char-greaterp) @i<chr1> @i<chr2>...)
@end(fgroup)
@begin(pdescription)
@i<chr1> @itemsep     the first string to compare

@i<chr2> @itemsep     the second string(s) to compare

returns  @itemsep   @code(t) if predicate is true, @code(nil) otherwise

          Note: case is not significant with these comparison functions.
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Input/Output Functions)@index(Input/Output Functions)
@begin(fdescription)
        (read@pragma(defn)@index(read) [@i<stream> [@i<eof> [@i<rflag>]]])  @itemsep read an expression
@begin(pdescription)
            @i<stream> @itemsep   the input stream (default is standard input)

            @i<eof>    @itemsep   the value to return on end of file (default is @code(nil))

            @i<rflag>  @itemsep   recursive read flag (default is @code(nil))

            returns    @itemsep the expression read

@end(pdescription)
@blankspace(1)

        (print@pragma(defn)@index(print) @i<expr> [@i<stream>])  @itemsep print an expression on a new line
@begin(pdescription)
            @i<expr>   @itemsep   the expression to be printed

            @i<stream> @itemsep   the output stream (default is standard output)

            returns    @itemsep the expression

@end(pdescription)
@blankspace(1)

        (prin1@pragma(defn)@index(prin1) @i<expr> [@i<stream>])  @itemsep print an expression
@begin(pdescription)
            @i<expr>   @itemsep   the expression to be printed

            @i<stream> @itemsep   the output stream (default is standard output)

            returns    @itemsep the expression

@end(pdescription)
@blankspace(1)

        (princ@pragma(defn)@index(princ) @i<expr> [@i<stream>])  @itemsep print an expression without quoting
@begin(pdescription)
            @i<expr>   @itemsep   the expressions to be printed

            @i<stream> @itemsep   the output stream (default is standard output)

            returns   @itemsep  the expression

@end(pdescription)
@blankspace(1)

        (pprint@pragma(defn)@index(pprint) @i<expr> [@i<stream>])  @itemsep pretty print an expression
@begin(pdescription)
            @i<expr>  @itemsep    the expressions to be printed

            @i<stream> @itemsep   the output stream (default is standard output)

            returns @itemsep    the expression

@end(pdescription)
@blankspace(1)

        (terpri@pragma(defn)@index(terpri) [@i<stream>])  @itemsep terminate the current print line
@begin(pdescription)
            @i<stream> @itemsep   the output stream (default is standard output)

            returns   @itemsep  @code(nil)

@end(pdescription)
@blankspace(1)

        (flatsize@pragma(defn)@index(flatsize) @i<expr>)  @itemsep length of printed representation using prin1
@begin(pdescription)
            @i<expr>  @itemsep    the expression

            returns  @itemsep   the length

@end(pdescription)
@blankspace(1)

        (flatc@pragma(defn)@index(flatc) @i<expr>)  @itemsep length of printed representation using princ
@begin(pdescription)
            @i<expr> @itemsep     the expression

            returns  @itemsep   the length
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(The Format Function)@index(The Format Function)
@begin(fdescription)
(format@pragma(defn)@index(format) @i<stream> @i<fmt> @i<arg>...)   @itemsep do formated
output
@begin(pdescription)
            @i<stream> @itemsep   the output stream

            @i<fmt>    @itemsep   the format string

            @i<arg>    @itemsep   the format arguments

            returns   @itemsep  output string if @i<stream> is @code(nil), @code(nil) otherwise

@end(pdescription)
@blankspace(1)
@end(fdescription)
       The format string can contain characters that should be copied
        directly to the output and formatting directives.  The
        formatting directives are:
@begin(display)
~A @itemsep print next argument using princ
~S @itemsep print next argument using prin1
~% @itemsep start a new line
~~ @itemsep print a tilde character
~<newline> @itemsep ignore this one newline and white space on the 
next line up to the first non-white-space character or newline. This 
allows strings to continue across multiple lines
@end(display)

@section(File I/O Functions)@index(File I/O Functions)
Note that files are ordinarily opened as text. Binary files (such as standard midi files) must be opened with @code(open-binary) on non-unix systems.
@begin(fdescription)
        (open@pragma(defn)@index(open) @i<fname> &key :direction) @itemsep  open a file stream
@begin(pdescription)
            @i<fname> @itemsep    the file name string or symbol

            :direction @itemsep :input or :output (default is :input)

            returns   @itemsep  a stream

@end(pdescription)
@blankspace(1)
        (open-binary@pragma(defn)@index(open-binary)@index(open)@index(binary files) @i<fname> &key :direction) @itemsep  open a binary file stream
@begin(pdescription)
            @i<fname> @itemsep    the file name string or symbol

            :direction @itemsep :input or :output (default is :input)

            returns   @itemsep  a stream

@end(pdescription)
@blankspace(1)

        (close@pragma(defn)@index(close) @i<stream>)  @itemsep close a file stream
@begin(pdescription)
            @i<stream> @itemsep   the stream

            returns    @itemsep @code(nil)

@end(pdescription)
@blankspace(1)

        (setdir@pragma(defn)@index(setdir)@index(change directory) @i<path>)@foot(This is not a standard XLISP 2.0 function.) @itemsep set current directory
@begin(pdescription)
            @i<path> @itemsep   the path of the new directory

            returns   @itemsep  the resulting full path, e.g. (setdir ".") gets the current working directory, or @code(nil) if an error occurs

@end(pdescription)
@blankspace(1)

        (listdir@pragma(defn)@index(listdir)@index(directory listing)@index(scan directory)@index(read directory)@index(list directory) @i<path>)@foot(This is not a standard XLISP 2.0 function.) @itemsep get a directory listing
@begin(pdescription)
            @i<path> @itemsep   the path of the directory to be listed

            returns   @itemsep  list of filenames in the directory

@end(pdescription)
@blankspace(1)

        (get-temp-path@pragma(defn)@index(get-temp-path)@index(temporary files)@index(temp file))@foot(This is not a standard XLISP 2.0 function.) @itemsep get a path where a temporary file can be created. Under Windows, this is based on environment variables. If XLISP is running as a sub-process to Java, the environment may not exist, in which case the default result is the unfortunate choice @code(c:\windows\).
@begin(pdescription)
            returns   @itemsep  the resulting full path as a string

@end(pdescription)
@blankspace(1)

        (get-user@pragma(defn)@index(get-user)@index(user name)@index(temp file))@foot(This is not a standard XLISP 2.0 function.) @itemsep get the user ID. In Unix systems (including OS X and Linux), this is the value of the USER environment variable. In Windows, this is currently just ``nyquist'', which is also returned if the environment variable cannot be accessed. This function is used to avoid the case of two users creating files of the same name in the same temp directory.
@begin(pdescription)
            returns   @itemsep the string naming the user

@end(pdescription)
@blankspace(1)
        (find-in-xlisp-path@pragma(defn)@index(find-in-xlisp-path) @i<filename>)@foot(This is not a standard XLISP 2.0 function.) @itemsep search the XLISP
search path (e.g. @code(XLISPPATH) from the environment) for @i(filename). If
@i(filename) is not found as is, and there is no file extension, 
append "@code(.lsp)" to @i(filename) and search again. The current directory
is not searched.
@begin(pdescription)
            @i<filename> @itemsep the name of the file to search for

            returns @itemsep a full path name to the first occurrence found

@end(pdescription)
@blankspace(1)

        (read-char@pragma(defn)@index(read-char)@index(get char) [@i<stream>])  @itemsep read a character from a stream
@begin(pdescription)
            @i<stream> @itemsep   the input stream (default is standard input)

            returns   @itemsep  the character

@end(pdescription)
@blankspace(1)

        (peek-char@pragma(defn)@index(peek-char) [@i<flag> [@i<stream>]])  @itemsep peek at the next character
@begin(pdescription)
            @i<flag>  @itemsep    flag for skipping white space (default is @code(nil))

            @i<stream> @itemsep    the input stream (default is standard input)

            returns   @itemsep  the character (integer)

@end(pdescription)
@blankspace(1)

        (write-char@pragma(defn)@index(write-char) @i<ch> [@i<stream>])  @itemsep write a character to a stream
@begin(pdescription)
            @i<ch>    @itemsep    the character to write

            @i<stream> @itemsep   the output stream (default is standard output)

            returns   @itemsep  the character

@end(pdescription)
@blankspace(1)

        (read-int@pragma(defn)@index(read-int) [@i<stream> [@i<length>]])  @itemsep read a binary integer from a stream
@begin(pdescription)
            @i<stream> @itemsep   the input stream (default is standard input)

            @i<length> @itemsep the length of the integer in bytes (default is 4)

            returns   @itemsep  the integer

Note: Integers are assumed to be big-endian (high-order byte first) and 
signed, regardless of the platform. To read little-endian format, use a
negative number for the length, e.g. -4 indicates a 4-bytes, low-order
byte first. The file should be opened in binary mode.

@end(pdescription)
@blankspace(1)

        (write-int@pragma(defn)@index(write-int) @i<ch> [@i<stream> [@i<length>]])  @itemsep write a binary integer to a stream
@begin(pdescription)
            @i<ch>    @itemsep    the character to write

            @i<stream> @itemsep   the output stream (default is standard output)

            @i<length> @itemsep the length of the integer in bytes (default is 4)

            returns   @itemsep  the integer

Note: Integers are assumed to be big-endian (high-order byte first) and 
signed, regardless of the platform. To write in little-endian format, use a
negative number for the length, e.g. -4 indicates a 4-bytes, low-order
byte first. The file should be opened in binary mode.

@end(pdescription)
@blankspace(1)

        (read-float@pragma(defn)@index(read-float) [@i<stream> [@i<length>]])  @itemsep read a binary floating-point number from a stream
@begin(pdescription)
            @i<stream> @itemsep   the input stream (default is standard input)

            @i<length> @itemsep the length of the float in bytes (default is 4, legal values are -4, -8, 4, and 8)

            returns   @itemsep  the integer

Note: Floats are assumed to be big-endian (high-order byte first) and 
signed, regardless of the platform. To read little-endian format, use a
negative number for the length, e.g. -4 indicates a 4-bytes, low-order
byte first. The file should be opened in binary mode.

@end(pdescription)
@blankspace(1)

        (write-float@pragma(defn)@index(write-float) @i<ch> [@i<stream> [@i<length>]])  @itemsep write a binary floating-point number to a stream
@begin(pdescription)
            @i<ch>    @itemsep    the character to write

            @i<stream> @itemsep   the output stream (default is standard output)

            @i<length> @itemsep the length of the float in bytes (default is 4, legal values are -4, -8, 4, and 8)

            returns   @itemsep  the integer

Note: Floats are assumed to be big-endian (high-order byte first) and 
signed, regardless of the platform. To write in little-endian format, use a
negative number for the length, e.g. -4 indicates a 4-bytes, low-order
byte first. The file should be opened in binary mode.

@end(pdescription)
@blankspace(1)

        (read-line@pragma(defn)@index(read-line) [@i<stream>])  @itemsep read a line from a stream
@begin(pdescription)
            @i<stream> @itemsep   the input stream (default is standard input)

            returns   @itemsep  the string

@end(pdescription)
@blankspace(1)

        (read-byte@pragma(defn)@index(read-byte) [@i<stream>])  @itemsep read a byte from a stream
@begin(pdescription)
            @i<stream> @itemsep   the input stream (default is standard input)

            returns    @itemsep the byte (integer)

@end(pdescription)
@blankspace(1)

        (write-byte@pragma(defn)@index(write-byte) @i<byte> [@i<stream>])  @itemsep write a byte to a stream
@begin(pdescription)
            @i<byte>   @itemsep   the byte to write (integer)

            @i<stream> @itemsep   the output stream (default is standard output)

            returns    @itemsep the byte (integer)

@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(String Stream Functions)@index(String Stream Functions)
        These functions operate on unnamed streams.  An unnamed output
        stream collects characters sent to it when it is used as the
        destination of any output function.  The functions 
@code(get-output-stream-string) and @code(get-output-stream-list) return a string or a list of characters.

An unnamed input stream is setup with the 
 @code(make-string-input-stream) function and returns each character of the string when
        it is used as the source of any input function.

@begin(fdescription)
@blankspace(1)
        (make-string-input-stream@pragma(defn)@index(make-string-input-stream) @i<str> [@i<start> [@i<end>]])
@begin(pdescription)
            @i<str>    @itemsep   the string

            @i<start>  @itemsep   the starting offset

            @i<end>    @itemsep   the ending offset + 1

            returns    @itemsep an unnamed stream that reads from the string

@end(pdescription)
@blankspace(1)

        (make-string-output-stream)@pragma(defn)@index(make-string-output-stream)
@begin(pdescription)
            returns   @itemsep  an unnamed output stream

@end(pdescription)
@blankspace(1)

        (get-output-stream-string@pragma(defn)@index(get-output-stream-string) @i<stream>)
@begin(pdescription)
            @i<stream> @itemsep    the output stream

            returns    @itemsep the output so far as a string

          Note:  the output stream is emptied by this function
@end(pdescription)
@blankspace(1)

        (get-output-stream-list@pragma(defn)@index(get-output-stream-list) @i<stream>)
@begin(pdescription)
            @i<stream> @itemsep   the output stream

            returns   @itemsep  the output so far as a list

          Note:  the output stream is emptied by this function
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(System Functions)@index(System Functions)
Note: the @code(load) function first tries to load a file from the current directory. A @code(.lsp) extension is added if there is not already an alphanumeric extension following  a period.  If that fails, XLISP searches the path, which is obtained from the XLISPPATH environment variable in Unix and  HKEY_LOCAL_MACHINE\SOFTWARE\CMU\Nyquist\XLISPPATH under Win32. (The Macintosh version has no search path.)

@begin(fdescription)
        (get-env@pragma(defn)@index(get-env)@index(getenv)@index(environment variables) @i<name>) @itemsep get from an environment variable
@begin(pdescription)
           @i<name> @itemsep the name of the environment variable

           returns  @itemsep string value of the environment variable, @code(nil) if variable does not exist

@end(pdescription)
@blankspace(1)

        (load@pragma(defn)@index(load) @i<fname> &key :verbose :print)   @itemsep load a source file
@begin(pdescription)
            @i<fname>   @itemsep  the filename string or symbol

            :verbose  @itemsep  the verbose flag (default is t)

            :print    @itemsep  the print flag (default is @code(nil))

            returns   @itemsep  the filename

@end(pdescription)
@blankspace(1)

        (save@pragma(defn)@index(save) @i<fname>) @itemsep save workspace to a file
@begin(pdescription)
            @i<fname> @itemsep    the filename string or symbol

            returns   @itemsep @code(t) if workspace was written, @code(nil) otherwise

@end(pdescription)
@blankspace(1)

        (restore@pragma(defn)@index(restore) @i<fname>)  @itemsep restore workspace from a file
@begin(pdescription)
            @i<fname> @itemsep    the filename string or symbol

            returns   @itemsep  @code(nil) on failure, otherwise never returns

@end(pdescription)
@blankspace(1)

        (dribble@pragma(defn)@index(dribble) [@i<fname>])  @itemsep create a file with a transcript of a session
@begin(pdescription)
            @i<fname> @itemsep    file name string or symbol
                        (if missing, close current transcript)

            returns   @itemsep @code(t) if the transcript is opened, @code(nil) if it is closed

@end(pdescription)
@blankspace(1)

        (gc@pragma(defn)@index(gc))  @itemsep force garbage collection
@begin(pdescription)
            returns @itemsep    @code(nil)

@end(pdescription)
@blankspace(1)

        (expand@pragma(defn)@index(expand) @i<num>)  @itemsep expand memory by adding segments
@begin(pdescription)
            @i<num> @itemsep      the number of segments to add

            returns @itemsep    the number of segments added

@end(pdescription)
@blankspace(1)

        (alloc@pragma(defn)@index(alloc) @i<num>)  @itemsep change number of nodes to allocate in each segment
@begin(pdescription)
            @i<num> @itemsep      the number of nodes to allocate

            returns @itemsep    the old number of nodes to allocate

@end(pdescription)
@blankspace(1)

        (info@pragma(defn)@index(info))  @itemsep show information about memory usage.
@begin(pdescription)
            returns @itemsep    @code(nil)

@end(pdescription)
@blankspace(1)

        (room@pragma(defn)@index(room))  @itemsep show memory allocation statistics
@begin(pdescription)
            returns @itemsep    @code(nil)

@end(pdescription)
@blankspace(1)

        (type-of@pragma(defn)@index(type-of) @i<expr>)  @itemsep returns the type of the expression
@begin(pdescription)
            @i<expr> @itemsep     the expression to return the type of

            returns  @itemsep   @code(nil) if the value is @code(nil) otherwise one of the symbols:

@begin(pdescription)
                          SYMBOL      @itemsep    for symbols

                          OBJECT      @itemsep    for objects

                          CONS        @itemsep    for conses

                          SUBR        @itemsep    for built-in functions

                          FSUBR       @itemsep    for special forms

                          CLOSURE     @itemsep    for defined functions

                          STRING      @itemsep    for strings

                          FIXNUM      @itemsep    for integers

                          FLONUM      @itemsep    for floating point numbers

                          CHARACTER   @itemsep    for characters

                          FILE-STREAM @itemsep    for file pointers

                          UNNAMED-STREAM @itemsep for unnamed streams

                          ARRAY          @itemsep for arrays

@end(pdescription)
@end(pdescription)
@blankspace(1)

        (peek@pragma(defn)@index(peek) @i<addrs>)   @itemsep peek at a location in memory
@begin(pdescription)
            @i<addrs>  @itemsep   the address to peek at (integer)

            returns    @itemsep the value at the specified address (integer)

@end(pdescription)
@blankspace(1)

        (poke@pragma(defn)@index(poke) @i<addrs> @i<value>) @itemsep  poke a value into memory
@begin(pdescription)
            @i<addrs>  @itemsep   the address to poke (integer)

            @i<value>  @itemsep   the value to poke into the address (integer)

            returns    @itemsep the value

@end(pdescription)
@blankspace(1)

        (bigendianp@pragma(defn)@index(bigendianp)@index(endian)@index(big endian)@index(little endian)) @itemsep  is this a big-endian machine?
@begin(pdescription)
            returns    @itemsep T if this a big-endian architecture, storing the high-order byte of an integer at the lowest byte address of the integer; otherwise, NIL.
@foot(This is not a standard XLISP 2.0 function.)

@end(pdescription)
@blankspace(1)

        (address-of@pragma(defn)@index(address-of) @i<expr>)  @itemsep get the address of an xlisp node
@begin(pdescription)
            @i<expr>   @itemsep   the node

            returns    @itemsep the address of the node (integer)

@end(pdescription)
@blankspace(1)

        (exit@pragma(defn)@index(exit))  @itemsep exit xlisp
@begin(pdescription)
            returns    @itemsep never returns

@end(pdescription)
@blankspace(1)

        (setup-console@pragma(defn)@index(setup-console)@index(window initialization))  @itemsep set default console attributes
@begin(pdescription)
            returns    @itemsep NIL

Note: Under Windows, Nyquist normally starts up in a medium-sized console window with black text and a white background, with a window title of ``Nyquist.'' This is normally accomplished by calling @code(setup-console) in @code(system.lsp). In Nyquist, you can avoid this behavior by setting @code(*setup-console*) to NIL in your @code(init.lsp) file. If @code(setup-console) is not called, Nyquist uses standard input and output as is. This is what you want if you are running Nyquist inside of emacs, for example.@index(emacs, using Nyquist with)

@end(pdescription)
@blankspace(1)

        (echoenabled@pragma(defn)@index(echoenabled)@index(console, XLISP) @i<flag>)  @itemsep turn console input echoing on or off
@begin(pdescription)
            @i<flag>   @itemsep T to enable echo, NIL to disable

            returns    @itemsep NIL

Note: This function is only implemented under Linux and Mac OS X. If Nyquist I/O is redirected through pipes,
the Windows version does not echo the input, but the Linux and Mac versions do. You can turn off echoing with
this function. Under windows it is defined to do nothing.

@end(pdescription)
@end(fdescription)

@section(File I/O Functions)@index(File I/O Functions)

@subsection(Input from a File)@index(Input from a File)

To open a file for input, use the @code(open) function with the keyword
argument @code(:direction) set to @code(:input).  To open a file for output,
use the @code(open) function with the keyword argument @code(:direction) set
to @code(:output).  The @code(open) function takes a single required argument which
is the name of the file to be opened.  This name can be in the form of a
string or a symbol.  The @code(open) function returns an object of type
@code(FILE-STREAM) if it succeeds in opening the specified file.  It returns the
value @code(nil) if it fails.  In order to manipulate the file, it is
necessary to save the value returned by the @code(open) function.  This is
usually done by assigning it to a variable with the @code(setq) special form or by
binding it using @code(let) or @code(let*).  Here is an example:
@begin(example)
(setq fp (open "init.lsp" :direction :input))
@end(example)
        Evaluating this expression will result in the file @code(init.lsp)
        being opened.  The file object that will be returned by the @code(open)
        function will be assigned to the variable @code(fp).

        It is now possible to use the file for input.  To read an
        expression from the file, just supply the value of the @code(fp)
        variable as the optional @i(stream) argument to @code(read).
@begin(example)
(read fp)
@end(example)
        Evaluating this expression will result in reading the first
        expression from the file @code(init.lsp).  The expression will be
        returned as the result of the @code(read) function.  More expressions
        can be read from the file using further calls to the @code(read)
        function.  When there are no more expressions to read, the @code(read)
        function will return @code(nil) (or whatever value was supplied as the
        second argument to @code(read)).

        Once you are done reading from the file, you should close it.
        To close the file, use the following expression:
@begin(example)
(close fp)
@end(example)
        Evaluating this expression will cause the file to be closed.

@subsection(Output to a File)@index(Output to a File)

        Writing to a file is pretty much the same as reading from one.
        You need to open the file first.  This time you should use the
        @code(open) function to indicate that you will do output to the file.
        For example:
@begin(example)
(setq fp (open "test.dat" :direction :output))
@end(example)
        Evaluating this expression will open the file @code(test.dat) for
        output.  If the file already exists, its current contents will
        be discarded.  If it doesn't already exist, it will be created.
        In any case, a @code(FILE-STREAM) object will be returned by the @code(OPEN)
        function.  This file object will be assigned to the @code(fp)
        variable.

        It is now possible to write to this file by supplying the value
        of the @code(fp) variable as the optional @i(stream) parameter in the  @code(print) function.
@begin(example)
(print "Hello there" fp)
@end(example)
        Evaluating this expression will result in the string ``Hello
        there'' being written to the file @code(test.dat).  More data can be
        written to the file using the same technique.

        Once you are done writing to the file, you should close it.
        Closing an output file is just like closing an input file.
@begin(example)
(close fp)
@end(example)
        Evaluating this expression will close the output file and make
        it permanent.

@subsection(A Slightly More Complicated File Example)

        This example shows how to open a file, read each Lisp expression
        from the file and print it.  It demonstrates the use of files
        and the use of the optional @i(stream) argument to the @code(read)
        function.
@begin(programexample)
(do* ((fp (open "test.dat" :direction :input))
      (ex (read fp) (read fp)))
     ((null ex) nil)
  (print ex))
@end(programexample)

