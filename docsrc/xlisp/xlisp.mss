@define(codef, FaceCode T, size 11)
@comment{In my original scribe conversion of the ascii xlisp documentation, I used
   times roman fonts for xlisp function names and code text in general. To be
   consistent with Nyquist documentation, I have changed the code font to xlcode
   which is defined here. If this turns out to be a problem, redefine xlcode to 
   use the regular FaceCode. -RBD}
@define(xlcode, FaceCode T, size 11)
@textform(pragma=[])
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

        If the symbol @xlcode(*breakenable*@index(*breakenable*)) is
        true, the message corresponding to the error is printed.  If
        the error is correctable, the correction message is printed.

        If the symbol @xlcode(*tracenable*@index(*tracenable*)) is true, a trace back is printed.
        The number of entries printed depends on the value of the symbol
        @xlcode(*tracelimit*@index(*tracelimit*)).  If this symbol is set to something other than a
        number, the entire trace back stack is printed.

        XLISP then enters a read/eval/print loop to allow the user to
        examine the state of the interpreter in the context of the
        error.  This loop differs from the normal top-level
        read/eval/print loop in that if the user invokes the function
        @xlcode(continue), XLISP will continue from a correctable error.  If
        the user invokes the function @xlcode(clean-up), XLISP will abort the
        break loop and return to the top level or the next lower
        numbered break loop.  When in a break loop, XLISP prefixes the
        break level to the normal prompt.

        If the symbol @xlcode(*breakenable*@index(*breakenable*)) is @xlcode(nil), XLISP looks for a
        surrounding errset function.  If one is found, XLISP examines
        the value of the print flag.  If this flag is true, the error
        message is printed.  In any case, XLISP causes the errset
        function call to return @xlcode(nil).

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
        called a @i(readtable).  The reader uses the symbol @xlcode(*readtable*@index(*readtable*)) to
        locate the current readtable.  This table controls the
        interpretation of input characters.  It is an array with 128
        entries, one for each of the ASCII character codes.  Each entry
        contains one of the following things:
@begin(itemize)
                @xlcode(NIL) @itemsep          Indicating an invalid character

                @xlcode(:CONSTITUENT) @itemsep   Indicating a symbol constituent

                @xlcode(:WHITE-SPACE)  @itemsep  Indicating a whitespace character

                @xlcode[(:TMACRO . @i(fun))] @itemsep Terminating readmacro

                @xlcode[(:NMACRO . @i(fun))] @itemsep Non-terminating readmacro

                @xlcode(:SESCAPE) @itemsep       Single escape character ('\')

                @xlcode(:MESCAPE) @itemsep       Multiple escape character ('|')
@end(itemize)

        In the case of @xlcode(:TMACRO) and @xlcode(:NMACRO), the @i(fun) component is a
        function.  This can either be a built-in readmacro function or a
        lambda expression.  The function should take two parameters.
        The first is the input stream and the second is the character
        that caused the invocation of the readmacro.  The readmacro
        function should return @xlcode(NIL) to indicate that the character should
        be treated as white space or a value consed with @xlcode(NIL) to indicate
        that the readmacro should be treated as an occurence of the
        specified value.  Of course, the readmacro code is free to read
        additional characters from the input stream.

        XLISP defines several useful read macros@index(read macros):
@begin(itemize)
                @xlcode(')@i[<expr>]         == @xlcode{(quote} @i[<expr>]@xlcode{)}

                @xlcode(#')@i[<expr>]        == @xlcode{(function} @i[<expr>]@xlcode{)}

                @xlcode{#(}@i[<expr>]...@xlcode{)}    == an array of the specified expressions

                @xlcode(#x)@i[<hdigits>]     == a hexadecimal number (0-9,A-F)

                @xlcode(#o)@i[<odigits>]     == an octal number (0-7)

                @xlcode(#b)@i[<bdigits>]     == a binary number (0-1)

                @xlcode(#\)@i[<char>] == the ASCII code of the character

                @xlcode(#|) ... @xlcode(|#)       == a comment

                @xlcode(#:)@i[<symbol>]      == an uninterned symbol

                @xlcode(`)@i[<expr>]        == @xlcode{(backquote} @i[<expr>]@xlcode{)}

                @xlcode(,)@i[<expr>]        == @xlcode{(comma} @i[<expr>]@xlcode{)}

   @xlcode(,@@)@i[<expr>] == @xlcode{(comma-at} @i[<expr>]@xlcode{)}@end(itemize)
@section(Lambda Lists)@index(Lambda Lists)

        There are several forms in XLISP that require that a ``lambda
        list'' be specified.  A lambda list is a definition of the
        arguments accepted by a function.  There are four different
        types of arguments.

        The lambda list starts with required arguments.  Required
        arguments must be specified in every call to the function.

        The required arguments are followed by the @xlcode(&optional) arguments.
        Optional arguments may be provided or omitted in a call.  An
        initialization expression may be specified to provide a default
        value for an @xlcode(&optional) argument if it is omitted from a call.
        If no initialization expression is specified, an omitted
        argument is initialized to @xlcode(NIL).  It is also possible to provide
        the name of a @xlcode(supplied-p) variable that can be used to
        determine if a call provided a value for the argument or if the
        initialization expression was used.  If specified, the supplied-
        p variable will be bound to T if a value was specified in the
        call and @xlcode(NIL) if the default value was used.

        The @xlcode(&optional) arguments are followed by the @xlcode(&rest) argument.  The
        @xlcode(&rest) argument gets bound to the remainder of the argument list
        after the required and @xlcode(&optional) arguments have been removed.

        The @xlcode(&rest) argument is followed by the @xlcode(&key) arguments.  When a
        keyword argument is passed to a function, a pair of values
        appears in the argument list.  The first expression in the pair
        should evaluate to a keyword symbol (a symbol that begins with a
        ``@code(:)'').  The value of the second expression is the value of the
        keyword argument.  Like @xlcode(&optional) arguments, @xlcode(&key) arguments can
        have initialization expressions and supplied-p variables.  In
        addition, it is possible to specify the keyword to be used in a
        function call.  If no keyword is specified, the keyword obtained
        by adding a ``@code(:)'' to the beginning of the keyword argument symbol
        is used.  In other words, if the keyword argument symbol is
        @xlcode(foo), the keyword will be @xlcode(:foo).

        The @xlcode(&key) arguments are followed by the @xlcode(&aux) variables.  These
        are local variables that are bound during the evaluation of the
        function body.  It is possible to have initialization
        expressions for the @xlcode(&aux) variables.

    Here is the complete syntax for lambda lists:@latex(\needspace{16\baselineskip})
@begin(display)
                (@i<rarg>...
                 [@xlcode(&optional) [@i<oarg> | (@i<oarg> [@i<init> [@i<svar>]])]...]
                 [@xlcode(&rest) @i<rarg>]
                 [@xlcode(&key)
                   [@i<karg> | ([@i<karg> | (@i<key> @i<karg>)] [@i<init> [@i<svar>]])]...
                   @xlcode(&allow)-other-keys]
                 [@xlcode(&aux)
                   [@i<aux> | (@i<aux> [@i<init>])]...])

            where:

                @i<rarg> is a required argument symbol
                @i<oarg> is an @xlcode(&optional) argument symbol
                @i<rarg> is the @xlcode(&rest) argument symbol
                @i<karg> is a @xlcode(&key) argument symbol
                @i<key> is a keyword symbol
                @i<aux> is an auxiliary variable symbol
                @i<init> is an initialization expression
                @i<svar> is a supplied-p variable symbol @end(display)
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

        You can send a message to an object using the @xlcode(send) function.
        This function takes the object as its first argument, the
        message selector as its second argument (which must be a symbol)
        and the message arguments as its remaining arguments.

        The @xlcode(send) function determines the class of the receiving object
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
        to the symbol @xlcode(self) and evaluates the method using the
        remaining elements of the original list as arguments to the
        method.  These arguments are always evaluated prior to being
        bound to their corresponding formal arguments.  The result of
        evaluating the method becomes the result of the expression.

        Within the body of a method, a message can be sent to the current
        object by calling the @xlcode[(send self ...)]. The method lookup
        starts with the object's class regardless of the class containing
        the current method.

        Sometimes it is desirable to invoke a general method in a superclass
        even when it is overridden by a more specific method in a subclass.
        This can be accomplished by calling @xlcode(send-super), which begins
        the method lookup in the superclass of the class defining the current
        method rather than in the class of the current object.

        The @xlcode(send-super) function takes a selector as its first argument
        (which must be a symbol) and the message arguments as its remaining
        arguments. Notice that @xlcode(send-super) can only be sent from within
        a method, and the target of the message is always the current object
        (@xlcode(self)). @xlcode[(send-super ...)] is similar to 
        @xlcode[(send self ...)] except that method lookup begins in the 
        superclass of the class containing the current method
        rather than the class of the current object.

@section(The ``Object'' Class)@index(Object Class)

@xlcode(Object)@index(Object) @itemsep the top of the class hierarchy.

Messages:
@begin(fdescription)
@xlcode(:show@index(:show)) @itemsep show an object's instance variables.
@begin(pdescription)
returns @itemsep  the object
@end(pdescription)
@blankspace(1)

@xlcode{:class@index(:class)} @itemsep return the class of an object
@begin(pdescription)
returns @itemsep  the class of the object
@end(pdescription)
@blankspace(1)

@xlcode{:isa@index(:isa)} @i(class) @itemsep test if object inherits from class
@begin(pdescription)
returns @itemsep @xlcode(t) if object is an instance of @i(class) or a subclass of @i(class), otherwise @xlcode(nil)
@end(pdescription)
@blankspace(1)

@xlcode(:isnew@index(:isnew)) @itemsep the default object initialization routine
@begin(pdescription)
returns @itemsep  the object
@end(pdescription)
@end(fdescription)

@section(The ``Class'' Class)@index(Class class)

@xlcode(Class@index(Class)) @itemsep class of all object classes (including itself)

            Messages:
@begin(fdescription)
                @xlcode(:new@index(:new)) @itemsep create a new instance of a class
@begin(pdescription)
                    returns @itemsep    the new class object
@end(pdescription)
@blankspace(1)

                @xlcode(:isnew@index(:isnew)) @i<ivars> [@i<cvars> [@i<super>]] @itemsep initialize a new class
@begin(pdescription)
                    @i<ivars> @itemsep    the list of instance variable symbols

                    @i<cvars> @itemsep    the list of class variable symbols

                    @i<super> @itemsep    the superclass (default is object)

                    returns @itemsep    the new class object
@end(pdescription)
@blankspace(1)

                @xlcode(:answer@index(:answer)) @i<msg> @i<fargs> @i<code> @itemsep add a message to a class
@begin(pdescription)
                    @i<msg> @itemsep      the message symbol

                @i<fargs> @itemsep    the formal argument list (lambda list)

                    @i<code> @itemsep     a list of executable expressions

                    returns @itemsep    the object
@end(pdescription)
@blankspace(1)
@end(fdescription)

        When a new instance of a class is created by sending the message
        @xlcode(:new) to an existing class, the message @xlcode(:isnew) followed by
        whatever parameters were passed to the @xlcode(:new) message is sent to
        the newly created object.

        When a new class is created by sending the @xlcode(:new) message to the
        object @xlcode(Class), an optional parameter may be specified
        indicating the superclass of the new class.  If this parameter
        is omitted, the new class will be a subclass of @xlcode(Object).  A
        class inherits all instance variables, class variables, and
        methods from its super-class.

@section(Profiling)@index(profiling)
The Xlisp 2.0 release has been extended with a profiling facility, which counts how many times and where @xlcode(eval) is executed.  A separate count is maintained for each named function, closure, or macro, and a count indicates an @xlcode(eval) in the immediately (lexically) enclosing named function, closure, or macro.  Thus, the count gives an indication of the amount of time spent in a function, not counting nested function calls.  The list of all functions executed is maintained on the global @xlcode(*profile*) variable.  These functions in turn have @xlcode(*profile*) properties, which maintain the counts.  The profile system merely increments counters and puts symbols on the @xlcode(*profile*) list.  It is up to the user to initialize data and gather results.  Profiling is turned on or off with the @xlcode(profile) function.  Unfortunately, methods cannot be profiled with this facility.

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
        @begin(fgroup)@xlcode{eval(@i(expr))} @c{[sal]}

        @xlcode{(eval@pragma(defn)@index(eval) @t(@i(expr)))} @c{[lisp]} @itemsep evaluate an xlisp expression
@end(fgroup)
@begin(pdescription)
            @i<expr> @itemsep     the expression to be evaluated

            returns @itemsep     the result of evaluating the expression
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{apply(@i(fun), @i(args))} @c{[sal]}

        @xlcode{(apply@pragma(defn)@index(apply) @t(@i(fun)) @t(@i(args)))} @c{[lisp]} @itemsep apply a function to a list of arguments
@end(fgroup)
@begin(pdescription)
            @i<fun> @itemsep      the function to apply (or function symbol)

            @i<args> @itemsep     the argument list

            returns @itemsep    the result of applying the function to the arguments
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{funcall(@i(fun), @i(arg)@r(...))} @c{[sal]}

        @xlcode{(funcall@pragma(defn)@index(funcall) @t(@i(fun)) @t(@i(arg))@r(...))} @c{[lisp]} @itemsep call a function with arguments
@end(fgroup)
@begin(pdescription)
            @i<fun> @itemsep      the function to call (or function symbol)

            @i<arg> @itemsep      arguments to pass to the function

            returns @itemsep    the result of calling the function with the arguments
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{quote(@i(expr))} @c{[sal]}

        @xlcode{(quote@pragma(defn)@index(quote) @t(@i(expr)))} @c{[lisp]} @itemsep  return an expression unevaluated
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to be quoted (quoted)

            returns   @itemsep  @i<expr> unevaluated
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{function(@i(expr))} @c{[sal]}

        @xlcode{(function@pragma(defn)@index(function) @t(@i(expr)))} @c{[lisp]} @itemsep  get the functional interpretation
@end(fgroup)
@begin(pdescription)
            @i<expr> @itemsep     the symbol or lambda expression (quoted)

            returns  @itemsep   the functional interpretation
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{backquote(@i(expr))} @c{[sal]}

        @xlcode{(backquote@pragma(defn)@index(backquote) @t(@i(expr)))} @c{[lisp]} @itemsep fill in a template
@end(fgroup)
@begin(pdescription)
            @i<expr> @itemsep     the template

            returns  @itemsep   a copy of the template with comma and comma-at

             expressions expanded
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{lambda(@i(args), @i(expr)@r(...))} @c{[sal]}

        @xlcode{(lambda@pragma(defn)@index(lambda) @t(@i(args)) @t(@i(expr))@r(...))} @c{[lisp]} @itemsep make a function closure
@end(fgroup)
@begin(pdescription)
            @i<args> @itemsep     formal argument list (lambda list) (quoted)

            @i<expr> @itemsep     expressions of the function body

            returns  @itemsep   the function closure
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{get-lambda-expression(@i(closure))} @c{[sal]}

        @xlcode{(get-lambda-expression@pragma(defn)@index(get-lambda-expression) @t(@i(closure)))} @c{[lisp]} @itemsep get the lambda expression
@end(fgroup)
@begin(pdescription)
            @i<closure> @itemsep  the closure

            returns  @itemsep   the original lambda expression
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{macroexpand(@i(form))} @c{[sal]}

        @xlcode{(macroexpand@pragma(defn)@index(macroexpand) @t(@i(form)))} @c{[lisp]} @itemsep recursively expand macro calls
@end(fgroup)
@begin(pdescription)
            @i<form> @itemsep     the form to expand

            returns  @itemsep   the macro expansion
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{macroexpand-1(@i(form))} @c{[sal]}

        @xlcode{(macroexpand-1@pragma(defn)@index(macroexpand-1) @t(@i(form)))} @c{[lisp]} @itemsep expand a macro call
@end(fgroup)
@begin(pdescription)
            @i<form> @itemsep     the macro call form

            returns  @itemsep   the macro expansion
@end(pdescription)
@blankspace(1)
@end(fdescription)


@section(Symbol Functions)@index(Symbol Functions)
@begin(fdescription)
        @begin(fgroup)@xlcode{set(@i(sym), @i(expr))} @c{[sal]}

        @xlcode{(set@pragma(defn)@index(set) @t(@i(sym)) @t(@i(expr)))} @c{[lisp]} @itemsep  set the value of a symbol
@end(fgroup)
@begin(pdescription)
            @i<sym>  @itemsep     the symbol being set

            @i<expr> @itemsep     the new value

            returns  @itemsep   the new value
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{setq([@i(sym), @i(expr)]@r(...))} @c{[sal]}

        @xlcode{(setq@pragma(defn)@index(setq) [@t(@i(sym)) @t(@i(expr))]@r(...))} @c{[lisp]} @itemsep  set the value of a symbol
@end(fgroup)
@begin(pdescription)
            @i<sym>  @itemsep     the symbol being set (quoted)

            @i<expr> @itemsep     the new value

            returns  @itemsep   the new value
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{psetq([@i(sym), @i(expr)]@r(...))} @c{[sal]}

        @xlcode{(psetq@pragma(defn)@index(psetq) [@t(@i(sym)) @t(@i(expr))]@r(...))} @c{[lisp]}  @itemsep parallel version of setq
@end(fgroup)
@begin(pdescription)
            @i<sym>  @itemsep     the symbol being set (quoted)

            @i<expr> @itemsep     the new value

            returns  @itemsep   the new value
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{setf([@i(place), @i(expr)]@r(...))} @c{[sal]}

        @xlcode{(setf@pragma(defn)@index(setf) [@t(@i(place)) @t(@i(expr))]@r(...))} @c{[lisp]}  @itemsep set the value of a field
@end(fgroup)
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
        @xlcode{(defun@pragma(defn)@index(defun) @t(@i(sym)) @t(@i(fargs)) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep define a function

@comment[@pragma(startcodef)
]        @xlcode{(defmacro@pragma(defn)@index(defmacro) @t(@i(sym)) @t(@i(fargs)) @t(@i(expr))@r(...))} @c{[lisp]} @itemsep  define a macro
@end(fgroup)
@begin(pdescription)
            @i<sym> @itemsep      symbol being defined (quoted)

            @i<fargs> @itemsep     formal argument list (lambda list) (quoted)

            @i<expr>  @itemsep    expressions constituting the body of the

                        function (quoted)
            returns   @itemsep  the function symbol
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{gensym([@i(tag)])} @c{[sal]}

        @xlcode{(gensym@pragma(defn)@index(gensym) [@t(@i(tag))])} @c{[lisp]}  @itemsep generate a symbol
@end(fgroup)
@begin(pdescription)
            @i<tag>   @itemsep    string or number

            returns   @itemsep  the new symbol
@end(pdescription)
@blankspace(1)

@begin(fgroup)@xlcode{intern(@i(pname))} @c{[sal]}

        @xlcode{(intern@pragma(defn)@index(intern) @t(@i(pname)))} @c{[lisp]}  @itemsep make an interned symbol
@end(fgroup)
@begin(pdescription)
            @i<pname> @itemsep    the symbol's print name string

            returns   @itemsep  the new symbol
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{make-symbol(@i(pname))} @c{[sal]}

        @xlcode{(make-symbol@pragma(defn)@index(make-symbol) @t(@i(pname)))} @c{[lisp]}  @itemsep make an uninterned symbol
@end(fgroup)
@begin(pdescription)
            @i<pname> @itemsep    the symbol's print name string

            returns   @itemsep  the new symbol
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{symbol-name(@i(sym))} @c{[sal]}

        @xlcode{(symbol-name@pragma(defn)@index(symbol-name) @t(@i(sym)))} @c{[lisp]}  @itemsep get the print name of a symbol
@end(fgroup)
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            returns   @itemsep  the symbol's print name
@end(pdescription)
@blankspace(1)

    @begin(fgroup)@xlcode{symbol-value(@i(sym))} @c{[sal]}

        @xlcode{(symbol-value@pragma(defn)@index(symbol-value) @t(@i(sym)))} @c{[lisp]}  @itemsep get the value of a symbol
@end(fgroup)
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            returns   @itemsep  the symbol's value
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{symbol-function(@i(sym))} @c{[sal]}

        @xlcode{(symbol-function@pragma(defn)@index(symbol-function) @t(@i(sym)))} @c{[lisp]}  @itemsep get the functional value of a symbol
@end(fgroup)
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            returns   @itemsep  the symbol's functional value
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{symbol-plist(@i(sym))} @c{[sal]}

        @xlcode{(symbol-plist@pragma(defn)@index(symbol-plist) @t(@i(sym)))} @c{[lisp]}  @itemsep get the property list of a symbol
@end(fgroup)
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            returns   @itemsep  the symbol's property list
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{hash(@i(sym), @i(n))} @c{[sal]}

        @xlcode{(hash@pragma(defn)@index(hash) @t(@i(sym)) @t(@i(n)))} @c{[lisp]}  @itemsep compute the hash index for a symbol
@end(fgroup)
@begin(pdescription)
            @i<sym>   @itemsep    the symbol or string

            @i<n>     @itemsep    the table size (integer)

            returns   @itemsep  the hash index (integer)
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Property List Functions)@index(Property List Functions)
@begin(fdescription)
        @begin(fgroup)@xlcode{get(@i(sym), @i(prop))} @c{[sal]}

        @xlcode{(get@pragma(defn)@index(get) @t(@i(sym)) @t(@i(prop)))} @c{[lisp]}  @itemsep get the value of a property
@end(fgroup)
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            @i<prop>  @itemsep    the property symbol

            returns   @itemsep  the property value or @xlcode(nil)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{putprop(@i(sym), @i(val), @i(prop))} @c{[sal]}

        @xlcode{(putprop@pragma(defn)@index(putprop) @t(@i(sym)) @t(@i(val)) @t(@i(prop)))} @c{[lisp]}  @itemsep put a property onto a property list
@end(fgroup)
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            @i<val>   @itemsep    the property value

            @i<prop>  @itemsep    the property symbol

            returns   @itemsep  the property value
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{remprop(@i(sym), @i(prop))} @c{[sal]}

        @xlcode{(remprop@pragma(defn)@index(remprop) @t(@i(sym)) @t(@i(prop)))} @c{[lisp]}  @itemsep remove a property
@end(fgroup)
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            @i<prop>  @itemsep    the property symbol

            returns   @itemsep  @xlcode(nil)
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Array Functions)@index(Array Functions)
@begin(fdescription)
        @begin(fgroup)@xlcode{aref(@i(array), @i(n))} @c{[sal]}

        @xlcode{(aref@pragma(defn)@index(aref) @t(@i(array)) @t(@i(n)))} @c{[lisp]}  @itemsep get the nth element of an array
@end(fgroup)
@begin(pdescription)
            @i<array> @itemsep    the array

            @i<n>     @itemsep    the array index (integer)

            returns   @itemsep  the value of the array element
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{make-array(@i(size))} @c{[sal]}

        @xlcode{(make-array@pragma(defn)@index(make-array) @t(@i(size)))} @c{[lisp]}  @itemsep make a new array
@end(fgroup)
@begin(pdescription)
            @i<size>  @itemsep    the size of the new array (integer)

            returns   @itemsep  the new array
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{vector(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(vector@pragma(defn)@index(vector) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep make an initialized vector
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the vector elements

            returns   @itemsep  the new vector
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(List Functions)@index(List Functions)
@begin(fdescription)
        @begin(fgroup)@xlcode{car(@i(expr))} @c{[sal]}

        @xlcode{(car@pragma(defn)@index(car) @t(@i(expr)))} @c{[lisp]} @itemsep  return the car of a list node
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the list node

            returns   @itemsep  the car of the list node
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{cdr(@i(expr))} @c{[sal]}

        @xlcode{(cdr@pragma(defn)@index(cdr) @t(@i(expr)))} @c{[lisp]}  @itemsep return the cdr of a list node
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the list node

            returns   @itemsep  the cdr of the list node
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{c@i(xx)r(@i(expr))} @c{[sal]}

        @xlcode{(c@i(xx)r@index(cxxr) @t(@i(expr)))} @c{[lisp]}  @itemsep all c@i(xx)r combinations
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{c@i(xxx)r(@i(expr))} @c{[sal]}

        @xlcode{(c@i(xxx)r@index(cxxxr) @t(@i(expr)))} @c{[lisp]}  @itemsep all c@i(xxx)r combinations
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{c@i(xxxx)r(@i(expr))} @c{[sal]}

        @xlcode{(c@i(xxxx)r@index(cxxxxr) @t(@i(expr)))} @c{[lisp]}  @itemsep all c@i(xxxx)r combinations
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{first(@i(expr))} @c{[sal]}

        @xlcode{(first@pragma(defn)@index(first) @t(@i(expr)))} @c{[lisp]}  @itemsep  a synonym for car
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{second(@i(expr))} @c{[sal]}

        @xlcode{(second@pragma(defn)@index(second) @t(@i(expr)))} @c{[lisp]}  @itemsep a synonym for cadr
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{third(@i(expr))} @c{[sal]}

        @xlcode{(third@pragma(defn)@index(third) @t(@i(expr)))} @c{[lisp]}  @itemsep  a synonym for caddr
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{fourth(@i(expr))} @c{[sal]}

        @xlcode{(fourth@pragma(defn)@index(fourth) @t(@i(expr)))} @c{[lisp]}  @itemsep a synonym for cadddr
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{rest(@i(expr))} @c{[sal]}

        @xlcode{(rest@pragma(defn)@index(rest) @t(@i(expr)))} @c{[lisp]}  @itemsep   a synonym for cdr
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{cons(@i(expr1), @i(expr2))} @c{[sal]}

        @xlcode{(cons@pragma(defn)@index(cons) @t(@i(expr1)) @t(@i(expr2)))} @c{[lisp]}  @itemsep construct a new list node
@end(fgroup)
@begin(pdescription)
            @i<expr1> @itemsep    the car of the new list node

            @i<expr2> @itemsep    the cdr of the new list node

            returns   @itemsep  the new list node
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{list(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(list@pragma(defn)@index(list) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep create a list of values
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    expressions to be combined into a list

            returns   @itemsep  the new list
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{append(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(append@pragma(defn)@index(append) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep append lists
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    lists whose elements are to be appended

            returns   @itemsep  the new list
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{reverse(@i(expr))} @c{[sal]}

        @xlcode{(reverse@pragma(defn)@index(reverse) @t(@i(expr)))} @c{[lisp]}  @itemsep reverse a list
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the list to reverse

            returns   @itemsep  a new list in the reverse order
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{last(@i(list))} @c{[sal]}

        @xlcode{(last@pragma(defn)@index(last) @t(@i(list)))} @c{[lisp]}  @itemsep return the last list node of a list
@end(fgroup)
@begin(pdescription)
            @i<list>  @itemsep    the list

            returns   @itemsep  the last list node in the list
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{member(@i(expr), @i(list), test: @i(test), test-not: @i(test-not))} @c{[sal]}

        @xlcode{(member@pragma(defn)@index(member) @t(@i(expr)) @t(@i(list)) @t(&key )@t(:test) @t(:test-not))} @c{[lisp]}  @itemsep find an expression in a list
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to find

            @i<list>  @itemsep    the list to search

            :test     @itemsep  the test function (defaults to eql)

            :test-not @itemsep  the test function (sense inverted)      

            returns   @itemsep  the remainder of the list starting with the expression
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{assoc(@i(expr), @i(alist), test: @i(test), test-not: @i(test-not))} @c{[sal]}

        @xlcode{(assoc@pragma(defn)@index(assoc) @t(@i(expr)) @t(@i(alist)) @t(&key )@t(:test) @t(:test-not))} @c{[lisp]}  @itemsep find an expression in an a-list
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to find

            @i<alist> @itemsep    the association list

            :test     @itemsep  the test function (defaults to eql)

            :test-not @itemsep  the test function (sense inverted)      

            returns   @itemsep  the alist entry or @xlcode(nil)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{remove(@i(expr), @i(list), test: @i(test), test-not: @i(test-not))} @c{[sal]}

        @xlcode{(remove@pragma(defn)@index(remove) @t(@i(expr)) @t(@i(list)) @t(&key )@t(:test) @t(:test-not))} @c{[lisp]}  @itemsep remove elements from a list
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the element to remove

            @i<list>  @itemsep    the list

            :test     @itemsep  the test function (defaults to eql)

            :test-not @itemsep  the test function (sense inverted)      

            returns   @itemsep  copy of list with matching expressions removed
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{remove-if(@i(test), @i(list))} @c{[sal]}

        @xlcode{(remove-if@pragma(defn)@index(remove-if) @t(@i(test)) @t(@i(list)))} @c{[lisp]}  @itemsep remove elements that pass test
@end(fgroup)
@begin(pdescription)
            @i<test>  @itemsep    the test predicate

            @i<list>  @itemsep    the list

            returns   @itemsep  copy of list with matching elements removed
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{remove-if-not(@i(test), @i(list))} @c{[sal]}

        @xlcode{(remove-if-not@pragma(defn)@index(remove-if-not) @t(@i(test)) @t(@i(list)))} @c{[lisp]}  @itemsep remove elements that fail test
@end(fgroup)
@begin(pdescription)
            @i<test>  @itemsep    the test predicate

            @i<list>  @itemsep    the list

            returns   @itemsep  copy of list with non-matching elements removed
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{length(@i(expr))} @c{[sal]}

        @xlcode{(length@pragma(defn)@index(length) @t(@i(expr)))} @c{[lisp]}  @itemsep find the length of a list, vector or string
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the list, vector or string

            returns   @itemsep  the length of the list, vector or string
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{nth(@i(n), @i(list))} @c{[sal]}

        @xlcode{(nth@pragma(defn)@index(nth) @t(@i(n)) @t(@i(list)))} @c{[lisp]}  @itemsep return the nth element of a list
@end(fgroup)
@begin(pdescription)
            @i<n>     @itemsep    the number of the element to return (zero origin)

            @i<list>  @itemsep    the list

            returns   @itemsep  the nth element or @xlcode(nil) if the list isn't that long
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{nthcdr(@i(n), @i(list))} @c{[sal]}

        @xlcode{(nthcdr@pragma(defn)@index(nthcdr) @t(@i(n)) @t(@i(list)))} @c{[lisp]}  @itemsep return the nth cdr of a list
@end(fgroup)
@begin(pdescription)
            @i<n>     @itemsep    the number of the element to return (zero origin)

            @i<list>  @itemsep    the list

            returns   @itemsep  the nth cdr or @xlcode(nil) if the list isn't that long
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{mapc(@i(fcn), @i(list1), @i(list)@r(...))} @c{[sal]}

        @xlcode{(mapc@pragma(defn)@index(mapc) @t(@i(fcn)) @t(@i(list1)) @t(@i(list))@r(...))} @c{[lisp]}  @itemsep apply function to successive cars
@end(fgroup)
@begin(pdescription)
            @i<fcn>   @itemsep    the function or function name

            @i<listn> @itemsep    a list for each argument of the function

            returns   @itemsep  the first list of arguments
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{mapcar(@i(fcn), @i(list1), @i(list)@r(...))} @c{[sal]}

        @xlcode{(mapcar@pragma(defn)@index(mapcar) @t(@i(fcn)) @t(@i(list1)) @t(@i(list))@r(...))} @c{[lisp]}  @itemsep apply function to successive cars
@end(fgroup)
@begin(pdescription)
            @i<fcn>   @itemsep    the function or function name

            @i<listn> @itemsep    a list for each argument of the function

            returns   @itemsep  a list of the values returned
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{mapl(@i(fcn), @i(list1), @i(list)@r(...))} @c{[sal]}

        @xlcode{(mapl@pragma(defn)@index(mapl) @t(@i(fcn)) @t(@i(list1)) @t(@i(list))@r(...))} @c{[lisp]}  @itemsep apply function to successive cdrs
@end(fgroup)
@begin(pdescription)
            @i<fcn>   @itemsep    the function or function name

            @i<listn> @itemsep    a list for each argument of the function

            returns   @itemsep  the first list of arguments
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{maplist(@i(fcn), @i(list1), @i(list)@r(...))} @c{[sal]}

        @xlcode{(maplist@pragma(defn)@index(maplist) @t(@i(fcn)) @t(@i(list1)) @t(@i(list))@r(...))} @c{[lisp]}  @itemsep apply function to successive cdrs
@end(fgroup)
@begin(pdescription)
            @i<fcn>   @itemsep    the function or function name

            @i<listn> @itemsep    a list for each argument of the function

            returns   @itemsep  a list of the values returned
@end(pdescription)
@blankspace(1)

       @begin(fgroup)@xlcode{subst(@i(to), @i(from), @i(expr), test: @i(test), test-not: @i(test-not))} @c{[sal]}

        @xlcode{(subst@pragma(defn)@index(subst) @t(@i(to)) @t(@i(from)) @t(@i(expr)) @t(&key )@t(:test) @t(:test-not))} @c{[lisp]}  @itemsep substitute expressions
@end(fgroup)
@begin(pdescription)
            @i<to>    @itemsep    the new expression

            @i<from>  @itemsep    the old expression

            @i<expr>  @itemsep    the expression in which to do the substitutions

            :test     @itemsep  the test function (defaults to eql)

            :test-not @itemsep  the test function (sense inverted)      

            returns   @itemsep  the expression with substitutions
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{sublis(@i(alist), @i(expr), test: @i(test), test-not: @i(test-not))} @c{[sal]}

        @xlcode{(sublis@pragma(defn)@index(sublis) @t(@i(alist)) @t(@i(expr)) @t(&key )@t(:test) @t(:test-not))} @c{[lisp]}  @itemsep substitute with an a-list
@end(fgroup)
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
        @begin(fgroup)@xlcode{rplaca(@i(list), @i(expr))} @c{[sal]}

        @xlcode{(rplaca@pragma(defn)@index(rplaca) @t(@i(list)) @t(@i(expr)))} @c{[lisp]}  @itemsep replace the car of a list node
@end(fgroup)
@begin(pdescription)
            @i<list> @itemsep     the list node

            @i<expr> @itemsep     the new value for the car of the list node

            returns  @itemsep   the list node after updating the car
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{rplacd(@i(list), @i(expr))} @c{[sal]}

        @xlcode{(rplacd@pragma(defn)@index(rplacd) @t(@i(list)) @t(@i(expr)))} @c{[lisp]}  @itemsep replace the cdr of a list node
@end(fgroup)
@begin(pdescription)
            @i<list> @itemsep     the list node

            @i<expr> @itemsep     the new value for the cdr of the list node

            returns  @itemsep   the list node after updating the cdr
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{nconc(@i(list)@r(...))} @c{[sal]}

        @xlcode{(nconc@pragma(defn)@index(nconc) @t(@i(list))@r(...))} @c{[lisp]}  @itemsep destructively concatenate lists
@end(fgroup)
@begin(pdescription)
            @i<list> @itemsep     lists to concatenate

            returns  @itemsep   the result of concatenating the lists
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{delete(@i(expr), test: @i(test), test-not: @i(test-not))} @c{[sal]}

        @xlcode{(delete@pragma(defn)@index(delete) @t(@i(expr)) @t(&key )@t(:test) @t(:test-not))} @c{[lisp]}  @itemsep delete elements from a list
@end(fgroup)
@begin(pdescription)
            @i<expr> @itemsep     the element to delete

            @i<list> @itemsep     the list

            :test    @itemsep   the test function (defaults to eql)

            :test-not @itemsep   the test function (sense inverted)      

            returns   @itemsep  the list with the matching expressions deleted
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{delete-if(@i(test), @i(list))} @c{[sal]}

        @xlcode{(delete-if@pragma(defn)@index(delete-if) @t(@i(test)) @t(@i(list)))} @c{[lisp]}  @itemsep delete elements that pass test
@end(fgroup)
@begin(pdescription)
            @i<test>  @itemsep    the test predicate

            @i<list>  @itemsep    the list

            returns   @itemsep  the list with matching elements deleted
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{delete-if-not(@i(test), @i(list))} @c{[sal]}

        @xlcode{(delete-if-not@pragma(defn)@index(delete-if-not) @t(@i(test)) @t(@i(list)))} @c{[lisp]}  @itemsep delete elements that fail test
@end(fgroup)
@begin(pdescription)
            @i<test>  @itemsep    the test predicate

            @i<list>  @itemsep    the list

            returns   @itemsep  the list with non-matching elements deleted
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{sort(@i(list), @i(test))} @c{[sal]}

        @xlcode{(sort@pragma(defn)@index(sort) @t(@i(list)) @t(@i(test)))} @c{[lisp]}  @itemsep sort a list
@end(fgroup)
@begin(pdescription)
            @i<list>  @itemsep    the list to sort

            @i<test>  @itemsep    the comparison function

            returns   @itemsep  the sorted list
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Predicate Functions)@index(Predicate Functions)
@begin(fdescription)
	@begin(fgroup)@xlcode{atom(@i(expr))} @c{[sal]}

        @xlcode{(atom@pragma(defn)@index(atom) @t(@i(expr)))} @c{[lisp]}  @itemsep is this an atom?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @xlcode(t) if the value is an atom, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{symbolp(@i(expr))} @c{[sal]}

        @xlcode{(symbolp@pragma(defn)@index(symbolp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this a symbol?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @xlcode(t) if the expression is a symbol, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{numberp(@i(expr))} @c{[sal]}

        @xlcode{(numberp@pragma(defn)@index(numberp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this a number?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @xlcode(t) if the expression is a number, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{null(@i(expr))} @c{[sal]}

        @xlcode{(null@pragma(defn)@index(null) @t(@i(expr)))} @c{[lisp]}  @itemsep is this an empty list?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the list to check

            returns   @itemsep @xlcode(t) if the list is empty, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{not(@i(expr))} @c{[sal]}

        @xlcode{(not@pragma(defn)@index(not) @t(@i(expr)))} @c{[lisp]}  @itemsep is this false?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            return    @itemsep @xlcode(t) if the value is @xlcode(nil), @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{listp(@i(expr))} @c{[sal]}

        @xlcode{(listp@pragma(defn)@index(listp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this a list?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @xlcode(t) if the value is a cons or @xlcode(nil), @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{endp(@i(list))} @c{[sal]}

        @xlcode{(endp@pragma(defn)@index(endp) @t(@i(list)))} @c{[lisp]}  @itemsep is this the end of a list
@end(fgroup)
@begin(pdescription)
            @i<list>  @itemsep    the list

            returns   @itemsep @xlcode(t) if the value is @xlcode(nil), @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{consp(@i(expr))} @c{[sal]}

        @xlcode{(consp@pragma(defn)@index(consp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this a non-empty list?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @xlcode(t) if the value is a cons, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{integerp(@i(expr))} @c{[sal]}

        @xlcode{(integerp@pragma(defn)@index(integerp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this an integer?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @xlcode(t) if the value is an integer, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{floatp(@i(expr))} @c{[sal]}

        @xlcode{(floatp@pragma(defn)@index(floatp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this a float?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @xlcode(t) if the value is a float, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{stringp(@i(expr))} @c{[sal]}

        @xlcode{(stringp@pragma(defn)@index(stringp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this a string?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @xlcode(t) if the value is a string, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{characterp(@i(expr))} @c{[sal]}

        @xlcode{(characterp@pragma(defn)@index(characterp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this a character?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @xlcode(t) if the value is a character, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{arrayp(@i(expr))} @c{[sal]}

        @xlcode{(arrayp@pragma(defn)@index(arrayp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this an array?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @xlcode(t) if the value is an array, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{streamp(@i(expr))} @c{[sal]}

        @xlcode{(streamp@pragma(defn)@index(streamp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this a stream?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @xlcode(t) if the value is a stream, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{objectp(@i(expr))} @c{[sal]}

        @xlcode{(objectp@pragma(defn)@index(objectp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this an object?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @xlcode(t) if the value is an object, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{filep(@i(expr))} @c{[sal]}

        @xlcode{(filep@pragma(defn)@index(filep) @t(@i(expr)))} @c{[lisp]}@foot(This is not part of standard XLISP nor is it built-in. Nyquist defines it though.)  @itemsep is this a file? 
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression to check

            returns   @itemsep @xlcode(t) if the value is an object, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{boundp(@i(sym))} @c{[sal]}

        @xlcode{(boundp@pragma(defn)@index(boundp) @t(@i(sym)))} @c{[lisp]}  @itemsep is a value bound to this symbol?
@end(fgroup)
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            returns   @itemsep @xlcode(t) if a value is bound to the symbol, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{fboundp(@i(sym))} @c{[sal]}

        @xlcode{(fboundp@pragma(defn)@index(fboundp) @t(@i(sym)))} @c{[lisp]}  @itemsep is a functional value bound to this symbol?
@end(fgroup)
@begin(pdescription)
            @i<sym>   @itemsep    the symbol

            returns   @itemsep @xlcode(t) if a functional value is bound to the symbol,

                        @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{minusp(@i(expr))} @c{[sal]}

        @xlcode{(minusp@pragma(defn)@index(minusp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this number negative?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the number to test

            returns   @itemsep @xlcode(t) if the number is negative, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{zerop(@i(expr))} @c{[sal]}

        @xlcode{(zerop@pragma(defn)@index(zerop) @t(@i(expr)))} @c{[lisp]}  @itemsep is this number zero?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the number to test

            returns   @itemsep @xlcode(t) if the number is zero, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{plusp(@i(expr))} @c{[sal]}

        @xlcode{(plusp@pragma(defn)@index(plusp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this number positive?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the number to test

            returns   @itemsep @xlcode(t) if the number is positive, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{evenp(@i(expr))} @c{[sal]}

        @xlcode{(evenp@pragma(defn)@index(evenp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this integer even?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the integer to test

            returns   @itemsep @xlcode(t) if the integer is even, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{oddp(@i(expr))} @c{[sal]}

        @xlcode{(oddp@pragma(defn)@index(oddp) @t(@i(expr)))} @c{[lisp]}  @itemsep is this integer odd?
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the integer to test

            returns   @itemsep @xlcode(t) if the integer is odd, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{eq(@i(expr1), @i(expr2))} @c{[sal]}

        @xlcode{(eq@pragma(defn)@index(eq) @t(@i(expr1)) @t(@i(expr2)))} @c{[lisp]}  @itemsep are the expressions identical?
@end(fgroup)
@begin(pdescription)
            @i<expr1> @itemsep    the first expression

            @i<expr2> @itemsep    the second expression

            returns   @itemsep @xlcode(t) if they are equal, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

@begin(fgroup)@xlcode{eql(@i(expr1), @i(expr2))} @c{[sal]}

        @xlcode{(eql@pragma(defn)@index(eql) @t(@i(expr1)) @t(@i(expr2)))} @c{[lisp]}  @itemsep are the expressions identical? (@xlcode(eql) ``works'' with all numbers, but a FIXNUM is never eql to a FLONUM, i.e. @xlcode{(eql 1 1.0)} is false.)
@end(fgroup)
@begin(pdescription)
            @i<expr1> @itemsep    the first expression

            @i<expr2> @itemsep    the second expression

            returns   @itemsep @xlcode(t) if they are equal, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{equal(@i(expr1), @i(expr2))} @c{[sal]}

        @xlcode{(equal@pragma(defn)@index(equal) @t(@i(expr1)) @t(@i(expr2)))} @c{[lisp]}  @itemsep are the expressions equal?
@end(fgroup)
@begin(pdescription)
            @i<expr1> @itemsep    the first expression

            @i<expr2> @itemsep    the second expression

            returns   @itemsep @xlcode(t) if they are equal, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Control Constructs)@index(Control Constructs)
@begin(fdescription)
        @xlcode{(cond@pragma(defn)@index(cond) @t(@i(pair))@r(...))} @c{[lisp]}  @itemsep evaluate conditionally
@begin(pdescription)
            @i<pair>  @itemsep    pair consisting of:
@begin(pdescription)
                            (@i<pred> @i<expr>...)
@end(pdescription)@pragma(stopcodef)

                          where:
@begin(pdescription)
                            @i<pred> @itemsep     is a predicate expression

                            @i<expr> @itemsep     evaluated if the predicate
 is not @xlcode(nil)
@end(pdescription)@pragma(stopcodef)

returns  @itemsep   the value of the first expression whose predicate is not 
@xlcode(nil)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{and(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(and@pragma(defn)@index(and) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep the logical and of a list of expressions
@end(fgroup)
@begin(pdescription)
            @i<expr> @itemsep     the expressions to be anded

            returns  @itemsep   @xlcode(nil) if any expression evaluates to @xlcode(nil),
                        otherwise the value of the last expression
                        (evaluation of expressions stops after the first
                         expression that evaluates to @xlcode(nil))
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{or(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(or@pragma(defn)@index(or) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep the logical or of a list of expressions
@end(fgroup)
@begin(pdescription)
            @i<expr> @itemsep     the expressions to be ored

            returns  @itemsep   @xlcode(nil) if all expressions evaluate to @xlcode(nil),
                  otherwise the value of the first non-@xlcode(nil) expression
                        (evaluation of expressions stops after the first
                         expression that does not evaluate to @xlcode(nil))
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{if(@i(texpr), @i(expr1) [, @i(expr2)])} @c{[sal]}

        @xlcode{(if@pragma(defn)@index(if) @t(@i(texpr)) @t(@i(expr1)) [@t(@i(expr2))])} @c{[lisp]}  @itemsep evaluate expressions conditionally
@end(fgroup)
@begin(pdescription)
            @i<texpr> @itemsep    the test expression

            @i<expr1> @itemsep    the expression to be evaluated if texpr is non-@xlcode(nil)

            @i<expr2> @itemsep    the expression to be evaluated if texpr is @xlcode(nil)

            returns   @itemsep  the value of the selected expression
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{when(@i(texpr), @i(expr)@r(...))} @c{[sal]}

        @xlcode{(when@pragma(defn)@index(when) @t(@i(texpr)) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep evaluate only when a condition is true
@end(fgroup)
@begin(pdescription)
            @i<texpr> @itemsep    the test expression

            @i<expr>  @itemsep    the expression(s) to be evaluated if texpr is non-@xlcode(nil)

            returns @itemsep the value of the last expression or @xlcode(nil)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{unless(@i(texpr), @i(expr)@r(...))} @c{[sal]}

        @xlcode{(unless@pragma(defn)@index(unless) @t(@i(texpr)) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep evaluate only when a condition is false
@end(fgroup)
@begin(pdescription)
            @i<texpr> @itemsep    the test expression

            @i<expr>  @itemsep    the expression(s) to be evaluated if texpr is @xlcode(nil)

            returns   @itemsep  the value of the last expression or @xlcode(nil)
@end(pdescription)
@blankspace(1)

          @xlcode{(case@pragma(defn)@index(case) @t(@i(expr)) @t(@i(case))@r(...))} @c{[lisp]}  @itemsep select by case
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
       @xlcode{(let@pragma(defn)@index(let) (@t(@i(binding))@r(...)) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep create local bindings

@pragma(startcodef)
        @xlcode{(let*@pragma(defn)@index(let*) (@t(@i(binding))@r(...)) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep let with sequential binding
@end(fgroup)
@begin(pdescription)
            @i<binding> @itemsep   the variable bindings each of which is either:
@begin(pdescription)
                        1)  a symbol (which is initialized to @xlcode(nil))

                        2)  a list whose car is a symbol and whose cadr
                                is an initialization expression
@end(pdescription)@pragma(stopcodef)

            @i<expr> @itemsep     the expressions to be evaluated

            returns  @itemsep   the value of the last expression
@end(pdescription)
@blankspace(1)
@begin(fgroup)
        @xlcode{(flet@pragma(defn)@index(flet) (@t(@i(binding))@r(...)) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep create local functions

@pragma(startcodef)
        @xlcode{(labels@pragma(defn)@index(labels) (@t(@i(binding))@r(...)) @t(@i(expr))@r(...))} @c{[lisp]} @itemsep  flet with recursive functions

@pragma(startcodef)
        @xlcode{(macrolet@pragma(defn)@index(macrolet) (@t(@i(binding))@r(...)) @t(@i(expr))@r(...))} @c{[lisp]} @itemsep  create local macros
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

        @begin(fgroup)@xlcode{catch(@i(sym), @i(expr)@r(...))} @c{[sal]}

        @xlcode{(catch@pragma(defn)@index(catch) @t(@i(sym)) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep evaluate expressions and catch throws
@end(fgroup)
@begin(pdescription)
            @i<sym>  @itemsep     the catch tag

            @i<expr> @itemsep     expressions to evaluate

            returns  @itemsep   the value of the last expression the throw expression
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{throw(@i(sym) [, @i(expr)])} @c{[sal]}

        @xlcode{(throw@pragma(defn)@index(throw) @t(@i(sym)) [@t(@i(expr))])} @c{[lisp]}  @itemsep throw to a catch
@end(fgroup)
@begin(pdescription)
            @i<sym>  @itemsep     the catch tag

            @i<expr> @itemsep     the value for the catch to return (defaults to @xlcode(nil))

            returns  @itemsep   never returns
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{unwind-protect(@i(expr), @i(cexpr)@r(...))} @c{[sal]}

        @xlcode{(unwind-protect@pragma(defn)@index(unwind-protect) @t(@i(expr)) @t(@i(cexpr))@r(...))} @c{[lisp]}  @itemsep protect evaluation of an expression
@end(fgroup)
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
        @xlcode{(loop@pragma(defn)@index(loop) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep basic looping form
@begin(pdescription)
            @i<expr> @itemsep     the body of the loop

            returns  @itemsep   never returns (must use non-local exit)
@end(pdescription)
@blankspace(1)
@begin(fgroup)
        @xlcode{(do@pragma(defn)@index(do) (@t(@i(binding))@r(...)) (@t(@i(texpr)) @t(@i(rexpr))@r(...)) @t(@i(expr))@r(...))} @c{[lisp]}
@pragma(endcodef)
        @xlcode{(do*@pragma(defn)@index(do*) (@t(@i(binding))@r(...)) (@t(@i(texpr)) @t(@i(rexpr))@r(...)) @t(@i(expr))@r(...))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
            @i<binding> @itemsep  the variable bindings each of which is either:
@begin(pdescription)
                        1)  a symbol (which is initialized to @xlcode(nil))

                        2)  a list of the form: (@i<sym> @i<init> [@i<step>])
                            where:
@begin(pdescription)
                                @i<sym> @itemsep is the symbol to bind

                                @i<init> @itemsep is the initial value of the symbol

                                @i<step> @itemsep is a step expression
@end(pdescription)@end(pdescription)@pragma(stopcodef)

            @i<texpr> @itemsep    the termination test expression

            @i<rexpr> @itemsep    result expressions (the default is @xlcode(nil))

            @i<expr>  @itemsep    the body of the loop (treated like an implicit prog)

            returns   @itemsep  the value of the last result expression
@end(pdescription)
@blankspace(1)

    @xlcode{(dolist@pragma(defn)@index(dolist) (@t(@i(sym)) @t(@i(expr)) [@t(@i(rexpr))]) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep loop through a list
@begin(pdescription)
            @i<sym>   @itemsep    the symbol to bind to each list element

            @i<expr>  @itemsep    the list expression

            @i<rexpr> @itemsep    the result expression (the default is @xlcode(nil))

            @i<expr>  @itemsep    the body of the loop (treated like an implicit prog)
@end(pdescription)
@blankspace(1)

        @xlcode{(dotimes@pragma(defn)@index(dotimes) (@t(@i(sym)) @t(@i(expr)) [@t(@i(rexpr))]) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep loop from zero to n-1
@begin(pdescription)
            @i<sym>   @itemsep    the symbol to bind to each value from 0 to n-1

            @i<expr>  @itemsep    the number of times to loop

            @i<rexpr> @itemsep    the result expression (the default is @xlcode(nil))

            @i<expr>  @itemsep    the body of the loop (treated like an implicit prog)
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(The Program Feature)@index(The Program Feature)
@begin(fdescription)
@begin(fgroup)
@xlcode{(prog@pragma(defn)@index(prog) (@t(@i(binding))@r(...)) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep the program feature

@pragma(startcodef)
@xlcode{(prog*@pragma(defn)@index(prog*) (@t(@i(binding))@r(...)) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep prog with sequential binding
@end(fgroup)
@begin(pdescription)
            @i<binding> @itemsep  the variable bindings each of which is either:
@begin(pdescription)
                        1)  a symbol (which is initialized to @xlcode(nil))

                        2)  a list whose car is a symbol and whose cadr
                                is an initialization expression
@end(pdescription)@pragma(stopcodef)

            @i<expr>  @itemsep    expressions to evaluate or tags (symbols)

            returns   @itemsep  @xlcode(nil) or the argument passed to the return function
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{block(@i(name), @i(expr)@r(...))} @c{[sal]}

        @xlcode{(block@pragma(defn)@index(block) @t(@i(name)) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep named block
@end(fgroup)
@begin(pdescription)
            @i<name>  @itemsep    the block name (symbol)

            @i<expr>  @itemsep    the block body

            returns   @itemsep  the value of the last expression
@end(pdescription)
@blankspace(1)

        @xlcode{(return@pragma(defn)@index(return) [@t(@i(expr))])} @c{[lisp]}  @itemsep cause a prog construct to return a value
@begin(pdescription)
            @i<expr>  @itemsep    the value (defaults to @xlcode(nil))

            returns   @itemsep  never returns
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{return-from(@i(name) [, @i(value)])} @c{[sal]}

        @xlcode{(return-from@pragma(defn)@index(return-from) @t(@i(name)) [@t(@i(value))])} @c{[lisp]}  @itemsep return from a named block
@end(fgroup)
@begin(pdescription)
            @i<name>  @itemsep    the block name (symbol)

            @i<value> @itemsep    the value to return (defaults to @xlcode(nil))

            returns   @itemsep  never returns
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{tagbody(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(tagbody@pragma(defn)@index(tagbody) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep block with labels
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    expression(s) to evaluate or tags (symbols)

            returns   @itemsep  @xlcode(nil)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{go(@i(sym))} @c{[sal]}

        @xlcode{(go@pragma(defn)@index(go) @t(@i(sym)))} @c{[lisp]}  @itemsep go to a tag within a tagbody or prog
@end(fgroup)
@begin(pdescription)
            @i<sym>   @itemsep    the tag (quoted)

            returns   @itemsep  never returns
@end(pdescription)
@blankspace(1)

        @xlcode{(progv@pragma(defn)@index(progv) @t(@i(slist)) @t(@i(vlist)) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep dynamically bind symbols
@begin(pdescription)
            @i<slist> @itemsep    list of symbols

            @i<vlist> @itemsep    list of values to bind to the symbols

            @i<expr>  @itemsep    expression(s) to evaluate

            returns   @itemsep  the value of the last expression
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{prog1(@i(expr1), @i(expr)@r(...))} @c{[sal]}

        @xlcode{(prog1@pragma(defn)@index(prog1) @t(@i(expr1)) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep execute expressions sequentially
@end(fgroup)
@begin(pdescription)
            @i<expr1> @itemsep    the first expression to evaluate

            @i<expr>  @itemsep    the remaining expressions to evaluate

            returns   @itemsep  the value of the first expression
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{prog2(@i(expr1), @i(expr2), @i(expr)@r(...))} @c{[sal]}

        @xlcode{(prog2@pragma(defn)@index(prog2) @t(@i(expr1)) @t(@i(expr2)) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep execute expressions sequentially
@end(fgroup)
@begin(pdescription)
            @i<expr1> @itemsep    the first expression to evaluate

            @i<expr2> @itemsep    the second expression to evaluate

            @i<expr>  @itemsep    the remaining expressions to evaluate

            returns   @itemsep  the value of the second expression
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{progn(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(progn@pragma(defn)@index(progn) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep execute expressions sequentially
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expressions to evaluate

            returns   @itemsep  the value of the last expression (or @xlcode(nil))
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Debugging and Error Handling)@index(Debugging)@index(Error Handling)
@begin(fdescription)
        @begin(fgroup)@xlcode{trace(@i(sym))} @c{[sal]}

        @xlcode{(trace@pragma(defn)@index(trace) @t(@i(sym)))} @c{[lisp]}  @itemsep add a function to the trace list
@end(fgroup)
@begin(pdescription)
            @i<sym>   @itemsep    the function to add (quoted)

            returns   @itemsep  the trace list
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{untrace(@i(sym))} @c{[sal]}

        @xlcode{(untrace@pragma(defn)@index(untrace) @t(@i(sym)))} @c{[lisp]}  @itemsep remove a function from the trace list
@end(fgroup)
@begin(pdescription)
            @i<sym>   @itemsep    the function to remove (quoted)

            returns   @itemsep  the trace list
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{error(@i(emsg) [, @i(arg)])} @c{[sal]}

        @xlcode{(error@pragma(defn)@index(error) @t(@i(emsg)) [@t(@i(arg))])} @c{[lisp]}  @itemsep signal a non-correctable error
@end(fgroup)
@begin(pdescription)
            @i<emsg>  @itemsep    the error message string

            @i<arg>   @itemsep    the argument expression (printed after the message)

            returns   @itemsep  never returns
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{cerror(@i(cmsg), @i(emsg) [, @i(arg)])} @c{[sal]}

        @xlcode{(cerror@pragma(defn)@index(cerror) @t(@i(cmsg)) @t(@i(emsg)) [@t(@i(arg))])} @c{[lisp]}  @itemsep signal a correctable error
@end(fgroup)
@begin(pdescription)
            @i<cmsg>  @itemsep    the continue message string

            @i<emsg>  @itemsep    the error message string

            @i<arg>   @itemsep    the argument expression (printed after the message)

            returns   @itemsep  @xlcode(nil) when continued from the break loop
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{break([@i(bmsg) [, @i(arg)]])} @c{[sal]}

        @xlcode{(break@pragma(defn)@index(break) [@t(@i(bmsg)) [@t(@i(arg))]])} @c{[lisp]}  @itemsep enter a break loop
@end(fgroup)
@begin(pdescription)
            @i<bmsg>  @itemsep    the break message string (defaults to @xlcode(**break**))

            @i<arg>   @itemsep    the argument expression (printed after the message)

            returns   @itemsep  @xlcode(nil) when continued from the break loop
@end(pdescription)
@blankspace(1)

        @xlcode{(clean-up@pragma(defn)@index(clean-up))} @c{[lisp]}  @itemsep clean-up after an error
@begin(pdescription)
            returns   @itemsep  never returns
@end(pdescription)
@blankspace(1)

        @xlcode{(top-level@pragma(defn)@index(top-level))} @c{[lisp]}  @itemsep clean-up after an error and return to the top level
@begin(pdescription)
            returns   @itemsep  never returns
@end(pdescription)
@blankspace(1)

        @xlcode{(continue@pragma(defn)@index(continue))} @c{[lisp]}  @itemsep continue from a correctable error
@begin(pdescription)
            returns   @itemsep  never returns
@end(pdescription)
@blankspace(1)

        @xlcode{(errset@pragma(defn)@index(errset) @t(@i(expr)) [@t(@i(pflag))])} @c{[lisp]}  @itemsep trap errors
@begin(pdescription)
            @i<expr>  @itemsep    the expression to execute

            @i<pflag> @itemsep    flag to control printing of the error message

            returns   @itemsep  the value of the last expression consed with @xlcode(nil)

                        or @xlcode(nil) on error
@end(pdescription)
@blankspace(1)

        @xlcode{(baktrace@pragma(defn)@index(baktrace)@index(debugging)@index(stack trace) [@t(@i(n))])} @c{[lisp]}  @itemsep print n levels of trace back information
@begin(pdescription)
            @i<n>     @itemsep    the number of levels (defaults to all levels)

            returns   @itemsep  @xlcode(nil)
@end(pdescription)
@blankspace(1)

        @xlcode{(evalhook@pragma(defn)@index(evalhook) @t(@i(expr)) @t(@i(ehook)) @t(@i(ahook)) [@t(@i(env))])} @c{[lisp]}  @itemsep evaluate with hooks
@begin(pdescription)
            @i<expr>  @itemsep    the expression to evaluate

            @i<ehook> @itemsep    the value for @xlcode(*evalhook*)

            @i<ahook> @itemsep    the value for @xlcode(*applyhook*)

            @i<env>   @itemsep    the environment (default is @xlcode(nil))

            returns   @itemsep  the result of evaluating the expression
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{profile(@i(flag))} @c{[sal]}

        @xlcode{(profile@pragma(defn)@index(profile) @t(@i(flag)))} @c{[lisp]}@foot(This is not a standard XLISP 2.0 function.)  @itemsep turn profiling on or off.
@end(fgroup)
@begin(pdescription)
            @i<flag>   @itemsep    @xlcode(nil) turns profiling off, otherwise on

            returns   @itemsep  the previous state of profiling.
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Arithmetic Functions)@index(Arithmetic Functions)
@begin(fdescription)
        @begin(fgroup)@xlcode{truncate(@i(expr))} @c{[sal]}

        @xlcode{(truncate@pragma(defn)@index(truncate) @t(@i(expr)))} @c{[lisp]}  @itemsep truncates a floating point number to an integer
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the number

            returns   @itemsep  the result of truncating the number
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{float(@i(expr))} @c{[sal]}

        @xlcode{(float@pragma(defn)@index(float) @t(@i(expr)))} @c{[lisp]}  @itemsep converts an integer to a floating point number
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the number

            returns   @itemsep  the result of floating the integer
@end(pdescription)
@blankspace(1)

        @xlcode{(+@pragma(defn)@index(+) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep add a list of numbers
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the addition
@end(pdescription)
@blankspace(1)

        @xlcode{(-@pragma(defn)@index(-) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep subtract a list of numbers or negate a single number
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the subtraction
@end(pdescription)
@blankspace(1)

        @xlcode{(*@pragma(defn)@index(*) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep multiply a list of numbers
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the multiplication
@end(pdescription)
@blankspace(1)

        @xlcode{(/@pragma(defn)@index(/) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep divide a list of numbers
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the division
@end(pdescription)
@blankspace(1)

        @xlcode{(1+@pragma(defn)@index(1+) @t(@i(expr)))} @c{[lisp]}  @itemsep add one to a number
@begin(pdescription)
            @i<expr>  @itemsep    the number

            returns   @itemsep  the number plus one
@end(pdescription)
@blankspace(1)

        @xlcode{(1-@pragma(defn)@index(1-) @t(@i(expr)))} @c{[lisp]}  @itemsep subtract one from a number
@begin(pdescription)
            @i<expr>  @itemsep    the number

            returns   @itemsep  the number minus one
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{rem(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(rem@pragma(defn)@index(rem)@index(remainder)@index(modulo (rem) function) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep remainder of a list of numbers
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the remainder operation
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{min(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(min@pragma(defn)@index(min)@index(minimum) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep the smallest of a list of numbers
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expressions to be checked

            returns   @itemsep  the smallest number in the list
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{max(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(max@pragma(defn)@index(max)@index(maximum) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep the largest of a list of numbers
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expressions to be checked

            returns   @itemsep  the largest number in the list
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{abs(@i(expr))} @c{[sal]}

        @xlcode{(abs@pragma(defn)@index(abs) @t(@i(expr)))} @c{[lisp]}  @itemsep the absolute value of a number
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the number

            returns   @itemsep  the absolute value of the number
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{gcd(@i(n1), @i(n2)@r(...))} @c{[sal]}

        @xlcode{(gcd@pragma(defn)@index(gcd) @t(@i(n1)) @t(@i(n2))@r(...))} @c{[lisp]}  @itemsep compute the greatest common divisor
@end(fgroup)
@begin(pdescription)
            @i<n1>    @itemsep    the first number (integer)

            @i<n2>    @itemsep    the second number(s) (integer)

            returns   @itemsep  the greatest common divisor
@end(pdescription)
@blankspace(1)

       @begin(fgroup)@xlcode{random(@i(n))} @c{[sal]}

        @xlcode{(random@pragma(defn)@index(random) @t(@i(n)))}
        @c{[lisp]}  @itemsep compute a random number between 0 and |n|-1
        inclusive. If n is 0, return 0.
@end(fgroup)
@begin(pdescription)
            @i<n>     @itemsep    the upper bound (integer)

            returns   @itemsep  a random number
@end(pdescription)
@blankspace(1)

       @begin(fgroup)@xlcode{rrandom()} @c{[sal]}

        @xlcode{(rrandom@pragma(defn)@index(rrandom)@index(uniform random))} @c{[lisp]}  @itemsep compute a random real number between 0 and 1 inclusive
@end(fgroup)
@begin(pdescription)
            returns   @itemsep  a random floating point number
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{sin(@i(expr))} @c{[sal]}

        @xlcode{(sin@pragma(defn)@index(sin) @t(@i(expr)))} @c{[lisp]}  @itemsep compute the sine of a number
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the floating point number

            returns   @itemsep  the sine of the number
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{cos(@i(expr))} @c{[sal]}

        @xlcode{(cos@pragma(defn)@index(cos) @t(@i(expr)))} @c{[lisp]}  @itemsep compute the cosine of a number
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the floating point number

            returns   @itemsep  the cosine of the number
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{tan(@i(expr))} @c{[sal]}

        @xlcode{(tan@pragma(defn)@index(tan) @t(@i(expr)))} @c{[lisp]}  @itemsep compute the tangent of a number
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the floating point number

            returns   @itemsep  the tangent of the number
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{atan(@i(expr) [, @i(expr2)])} @c{[sal]}

        @xlcode{(atan@pragma(defn)@index(atan) @t(@i(expr)) [@t(@i(expr2))])} @c{[lisp]}@foot(This is not a standard XLISP 2.0 function.)  @itemsep compute the arctangent
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the value of @i(x)

            @i<expr2> @itemsep    the value of @i(y) (default value is 1.0)

            returns   @itemsep  the arctangent of @i(x)/@i(y)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{expt(@i(x-expr), @i(y-expr))} @c{[sal]}

        @xlcode{(expt@pragma(defn)@index(expt) @t(@i(x-expr)) @t(@i(y-expr)))} @c{[lisp]}  @itemsep compute x to the y power
@end(fgroup)
@begin(pdescription)
            @i<x-expr> @itemsep    the floating point number

            @i<y-expr> @itemsep   the floating point exponent

            returns    @itemsep x to the y power
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{exp(@i(x-expr))} @c{[sal]}

        @xlcode{(exp@pragma(defn)@index(exp) @t(@i(x-expr)))} @c{[lisp]}  @itemsep compute e to the x power
@end(fgroup)
@begin(pdescription)
            @i<x-expr> @itemsep   the floating point number

            returns   @itemsep  e to the x power
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{sqrt(@i(expr))} @c{[sal]}

        @xlcode{(sqrt@pragma(defn)@index(sqrt) @t(@i(expr)))} @c{[lisp]}  @itemsep compute the square root of a number
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the floating point number

            returns   @itemsep  the square root of the number
@end(pdescription)
@blankspace(1)
@begin(fgroup)
@xlcode{(<@pragma(defn)@index(<) @t(@i(n1)) @t(@i(n2))@r(...))} @c{[lisp]}  @itemsep test for less than

@xlcode{(<=@pragma(defn)@index(<=) @t(@i(n1)) @t(@i(n2))@r(...))} @c{[lisp]} @itemsep  test for less than or equal to

@xlcode{(=@pragma(defn)@index(=) @t(@i(n1)) @t(@i(n2))@r(...))} @c{[lisp]}  @itemsep test for equal to

@xlcode{(/=@pragma(defn)@index(/=) @t(@i(n1)) @t(@i(n2))@r(...))} @c{[lisp]} @itemsep  test for not equal to

@xlcode{(>=@pragma(defn)@index(>=) @t(@i(n1)) @t(@i(n2))@r(...))} @c{[lisp]} @itemsep   test for greater than or equal to

@xlcode{(>@pragma(defn)@index(>) @t(@i(n1)) @t(@i(n2))@r(...))} @c{[lisp]} @itemsep   test for greater than
@end(fgroup)
@begin(pdescription)
            @i<n1>    @itemsep    the first number to compare

            @i<n2>    @itemsep    the second number to compare

returns   @itemsep  @xlcode(t) if the results of comparing @i<n1> with @i<n2>,
@i<n2> with @i<n3>, etc., are all true.
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Bitwise Logical Functions)@index(Bitwise Logical Functions)
@begin(fdescription)
        @begin(fgroup)@xlcode{logand(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(logand@pragma(defn)@index(logand) @t(@i(expr))@r(...))} @c{[lisp]} @itemsep  the bitwise and of a list of numbers
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the and operation
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{logior(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(logior@pragma(defn)@index(logior) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep the bitwise inclusive or of a list of numbers
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the inclusive or operation
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{logxor(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(logxor@pragma(defn)@index(logxor) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep the bitwise exclusive or of a list of numbers
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the numbers

            returns   @itemsep  the result of the exclusive or operation
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{lognot(@i(expr))} @c{[sal]}

        @xlcode{(lognot@pragma(defn)@index(lognot) @t(@i(expr)))} @c{[lisp]}  @itemsep the bitwise not of a number
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the number

            returns   @itemsep  the bitwise inversion of number
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(String Functions)@index(String Functions)
@begin(fdescription)
        @begin(fgroup)@xlcode{string(@i(expr))} @c{[sal]}

        @xlcode{(string@pragma(defn)@index(string) @t(@i(expr)))} @c{[lisp]} @itemsep  make a string from a value
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    an integer (which is first converted into its ASCII character value), string, character, or symbol

            returns   @itemsep  the string representation of the argument
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{string-search(@i(pat), @i(str), start: @i(start), end: @i(end))} @c{[sal]}

        @xlcode{(string-search@pragma(defn)@index(string-search)@index(find string) @t(@i(pat)) @t(@i(str)) @t(&key )@t(:start) @t(:end))} @c{[lisp]}@foot(This is not a standard XLISP 2.0 function.)  @itemsep search for pattern in string
@end(fgroup)
@begin(pdescription)
            @i<pat>   @itemsep    a string to search for

            @i<str>   @itemsep    the string to be searched

            :start    @itemsep  the starting offset in str

            :end      @itemsep  the ending offset + 1

            returns   @itemsep  index of pat in str or NIL if not found
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{string-trim(@i(bag), @i(str))} @c{[sal]}

        @xlcode{(string-trim@pragma(defn)@index(string-trim) @t(@i(bag)) @t(@i(str)))} @c{[lisp]}  @itemsep trim both ends of a string
@end(fgroup)
@begin(pdescription)
            @i<bag>   @itemsep    a string containing characters to trim

            @i<str>   @itemsep    the string to trim

            returns   @itemsep  a trimed copy of the string
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{string-left-trim(@i(bag), @i(str))} @c{[sal]}

        @xlcode{(string-left-trim@pragma(defn)@index(string-left-trim) @t(@i(bag)) @t(@i(str)))} @c{[lisp]}  @itemsep trim the left end of a string
@end(fgroup)
@begin(pdescription)
            @i<bag>   @itemsep    a string containing characters to trim

            @i<str>   @itemsep    the string to trim

            returns   @itemsep  a trimed copy of the string
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{string-right-trim(@i(bag), @i(str))} @c{[sal]}

        @xlcode{(string-right-trim@pragma(defn)@index(string-right-trim) @t(@i(bag)) @t(@i(str)))} @c{[lisp]}  @itemsep trim the right end of a string
@end(fgroup)
@begin(pdescription)
            @i<bag>   @itemsep    a string containing characters to trim

            @i<str>   @itemsep    the string to trim

            returns   @itemsep  a trimed copy of the string
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{string-upcase(@i(str), start: @i(start), end: @i(end))} @c{[sal]}

        @xlcode{(string-upcase@pragma(defn)@index(string-upcase) @t(@i(str)) @t(&key )@t(:start) @t(:end))} @c{[lisp]}  @itemsep convert to uppercase
@end(fgroup)
@begin(pdescription)
            @i<str>   @itemsep    the string

            :start    @itemsep  the starting offset

            :end      @itemsep  the ending offset + 1

            returns   @itemsep  a converted copy of the string
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{string-downcase(@i(str), start: @i(start), end: @i(end))} @c{[sal]}

        @xlcode{(string-downcase@pragma(defn)@index(string-downcase) @t(@i(str)) @t(&key )@t(:start) @t(:end))} @c{[lisp]}  @itemsep convert to lowercase
@end(fgroup)
@begin(pdescription)
            @i<str>   @itemsep    the string

            :start    @itemsep  the starting offset

            :end      @itemsep  the ending offset + 1

            returns   @itemsep  a converted copy of the string
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{nstring-upcase(@i(str), start: @i(start), end: @i(end))} @c{[sal]}

        @xlcode{(nstring-upcase@pragma(defn)@index(nstring-upcase) @t(@i(str)) @t(&key )@t(:start) @t(:end))} @c{[lisp]}  @itemsep convert to uppercase
@end(fgroup)
@begin(pdescription)
            @i<str>   @itemsep    the string

            :start    @itemsep  the starting offset

            :end      @itemsep  the ending offset + 1

            returns   @itemsep  the converted string (not a copy)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{nstring-downcase(@i(str), start: @i(start), end: @i(end))} @c{[sal]}

        @xlcode{(nstring-downcase@pragma(defn)@index(nstring-downcase) @t(@i(str)) @t(&key )@t(:start) @t(:end))} @c{[lisp]}  @itemsep convert to lowercase
@end(fgroup)
@begin(pdescription)
            @i<str>   @itemsep    the string

            :start    @itemsep  the starting offset

            :end      @itemsep  the ending offset + 1

            returns  @itemsep   the converted string (not a copy)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{strcat(@i(expr)@r(...))} @c{[sal]}

        @xlcode{(strcat@pragma(defn)@index(strcat)@index(concatenate strings) @t(@i(expr))@r(...))} @c{[lisp]}  @itemsep concatenate strings
@end(fgroup)
@begin(pdescription)
            @i<expr> @itemsep     the strings to concatenate

            returns  @itemsep   the result of concatenating the strings
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{subseq(@i(string), @i(start) [, @i(end)])} @c{[sal]}

        @xlcode{(subseq@pragma(defn)@index(subseq) @t(@i(string)) @t(@i(start)) [@t(@i(end))])} @c{[lisp]}  @itemsep extract a substring
@end(fgroup)
@begin(pdescription)
            @i<string> @itemsep   the string

            @i<start>  @itemsep   the starting position (zero origin)

            @i<end>    @itemsep   the ending position + 1 (defaults to end)

            returns    @itemsep substring between @i<start> and @i<end>
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{string<(@i(str1), @i(str2), start1: @i(start1), end1: @i(end1), start2: @i(start2), end2: @i(end2))} @c{[sal]}

        @xlcode{(string<@pragma(defn)@index(string<) @t(@i(str1)) @t(@i(str2)) @t(&key )@t(:start1) @t(:end1) @t(:start2) @t(:end2))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
        @begin(fgroup)@xlcode{string<=(@i(str1), @i(str2), start1: @i(start1), end1: @i(end1), start2: @i(start2), end2: @i(end2))} @c{[sal]}

        @xlcode{(string<=@pragma(defn)@index(string<=) @t(@i(str1)) @t(@i(str2)) @t(&key )@t(:start1) @t(:end1) @t(:start2) @t(:end2))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
        @begin(fgroup)@xlcode{string=(@i(str1), @i(str2), start1: @i(start1), end1: @i(end1), start2: @i(start2), end2: @i(end2))} @c{[sal]}

        @xlcode{(string=@pragma(defn)@index(string=) @t(@i(str1)) @t(@i(str2)) @t(&key )@t(:start1) @t(:end1) @t(:start2) @t(:end2))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
        @begin(fgroup)@xlcode{string/=(@i(str1), @i(str2), start1: @i(start1), end1: @i(end1), start2: @i(start2), end2: @i(end2))} @c{[sal]}

        @xlcode{(string/=@pragma(defn)@index(string/=) @t(@i(str1)) @t(@i(str2)) @t(&key )@t(:start1) @t(:end1) @t(:start2) @t(:end2))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
        @begin(fgroup)@xlcode{string>=(@i(str1), @i(str2), start1: @i(start1), end1: @i(end1), start2: @i(start2), end2: @i(end2))} @c{[sal]}

        @xlcode{(string>=@pragma(defn)@index(string>=) @t(@i(str1)) @t(@i(str2)) @t(&key )@t(:start1) @t(:end1) @t(:start2) @t(:end2))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
        @begin(fgroup)@xlcode{string>(@i(str1), @i(str2), start1: @i(start1), end1: @i(end1), start2: @i(start2), end2: @i(end2))} @c{[sal]}

        @xlcode{(string>@pragma(defn)@index(string>) @t(@i(str1)) @t(@i(str2)) @t(&key )@t(:start1) @t(:end1) @t(:start2) @t(:end2))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
            @i<str1> @itemsep     the first string to compare

            @i<str2> @itemsep     the second string to compare

            :start1  @itemsep   first substring starting offset

            :end1    @itemsep   first substring ending offset + 1

            :start2  @itemsep   second substring starting offset

            :end2    @itemsep   second substring ending offset + 1

            returns  @itemsep   @xlcode(t) if predicate is true, @xlcode(nil) otherwise

          Note: case is significant with these comparison functions.
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{string-lessp(@i(str1), @i(str2), start1: @i(start1), end1: @i(end1), @latex(\\\hspace*{15em})start2: @i(start2), end2: @i(end2))} @c{[sal]}

        @xlcode{(string-lessp@pragma(defn)@index(string-lessp) @t(@i(str1)) @t(@i(str2)) @t(&key )@t(:start1) @t(:end1) @t(:start2) @t(:end2))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{string-not-greaterp(@i(str1), @i(str2), start1: @i(start1), end1: @i(end1), @latex(\\\hspace*{15em})start2: @i(start2), end2: @i(end2))} @c{[sal]}

        @xlcode{(string-not-greaterp@pragma(defn)@index(string-not-greaterp) @t(@i(str1)) @t(@i(str2)) @t(&key )@t(:start1) @t(:end1) @t(:start2) @t(:end2))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{string-equal(@i(str1), @i(str2), start1: @i(start1), end1: @i(end1), @latex(\\\hspace*{15em})start2: @i(start2), end2: @i(end2))} @c{[sal]}

        @xlcode{(string-equal@pragma(defn)@index(string-equal) @t(@i(str1)) @t(@i(str2)) @t(&key )@t(:start1) @t(:end1) @t(:start2) @t(:end2))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{string-not-equal(@i(str1), @i(str2), start1: @i(start1), end1: @i(end1), @latex(\\\hspace*{15em})start2: @i(start2), end2: @i(end2))} @c{[sal]}

        @xlcode{(string-not-equal@pragma(defn)@index(string-not-equal) @t(@i(str1)) @t(@i(str2)) @t(&key )@t(:start1) @t(:end1) @t(:start2) @t(:end2))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{string-not-lessp(@i(str1), @i(str2), start1: @i(start1), end1: @i(end1), @latex(\\\hspace*{15em})start2: @i(start2), end2: @i(end2))} @c{[sal]}

        @xlcode{(string-not-lessp@pragma(defn)@index(string-not-lessp) @t(@i(str1)) @t(@i(str2)) @t(&key )@t(:start1) @t(:end1) @t(:start2) @t(:end2))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{string-greaterp(@i(str1), @i(str2), start1: @i(start1), end1: @i(end1), @latex(\\\hspace*{15em})start2: @i(start2), end2: @i(end2))} @c{[sal]}

        @xlcode{(string-greaterp@pragma(defn)@index(string-greaterp) @t(@i(str1)) @t(@i(str2)) @t(&key )@t(:start1) @t(:end1) @t(:start2) @t(:end2))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
            @i<str1> @itemsep     the first string to compare

            @i<str2> @itemsep     the second string to compare

            :start1  @itemsep   first substring starting offset

            :end1    @itemsep   first substring ending offset + 1

            :start2  @itemsep   second substring starting offset

            :end2    @itemsep   second substring ending offset + 1

    returns  @itemsep   @xlcode(t) if predicate is true, @xlcode(nil) otherwise

          Note: case is not significant with these comparison functions.
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Character Functions)@index(Character Functions)
@begin(fdescription)
        @begin(fgroup)@xlcode{char(@i(string), @i(index))} @c{[sal]}

        @xlcode{(char@pragma(defn)@index(char) @t(@i(string)) @t(@i(index)))} @c{[lisp]} @itemsep  extract a character from a string
@end(fgroup)
@begin(pdescription)
            @i<string> @itemsep   the string

            @i<index>  @itemsep   the string index (zero relative)

            returns    @itemsep the ascii code of the character
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{upper-case-p(@i(chr))} @c{[sal]}

        @xlcode{(upper-case-p@pragma(defn)@index(upper-case-p) @t(@i(chr)))} @c{[lisp]}  @itemsep is this an upper case character?
@end(fgroup)
@begin(pdescription)
            @i<chr> @itemsep      the character

            returns @itemsep    @xlcode(t) if the character is upper case, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{lower-case-p(@i(chr))} @c{[sal]}

        @xlcode{(lower-case-p@pragma(defn)@index(lower-case-p) @t(@i(chr)))} @c{[lisp]}  @itemsep is this a lower case character?
@end(fgroup)
@begin(pdescription)
            @i<chr> @itemsep      the character

            returns @itemsep    @xlcode(t) if the character is lower case, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{both-case-p(@i(chr))} @c{[sal]}

        @xlcode{(both-case-p@pragma(defn)@index(both-case-p) @t(@i(chr)))} @c{[lisp]}  @itemsep is this an alphabetic (either case) character?
@end(fgroup)
@begin(pdescription)
            @i<chr> @itemsep      the character

            returns @itemsep    @xlcode(t) if the character is alphabetic, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{digit-char-p(@i(chr))} @c{[sal]}

        @xlcode{(digit-char-p@pragma(defn)@index(digit-char-p) @t(@i(chr)))} @c{[lisp]}  @itemsep is this a digit character?
@end(fgroup)
@begin(pdescription)
            @i<chr> @itemsep      the character

            returns @itemsep    the digit weight if character is a digit, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{char-code(@i(chr))} @c{[sal]}

        @xlcode{(char-code@pragma(defn)@index(char-code) @t(@i(chr)))} @c{[lisp]}  @itemsep get the ascii code of a character
@end(fgroup)
@begin(pdescription)
            @i<chr> @itemsep      the character

            returns @itemsep    the ascii character code (integer)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{code-char(@i(code))} @c{[sal]}

        @xlcode{(code-char@pragma(defn)@index(code-char) @t(@i(code)))} @c{[lisp]}  @itemsep get the character with a specified ascii code
@end(fgroup)
@begin(pdescription)
            @i<code> @itemsep     the ascii code (integer)

            returns  @itemsep   the character with that code or @xlcode(nil)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{char-upcase(@i(chr))} @c{[sal]}

        @xlcode{(char-upcase@pragma(defn)@index(char-upcase) @t(@i(chr)))} @c{[lisp]}  @itemsep convert a character to upper case
@end(fgroup)
@begin(pdescription)
            @i<chr>  @itemsep     the character

            returns  @itemsep   the upper case character
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{char-downcase(@i(chr))} @c{[sal]}

        @xlcode{(char-downcase@pragma(defn)@index(char-downcase) @t(@i(chr)))} @c{[lisp]}  @itemsep convert a character to lower case
@end(fgroup)
@begin(pdescription)
            @i<chr>  @itemsep     the character

            returns  @itemsep   the lower case character
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{digit-char(@i(n))} @c{[sal]}

        @xlcode{(digit-char@pragma(defn)@index(digit-char) @t(@i(n)))} @c{[lisp]}  @itemsep convert a digit weight to a digit
@end(fgroup)
@begin(pdescription)
            @i<n>    @itemsep     the digit weight (integer)

            returns  @itemsep   the digit character or @xlcode(nil)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{char-int(@i(chr))} @c{[sal]}

        @xlcode{(char-int@pragma(defn)@index(char-int) @t(@i(chr)))} @c{[lisp]}  @itemsep convert a character to an integer
@end(fgroup)
@begin(pdescription)
            @i<chr>  @itemsep     the character

            returns  @itemsep   the ascii character code
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{int-char(@i(int))} @c{[sal]}

        @xlcode{(int-char@pragma(defn)@index(int-char) @t(@i(int)))} @c{[lisp]}  @itemsep convert an integer to a character
@end(fgroup)
@begin(pdescription)
            @i<int>  @itemsep     the ascii character code

            returns  @itemsep   the character with that code
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{char<(@i(chr1), @i(chr2)@r(...))} @c{[sal]}

        @xlcode{(char<@pragma(defn)@index(char<) @t(@i(chr1)) @t(@i(chr2))@r(...))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{char<=(@i(chr1), @i(chr2)@r(...))} @c{[sal]}

        @xlcode{(char<=@pragma(defn)@index(char<=) @t(@i(chr1)) @t(@i(chr2))@r(...))} @c{[lisp]}
@end(fgroup)@pragma(endcodef)@begin(fgroup)@xlcode{char=(@i(chr1), @i(chr2)@r(...))} @c{[sal]}

        @xlcode{(char=@pragma(defn)@index(char=) @t(@i(chr1)) @t(@i(chr2))@r(...))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{char/=(@i(chr1), @i(chr2)@r(...))} @c{[sal]}

        @xlcode{(char/=@pragma(defn)@index(char/=) @t(@i(chr1)) @t(@i(chr2))@r(...))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{char>=(@i(chr1), @i(chr2)@r(...))} @c{[sal]}

        @xlcode{(char>=@pragma(defn)@index(char>=) @t(@i(chr1)) @t(@i(chr2))@r(...))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{char>(@i(chr1), @i(chr2)@r(...))} @c{[sal]}

        @xlcode{(char>@pragma(defn)@index(char>) @t(@i(chr1)) @t(@i(chr2))@r(...))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
            @i<chr1> @itemsep     the first character to compare

            @i<chr2> @itemsep     the second character(s) to compare

            returns  @itemsep   @xlcode(t) if predicate is true, @xlcode(nil) otherwise

          Note: case is significant with these comparison functions.
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{char-lessp(@i(chr1), @i(chr2)@r(...))} @c{[sal]}

        @xlcode{(char-lessp@pragma(defn)@index(char-lessp) @t(@i(chr1)) @t(@i(chr2))@r(...))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{char-not-greaterp(@i(chr1), @i(chr2)@r(...))} @c{[sal]}

        @xlcode{(char-not-greaterp@pragma(defn)@index(char-not-greaterp) @t(@i(chr1)) @t(@i(chr2))@r(...))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{char-equal(@i(chr1), @i(chr2)@r(...))} @c{[sal]}

        @xlcode{(char-equal@pragma(defn)@index(char-equal) @t(@i(chr1)) @t(@i(chr2))@r(...))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{char-not-equal(@i(chr1), @i(chr2)@r(...))} @c{[sal]}

        @xlcode{(char-not-equal@pragma(defn)@index(char-not-equal) @t(@i(chr1)) @t(@i(chr2))@r(...))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{char-not-lessp(@i(chr1), @i(chr2)@r(...))} @c{[sal]}

        @xlcode{(char-not-lessp@pragma(defn)@index(char-not-lessp) @t(@i(chr1)) @t(@i(chr2))@r(...))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@end(pdescription)
@blankspace(1)
@begin(fgroup)@xlcode{char-greaterp(@i(chr1), @i(chr2)@r(...))} @c{[sal]}

        @xlcode{(char-greaterp@pragma(defn)@index(char-greaterp) @t(@i(chr1)) @t(@i(chr2))@r(...))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
@i<chr1> @itemsep     the first string to compare

@i<chr2> @itemsep     the second string(s) to compare

returns  @itemsep   @xlcode(t) if predicate is true, @xlcode(nil) otherwise

          Note: case is not significant with these comparison functions.
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(Input/Output Functions)@index(Input/Output Functions)
@begin(fdescription)
        @begin(fgroup)@xlcode{read([@i(stream) [, @i(eof) [, @i(rflag)]]])} @c{[sal]}

        @xlcode{(read@pragma(defn)@index(read) [@t(@i(stream)) [@t(@i(eof)) [@t(@i(rflag))]]])} @c{[lisp]}  @itemsep read an expression
@end(fgroup)
@begin(pdescription)
            @i<stream> @itemsep   the input stream (default is standard input)

            @i<eof>    @itemsep   the value to return on end of file (default is @xlcode(nil))

            @i<rflag>  @itemsep   recursive read flag (default is @xlcode(nil))

            returns    @itemsep the expression read
@end(pdescription)
@blankspace(1)

        @xlcode{(print@pragma(defn)@index(print) @t(@i(expr)) [@t(@i(stream))])} @c{[lisp]}  @itemsep print an expression on a new line
@begin(pdescription)
            @i<expr>   @itemsep   the expression to be printed

            @i<stream> @itemsep   the output stream (default is standard output)

            returns    @itemsep the expression
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{prin1(@i(expr) [, @i(stream)])} @c{[sal]}

        @xlcode{(prin1@pragma(defn)@index(prin1) @t(@i(expr)) [@t(@i(stream))])} @c{[lisp]}  @itemsep print an expression
@end(fgroup)
@begin(pdescription)
            @i<expr>   @itemsep   the expression to be printed

            @i<stream> @itemsep   the output stream (default is standard output)

            returns    @itemsep the expression
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{princ(@i(expr) [, @i(stream)])} @c{[sal]}

        @xlcode{(princ@pragma(defn)@index(princ) @t(@i(expr)) [@t(@i(stream))])} @c{[lisp]}  @itemsep print an expression without quoting
@end(fgroup)
@begin(pdescription)
            @i<expr>   @itemsep   the expressions to be printed

            @i<stream> @itemsep   the output stream (default is standard output)

            returns   @itemsep  the expression
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{pprint(@i(expr) [, @i(stream)])} @c{[sal]}

        @xlcode{(pprint@pragma(defn)@index(pprint) @t(@i(expr)) [@t(@i(stream))])} @c{[lisp]}  @itemsep pretty print an expression
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expressions to be printed

            @i<stream> @itemsep   the output stream (default is standard output)

            returns @itemsep    the expression
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{terpri([@i(stream)])} @c{[sal]}

        @xlcode{(terpri@pragma(defn)@index(terpri) [@t(@i(stream))])} @c{[lisp]}  @itemsep terminate the current print line
@end(fgroup)
@begin(pdescription)
            @i<stream> @itemsep   the output stream (default is standard output)

            returns   @itemsep  @xlcode(nil)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{flatsize(@i(expr))} @c{[sal]}

        @xlcode{(flatsize@pragma(defn)@index(flatsize) @t(@i(expr)))} @c{[lisp]}  @itemsep length of printed representation using prin1
@end(fgroup)
@begin(pdescription)
            @i<expr>  @itemsep    the expression

            returns  @itemsep   the length
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{flatc(@i(expr))} @c{[sal]}

        @xlcode{(flatc@pragma(defn)@index(flatc) @t(@i(expr)))} @c{[lisp]}  @itemsep length of printed representation using princ
@end(fgroup)
@begin(pdescription)
            @i<expr> @itemsep     the expression

            returns  @itemsep   the length
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(The Format Function)@index(The Format Function)
@begin(fdescription)
@begin(fgroup)@xlcode{format(@i(stream), @i(fmt), @i(arg)@r(...))} @c{[sal]}

        @xlcode{(format@pragma(defn)@index(format) @t(@i(stream)) @t(@i(fmt)) @t(@i(arg))@r(...))} @c{[lisp]}   @itemsep do formated
@end(fgroup)
output
@begin(pdescription)
            @i<stream> @itemsep   the output stream

            @i<fmt>    @itemsep   the format string

            @i<arg>    @itemsep   the format arguments

            returns   @itemsep  output string if @i<stream> is @xlcode(nil), @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)
@end(fdescription)
       The format string can contain characters that should be copied
        directly to the output and formatting directives.  The
        formatting directives are:
@begin(display)
@xlcode(~A) @itemsep print next argument using princ
@xlcode(~S) @itemsep print next argument using prin1
@xlcode(~%) @itemsep start a new line
@xlcode(~~) @itemsep print a tilde character
@xlcode(~)<newline> @itemsep ignore this one newline and white space on the 
next line up to the first non-white-space character or newline. This 
allows strings to continue across multiple lines
@end(display)

@section(File I/O Functions)@index(File I/O Functions)
Note that files are ordinarily opened as text. Binary files (such as standard midi files) must be opened with @xlcode(open-binary) on non-unix systems.
@blankspace(1)@comment(probably a bug that html output does not have paragraph break)
@blankspace(1)
@begin(fdescription)
        @begin(fgroup)@xlcode{open(@i(fname), direction: @i(direction))} @c{[sal]}

        @xlcode{(open@pragma(defn)@index(open) @t(@i(fname)) @t(&key )@t(:direction))} @c{[lisp]} @itemsep  open a file stream
@end(fgroup)
@begin(pdescription)
            @i<fname> @itemsep    the file name string or symbol

            :direction @itemsep :input or :output (default is :input)

            returns   @itemsep  a stream
@end(pdescription)
@blankspace(1)
        @begin(fgroup)@xlcode{open-binary(@i(fname), direction: @i(direction))} @c{[sal]}

        @xlcode{(open-binary@pragma(defn)@index(open-binary)@index(open)@index(binary files) @t(@i(fname)) @t(&key )@t(:direction))} @c{[lisp]} @itemsep  open a binary file stream
@end(fgroup)
@begin(pdescription)
            @i<fname> @itemsep    the file name string or symbol

            :direction @itemsep :input or :output (default is :input)

            returns   @itemsep  a stream
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{close(@i(stream))} @c{[sal]}

        @xlcode{(close@pragma(defn)@index(close) @t(@i(stream)))} @c{[lisp]}  @itemsep close a file stream
@end(fgroup)
@begin(pdescription)
            @i<stream> @itemsep   the stream

            returns    @itemsep @xlcode(nil)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{setdir(@i(path) [, @i(verbose)])} @c{[sal]}

        @xlcode{(setdir@pragma(defn)@index(setdir)@index(change directory) @t(@i(path)) [@t(@i(verbose))])} @c{[lisp]}@foot(This is not a standard XLISP 2.0 function.) @itemsep set current directory
@end(fgroup)
@begin(pdescription)
            @i<path> @itemsep   the path of the new directory

            @i<verbose> @itemsep print error message if current directory cannot be changed to @i(path)

            returns   @itemsep  the resulting full path, e.g. (setdir ".") gets the current working directory, or @xlcode(nil) if an error occurs
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{listdir(@i(path))} @c{[sal]}

        @xlcode{(listdir@pragma(defn)@index(listdir)@index(directory listing)@index(scan directory)@index(read directory)@index(list directory) @t(@i(path)))} @c{[lisp]}@foot(This is not a standard XLISP 2.0 function.) @itemsep get a directory listing
@end(fgroup)
@begin(pdescription)
            @i<path> @itemsep   the path of the directory to be listed

            returns   @itemsep  list of filenames in the directory
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{get-temp-path()} @c{[sal]}

        @xlcode{(get-temp-path@pragma(defn)@index(get-temp-path)@index(temporary files)@index(temp file))} @c{[lisp]}@foot(This is not a standard XLISP 2.0 function.) @itemsep get a path where a temporary file can be created. Under Windows, this is based on environment variables. If XLISP is running as a sub-process to Java, the environment may not exist, in which case the default result is the unfortunate choice @xlcode(c:\windows\).
@end(fgroup)
@begin(pdescription)
            returns   @itemsep  the resulting full path as a string
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{get-user()} @c{[sal]}

        @xlcode{(get-user@pragma(defn)@index(get-user)@index(user name)@index(temp file))} @c{[lisp]}@foot(This is not a standard XLISP 2.0 function.) @itemsep get the user ID. In Unix systems (including OS X and Linux), this is the value of the USER environment variable. In Windows, this is currently just ``nyquist'', which is also returned if the environment variable cannot be accessed. This function is used to avoid the case of two users creating files of the same name in the same temp directory.
@end(fgroup)
@begin(pdescription)
            returns   @itemsep the string naming the user
@end(pdescription)
@blankspace(1)
        @begin(fgroup)@xlcode{find-in-xlisp-path(@i(filename))} @c{[sal]}

        @xlcode{(find-in-xlisp-path@pragma(defn)@index(find-in-xlisp-path) @t(@i(filename)))} @c{[lisp]}@foot(This is not a standard XLISP 2.0 function.) @itemsep search the XLISP search path (e.g. @xlcode(XLISPPATH) from the environment) for @i(filename). If @i(filename) is not found as is, and there is no file extension, append "@code(.lsp)" to @i(filename) and search again. The current directory is not searched.
@end(fgroup)
@begin(pdescription)
            @i<filename> @itemsep the name of the file to search for

            returns @itemsep a full path name to the first occurrence found
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{read-char([@i(stream)])} @c{[sal]}

        @xlcode{(read-char@pragma(defn)@index(read-char)@index(get char) [@t(@i(stream))])} @c{[lisp]}  @itemsep read a character from a stream
@end(fgroup)
@begin(pdescription)
            @i<stream> @itemsep   the input stream (default is standard input)

            returns   @itemsep  the character
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{peek-char([@i(flag) [, @i(stream)]])} @c{[sal]}

        @xlcode{(peek-char@pragma(defn)@index(peek-char) [@t(@i(flag)) [@t(@i(stream))]])} @c{[lisp]}  @itemsep peek at the next character
@end(fgroup)
@begin(pdescription)
            @i<flag>  @itemsep    flag for skipping white space (default is @xlcode(nil))

            @i<stream> @itemsep    the input stream (default is standard input)

            returns   @itemsep  the character (integer)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{write-char(@i(ch)  [, @i(stream)])} @c{[sal]}

        @xlcode{(write-char@pragma(defn)@index(write-char) @t(@i(ch)) [@t(@i(stream))])} @c{[lisp]}  @itemsep write a character to a stream
@end(fgroup)
@begin(pdescription)
            @i<ch>    @itemsep    the character to write

            @i<stream> @itemsep   the output stream (default is standard output)

            returns   @itemsep  the character
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{read-int([@i(stream) [, @i(length)]])} @c{[sal]}

        @xlcode{(read-int@pragma(defn)@index(read-int) [@t(@i(stream)) [@t(@i(length))]])} @c{[lisp]}  @itemsep read a binary integer from a stream
@end(fgroup)
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

        @begin(fgroup)@xlcode{write-int(@i(ch) [, @i(stream) [, @i(length)]])} @c{[sal]}

        @xlcode{(write-int@pragma(defn)@index(write-int) @t(@i(ch)) [@t(@i(stream)) [@t(@i(length))]])} @c{[lisp]}  @itemsep write a binary integer to a stream
@end(fgroup)
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

        @begin(fgroup)@xlcode{read-float([@i(stream) [, @i(length)]])} @c{[sal]}

        @xlcode{(read-float@pragma(defn)@index(read-float) [@t(@i(stream)) [@t(@i(length))]])} @c{[lisp]}  @itemsep read a binary floating-point number from a stream
@end(fgroup)
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

        @begin(fgroup)@xlcode{write-float(@i(ch) [, @i(stream) [, @i(length)]])} @c{[sal]}

        @xlcode{(write-float@pragma(defn)@index(write-float) @t(@i(ch)) [@t(@i(stream)) [@t(@i(length))]])} @c{[lisp]}  @itemsep write a binary floating-point number to a stream
@end(fgroup)
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

        @begin(fgroup)@xlcode{read-line([@i(stream)])} @c{[sal]}

        @xlcode{(read-line@pragma(defn)@index(read-line) [@t(@i(stream))])} @c{[lisp]}  @itemsep read a line from a stream
@end(fgroup)
@begin(pdescription)
            @i<stream> @itemsep   the input stream (default is standard input)

            returns   @itemsep  the string
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{read-byte([@i(stream)])} @c{[sal]}

        @xlcode{(read-byte@pragma(defn)@index(read-byte) [@t(@i(stream))])} @c{[lisp]}  @itemsep read a byte from a stream
@end(fgroup)
@begin(pdescription)
            @i<stream> @itemsep   the input stream (default is standard input)

            returns    @itemsep the byte (integer)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{write-byte(@i(byte) [, @i(stream)])} @c{[sal]}

        @xlcode{(write-byte@pragma(defn)@index(write-byte) @t(@i(byte)) [@t(@i(stream))])} @c{[lisp]}  @itemsep write a byte to a stream
@end(fgroup)
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
@xlcode(get-output-stream-string) and @xlcode(get-output-stream-list) return a string or a list of characters.

An unnamed input stream is setup with the 
 @xlcode(make-string-input-stream) function and returns each character of the string when
        it is used as the source of any input function.

@begin(fdescription)
        @begin(fgroup)@xlcode{make-string-input-stream(@i(str) [, @i(start) [, @i(end)]])} @c{[sal]}

        @xlcode{(make-string-input-stream@pragma(defn)@index(make-string-input-stream) @t(@i(str)) [@t(@i(start)) [@t(@i(end))]])} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
            @i<str>    @itemsep   the string

            @i<start>  @itemsep   the starting offset

            @i<end>    @itemsep   the ending offset + 1

            returns    @itemsep an unnamed stream that reads from the string
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{make-string-output-stream)()} @c{[sal]}

        @xlcode{(make-string-output-stream)} @c{[lisp]}@pragma(defn)@index(make-string-output-stream)
@end(fgroup)
@begin(pdescription)
            returns   @itemsep  an unnamed output stream
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{get-output-stream-string(@i(stream))} @c{[sal]}

        @xlcode{(get-output-stream-string@pragma(defn)@index(get-output-stream-string) @t(@i(stream)))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
            @i<stream> @itemsep    the output stream

            returns    @itemsep the output so far as a string

          Note:  the output stream is emptied by this function
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{get-output-stream-list(@i(stream))} @c{[sal]}

        @xlcode{(get-output-stream-list@pragma(defn)@index(get-output-stream-list) @t(@i(stream)))} @c{[lisp]}
@end(fgroup)
@begin(pdescription)
            @i<stream> @itemsep   the output stream

            returns   @itemsep  the output so far as a list

          Note:  the output stream is emptied by this function
@end(pdescription)
@blankspace(1)
@end(fdescription)

@section(System Functions)@index(System Functions)
Note: the @xlcode(load) function first tries to load a file from the current directory. A @code(.lsp) extension is added if there is not already an alphanumeric extension following  a period.  If that fails, XLISP searches the path, which is obtained from the XLISPPATH environment variable in Unix and  HKEY_LOCAL_MACHINE\SOFTWARE\CMU\Nyquist\XLISPPATH under Win32. (The Macintosh version has no search path.)
@blankspace(1)@comment(probably a bug that html output does not have paragraph break)
@blankspace(1)
@begin(fdescription)
        @begin(fgroup)@xlcode{get-env(@i(name))} @c{[sal]}

        @xlcode{(get-env@pragma(defn)@index(get-env)@index(getenv)@index(environment variables) @t(@i(name)))} @c{[lisp]} @itemsep get from an environment variable
@end(fgroup)
@begin(pdescription)
           @i<name> @itemsep the name of the environment variable

           returns  @itemsep string value of the environment variable, @xlcode(nil) if variable does not exist
@end(pdescription)
@blankspace(1)

        @xlcode{(load@pragma(defn)@index(load) @t(@i(fname)) @t(&key )@t(:verbose) @t(:print))} @c{[lisp]}   @itemsep load a source file
@begin(pdescription)
            @i<fname>   @itemsep  the filename string or symbol

            :verbose  @itemsep  the verbose flag (default is t)

            :print    @itemsep  the print flag (default is @xlcode(nil))

            returns   @itemsep  the filename
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{save(@i(fname))} @c{[sal]}

        @xlcode{(save@pragma(defn)@index(save) @t(@i(fname)))} @c{[lisp]} @itemsep save workspace to a file
@end(fgroup)
@begin(pdescription)
            @i<fname> @itemsep    the filename string or symbol

            returns   @itemsep @xlcode(t) if workspace was written, @xlcode(nil) otherwise
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{restore(@i(fname))} @c{[sal]}

        @xlcode{(restore@pragma(defn)@index(restore) @t(@i(fname)))} @c{[lisp]}  @itemsep restore workspace from a file
@end(fgroup)
@begin(pdescription)
            @i<fname> @itemsep    the filename string or symbol

            returns   @itemsep  @xlcode(nil) on failure, otherwise never returns
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{dribble([@i(fname)])} @c{[sal]}

        @xlcode{(dribble@pragma(defn)@index(dribble) [@t(@i(fname))])} @c{[lisp]}  @itemsep create a file with a transcript of a session
@end(fgroup)
@begin(pdescription)
            @i<fname> @itemsep    file name string or symbol
                        (if missing, close current transcript)

            returns   @itemsep @xlcode(t) if the transcript is opened, @xlcode(nil) if it is closed
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{gc()} @c{[sal]}

        @xlcode{(gc@pragma(defn)@index(gc))} @c{[lisp]}  @itemsep force garbage collection
@end(fgroup)
@begin(pdescription)
            returns @itemsep    @xlcode(nil)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{expand(@i(num))} @c{[sal]}

        @xlcode{(expand@pragma(defn)@index(expand) @t(@i(num)))} @c{[lisp]}  @itemsep expand memory by adding segments
@end(fgroup)
@begin(pdescription)
            @i<num> @itemsep      the number of segments to add

            returns @itemsep    the number of segments added
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{alloc(@i(num))} @c{[sal]}

        @xlcode{(alloc@pragma(defn)@index(alloc) @t(@i(num)))} @c{[lisp]}  @itemsep change number of nodes to allocate in each segment
@end(fgroup)
@begin(pdescription)
            @i<num> @itemsep      the number of nodes to allocate

            returns @itemsep    the old number of nodes to allocate
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{info()} @c{[sal]}

        @xlcode{(info@pragma(defn)@index(info))} @c{[lisp]}  @itemsep show information about memory usage.
@end(fgroup)
@begin(pdescription)
            returns @itemsep    @xlcode(nil)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{room()} @c{[sal]}

        @xlcode{(room@pragma(defn)@index(room))} @c{[lisp]}  @itemsep show memory allocation statistics
@end(fgroup)
@begin(pdescription)
            returns @itemsep    @xlcode(nil)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{type-of(@i(expr))} @c{[sal]}

        @xlcode{(type-of@pragma(defn)@index(type-of) @t(@i(expr)))} @c{[lisp]}  @itemsep returns the type of the expression
@end(fgroup)
@begin(pdescription)
            @i<expr> @itemsep     the expression to return the type of

            returns  @itemsep   @xlcode(nil) if the value is @xlcode(nil) otherwise one of the symbols:
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

        @begin(fgroup)@xlcode{peek(@i(addrs))} @c{[sal]}

        @xlcode{(peek@pragma(defn)@index(peek) @t(@i(addrs)))} @c{[lisp]}   @itemsep peek at a location in memory
@end(fgroup)
@begin(pdescription)
            @i<addrs>  @itemsep   the address to peek at (integer)

            returns    @itemsep the value at the specified address (integer)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{poke(@i(addrs), @i(value))} @c{[sal]}

        @xlcode{(poke@pragma(defn)@index(poke) @t(@i(addrs)) @t(@i(value)))} @c{[lisp]} @itemsep  poke a value into memory
@end(fgroup)
@begin(pdescription)
            @i<addrs>  @itemsep   the address to poke (integer)

            @i<value>  @itemsep   the value to poke into the address (integer)

            returns    @itemsep the value
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{bigendianp()} @c{[sal]}

        @xlcode{(bigendianp@pragma(defn)@index(bigendianp)@index(endian)@index(big endian)@index(little endian))} @c{[lisp]} @itemsep  is this a big-endian machine?
@end(fgroup)
@begin(pdescription)
            returns    @itemsep T if this a big-endian architecture, storing the high-order byte of an integer at the lowest byte address of the integer; otherwise, NIL.
@foot(This is not a standard XLISP 2.0 function.)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{address-of(@i(expr))} @c{[sal]}

        @xlcode{(address-of@pragma(defn)@index(address-of) @t(@i(expr)))} @c{[lisp]}  @itemsep get the address of an xlisp node
@end(fgroup)
@begin(pdescription)
            @i<expr>   @itemsep   the node

            returns    @itemsep the address of the node (integer)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{exit()} @c{[sal]}

        @xlcode{(exit@pragma(defn)@index(exit))} @c{[lisp]}  @itemsep
        exit xlisp. (Note: in Audacity plug-ins, @code(exit) is
        undefined because exiting would terminate Audacity.)
@end(fgroup)
@begin(pdescription)
            returns    @itemsep never returns
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{setup-console()} @c{[sal]}

        @xlcode{(setup-console@pragma(defn)@index(setup-console)@index(window initialization))} @c{[lisp]}  @itemsep set default console attributes
@end(fgroup)
@begin(pdescription)
            returns    @itemsep NIL

Note: Under Windows, Nyquist normally starts up in a medium-sized console window with black text and a white background, with a window title of ``Nyquist.'' This is normally accomplished by calling @xlcode(setup-console) in @code(system.lsp). In Nyquist, you can avoid this behavior by setting @xlcode(*setup-console*) to NIL in your @code(init.lsp) file. If @xlcode(setup-console) is not called, Nyquist uses standard input and output as is. This is what you want if you are running Nyquist inside of emacs, for example.@index(emacs, using Nyquist with)
@end(pdescription)
@blankspace(1)

        @begin(fgroup)@xlcode{echoenabled(@i(flag))} @c{[sal]}

        @xlcode{(echoenabled@pragma(defn)@index(echoenabled)@index(console, XLISP) @t(@i(flag)))} @c{[lisp]}  @itemsep turn console input echoing on or off
@end(fgroup)
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

To open a file for input, use the @xlcode(open) function with the keyword
argument @xlcode(:direction) set to @xlcode(:input).  To open a file for output,
use the @xlcode(open) function with the keyword argument @xlcode(:direction) set
to @xlcode(:output).  The @xlcode(open) function takes a single required argument which
is the name of the file to be opened.  This name can be in the form of a
string or a symbol.  The @xlcode(open) function returns an object of type
@xlcode(FILE-STREAM) if it succeeds in opening the specified file.  It returns the
value @xlcode(nil) if it fails.  In order to manipulate the file, it is
necessary to save the value returned by the @xlcode(open) function.  This is
usually done by assigning it to a variable with the @xlcode(setq) special form or by
binding it using @xlcode(let) or @xlcode(let*).  Here is an example:
@begin(example)
(setq fp (open "init.lsp" :direction :input))
@end(example)
        Evaluating this expression will result in the file @code(init.lsp)
        being opened.  The file object that will be returned by the @xlcode(open)
        function will be assigned to the variable @xlcode(fp).

        It is now possible to use the file for input.  To read an
        expression from the file, just supply the value of the @xlcode(fp)
        variable as the optional @i(stream) argument to @xlcode(read).
@begin(example)
(read fp)
@end(example)
        Evaluating this expression will result in reading the first
        expression from the file @code(init.lsp).  The expression will be
        returned as the result of the @xlcode(read) function.  More expressions
        can be read from the file using further calls to the @xlcode(read)
        function.  When there are no more expressions to read, the @xlcode(read)
        function will return @xlcode(nil) (or whatever value was supplied as the
        second argument to @xlcode(read)).

        Once you are done reading from the file, you should close it.
        To close the file, use the following expression:
@begin(example)
(close fp)
@end(example)
        Evaluating this expression will cause the file to be closed.

@subsection(Output to a File)@index(Output to a File)

        Writing to a file is pretty much the same as reading from one.
        You need to open the file first.  This time you should use the
        @xlcode(open) function to indicate that you will do output to the file.
        For example:
@begin(example)
(setq fp (open "test.dat" :direction :output))
@end(example)
        Evaluating this expression will open the file @code(test.dat) for
        output.  If the file already exists, its current contents will
        be discarded.  If it doesn't already exist, it will be created.
        In any case, a @xlcode(FILE-STREAM) object will be returned by the @xlcode(OPEN)
        function.  This file object will be assigned to the @xlcode(fp)
        variable.

        It is now possible to write to this file by supplying the value
        of the @xlcode(fp) variable as the optional @i(stream) parameter in the  @xlcode(print) function.
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
        and the use of the optional @i(stream) argument to the @xlcode(read)
        function.
@begin(programexample)
(do* ((fp (open "test.dat" :direction :input))
      (ex (read fp) (read fp)))
     ((null ex) nil)
  (print ex))
@end(programexample)
