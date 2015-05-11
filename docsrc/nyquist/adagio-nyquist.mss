Adagio @Index(Adagio) is an easy-to-use, non-procedural notation
for scores.   In Adagio, text commands are used to specify each
note.  If you are new to Adagio, you may want to glance at the
examples in Section @ref(adag-examples-sec) starting on page
@pageref(adag-examples-sec) before reading any further.

A note is described in Adagio by a set of attributes@Index(attributes), and
any attribute not specified is ``inherited'' from the previous line.
Attributes may appear in any order and must be separated by one or more
blanks.  An attribute may not contain any blanks.  The attributes are:
time@Index(time), pitch@Index(pitch), loudness@Index(loudness),
voice@Index(voice) number, duration@Index(duration), and articulation@Index(articulation).

Adagio has been used to program a variety of hardware and software
synthesizers, and the Adagio compiler can be easily adapted to new
environments.  Although not originally intended for MIDI, Adagio works quite
well as a representation for MIDI scores.  Adagio has been extended to allow MIDI controller
data such as modulation wheels, pitch bend, and volume, MIDI program commands to change timbre, and System Exclusive messages.

A note command in Adagio must be separated from other notes.  Usually,
notes are distinguished by writing each one on a separate line.
Notes can also be separated by using a comma or semicolon as will
be described below.

Besides notes, there are several other types of commands:
@begin(enumerate)
An asterisk@Index(asterisk) (@code(*)) in column one (or immediately after a comma,
semicolon, or space) indicates that the rest of the line is a
comment@Index(comment).  The line is ignored by Adagio, and is therefore a
good way to insert text to be read by people.  Here are some examples:
@begin(programexample)
* This is a comment.
T150 G4  * This is a comment too!
T150 G4  ;* So is this.
@end(programexample)

An empty command (a blank@Index(blank) line, for example) is ignored as if it
were a comment@Index(comment)@foot(To be consistent, a blank line ought to specify zero attributes and 
generate a note that inherits all of its attributes from the previous one.
Adagio is intentionally inconsistent in this respect.).

An exclamation point@Index(exclamation point)@index(!) (!) in column one (or
immediately after a comma or semicolon) indicates a special
command@Index(special command).  A special command does not generate a note.
Special commands follow the ``!'' with no intervening spaces and extend to the end of the line, for example:
@begin(programexample)
!TEMPO 100
@end(programexample)

Control change commands are used to control parameters like 
pitch bend, modulation, and program (timbre).  Control
change commands can be specified along with notes or by
themselves.
A command that specifies control changes without specifying
a pitch will not produce a note.
@end(enumerate)

Adagio is insensitive to case@Index(case),
thus ``A'' is equivalent to ``a'', and you
can mix upper and lower case letters freely.

@section[Specifying Attributes]
A note is indicated by a set of attributes.  Attributes are indicated by a string of characters with no intervening spaces because spaces separate  attributes.  The attributes are described below.

The default unit of time is  a centisecond
(100@+[th]'s), but this can be changed to a millisecond (1000@+[th]'s) using the @code(!MSEC) command and reset to centiseconds with @code(!CSEC)  (see Section @ref(millisec-sec)).  In the descriptions below, the term ``time unit'' will be used to mean whichever convention is currently in effect.

@subsection[Time]

The time@Index(time) attribute specifies when to start the note.  A time is
specified by a ``T@Index(T)'' followed by a number representing time units
 or by a duration (durations are described below).  Examples:
@begin(programexample)
T150	** 1.5 sec (or .15 sec)
TQ3	** 3 quarter note's duration
@end(programexample)
If no time is specified, the default time@Index(default time) is the sum of
the time and duration attributes of the previous note.  (But see Section
@ref(next-time-sec).) Time is measured relative to the time of the
most recent Tempo@Index(Tempo) or Rate@Index(Rate) command.  (See the
examples in Section
@ref(adag-examples-sec) for some clarification of this point.)

@subsection[Pitch]

The pitch@Index(pitch) attribute specifies what frequency to produce.
Standard scale pitches are named by name, using @code(S) for sharp@Index(sharp),
 @code(F) for flat@Index(flat),
and (optionally) @code(N) for natural@Index(natural).
For example, @code(C) and @code(CN) represent the same pitch, as do @code(FS) and @code(GF) (F sharp and G flat).  Note that there are no bar lines, and accidentals to not carry forward to any other notes as in common practice notation.

Octaves@Index(octave specification) are specified by
number. @code(C4) is middle C, and @code(B3) is a half step lower.  @code(F5) is the top line of
the treble clef, etc.  (Adagio octave numbering follows the ISO standard, but note that this is not universal.  In particular, Yamaha refers to middle C as C3.)  Accidentals@Index(accidentals)@index[S (Adagio Sharp)]@index[F (Adagio Flat)] can go before or after
the octave number, so  @code(FS3) and @code(F3S) have the same meaning.  

An alternate
notation for pitch is @code(P)@i(n), where @i(n) is an integer representing the pitch.@index[P (Adagio Pitch)]
Middle C@index(Middle C) (C4) is equivalent to @code(P60), @code(CS4) is @code(P61), etc.  

If you do not specify an octave@Index(octave specification),
Adagio will choose one for you.  This
is done by picking the octave that will make the current pitch as close
to the previous pitch as possible.  In the case of augmented fourths
 or diminished fifths, there are two equally good choices.  Adagio
chooses the lower octave.

@subsection[Duration]

Duration@Index(duration) is specified by a letter indicating a number of
beats, followed by one or several modifiers.  The basic duration codes are:
@pragma(startscribe)
@begin(display)
@code(W)@Index[W (Adagio Whole note)] (whole@index(whole note), 4 beats), 
@code(H)@Index[H (Adagio Half note)] (half@index(half note), 2 beats), 
@code(Q)@Index[Q (Adagio Quarter note)] (quarter@index(quarter note), 1 beat), 
@code(I)@Index[I (Adagio eIght note)] (eighth@Index(eighth note), 1/2 beat), 
@code(S)@Index[S (Adagio Sixteenth note)] (sixteenth@Index(sixteenth note), 1/4 beat), 
@code(%)@Index[% (Adagio thirtysecond note)] (thirtysecond@index(thirtysecond note), 1/8 beat), and
@code(^)@index[^ (Adagio sixtyfourth note)] (sixtyfourth@index(sixtyfourth note), 1/16 beat). @end(display) 
@pragma(endscribe) @begin(latex)
\begin{quote}
\begin{tabbing}
\hspace*{2.5em} \= \kill
\begin{tt}W\end{tt}\index{W (adagio whole note)} \> (whole\index{Whole note}, 4 beats), \\
\begin{tt}H\end{tt}\index{H (adagio half note)} \> (half\index{Half note}, 2 beats), \\
\begin{tt}Q\end{tt}\index{Q (adagio quarter note)} \> (quarter\index{Quarter note}, 1 beat), \\
\begin{tt}I\end{tt}\index{I (adagio eight note)} \> (eighth\index{Eighth note}, 1/2 beat), \\
\begin{tt}S\end{tt}\index{S (adagio sixteenth note)} \> (sixteenth\index{Sixteenth note}, 1/4 beat), \\
\begin{tt}\%\end{tt}\index{\% (adagio thirtysecond note)} \> (thirtysecond\index{Thirtysecond note}, 1/8 beat), and\\
\begin{tt}\textasciicircum \end{tt}\index{\textasciicircum  \> (adagio sixtyfourth note)} \> (sixtyfourth\index{Sixtyfourth note}, 1/16 beat). 
\end{tabbing}
\end{quote} @end(latex)@pragma(htmlonly)
@begin(display)
@code(W)@Index[W (Adagio Whole note)]@html(&emsp;&emsp;&emsp;)(whole@index(whole note), 4 beats), 
@code(H)@Index[H (Adagio Half note)]@html(&emsp;&emsp;&emsp;)(half@index(half note), 2 beats), 
@code(Q)@Index[Q (Adagio Quarter note)]@html(&emsp;&emsp;&emsp;)(quarter@index(quarter note), 1 beat), 
@code(I)@Index[I (Adagio eIght note)]@html(&emsp;&emsp;&emsp;)(eighth@Index(eighth note), 1/2 beat), 
@code(S)@Index[S (Adagio Sixteenth note)]@html(&emsp;&emsp;&emsp;)(sixteenth@Index(sixteenth note), 1/4 beat), 
@code(%)@Index[% (Adagio thirtysecond note)]@html(&emsp;&emsp;&emsp;)(thirtysecond@index(thirtysecond note), 1/8 beat), and
@code(^)@index[^ (Adagio sixtyfourth note)]@html(&emsp;&emsp;&emsp;)(sixtyfourth@index(sixtyfourth note), 1/16 beat). @end(display) 
@pragma(all)@pragma(debugoff) Note that @code(E) is a pitch, so eighth-notes use the duration code @code(I).
The default tempo is 100 beats per
minute (see Section @ref(tempo-sec)).  These codes may be followed by a  @code(T)
(triplet@Index(triplet)@index[T (Adagio Triplet)]), indicating a duration of 2/3 the normal.  A dot@Index(dot)@index[. (Adagio)] (@code(.)) after a
duration code extends it by half to 3/2 the normal.  An integer
after a note multiplies its duration by the indicated value (the result is
still just one note).  Finally, a slash followed by an integer divides
the duration by the integer.  Like all attributes, duration attributes may not have embedded spaces.  Examples:
@pragma(startscribe)
@begin(display)
@tabclear
@tabset(.5 inches)
@code(Q)@\1   beat (quarter note)
@code(QT)@\2/3 beat (quarter triplet)
@code(W.)@\6   beats(dotted whole note)
@code(ST6)@\1   beat (6 sixteenth triplets) 
@code(H5)@\10  beats(5 half notes)
@code(Q3/7)@\3/7 beats
@end(display)
@pragma(endscribe)
@begin(latex)
\begin{quote}
\begin{tabbing}
\hspace*{2.5em} \= \kill
\texttt{Q} \> 1 beat (quarter note) \\
\texttt{QT} \> 2/3 beat (quarter triplet) \\
\texttt{W.} \> 6 beats(dotted whole note) \\
\texttt{ST6} \> 1 beat (6 sixteenth triplets) \\ 
\texttt{H5} \> 10 beats(5 half notes) \\
\texttt{Q3/7} \> 3/7 beats
\end{tabbing}
\end{quote}
@end(latex) @begin(html)
<blockquote>
<table>
<tr><td><code>Q</code></td><td>&nbsp;&nbsp;&nbsp;</td><td>1 beat (quarter note)</td></tr>
<tr><td><code>QT</code></td><td></td><td>2/3 beat (quarter triplet)</td></tr>
<tr><td><code>W.</code></td><td></td><td>6   beats(dotted whole note)</td></tr>
<tr><td><code>ST6</code></td><td></td><td>1   beat (6 sixteenth triplets) </td></tr>
<tr><td><code>H5</code></td><td></td><td>10  beats(5 half notes)</td></tr>
<tr><td><code>Q3/7</code></td><td></td><td>3/7 beats</td></tr></table>
</blockquote>
@end(html)
A duration may be noted by @code(U)@i(n)@Index(U), where @i(n) is an integer
indicating 100@+[th]'s of a second 
(or 1000@+[th]'s), see Section @ref(millisec-sec).
For example, @code(U25) is twenty-five time units.

Durations may be combined using a plus sign: @pragma(htmlonly)
@begin(programexample)
Q+IT        ** a quarter tied to an eighth triplet
Q/7+W+Q2/7  ** a 7th beat tied to a whole tied to 2/7th beat
Q+U10       ** a quarter plus 10 time units
@end(programexample)@pragma(all) @begin(latex) \begin{quote}
\begin{tabbing}
\hspace*{6em} \= \kill
\texttt{Q+IT} \> a quarter tied to an eighth triplet \\
\texttt{Q/7+W+Q2/7} \> a 7th beat tied to a whole tied to 2/7th beat \\
\texttt{Q+U10} \> a quarter plus 10 time units
\end{tabbing}
\end{quote} @end(latex)
@subsection(Next Time)
@label(next-time-sec)
The time of the next@Index(next Adagio command)@index[N (Adagio Next)] command (the next command in the Adagio
program text) is
normally the time of the current note command
plus the duration of the current note.
This can be overridden by a field consisting of the letter @code(N)
followed by a number indicating time units, or followed by a
duration as described above.  The next note will then start at the time of
the current note plus the duration specified after @code(N).  If the next note
has an explicit time attribute (@code(T)), then the specified time will override
the one based on the previous note.  Examples:
@pragma(htmlonly) @begin(programexample)
N0	** start the next note at the same time as this one
N50	** start the next note 0.5 seconds after this one
NQT	** start the next note 2/3 beat after the current one
NU10+Q  ** start after 0.1 seconds plus a quarter
@end(programexample) @pragma(all) @begin(latex)  \begin{quote}
\begin{tabbing}
\hspace*{6em} \= \kill
\texttt{N0} \> start the next note at the same time as this one \\
\texttt{N50} \>	start the next note 0.5 seconds after this one \\
\texttt{NQT} \> start the next note 2/3 beat after the current one \\
\texttt{NU10+Q} \> start after 0.1 seconds plus a quarter
\end{tabbing}
\end{quote} @end(latex)
A comma has an effect similar to  @code(N0) and is explained in Section @ref(comma-sec).  Articulation effects such as @i(staccato) can be produced using @code(N), but it is more convenient to use the articulation attribute described in Section @ref(articulation-sec).

@subsection(Rest)
Rests@Index(rests)@index[R (Adagio Rest)] are obtained by including the field @code(R) in a
note command.  The effect of an @code(R) field is to omit the note that would
otherwise occur as the result of the current note command.  In all other
respects, the command is processed just like any other line.  This means that
attributes such as duration, loudness, and pitch can be specified, and
anything specified will be inherited by the note in the next command.
Normally, a rest will include just @code(R) and a duration.  The fact that a
note command specifies a rest is not inherited.  For example:
@pragma(htmlonly) @begin(programexample)
R H	** a half (two beat) rest
RH	** illegal, R must be separated from H by space(s)
@end(programexample) @pragma(all) @begin(latex)  \begin{quote}
\begin{tabbing}
\hspace*{6em} \= \kill
\texttt{R H} \> a half (two beat) rest \\
\texttt{RH} \> illegal, R must be separated from H by space(s)
\end{tabbing}
\end{quote} @end(latex)

Because some synthesizers (e.g. a DX7@Index(DX7)) cannot change programs
@Index(program change)
(presets) rapidly, it may be desirable to change programs in
a rest so that the synthesizer will be ready to play by
the end of the rest.  See Section @ref(adag-timbre-sec) for an example.

@subsection[Articulation]
@label(articulation-sec)
Articulation@Index(articulation)@index(staccato)@index(legato) in Adagio refers to the
percentage of time a note is on relative to the indicated duration.  For
example, to play a note @i(staccato), you would normally play the note about
half of its indicated duration.  In Adagio, articulation is indicated by
@code(#)@index[# (Adagio articulation)] followed by an integer number indicating a percentage.  The
articulation attribute does not affect the time of the next command.  This
example plays two @i(staccato) quarter notes:
@begin(programexample)
C Q #50
D
@end(programexample)
To produce overlapping notes, the articulation may be greater than 100.  

@begin(detail)
Be aware that overlapping notes on the same pitch can be a problem for some synthesizers.  The following example illustrates this potential problem:
@begin(programexample)
!TEMPO 60
C Q #160   * starts at time 0,   ends at 1.6 sec
D I        * starts at time 1,   ends at 1.8 sec
C Q        * starts at time 1.5, ends at 3.1 sec?
@end(programexample)
At one beat per second (tempo 60), these three notes will start at times 0, 1, and 1.5 seconds, respectively.  Since these notes have an articulation of 160, each will be on 160% of its nominal duration, so the first note (C) will remain on until 1.6 seconds.  But the third note (another C) will start at time 1.5 seconds.  Thus, the second C will be started before the first one ends.  Depending on the synthesizer, this may cancel the first C or play a second C in unison.  In either case, a note-off message will be sent at time 1.6 seconds.  If this cancels the second C, its actual duration will be 0.1 rather than 1.6 seconds as intended.  A final note-off will be sent at time 3.1 seconds.
@end(detail)

@subsection[Loudness]

Loudness@Index(loudness)@index(velocity) is indicated by an @code(L)
followed by a dynamic marking from the following: @code(PPP)@Index[PPP (Adagio dynamic)]@Index[LPPP (Adagio dynamic)],
@code(PP)@Index[PP (Adagio dynamic)]@Index[LPP (Adagio dynamic)], @code(P)@Index[P (Adagio dynamic)]@Index[LP (Adagio dynamic)], @code(MP)@Index[MP (Adagio dynamic)]@Index[LMP (Adagio dynamic)], @code(MF)@Index[MF (Adagio dynamic)]@Index[LMF (Adagio dynamic)], @code(F)@Index[F (Adagio dynamic)]@Index[LF (Adagio dynamic)],
@code(FF)@Index[FF (Adagio dynamic)]@Index[LFF (Adagio dynamic)], @code(FFF)@Index[FFF (Adagio dynamic)]@Index[LFFF (Adagio dynamic)].  Alternatively, a number from 1 to 127 may be
used.  The loudness attribute is the MIDI note velocity.  (Note that a MIDI velocity of 0 means ``note-off,'' so the minimum loudness is 1.)  The
dynamic@Index(dynamic markings)
markings are translated into numbers as follows:
@pragma(startscribe)
@begin(display)
@tabclear
@tabset(0.8 in, 3 in, 3.8 in)
@code(Lppp)@\20@\@code(Lmf)@\58
@code(Lpp)@\26@\@code(Lf)@\75
@code(Lp)@\34@\@code(Lff)@\98
@code(Lmp)@\44@\@code(Lfff)@\127
@end(display) @pragma(endscribe) @begin(latex)  \begin{quote}
\begin{tabbing}
\hspace*{2.5em} \= \hspace*{3em} \= \hspace*{2.5em} \= \kill
\texttt{Lppp} \> 20 \> \texttt{Lmf} \> 58 \\
\texttt{Lpp} \> 26 \> \texttt{Lf} \> 75 \\
\texttt{Lp} \> 34 \> \texttt{Lff} \> 98 \\
\texttt{Lmp} \> 44 \> \texttt{Lfff} \> 127
\end{tabbing} 
\end{quote} @end(latex) @begin(html)
<blockquote>
<table>
<tr><td><code>Lppp</code></td><td>&nbsp;&nbsp;</td><td>20</td>
<td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td><code>Lmf</code></td><td>&nbsp</td><td>58</td></tr>
<tr><td><code>Lpp</code></td><td></td><td>26</td>
<td></td>
<td><code>Lf</code></td><td></td><td>75</td></tr>
<tr><td><code>Lp</code></td><td></td><td>34</td>
<td></td>
<td><code>Lff</code></td><td></td><td>98</td></tr>
<tr><td><code>Lmp</code></td><td></td><td>44</td>
<td></td>
<td><code>Lfff</code></td><td></td><td>127</td></tr>
</table>
</blockquote>
@end(html)

@subsection[Voice]

The voice@Index(voice)@index[V (Adagio Voice)] attribute tells which of the 16 MIDI channels to use
for the note.  The voice attribute consists of a @code(V) followed by
an integer from 1 (the default) to 16.  

@begin(detail)
There is a limit to how many notes
can be played at the same time on a given voice (MIDI channel).  Since the
limit depends upon the synthesizer, Adagio cannot tell you when you exceed
the limit.  Similarly, Adagio cannot tell whether your synthesizer is set up
to respond to a given channel, so there is no guarantee that what you write
will actually be heard.
@end(detail)

@subsection[Timbre (MIDI Program)]
@label(adag-timbre-sec)
A MIDI program@Index(MIDI program)@index[Z (Adagio program)] (synthesizer preset@Index(preset)) can be
selected using the attribute @code(Z)@i(n), where @i(n)
is the program number (from 1 to 128).
Notice that in MIDI, changing the program on a given channel will affect
@i(all) notes on that channel and possibly others.  Adagio treats MIDI program changes as a form of control change.

@begin(detail)
For many synthesizers, you will not be
able to change programs at the start of a note or during a note.  Change the
program during a rest instead.  For example:
@begin(programexample)
R I Z23 V4	** change MIDI channel 4 to program 23 during rest
A4		** play a note on channel 4
@end(programexample)
Check how your synthesizer interprets program numbers.  For example,
the cartridge programs on a DX7 can be accessed by adding 32 to the
cartridge program number.  Cartridge program number 10
is specified by @code(Z42).
@end(detail)

As in MIDI, the Adagio timbre is a property of the voice (MIDI channel), so
the timbre will not be inherited by notes on a different channel;
to change the timbre on multiple voices (channels), you must explicitly
notate each change.

@subsection[Tempo]
@label(tempo-sec)
The length of a beat may be changed using a Tempo@Index(Tempo) command@index(!Tempo):
@begin(programexample)
!TEMPO @i(n)
@end(programexample)
where @i(n) indicates beats per minute.  The exclamation mark tells Adagio that
this is a special command line rather than a note definition.  A special
command takes the place of a note specification.
No other attributes should be written on a line with a special command.
The @code(!TEMPO) command is associated with a time, computed as if the @code(!TEMPO) command were a note.  The time@Index(time) attribute (@code(T)) of all
succeeding notes is now measured relative to the time of the @code(!TEMPO) command.  The new tempo starts at the @code(!TEMPO) command time and
affects all succeeding notes.  
Durations specified in time units (for example @code(U58), @code(N15)) are not affected by the @code(!TEMPO) command, and numerical times (for example  @code(T851)) are computed relative to the time of the last @code(!TEMPO) command.

The @code(!TEMPO) command is fairly clever about default durations@Index(default
durations).  If the last duration specified before the @code(!TEMPO) command is
symbolic (using one of @code(^),@code(%), @code(S), @code(I), @code(Q), @code(H), or @code(W) ), then the default duration for the
node after the @code(!TEMPO) command will be modified according to the tempo change.
Consider the following tempo change:
@begin(programexample)
!TEMPO 60
A4 H
!TEMPO 120
G
@end(programexample)
In this example, the first note will last 2 seconds (2 beats at 60
beats per minute).  The second note inherits the duration (H) from
the first note, but at 120 beats per minute, the second note will last
only 1 second.  If the duration had been specified @code(U200) (also a
duration of 2 seconds), the second note would also last 2 seconds because the @code(!TEMPO) command does not affect times or durations specified numerically in time units.  If the duration is the sum of a symbolic and a numeric specification, the inherited duration after a @code(!TEMPO) command is undefined.

@subsection(Rate)
The @code(!RATE)@Index(rate)@index(!Rate) command scales all times including those specified in
hundredths of seconds.  A rate of 100 means no change, 200 means twice as
fast, and 50 means half as fast.  For example, to make a piece play 10%
faster, you can add the following command at the beginning of the score:
@begin(programexample)
!RATE 110
@end(programexample)
@code(!RATE) and @code(!TEMPO) commands combine, so
@begin(programexample)
!RATE 200
!TEMPO 70
@end(programexample)
will play 70 beats per minute at double the normal speed, or 140 beats
per minute.  Like @code(!TEMPO), the time of the @code(!RATE) command is added to the
time attribute of all following notes up to the next @code(!TEMPO)  or  @code(!RATE)
command.

Two @code(!RATE) commands do not combine, so a @code(!RATE) command only affects the rate until the next @code(!RATE) command.

Although  @code(!TEMPO) and @code(!RATE) can occur in the middle of a note (using @code(N), @code(T), etc.) they do not affect a note already specified.   This property allows multiple tempi to exist simultaneously (see Section @ref(multipletempi-sec)).

@section[Default Attributes]
@label(default-sec)
If an attribute is omitted, the previous one is used by
default@Index(default) (with the exception of the time attribute).  The
default values for the first note, which are inherited by succeeding notes
until something else is specified, are given below in Adagio notation:
@pragma(startscribe) @begin(display)
@tabclear
@tabset(1.5 inch)
Time  @\@code(T0)
Pitch  @\@code(C4)
Duration  @\@code(Q)
Articulation  @\@code(#100)
Loudness  @\@code(LFFF)
Voice  @\@code(V1)
Tempo  @\@code(!TEMPO 100)
Rate  @\@code(!RATE 100)
@end(display)
@pragma(endscribe) @begin(latex)  \begin{quote}
\begin{tabbing}
\hspace*{10em} \= \kill
Time  \> \texttt{T0} \\
Pitch  \> \texttt{C4} \\
Duration  \> \texttt{Q} \\
Articulation  \> \texttt{\#100} \\
Loudness  \> \texttt{LFFF} \\
Voice  \> \texttt{V1} \\
Tempo  \> \texttt{!TEMPO 100} \\
Rate  \> \texttt{!RATE 100}
\end{tabbing} 
\end{quote} @end(latex) @begin(html)
<blockquote>
<table>
<tr><td>Time        </td><td>&nbsp;&nbsp;&nbsp;</td><td><code>T0  </code></td></tr>
<tr><td>Pitch       </td><td>                  </td><td><code>C4  </code></td></tr>
<tr><td>Duration    </td><td>                  </td><td><code>Q   </code></td></tr>
<tr><td>Articulation</td><td>                  </td><td><code>#100</code></td></tr>
<tr><td>Loudness    </td><td>                  </td><td><code>LFFF</code></td></tr>
<tr><td>Voice       </td><td>                  </td><td><code>V1  </code></td></tr>
<tr><td>Tempo       </td><td>                  </td><td><code>!TEMPO 100</code></td></tr>
<tr><td>Rate        </td><td>                  </td><td><code>!RATE 100</code></td></tr></table>
</blockquote>
@end(html)
Control changes (including timbre or MIDI program, specified by @code(Z)) have no default value and are only sent as specified in the score.

@p(Important:) the rules for determining when a command will play a note are as follows (and this has changed slightly from previous versions):
@begin(enumerate)
If a special (@code(!)) command or nothing is specified, e.g. a blank line, do @i(not) play a note.

If @code(R) (for ``rest'') is specified, do @i(not) play a note.

Otherwise, if a pitch is specified, @i(do) play a note.

Otherwise, if no control changes (or program changes) are specified (so this is a command with non-pitch attributes and no control changes), @i(do) play a note.
@end(enumerate)
Another way to say this is ``Special commands and commands with rests (@code(R)) do not play notes.  Otherwise, play a note if a pitch is specified or if no control is specified.''


@section[Examples]
@label(adag-examples-sec)
The following plays the first two bars of ``Happy Birthday''.  Note that
Adagio knows nothing of bar lines, so the fact that the first note occurs
on beat 3 or that the meter is three-four is of no consequence:
@begin(programexample)
*Example 1 ** Happy Birthday tune (C major)
!TEMPO 120
G4 I. LF
G4 S
A4 Q
G4
C5
B4 H
@end(programexample)
The time attribute for the first note is zero (@code(0)).  The second note
will occur a dotted eighth later, etc. 
Notice that no timbre or rate was specified.
Adagio will provide reasonable default
values of 1 and 100, respectively.

The following example plays the first four bars of  an exercise  from
Bartok@Index(Bartok)'s Mikrokosmos@Index(Mikrokosmos) (Vol.  1, No.  12).
An extra quarter note is inserted at the beginning of each voice in order to
allow time to change MIDI programs.  The right hand part is played on voice
(MIDI channel) 1 and the left hand part on voice 2.  Notice the
specification of the time attribute to indicate that voice 2 starts at time
0.  Also, default octaves are used to reduce typing.
@begin(programexample)
*Example 2 ** Bartok
*voice 1, right hand
R Q Z10 V1   ** extra rest for program change
A4 H
B Q
C
D H
C
D Q
C
B
A
B
C
D
R
@end(programexample)
@begin(programexample)
*voice 2, left hand
T0 R Q Z15 V2   ** extra rest for program change
G3 H
F Q
E
D H
E
D Q
E
F
G
F
E
D
R
@end(programexample)

@begin(latex)\needspace{3\baselineskip}@end(latex)
The next example is the same piece expressed in a different manner,
illustrating the interaction
between the @code(!TEMPO)  command and the time attribute.  Recall that the
time attribute is measured relative to the time of the last @code(!TEMPO) command:
@begin(programexample)
*Example 3 ** 4 measures in 2 sections
!Tempo 100
*Voice 1, Measures 1 & 2
R Q Z10 V1
A4 H
B Q
C
D H
C
@end(programexample)
@begin(programexample)
*Voice 2, Measures 1 & 2
T0 R Q Z15 V2
G3 H
F Q
E
D H
E H
@end(programexample)
@begin(programexample)
!TEMPO 100
*Voice 1, Measures 3 & 4
* note that Z10 is still in effect for V1
V1 D4 Q
C
B
A
B
C
D
R
@end(programexample)
@begin(programexample)
*Voice 2, Measures 3 & 4
T0 V2 D3 Q
E
F
G
F
E
D
R
@end(programexample)

The piece is written in 4 sections.  The first
plays a rest followed by two measures, starting
at time 0.  The next section changes the time back to
zero and plays two measures of the left hand part (voice 2).
The next
command (!TEMPO 100) sets the tempo to 100 (it already is)
@i(and) sets the reference time to
be two measures into the piece.  Therefore, the next note
@code((D4)) will begin measure 3.  The @code(D3) that begins the last
group of notes has a @code(T0) attribute, so it will also start at measure
3.  Notice how the @code(!TEMPO) command can serve to divide a piece into
sections@Index(sections, Adagio).


The last example will show yet another way to express the same piece of
music using the ``Next'' attribute.  Only the first bar of music is
given.
@begin(programexample)
*Example 4 ** use of the Next attribute
!Tempo 100
R Q Z10 V1 N0
R Q Z15 V2

A4 H V1 N0
G3   V2

B4 Q V1 N0
F3   V2

C4 Q V1 N0
E3   V2
@end(programexample)
Here, each pair of 
lines represents two simultaneous notes.  The @code(N0) attribute
forces the second line to start at the same time as the first line of each
pair.  Because of the large intervals, octave numbers (3 and 4) are
necessary to override the default octave for these pitches.

@section(Advanced Features)
Beyond the simple notation described above, Adagio supports a number of 
features.  (See also the next chapter.)

@subsection(Time Units and Resolution)
@label(millisec-sec)@index(time units)@index(resolution)
The default time unit is 10ms (ten milliseconds or one centisecond or
100@+(th) of a second), but it is
possible to change the basic unit to 1ms, or 1000@+(th) of a second.
The time unit can be specified by:
@pragma(startscribe) 
@begin(display)
@tabclear
@tabset(0.8 inches)
@t(!CSEC)@index(!csec)@\centisecond time units = 100@+(th) 
@t(!MSEC)@index(!msec)@\millisecond time units = 1000@+(th)
@end(display) @pragma(endscribe) @begin(latex)  \begin{quote}
\begin{tabbing}
\hspace*{10em} \= \kill
\texttt{!CSEC}\index{!csec} \> centisecond time units = 100\textsuperscript{th} \\
\texttt{!MSEC}\index{!msec} \> millisecond time units = 1000\textsuperscript{th}
\end{tabbing} 
\end{quote} @end(latex) @begin(html) <blockquote>@end(html)@htmlindex(!csec)
@htmlindex(!msec)@begin(html)<table>
<tr><td><code>!CSEC</code></td><td>&nbsp;&nbsp;&nbsp;</td><td>centisecond time units = 100@+(th)</td></tr>
<tr><td><code>!MSEC</code></td><td></td><td>millisecond time units = 1000@+(th)</td></tr></table>
</blockquote> @end(html) 
The time unit remains in effect until the next @code(!CSEC) or @code(!MSEC) command.

@subsection(Multiple Notes Per Line)
@label(comma-sec)
@index(multiple commands)
Notes can be separated by commas@Index(commas)@index[, (Adagio)] or
semicolons@Index(semicolon, Adagio)@index[; (Adagio)] as well as by starting a new line.  A comma is
equivalent to typing  @code(N0) and starting a new line.  In other words, the next note after a comma will start at the same time as the note before the comma.   In general, @i(use commas to separate the notes of a chord.)

A semicolon is equivalent to starting a new line.  In general, @i(use semicolons to group notes in a melody).  Here is yet another rendition of the Bartok:
@begin(programexample)
*Example 5 ** use of semicolons
!Tempo 100
R Q Z10 V1
A4 H; B Q; C; D H; C; D Q; C; B; A; B; C; D; R

T0 R Q Z15 V2
G3 H; F Q; E; D H; E; D Q; E; F; G; F; E; D; R
@end(programexample)
This example is similar to Example 2, except semicolons are used.  Note how semicolons make the two lines of music stand out.
The next example is similar to Example 4, except commas are used
and four bars are notated.  The music below is treated as a sequence of 2-note chords, with each chord on a separate line:
@begin(programexample)
*Example 6 ** use of commas
!Tempo 100
R Q Z10 V1, R Q Z15 V2
A4 H V1, G3 V2
B4 Q V1, F3 V2
C4   V1, E3 V2
D4 H V1, D3 V2
C4   V1, E3 V2
D4 Q V1, D3 V2
C4   V1, E3 V2
B4   V1, F3 V2
A4   V1, G3 V2
B4   V1, F3 V2
C4   V1, E3 V2
D4   V1, D3 V2
R
@end(programexample)

@subsection(Control Change Commands)
@index(Control change)@index[~ (Adagio)]
Any control change can be specified using
the syntax ``@t[~@i(n)(@i(v))]'', where @i(n) is the controller number (0 - 127), and 
@i(v) is the value.  In addition, Adagio has some special syntax for
some of the commonly used control changes (note that Pitch bend, Aftertouch, and MIDI Program Change are technically not MIDI control changes but have their own 
special message format and status bytes):
@pragma(startscribe)
@begin(display)
@tabclear
@tabset(0.5 inch)						
K@\Portamento switch@Index(Portamento switch)@Index[K (Adagio control)]
M@\Modulation wheel@Index(Modulation wheel)@Index[M (Adagio control)]
O@\Aftertouch@Index(Aftertouch)@Index[O (Adagio control)]
X@\Volume@Index(Volume)@Index[X (Adagio control)]
Y@\Pitch bend@Index(Pitch Bend)@Index[Y (Adagio control)]
Z@\Program Change@Index(Program)@Index[Z (Adagio program)] @end(display) 
@pragma(endscribe)
@begin(latex)
\begin{quote}
\begin{tabbing}
\hspace*{2.5em} \= \kill
\texttt{K} \> Portamento switch \\
\texttt{M} \> Modulation wheel \\
\texttt{O} \> Aftertouch \\
\texttt{X} \> Volume \\
\texttt{Y} \> Pitch Bend \\
\texttt{Z} \> Program Change
\end{tabbing}
\end{quote}
@end(latex) @begin(html)
<blockquote>
<table>
<tr><td><code>K</code></td>@end(html)@htmlindex(Portamento switch)@htmlindex[K (Adagio control)]@begin(html)<td>&nbsp;&nbsp;&nbsp;</td><td>Portamento switch</td></tr>
<tr><td><code>M</code></td>@end(html)@htmlindex(Modulation wheel)@htmlindex[M (Adagio control)]@begin(html)<td></td><td>Modulation wheel</td></tr>
<tr><td><code>O</code></td>@end(html)@htmlindex(Aftertouch)@htmlindex[O (Adagio control)]@begin(html)<td></td><td>Aftertouch</td></tr>
<tr><td><code>X</code></td>@end(html)@htmlindex(Volume)@htmlindex[X (Adagio control)]@begin(html)<td></td><td>Volume</td></tr>
<tr><td><code>Y</code></td>@end(html)@htmlindex(Pitch bend)@htmlindex[Y (Adagio control)]@begin(html)<td></td><td>Pitch bend</td></tr>
<tr><td><code>Z</code></td>@end(html)@htmlindex(Program)@htmlindex[Z (Adagio control)]@begin(html)<td></td><td>Program Change</td></tr></table>
</blockquote>
@end(html)
The letter listed beside each control function is the Adagio command
letter.  For example,  @code(M23) is the command for setting the modulation
wheel to 23.  Except for pitch bend, the portamento switch, and MIDI Program Change, all
values range from 0 to 127.  Pitch bend is ``off'' or centered at
128, and has a range from 0 to 255 (MIDI allows for more precision, but
Adagio does not).  Turn on portamento with @code(K127) and off with @code(K0).  Programs are numbered 1 to 128 to correspond to synthesizer displays.

@p(About volume:) Midi volume is just a control, and the Midi standard does not say what it means. Typically it does what the volume pedal does; that is, it scales the amplitude in a continuously changeable fashion. In contrast, Midi velocity, which is controlled by the @code(L) (loudness) attribute, is part of a Midi note-on command and is fixed for the duration of the note. Typically, these two ways of controlling loudness and amplitude operate independently. In some low-cost synthesizers the numbers seem to be added together internally and volume changes are ignored after the note starts.

@p(About pitch bend:) Midi pitch bend is a number from 0 to 16383, where 8192 is the center position. To convert to Midi, Adagio simply multiplies your number by 64, giving values from 0 to 16320. Note that @code(Y128) translates exactly to 8192. The @i(meaning) of pitch bend depends upon your synthesizer and its setting. Most synthesizers let you specify a ``pitch bend range.'' A range of one semitone means that @code(Y255) will produce a bend of approximately one semitone up, and @code(Y0) will bend one semitone down.  If the range is 12 semitones, then the same @code(Y255) will bend an octave. Typically, pitch bend is exponential, so each increment in the pitch bend value will bend an equal number of cents in pitch.

Control changes can be part of a note specification or independent.
In the following example, a middle C is played with a modulation
wheel setting of 50 and a pitch bend of 120.  Then, at 10 unit
intervals, the pitch bend is decreased by 10.  The last line sets the
portamento time (controller 5) to 80:
@begin(programexample)
*Example 7
C4 LMF M50 Y120 U100 N10
Y110 N10; Y100 N10; Y90 N10; Y80 N10
Y70 N10; Y60 N10; Y50 N10
~5(80)
@end(programexample)

See Section @ref(default-sec) on page @pageref(default-sec) for rules on whether or not a command will play a note.

@subsection(Multiple Tempi)@Index(multiple tempi)
@label(multipletempi-sec)
@Index(polyrhythm)
Writing a piece with multiple tempi requires no new commands; you
just have to be clever in the use of Tempo and Time.  The following
plays a 7 note diatonic scale on voice 1, and a 12 note chromatic
scale on voice 2:
@begin(programexample)
*Example 8 ** multiple tempi
!TEMPO 70
V1 C4; D; E; F; G; A; B
T0 R N0

!TEMPO 120
V2 C4; CS; D; DS; E; F; FS; G; GS; A; AS; B

!TEMPO 100
V1 C5, V2 C5
@end(programexample)
The third line plays the 7-note diatonic scale on voice 1.  The
next line contains the tricky part:  notice that the time is
set back to zero, there is a rest, and a next (@code(N)) attribute is used
to specify that the next default time will be at the same time as
the current one.  This is tricky because a @code(!TEMPO) command cannot have a time (@code(T0)) attribute, and a @code(T0) by itself would create a note with a duration.  @code(T0 R N0) says: ``go to time 0, do not play a note, and do not advance the time before the next command''.
Thus, the time of the @code(!TEMPO 120) command is zero.
After the 12 note scale, the tempo is changed to 100 and a final note
is played on each voice.  A little arithmetic will show that 7 notes
at tempo 70 and 12 notes at tempo 120 each take 6 seconds, so the
final notes (@code(C5)) of each scale will happen at the same time.
 
@subsection(MIDI Synchronization)
@index(synchronization)@index(clock)@index(MIDI Clock)@index(clock command)

The Adagio program (but not Nyquist) can synchronize with external devices using MIDI real time messages. Thus, Adagio has a @code(!CLOCK) command. This command is currently of no use to Nyquist users but is documented here for completeness (it's part of the language syntax even if it does not do anything). 

Since Adagio supports multiple tempi, and Midi clock is
based on beats, it is necessary to be explicit in the score about where the
clock should start and what is the duration of a quarter note.  The
@code(!CLOCK) command@index(!Clock) in Adagio turns on a 24 pulse-per-quarter (PPQ) clock at
the current tempo and time:
@begin(programExample)
!TEMPO 100
!CLOCK
@end(programexample)
A @code(!CLOCK) command must also be inserted for each tempo change that is to be
reflected in the Midi clock.  Typically, each !TEMPO command will be followed by a !CLOCK command.

@begin(detail)
Clock commands and thus tempo
changes can take place at arbitrary times.  It is assumed that tempo changes
on an exact 24@+(th) of a beat subdivision (for example, exactly on a
beat).  If not, the tempo change will take place on the nearest exact
24@+(th) of a beat subdivision.  This may be earlier or later than the
requested time.
@end(detail)

@subsection[System Exclusive Messages] 
@label(macro-sec)
Adagio  has a definition facility that makes it possible to send system
exclusive parameters.  Often, there are parameters on Midi
synthesizers that can only be controlled by system exclusive messages.
Examples include the FM ratio and LFO rate on a DX7 synthesizer.  The
following example defines a macro for the DX7 LFO rate and then shows how
the macro is used to set the LFO rate for a B-flat whole note in the score.
The macro definition is given in hexadecimal, except @t(v) is replaced by 
the channel (voice) and @t(%1) is replaced by the first parameter.
A macro is invoked by writing ``~'' followed by the macro name and a
list of parameters@index(!Def):
@begin(programexample)
!DEF LFO F0 43 0v 01 09 %1 F7
Bf5 W ~LFO(25)
@end(programexample)

In general, the @t(!DEF) command can define any single MIDI message including
a system exclusive
message.  The message must be complete (including the status byte), and each @t(!DEF) must correspond to just one message.
The symbol following @t(!DEF) can be any name consisting of 
alphanumeric characters.  Following the name is a hexadecimal string
(with optional spaces), all on one line.  Embedded in the string may
be the following special characters:
@begin(description)
@t(v)@\Insert the 4-bit voice (MIDI channel) number.  If @t(v) occurs in the
place of a high-order hexadecimal digit, replace @t(v) with @t(0v) so that
the channel number is always placed in the low-order 4 bits of a data byte.  In other words, @t(v) is padded if necessary to fall into the low-order bits.

@t(%)@i(n)@\Insert a data byte with the low-order 7 bits of 
parameter number @i(n).  Parameters are numbered 1 through 9.
If the
parameter value is greater than 127, the high-order bits are discarded.

@t(^)@i(n)@\Insert a data byte with bits 7 through 13 of parameter number
@i(n).  In other words, shift the value right 7 places then clear all
but the first 7 bits.  Note that 14-bit numbers can be encoded by
referencing the same parameter twice; for example, @t(%4^4) will
insert the low-order followed by the high-order parts of parameter 4
into two successive data bytes.
@end(description)

Parameters are separated by commas, but there may be no spaces.  The
maximum number of parameters allowed is 9.  Here
is an example of definitions to send a full-resolution pitch
bend command and to send a system exclusive command to change a DX7 
parameter@foot[My TX816 Owner's Manual gives an incorrect format for the
change parameter sysex command (according to the manual, there is no data 
in the message!)  I am assuming that the data should be the last byte before
the EOX and that there is no byte count.  If you are reading this, assume
that I have not tested this guess, nor have I tested this example.]. 

@begin(programexample)
* Define macro for pitch bend commands:
!DEF bend Ev %1 ^1

A ~bend(8192)  ** 8192 is "pitch bend off"

* Change the LFO SPEED:
*  SYSEX = F0, Yamaha = 43, Substatus/Channel = 1v, 
*  Group# = 01, Parameter# = 9, Data = 0-99, EOX = F7
!DEF lfospeed F0 43 1v 01 09 %1 F7

* now use the definitions:
G4 ~bend(7567) N40
~lfospeed(30) N35

@end(programexample)


@subsection(Control Ramps)
@label(macroramp-sec)
The @t(!RAMP) command@index(!Ramp) can specify a smooth control change from one
value to another.  It consists of a specification of the starting and
ending values of some control change, a duration specifying how often
to send a new value, and a duration specifying the total length of the ramp.
@begin(programexample)
!RAMP X10 X100 Q W2
!RAMP ~23(10) ~23(50) U20 W
!RAMP ~lfo(15) ~lfo(35) U10
@end(programexample)
The first line says to ramp the volume control (controller number 7)
from 10 to 100, changing at each quarter note for the duration of two
whole notes.
The second line says to ramp controller number 23 from value 10 to value 50,
sending a new control change message every 20 time units.  The overall
duration of the ramp should be equivalent to a whole note (@t(W)).
As shown in the third line, even system exclusive messages controlled by
parameters can be specified.  If the system exclusive message has more
than one parameter, only one parameter may be ``ramped''; the others must
remain the same.  For example, the following would ramp the second parameter:
@begin(programexample)
!RAMP ~mysysex(4,23,75) ~mysysex(4,100,75) U10 W
@end(programexample)

@begin(detail)
A rather curious and extreme use of macros and ramps is illustrated in the
following example.  The @t(noteon) macro starts a note, and @t(noteoff) ends it.  Ramps can now be used to emit a series of notes with changing pitches or velocities.  Since Adagio has no idea that these macros are turning on notes, it is up to the programmer to turn them off!
@begin(programexample)
!DEF noteon 9v %1 %2
!DEF noteoff 8v %1 %2
~noteon(48,125)
~noteoff(48,126)
* turn on some notes
!RAMP ~noteon(36,125) ~noteon(60,125) Q W NW
* turn them off
!RAMP ~noteoff(60,50) ~noteoff(36,50) Q W NW
@end(programexample)
@end(detail)

@subsection(The !End Command)
@label(end-sec)@index(!End)@index(end command)
The special command  @code(!END) marks the end of a score.  Everything beyond that
is ignored, for example:
@begin(programexample)
* this is a score
C; D; E; F; G W
!END
since the score has ended, this text will be ignored
@end(programexample)

@subsection(Calling C Routines)
@label(call-sec)
It is possible to call C routines from within Adagio scores when using
specially linked versions, but this feature is disabled in Nyquist. The
syntax is described here for completeness.

The @code(!CALL) command@index(!Call)@index(Call command) calls a C routine that can in turn invoke a
complex sequence of operations.  Below is a call to a trill@index(trill) routine,
which is a standard routine in Adagio.  The parameters are the base
pitch of the trill, the total duration of the trill, the interval in semitones, the
duration of each note of the trill, and the loudness.  Notice
that both numbers and Adagio notation can be used as parameters:
@begin(programexample)
!CALL trill(A5,W,2,S,Lmf)  T278 V1
@end(programexample)
@i(The parameter list should have no spaces), and parameters are separated
by commas.  Following the close parenthesis, you may specify other
attributes such as the starting time and voice as shown in the example above.

A parameter may be an Adagio pitch specification, an Adagio duration,
an Adagio loudness, a number, or an ASCII character within single
quotes, e.g. @t('a') is equivalent to @t(97) because 97 is the decimal
encoding of ``a'' in ASCII.

The @code(!CALL) may be followed by a limited set of attributes.  These are time (@t(T)), voice (@t(V)), and next time (@t(N)).  The @code(!CALL) is made at the current time if no time is specified, and the time of the next adagio command is the time of the  @code(!CALL) unless a next time is specified.  In other words, the default is @t(N0).

@subsection(Setting C Variables)
In addition to calling C routines, there is another way in which scores
can communicate with C.  As with @code(!CALL), specific C code must be linked before these commands can be used, and this is not supported in Nyquist.  
The  @code(!SETI) command@index(!Seti)@index(seti commnad) sets an integer variable
to a value, and the @code(!SETV) command@index(!Setv)@index(setv command) sets an element of an integer array.  
For example, the next line sets the variable @t(delay) to 200 and sets
@t(transposition[5]) to -4 at time 200:
@begin(programexample)
!SETI delay 200
!SETV transposition 5 -4  T200
@end(programexample)
As with the @code(!CALL) command, these commands perform their operations at
particular times according to their place in the Adagio score.  This makes
it very easy to implement time-varying parameters that control various
aspects of an interactive music system.  

