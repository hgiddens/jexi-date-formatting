# JEXI date formatting

A reimplementation of the date formatting performed by JEQI/JESI.

## Usage

`JEXIDateFormatter` objects are creating by constructing them with a
`String` containting the date format to use, and instances can convert
`java.util.Date`, `org.joda.time.DateTime`, and `java.lang.Long` objects to
formatted strings with their `format` method (longs are interpreted as a
number of milliseconds since the UNIX epoch). `IllegalArgumentException` is
thrown if unhandled instances are passed to `format`.

See the JEQI documentation for details on the format specifiers available.

### An example

    import java.util.Date;
    import nz.co.robotines.JEXIDateFormatter;

    public class example {
        public static void main(String[] args) {
            Object now = new Date();
            // or new org.joda.time.DateTime(...), or a long.
            for (String arg : args) {
                JEXIDateFormatter instance = new JEXIDateFormatter(arg);
                System.out.println(instance.format(now));
            }
        }
    }

### Compatibility with JEXI

The behaviour of `JEXIDateFormatter` is intended to be _completely_
compatible with the JEQI/JESI specifications, except where the behaviour of
JEQI/JESI diverges from the specification, in which case this should do
what the real thing does. The following caveats apply:

* Several format specifiers are documented as depending on the server's
  current locale. I've implemented those format specifiers in a way that
  doesn't vary with locale, but as the server's current locale is only
  exposed to the client by way of date formatting, this shouldn't matter.
* The documentation states that if a `m`/`mm` specifier immediately follows
  a `h`/`hh` specifier, it represents the minute of the hour rather than
  the month of the year. We and JEQI both interpret “immediately follows” to
  mean “with no intervening non-text-literals”.
* Leap seconds are not handled at all. I'm not sure how JEQI/JESI handle
  these, but the documentation suggests they should be ignored.
* The documentation doesn't specify what the half-day specifiers should
  be formatted as at noon exactly. In common with JEQI, we use the
  post-noon value.
* The documentation says the “J” format specifier represents the modified
  Julian day, but JEQI instead returns the normal Julian day number - we do
  the same thing.
* In common with JEQI, and despite the documentation, we don't treat
  half-day specifiers case insensitively.

## Installation

Use [Leiningen](http://github.com/technomancy/leiningen) to build the jar file:

    $ lein uberjar

The above will create two `.jar` files, one
(`jexi-date-formatting-1.0.0.jar`) including only the jexi-date-formatting
files, the other (`jexi-date-formatting-1.0.0-standalone.jar`) also
including dependencies. Electing not to use the standalone jar will mean
you will have to supply the dependencies yourself:

* clojure 1.1
* clojure-contrib 1.1
* clj-time 0.1.0 (which in turn depends on Joda Time)
* fnparse 2.2.4

## License

Copyright © 2010 Hugh Giddens

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the “Software”), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
