# jexi-date-formatting

FIXME: write description

## Usage

`JEXIDateFormatter` objects are creating by constructing them with a
`String` containting the date format to use, and instances can convert
`java.util.Date`, `org.joda.time.DateTime`, and `java.lang.Long` objects to
formatted strings with their `format` method (longs are interpreted as a
number of milliseconds since the UNIX epoch).

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

## Installation

FIXME: write

## License

FIXME: write
