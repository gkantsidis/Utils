# Notes on the Schemas

## Generate `Atom.xsd`

This does not seem to exist on its own. Its specification is given in [RFC4287](https://validator.w3.org/feed/docs/rfc4287.html#rfc.section.B)
using the [RELAX NG Schema](https://relaxng.org/compact-20021121.html).
To convert use the [trang.jar](http://www.thaiopensource.com/download/) tool:

```cmd
java -jar trang.jar -I rnc -O xsd atom.rnc atom.xsd
```