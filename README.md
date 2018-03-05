# servant-xml

Servant support for XML.

Types with a `ToXml` instance will be automatically marshalled into XML
and successfully returned by Servant endpoints.
In implementing `toXml`, you can use the `element` and `text` primatives
found in the *xmlbf* library.
