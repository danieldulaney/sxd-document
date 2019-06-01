//! Formats a DOM structure to a Write
//!
//! ### Example
//! ```
//! use sxd_document::Package;
//! use sxd_document::writer::format_document;
//!
//! let package = Package::new();
//! let doc = package.as_document();
//!
//! let hello = doc.create_element("hello");
//! hello.set_attribute_value("planet", "Earth");
//! doc.root().append_child(hello);
//!
//! let mut output = Vec::new();
//! format_document(&doc, &mut output).expect("unable to output XML");
//! ```
//!
//! ### Potential options to support
//!
//! - Space before `/>`
//! - Single vs double quotes
//! - Fixed ordering of attributes

use std::borrow::ToOwned;
use std::io::{self,Write};
use std::slice;
use std::cmp::max;

use self::Content::*;

use super::QName;
use super::str_ext::{SplitKeepingDelimiterExt,SplitType};


use super::dom;
use super::dom::{ChildOfElement,ChildOfRoot};
use super::lazy_hash_map::LazyHashMap;

trait WriteStr: Write {
    fn write_str(&mut self, s: &str) -> io::Result<()> {
        self.write_all(s.as_bytes())
    }
}

impl<W: ?Sized> WriteStr for W where W: Write {}

struct ElementStackFrame<'d> {
    element: dom::Element<'d>,
    autons: LazyHashMap<&'d str, i32>,
}

impl<'d> From<dom::Element<'d>> for ElementStackFrame<'d> {
    fn from(el: dom::Element<'d>) -> Self {
        Self {
            element: el,
            autons: LazyHashMap::new(),
        }
    }
}

type ElementStack<'d> = Vec<ElementStackFrame<'d>>;

enum Content<'d> {
    Element(dom::Element<'d>),
    ElementEnd(dom::Element<'d>),
    Text(dom::Text<'d>),
    Comment(dom::Comment<'d>),
    ProcessingInstruction(dom::ProcessingInstruction<'d>),
}

/// Write a document, specifying some formatting options
///
/// For example, the default is to use single-quotes for attributes. To use
/// double quotes for attributes, you need to use `set_single_quotes(false)`.
///
/// ```
/// use sxd_document::{Package, writer::Writer};
///
/// // Create a new document
/// let p = Package::new();
/// let doc = p.as_document();
/// let el = doc.create_element("hello");
/// el.set_attribute_value("a", "b");
/// doc.root().append_child(el);
///
/// // Format the document as bytes
/// let mut output = Vec::new();
/// Writer::new().set_single_quotes(false).format_document(&doc, &mut output);
///
/// // Check that the output is correct
/// let output_string = String::from_utf8(output).unwrap();
/// assert_eq!(output_string, r#"<?xml version="1.0"?><hello a="b"/>"#);
/// ```
pub struct Writer {
    single_quotes: bool,
    write_encoding: bool,
}

impl Default for Writer {
    fn default() -> Self {
        Self {
            single_quotes: true,
            write_encoding: false,
        }
    }
}

impl Writer {
    /// Create a new `Writer` with default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set whether single quotes should be used for writing a document.
    pub fn set_single_quotes(mut self, single_quotes: bool) -> Self {
        self.single_quotes = single_quotes;
        self
    }

    /// Set whether the encoding should be specified in the output document header.
    pub fn set_write_encoding(mut self, write_encoding: bool) -> Self {
        self.write_encoding = write_encoding;
        self
    }

    fn quote_char(&self) -> &'static str {
        match self.single_quotes {
            true => "'",
            false => "\"",
        }
    }
}

impl Writer {
    fn format_qname<'d, W: ?Sized>(&self,
                                   qname: QName<'d>,
                                   stack: &mut ElementStack<'d>,
                                   preferred_prefix: Option<&'d str>,
                                   writer: &mut W)
                                   -> io::Result<()>
        where W: Write
    {
        if let Some(ns) = qname.namespace_uri() {
            let element = stack.last().expect("called format_qname with an empty stack").element;

            if element.recursive_default_namespace_uri() == Some(ns) {
                // Don't do anything -- we're using the default namespace
            } else if let Some(prefix) = element.prefix_for_namespace_uri(ns, preferred_prefix) {
                // A prefix was defined -- use that prefix
                try!(write!(writer, "{}:", prefix));
            } else {
                // We need to find or create an autons
                // Work up the stack: If we find a matching autons, use it
                // If we don't find a matching autons, define a new one
                let mut max_autons: i32 = -1;
                let mut autons_to_use: Option<i32> = None;

                for stack_frame in stack.iter().rev() {
                    if let Some(autons) = stack_frame.autons.get(ns) {
                        autons_to_use = Some(*autons);
                        break;
                    } else {
                        max_autons = max(max_autons, *stack_frame.autons.iter().map(|(_, v)| v).max().unwrap_or(&std::i32::MIN));
                    }
                }

                if autons_to_use.is_none() {
                    autons_to_use = Some(max_autons + 1);
                    stack.last_mut().expect("called format_qname with an empty stack").autons.insert(ns, max_autons + 1);
                }

                try!(write!(writer, "autons{}:", autons_to_use.expect("was manually set if it was None")));
            }
        }

        writer.write(qname.local_part().as_bytes()).map(|_| ())
    }

    fn format_attribute_value<W: ?Sized>(&self, value: &str, writer: &mut W) -> io::Result<()>
        where W: Write
    {
        for item in value.split_keeping_delimiter(|c| c == '<' || c == '>' || c == '&' || c == '\'' || c == '"') {
            match item {
                SplitType::Match(t)        => try!(writer.write_str(t)),
                SplitType::Delimiter("<")  => try!(writer.write_str("&lt;")),
                SplitType::Delimiter(">")  => try!(writer.write_str("&gt;")),
                SplitType::Delimiter("&")  => try!(writer.write_str("&amp;")),
                SplitType::Delimiter("'")  => try!(writer.write_str("&apos;")),
                SplitType::Delimiter("\"") => try!(writer.write_str("&quot;")),
                SplitType::Delimiter(..)   => unreachable!(),
            }
        }
        Ok(())
    }

    fn format_element<'d, W: ?Sized>(&self,
                                     stack: &mut ElementStack<'d>,
                                     writer: &mut W)
                                     -> io::Result<()>
        where W: Write
    {
        let element = stack.last().expect("format_element called with empty stack").element;
        let attrs = element.attributes();

        try!(writer.write_str("<"));
        try!(self.format_qname(element.name(), stack, element.preferred_prefix(), writer));

        for attr in &attrs {
            try!(writer.write_str(" "));
            try!(self.format_qname(attr.name(), stack, element.preferred_prefix(), writer));
            try!(write!(writer, "="));
            try!(write!(writer, "{}", self.quote_char()));
            try!(self.format_attribute_value(attr.value(), writer));
            try!(write!(writer, "{}", self.quote_char()));
        }

        let mut children = element.children();
        if children.is_empty() {
            try!(writer.write_str("/>"));
        } else {
            try!(writer.write_str(">"));

            children.reverse();
            for child in children.into_iter() {
                try!(match child {
                    ChildOfElement::Element(e) => {
                        stack.push(e.into());
                        self.format_element(stack, writer)
                    },
                    ChildOfElement::Text(t) => self.format_text(t, writer),
                    ChildOfElement::Comment(c) => self.format_comment(c, writer),
                    ChildOfElement::ProcessingInstruction(p) => self.format_processing_instruction(p, writer),
                })
            }

            try!(writer.write_str("</"));
            try!(self.format_qname(element.name(), stack, element.preferred_prefix(), writer));
            try!(writer.write_str(">"));
        }

        stack.pop();

        Ok(())
    }

    fn format_text<W: ?Sized>(&self, text: dom::Text, writer: &mut W) -> io::Result<()>
        where W: Write
    {
        for item in text.text().split_keeping_delimiter(|c| c == '<' || c == '>' || c == '&') {
            match item {
                SplitType::Match(t)       => try!(writer.write_str(t)),
                SplitType::Delimiter("<") => try!(writer.write_str("&lt;")),
                SplitType::Delimiter(">") => try!(writer.write_str("&gt;")),
                SplitType::Delimiter("&") => try!(writer.write_str("&amp;")),
                SplitType::Delimiter(..)  => unreachable!(),
            }
        }
        Ok(())
    }

    fn format_comment<W: ?Sized>(&self, comment: dom::Comment, writer: &mut W) -> io::Result<()>
        where W: Write
    {
        write!(writer, "<!--{}-->", comment.text())
    }

    fn format_processing_instruction<W: ?Sized>(&self, pi: dom::ProcessingInstruction, writer: &mut W)
                                                -> io::Result<()>
        where W: Write
    {
        match pi.value() {
            None    => write!(writer, "<?{}?>", pi.target()),
            Some(v) => write!(writer, "<?{} {}?>", pi.target(), v),
        }
    }

    fn format_body<W: ?Sized>(&self, element: dom::Element, writer: &mut W) -> io::Result<()>
        where W: Write
    {
        let mut stack: ElementStack = ElementStack::new();
        stack.push(element.into());

        try!(self.format_element(&mut stack, writer));

        Ok(())
    }

    fn format_declaration<'d, W: ?Sized>(&self, writer: &mut W) -> io::Result<()>
        where W: Write
    {
        try!(write!(writer, "<?xml version={}1.0{}", self.quote_char(), self.quote_char()));

        if self.write_encoding {
            try!(write!(writer, " encoding={}UTF-8{}", self.quote_char(), self.quote_char()));
        }

        try!(write!(writer, "?>"));

        Ok(())
    }

    /// Formats a document into a Write
    pub fn format_document<'d, W: ?Sized>(&self, doc: &'d dom::Document<'d>, writer: &mut W) -> io::Result<()>
        where W: Write
    {
        try!(self.format_declaration(writer));

        for child in doc.root().children().into_iter() {
            try!(match child {
                ChildOfRoot::Element(e) => self.format_body(e, writer),
                ChildOfRoot::Comment(c) => self.format_comment(c, writer),
                ChildOfRoot::ProcessingInstruction(p) => self.format_processing_instruction(p, writer),
            })
        }

        Ok(())
    }
}

/// Formats a document into a `Write` using the default `Writer`
pub fn format_document<'d, W: ?Sized>(doc: &'d dom::Document<'d>, writer: &mut W) -> io::Result<()>
    where W: Write
{
    Writer::default().format_document(doc, writer)
}

#[cfg(test)]
mod test {
    use super::super::Package;
    use super::super::dom;
    use super::Writer;

    fn format_xml<'d>(doc: &'d dom::Document<'d>) -> String {
        format_xml_writer(Writer::default(), doc)
    }

    fn format_xml_writer<'d>(writer: Writer, doc: &'d dom::Document) -> String {
        let mut w = Vec::new();
        writer.format_document(doc, &mut w).expect("Not formatted");
        String::from_utf8(w).expect("Not a string")
    }

    #[test]
    fn top_element() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello/>");
    }

    #[test]
    fn element_with_namespace() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element(("namespace", "local-part"));
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><autons0:local-part xmlns:autons0='namespace'/>");
    }

    #[test]
    fn element_with_default_namespace() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element(("namespace", "local-part"));
        e.set_default_namespace_uri(Some("namespace"));
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><local-part xmlns='namespace'/>");
    }

    #[test]
    fn element_with_preferred_namespace_prefix() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element(("namespace", "local-part"));
        e.set_preferred_prefix(Some("prefix"));
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><prefix:local-part xmlns:prefix='namespace'/>");
    }

    #[test]
    fn element_with_attributes() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        e.set_attribute_value("a", "b");
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello a='b'/>");
    }

    #[test]
    fn element_with_attributes_double_quotes() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        e.set_attribute_value("a", "b");
        d.root().append_child(e);

        let xml = format_xml_writer(Writer::new().set_single_quotes(false), &d);
        assert_eq!(xml, r#"<?xml version="1.0"?><hello a="b"/>"#);
    }


    #[test]
    fn attribute_with_namespace() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        e.set_attribute_value(("namespace", "a"), "b");
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello autons0:a='b' xmlns:autons0='namespace'/>");
    }

    #[test]
    fn attribute_with_preferred_namespace_prefix() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        let a = e.set_attribute_value(("namespace", "a"), "b");
        a.set_preferred_prefix(Some("p"));
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello p:a='b' xmlns:p='namespace'/>");
    }

    #[test]
    fn attribute_with_default_namespace_prefix() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element(("namespace", "hello"));
        e.set_preferred_prefix(Some("p"));
        e.set_default_namespace_uri(Some("namespace"));
        e.set_attribute_value(("namespace", "a"), "b");
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello p:a='b' xmlns='namespace' xmlns:p='namespace'/>");
    }

    #[test]
    fn attributes_with_conflicting_preferred_namespace_prefixes() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");

        let a = e.set_attribute_value(("namespace1", "a1"), "b1");
        a.set_preferred_prefix(Some("p"));

        let a = e.set_attribute_value(("namespace2", "a2"), "b2");
        a.set_preferred_prefix(Some("p"));

        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello p:a1='b1' autons0:a2='b2' xmlns:p='namespace1' xmlns:autons0='namespace2'/>");
    }

    #[test]
    fn attributes_with_different_preferred_namespace_prefixes_for_same_namespace() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");

        let a = e.set_attribute_value(("namespace", "a1"), "b1");
        a.set_preferred_prefix(Some("p1"));

        let a = e.set_attribute_value(("namespace", "a2"), "b2");
        a.set_preferred_prefix(Some("p2"));

        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello p1:a1='b1' p2:a2='b2' xmlns:p1='namespace' xmlns:p2='namespace'/>");
    }

    #[test]
    fn attribute_values_with_less_than_greater_than_ampersand_apostrophe_or_quote_are_escaped() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        e.set_attribute_value("name", r#"'1 < 2' & "4 > 3""#);
        d.root().append_child(e);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello name='&apos;1 &lt; 2&apos; &amp; &quot;4 &gt; 3&quot;'/>");
    }

    #[test]
    fn nested_element() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element("hello");
        let world = d.create_element("world");
        hello.append_child(world);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello><world/></hello>");
    }

    #[test]
    fn nested_element_with_namespaces() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element(("outer", "hello"));
        let world = d.create_element(("inner", "world"));
        hello.append_child(world);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><autons0:hello xmlns:autons0='outer'><autons1:world xmlns:autons1='inner'/></autons0:hello>");
    }

    #[test]
    fn nested_empty_element_with_namespaces() {
        let p = Package::new();
        let d = p.as_document();

        let hello = d.create_element(("outer", "hello"));
        hello.set_default_namespace_uri(Some("outer"));
        hello.set_preferred_prefix(Some("o"));

        let world = d.create_element("world");
        world.set_default_namespace_uri(Some("inner"));

        let empty = d.create_element("empty");
        world.append_child(empty);
        hello.append_child(world);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello xmlns='outer' xmlns:o='outer'><world xmlns='inner'><empty/></world></hello>");
    }

    #[test]
    fn nested_element_with_namespaces_with_reused_namespaces() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element(("ns", "hello"));
        let world = d.create_element(("ns", "world"));
        hello.append_child(world);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><autons0:hello xmlns:autons0='ns'><autons0:world/></autons0:hello>");
    }

    #[test]
    fn nested_element_with_with_conflicting_preferred_namespace_prefixes() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element(("outer", "hello"));
        let world = d.create_element(("inner", "world"));
        hello.set_preferred_prefix(Some("p"));
        world.set_preferred_prefix(Some("p"));
        hello.append_child(world);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><p:hello xmlns:p='outer'><p:world xmlns:p='inner'/></p:hello>");
    }

    #[test]
    fn nested_text() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element("hello");
        let text = d.create_text("A fine day to you!");
        hello.append_child(text);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello>A fine day to you!</hello>");
    }

    #[test]
    fn text_escapes_less_than_greater_than_and_ampersand() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element("escaped");
        let text = d.create_text("1 < 3 & 4 > 2");
        hello.append_child(text);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><escaped>1 &lt; 3 &amp; 4 &gt; 2</escaped>");
    }

    #[test]
    fn nested_comment() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element("hello");
        let comment = d.create_comment(" Fill this in ");
        hello.append_child(comment);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello><!-- Fill this in --></hello>");
    }

    #[test]
    fn nested_processing_instruction_without_value() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element("hello");
        let pi = d.create_processing_instruction("display", None);
        hello.append_child(pi);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello><?display?></hello>");
    }

    #[test]
    fn nested_processing_instruction_with_value() {
        let p = Package::new();
        let d = p.as_document();
        let hello = d.create_element("hello");
        let pi = d.create_processing_instruction("display", Some("screen"));
        hello.append_child(pi);
        d.root().append_child(hello);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><hello><?display screen?></hello>");
    }

    #[test]
    fn top_level_comment() {
        let p = Package::new();
        let d = p.as_document();
        let comment = d.create_comment(" Fill this in ");
        d.root().append_child(comment);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><!-- Fill this in -->");
    }

    #[test]
    fn top_level_processing_instruction() {
        let p = Package::new();
        let d = p.as_document();
        let pi = d.create_processing_instruction("display", None);
        d.root().append_child(pi);

        let xml = format_xml(&d);
        assert_eq!(xml, "<?xml version='1.0'?><?display?>");
    }

    #[test]
    fn declaration_with_encoding() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        d.root().append_child(e);

        let xml = format_xml_writer(Writer::new().set_write_encoding(true), &d);
        assert_eq!(xml, "<?xml version='1.0' encoding='UTF-8'?><hello/>");
    }

    #[test]
    fn declaration_with_encoding_and_double_quotes() {
        let p = Package::new();
        let d = p.as_document();
        let e = d.create_element("hello");
        d.root().append_child(e);

        let xml = format_xml_writer(Writer::new()
                                    .set_write_encoding(true)
                                    .set_single_quotes(false), &d);
        assert_eq!(xml, r#"<?xml version="1.0" encoding="UTF-8"?><hello/>"#);
    }
}
