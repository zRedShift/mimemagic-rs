use error::{Error, Result};
use quick_xml::{
    events::{attributes::Attributes, Event},
    Reader,
};
use std::{
    cmp::Ordering,
    collections::HashMap,
    convert::{TryFrom, TryInto},
    fs,
    hash::{Hash, Hasher},
    io::BufRead,
    path::{Path, PathBuf},
};

pub mod error;

fn hex_to_val(c: u8) -> Option<u8> {
    match c {
        b'0'..=b'9' => Some(c - b'0'),
        b'a'..=b'f' => Some(c - b'a' + 10),
        b'A'..=b'F' => Some(c - b'A' + 10),
        _ => None,
    }
}

fn unescape(s: &str) -> Vec<u8> {
    let mut unescaped: Vec<u8> = vec![];
    let mut bytes = s.as_bytes().iter().peekable();

    while let Some(&c) = bytes.next() {
        if c != b'\\' {
            unescaped.push(c);
            continue;
        }
        if let Some(&c) = bytes.next() {
            unescaped.push(match c {
                b'n' => b'\n',
                b'r' => b'\r',
                b'b' => b'\x08',
                b't' => b'\t',
                b'f' => b'\x0C',
                b'v' => b'\x0B',
                b'0'..=b'7' => {
                    let mut val = c - b'0';
                    if let Some(&&c @ b'0'..=b'7') = bytes.peek() {
                        val = (val << 3) + (c - b'0');
                        bytes.next();
                        if let Some(&&c @ b'0'..=b'7') = bytes.peek() {
                            val = (val << 3) + (c - b'0');
                            bytes.next();
                        }
                    }
                    val
                }
                b'x' => {
                    let mut val = b'x';
                    if let Some(b) = bytes.peek().and_then(|&&c| hex_to_val(c)) {
                        val = b;
                        bytes.next();
                        if let Some(b) = bytes.peek().and_then(|&&c| hex_to_val(c)) {
                            val = (val << 4) + b;
                            bytes.next();
                        }
                    }
                    val
                }
                _ => c,
            })
        }
    }
    unescaped
}

fn hex_to_bytes(s: &str) -> Result<Vec<u8>> {
    if !s.starts_with("0x") {
        return Err(Error::InvalidValue);
    }
    let s = &s.as_bytes()[2..];
    let mut bytes = Vec::with_capacity((s.len() + 1) / 2);
    for i in if s.len() % 2 == 1 {
        bytes.push(hex_to_val(s[0]).ok_or(Error::InvalidValue)?);
        1..s.len()
    } else {
        0..s.len()
    }
    .step_by(2)
    {
        bytes.push(
            hex_to_val(s[i])
                .and_then(|a| hex_to_val(s[i + 1]).map(|b| (a << 4) + b))
                .ok_or(Error::InvalidValue)?,
        );
    }
    Ok(bytes)
}

fn value_to_vec(s: &str, w: usize, le: bool) -> Result<Vec<u8>> {
    let (s, radix) = if s.starts_with("0x") {
        (&s[2..], 16)
    } else if s.starts_with('0') {
        (s, 8)
    } else {
        (s, 10)
    };
    let value: usize = usize::from_str_radix(s, radix)?;
    if value > (1 << (w * 8)) - 1 {
        return Err(Error::InvalidValue);
    }
    Ok(if le {
        value.to_le_bytes()[..w].to_vec()
    } else {
        value.to_be_bytes()[std::mem::size_of::<usize>() - w..].to_vec()
    })
}

fn parse_values(r#type: &str, value: &str, mask: Option<&str>) -> Result<(Vec<u8>, Vec<u8>)> {
    let (w, le) = match r#type {
        "string" => {
            let value = unescape(&value);
            let mask = if let Some(s) = mask { hex_to_bytes(s)? } else { vec![] };
            return if !value.is_empty() && (mask.is_empty() || mask.len() == value.len()) {
                Ok((value, mask))
            } else {
                Err(Error::InvalidValue)
            };
        }
        "big16" => (2, false),
        "big32" => (4, false),
        "little16" => (2, true),
        "little32" => (4, true),
        "host16" => (2, cfg!(target_endian = "little")),
        "host32" => (4, cfg!(target_endian = "little")),
        "byte" => (1, false),
        _ => return Err(Error::InvalidMatchType),
    };

    Ok((
        value_to_vec(&value, w, le)?,
        if let Some(s) = mask { value_to_vec(&s, w, le)? } else { vec![] },
    ))
}

fn unsorted_union<T: PartialEq>(curr: &mut Vec<T>, other: Vec<T>) {
    for val in other {
        if !curr.iter().any(|c| c == &val) {
            curr.push(val);
        }
    }
}

fn extract_single_attr<R: BufRead>(
    reader: &mut Reader<R>,
    mut attributes: Attributes,
    elem: &'static str,
    key: &'static str,
) -> Result<String> {
    for attr in attributes.with_checks(false) {
        return match attr {
            Ok(attr) if attr.key == key.as_bytes() => {
                attr.unescape_and_decode_value(reader).map_err(Error::Xml)
            }
            Err(e) => Err(Error::Xml(e)),
            _ => continue,
        };
    }
    Err(Error::MissingAttr { attr: key, elem })
}

fn extract_priority<R: BufRead>(reader: &mut Reader<R>, attributes: Attributes) -> Result<u8> {
    match extract_single_attr(reader, attributes, "", "priority") {
        Ok(s) => s.parse().map_err(Error::Int),
        Err(Error::MissingAttr { .. }) => Ok(50),
        Err(e) => Err(e),
    }
}

#[derive(Debug, Clone, Eq)]
pub struct MimeType {
    value: String,
    sep: usize,
}

impl MimeType {
    pub fn media(&self) -> &str {
        &self.value[..self.sep]
    }

    pub fn subtype(&self) -> &str {
        &self.value[self.sep + 1..]
    }
}

impl PartialEq for MimeType {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Hash for MimeType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl TryFrom<String> for MimeType {
    type Error = Error;
    fn try_from(s: String) -> Result<Self> {
        match s.as_bytes().iter().position(|&b| b == b'/') {
            Some(sep) => match &s[..sep] {
                "all" | "uri" | "print" | "text" | "application" | "image" | "audio" | "inode"
                | "video" | "message" | "model" | "multipart" | "x-content" | "x-epoc"
                | "x-scheme-handler" | "font" => Ok(Self { value: s, sep }),
                _ => Err(Error::InvalidType(s)),
            },
            None => Err(Error::InvalidType(s)),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Glob {
    pattern: String,
    weight: u8,
    case_sensitive: bool,
}

impl Glob {
    fn from_xml<R: BufRead>(reader: &mut Reader<R>, attributes: Attributes) -> Result<Self> {
        let mut pattern = None;
        let mut weight = 50;
        let mut case_sensitive = false;

        for attr in attributes {
            let attr = attr?;
            match attr.key {
                b"pattern" => pattern = Some(attr.unescape_and_decode_value(reader)?),
                b"weight" => weight = attr.unescape_and_decode_value(reader)?.parse()?,
                b"case-sensitive" => {
                    case_sensitive = attr.unescape_and_decode_value(reader)?.parse()?
                }
                _ => {}
            };
        }
        let pattern = match pattern {
            Some(pattern) if !case_sensitive => pattern.to_ascii_lowercase(),
            Some(pattern) => pattern,
            _ => return Err(Error::MissingAttr { attr: "pattern", elem: "glob" }),
        };
        Ok(Self { pattern, weight, case_sensitive })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Offset {
    Index(usize),
    Range { from: usize, to: usize },
}

#[derive(Debug, PartialEq)]
pub struct Match {
    pub offset: Offset,
    pub value: Vec<u8>,
    pub mask: Vec<u8>,
    pub matches: Vec<Match>,
}

impl Match {
    fn unmask(offset: usize, value: Vec<u8>, mask: Vec<u8>, matches: Vec<Self>) -> Self {
        let mut prev = 0;
        let mut dummy = vec![];
        let mut r = &mut dummy;
        for (i, &m) in mask.iter().enumerate() {
            match (m, mask[prev]) {
                (0xff, 0xff) => {}
                (_, 0x00) => prev = i,
                (_, 0xff) => {
                    r.push(Self {
                        offset: Offset::Index(offset + prev),
                        value: value[prev..i].to_vec(),
                        mask: vec![],
                        matches: vec![],
                    });
                    r = &mut r[0].matches;
                    prev = i
                }
                (0xff, _) | (0x00, _) => {
                    r.push(Self {
                        offset: Offset::Index(offset + prev),
                        value: value[prev..i].to_vec(),
                        mask: mask[prev..i].to_vec(),
                        matches: vec![],
                    });
                    r = &mut r[0].matches;
                    prev = i
                }
                _ => {}
            }
        }
        match mask[prev] {
            0x00 => *r = matches,
            m => r.push(Self {
                offset: Offset::Index(offset + prev),
                value: value[prev..].to_vec(),
                mask: if m == 0xff { vec![] } else { mask[prev..].to_vec() },
                matches,
            }),
        }
        dummy.remove(0)
    }

    fn inner_xml<R: BufRead>(reader: &mut Reader<R>) -> Result<Vec<Self>> {
        let mut buf = vec![];
        let mut matches = vec![];
        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(e) if e.name() == b"match" => {
                    matches.push(Match::from_xml(reader, e.attributes(), false)?)
                }
                Event::Empty(e) if e.name() == b"match" => {
                    matches.push(Match::from_xml(reader, e.attributes(), true)?)
                }
                Event::End(_) => break,
                _ => {}
            }
            buf.clear();
        }

        Ok(matches)
    }

    fn from_xml<R: BufRead>(
        reader: &mut Reader<R>,
        attributes: Attributes,
        empty: bool,
    ) -> Result<Self> {
        let mut offset = None;
        let mut r#type = None;
        let mut value = None;
        let mut mask = None;

        for attr in attributes {
            let attr = attr?;
            match attr.key {
                b"offset" => {
                    let s = attr.unescape_and_decode_value(reader)?;
                    let mut iter = s.splitn(2, ':').map(str::parse);
                    let from = match iter.next() {
                        None => return Err(Error::InvalidOffset),
                        Some(Err(e)) => return Err(Error::Int(e)),
                        Some(Ok(from)) => from,
                    };
                    offset = Some(match iter.next() {
                        None => Offset::Index(from),
                        Some(Err(e)) => return Err(Error::Int(e)),
                        Some(Ok(to)) => match to.cmp(&from) {
                            Ordering::Less => return Err(Error::InvalidOffset),
                            Ordering::Equal => Offset::Index(from),
                            Ordering::Greater => Offset::Range { from, to },
                        },
                    });
                }
                b"type" => r#type = Some(attr.unescape_and_decode_value(reader)?),
                b"value" => value = Some(attr.unescape_and_decode_value(reader)?),
                b"mask" => mask = Some(attr.unescape_and_decode_value(reader)?),
                _ => {}
            };
        }
        let offset = offset.ok_or(Error::MissingAttr { attr: "offset", elem: "match" })?;
        let r#type = r#type.ok_or(Error::MissingAttr { attr: "type", elem: "match" })?;
        let value = value.ok_or(Error::MissingAttr { attr: "value", elem: "match" })?;

        let (value, mask) = parse_values(&r#type, &value, mask.as_deref())?;
        if !mask.is_empty() && mask.iter().all(|&v| v == 0x00) {
            return Err(Error::InvalidValue);
        }

        let matches = if empty { vec![] } else { Self::inner_xml(reader)? };

        match (&offset, mask.is_empty()) {
            (Offset::Index(idx), false) => Ok(Self::unmask(*idx, value, mask, matches)),
            (Offset::Range { .. }, _) => Ok(Self { offset, value, mask: vec![], matches }),
            (Offset::Index(_), true) => Ok(Self { offset, value, mask, matches }),
        }
    }

    fn concatenate(matches: Vec<Self>) -> Vec<Self> {
        let mut result = vec![];
        for mut m in matches {
            m.matches = Self::concatenate(m.matches);
            match (&m.offset, m.mask.is_empty(), m.matches.is_empty()) {
                (Offset::Index(idx), true, false) => {
                    let offset = Offset::Index(idx + m.value.len());
                    let mut invalid = vec![];
                    for mm in m.matches {
                        if mm.offset == offset && mm.mask.is_empty() {
                            result.push(Self {
                                offset: m.offset.clone(),
                                value: [&m.value[..], &mm.value[..]].concat(),
                                mask: vec![],
                                matches: mm.matches,
                            })
                        } else {
                            invalid.push(mm);
                        }
                    }
                    if !invalid.is_empty() {
                        m.matches = invalid;
                        result.push(m);
                    }
                }
                _ => result.push(m),
            }
        }
        result
    }
}

#[derive(Debug, PartialEq)]
pub struct Magic {
    priority: u8,
    pub matches: Vec<Match>,
}

impl Magic {
    fn from_xml<R: BufRead>(reader: &mut Reader<R>, attributes: Attributes) -> Result<Self> {
        let priority = extract_priority(reader, attributes)?;
        let matches = Match::concatenate(Match::inner_xml(reader)?);

        if matches.is_empty() {
            Err(Error::MissingElem { top: "magic", sub: "match" })
        } else {
            Ok(Self { priority, matches })
        }
    }
}

#[derive(Debug, PartialEq)]
enum Descriptor {
    None,
    File,
    Directory,
    Link,
}

impl Descriptor {
    fn from_str(s: &str) -> Self {
        use Descriptor::*;
        match s {
            "file" => File,
            "directory" => Directory,
            "link" => Link,
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct TreeMatch {
    path: String,
    descriptor: Descriptor,
    mime_type: Option<MimeType>,
    match_case: bool,
    executable: bool,
    non_empty: bool,
    matches: Vec<TreeMatch>,
}

impl TreeMatch {
    fn inner_xml<R: BufRead>(reader: &mut Reader<R>) -> Result<Vec<Self>> {
        let mut buf = vec![];
        let mut matches = vec![];
        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(e) if e.name() == b"treematch" => {
                    matches.push(TreeMatch::from_xml(reader, e.attributes(), false)?)
                }
                Event::Empty(e) if e.name() == b"treematch" => {
                    matches.push(TreeMatch::from_xml(reader, e.attributes(), true)?)
                }
                Event::End(_) => break,
                _ => {}
            }
            buf.clear();
        }

        Ok(matches)
    }

    fn from_xml<R: BufRead>(
        reader: &mut Reader<R>,
        attributes: Attributes,
        empty: bool,
    ) -> Result<Self> {
        let mut path = None;
        let mut descriptor = Descriptor::None;
        let mut mime_type = None;
        let mut match_case = false;
        let mut executable = false;
        let mut non_empty = false;

        for attr in attributes {
            let attr = attr?;
            match attr.key {
                b"path" => path = Some(attr.unescape_and_decode_value(reader)?),
                b"type" => {
                    descriptor = Descriptor::from_str(&attr.unescape_and_decode_value(reader)?)
                }
                b"match-case" => match_case = attr.unescape_and_decode_value(reader)?.parse()?,
                b"executable" => executable = attr.unescape_and_decode_value(reader)?.parse()?,
                b"non-empty" => non_empty = attr.unescape_and_decode_value(reader)?.parse()?,
                b"mimetype" => {
                    mime_type = Some(attr.unescape_and_decode_value(reader)?.try_into()?)
                }
                _ => {}
            };
        }
        let path = path.ok_or(Error::MissingAttr { attr: "path", elem: "treematch" })?;
        let matches = if empty { vec![] } else { Self::inner_xml(reader)? };

        Ok(Self { path, descriptor, mime_type, match_case, executable, non_empty, matches })
    }
}

#[derive(Debug, PartialEq)]
pub struct TreeMagic {
    priority: u8,
    pub matches: Vec<TreeMatch>,
}

impl TreeMagic {
    fn from_xml<R: BufRead>(reader: &mut Reader<R>, attributes: Attributes) -> Result<Self> {
        let priority = extract_priority(reader, attributes)?;
        let matches = TreeMatch::inner_xml(reader)?;

        if matches.is_empty() {
            Err(Error::MissingElem { top: "treemagic", sub: "treematch" })
        } else {
            Ok(Self { priority, matches })
        }
    }
}

#[derive(Debug, PartialEq)]
struct RootXML {
    namespace_uri: String,
    local_name: String,
}

impl RootXML {
    fn from_xml<R: BufRead>(reader: &mut Reader<R>, attributes: Attributes) -> Result<Self> {
        let mut namespace_uri = None;
        let mut local_name = None;

        for attr in attributes {
            let attr = attr?;
            match attr.key {
                b"namespaceURI" => namespace_uri = Some(attr.unescape_and_decode_value(reader)?),
                b"localName" => local_name = Some(attr.unescape_and_decode_value(reader)?),
                _ => {}
            };
        }
        let namespace_uri =
            namespace_uri.ok_or(Error::MissingAttr { attr: "namespaceURI", elem: "root-XML" })?;
        let local_name =
            local_name.ok_or(Error::MissingAttr { attr: "localName", elem: "root-XML" })?;
        Ok(Self { namespace_uri, local_name })
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum GenericIcon {
    None,
    Executable,
    Audio,
    Folder,
    Font,
    Image,
    Package,
    Html,
    Text,
    Template,
    Script,
    Video,
    AddressBook,
    Calendar,
    Document,
    Presentation,
    Spreadsheet,
}

impl GenericIcon {
    fn from_str(s: &str) -> Self {
        use GenericIcon::*;
        match s {
            "application-x-executable" => Executable,
            "audio-x-generic" => Audio,
            "folder" => Folder,
            "font-x-generic" => Font,
            "image-x-generic" => Image,
            "package-x-generic" => Package,
            "text-html" => Html,
            "text-x-generic" => Text,
            "text-x-generic-template" => Template,
            "text-x-script" => Script,
            "video-x-generic" => Video,
            "x-office-address-book" => AddressBook,
            "x-office-calendar" => Calendar,
            "x-office-document" => Document,
            "x-office-presentation" => Presentation,
            "x-office-spreadsheet" => Spreadsheet,
            _ => None,
        }
    }
}

impl Default for GenericIcon {
    fn default() -> Self {
        GenericIcon::None
    }
}

#[derive(Debug, PartialEq, Default)]
pub struct Entry {
    comment: String,
    acronym: Option<String>,
    expanded_acronym: Option<String>,
    icon: Option<String>,
    pub generic_icon: GenericIcon,
    globs: Vec<Glob>,
    pub magics: Vec<Magic>,
    pub tree_magics: Vec<TreeMagic>,
    root_xmls: Vec<RootXML>,
    pub aliases: Vec<MimeType>,
    pub sub_class_of: Vec<MimeType>,
    glob_delete_all: bool,
    magic_delete_all: bool,
}

impl Entry {
    fn from_xml<R: BufRead>(reader: &mut Reader<R>) -> Result<Self> {
        let buf = &mut vec![];
        let skip_buf = &mut vec![];
        let mut comment = String::new();
        let mut acronym = None;
        let mut expanded_acronym = None;
        let mut icon = None;
        let mut generic_icon = GenericIcon::None;
        let mut globs = vec![];
        let mut magics = vec![];
        let mut tree_magics = vec![];
        let mut root_xmls = vec![];
        let mut aliases = vec![];
        let mut sub_class_of = vec![];
        let mut glob_delete_all = false;
        let mut magic_delete_all = false;

        loop {
            match reader.read_event(buf)? {
                Event::Start(e) => match e.name() {
                    b"comment" | b"_comment"
                        if comment.is_empty() && e.attributes().next().is_none() =>
                    {
                        comment = reader.read_text(e.name(), skip_buf)?
                    }
                    b"acronym" if acronym == None && e.attributes().next().is_none() => {
                        acronym = Some(reader.read_text(e.name(), skip_buf)?)
                    }
                    b"expanded-acronym"
                        if expanded_acronym == None && e.attributes().next().is_none() =>
                    {
                        expanded_acronym = Some(reader.read_text(e.name(), skip_buf)?)
                    }
                    b"magic" => magics.push(Magic::from_xml(reader, e.attributes())?),
                    b"treemagic" => tree_magics.push(TreeMagic::from_xml(reader, e.attributes())?),
                    _ => reader.read_to_end(e.name(), skip_buf)?,
                },
                Event::Empty(e) => match e.name() {
                    b"icon" => {
                        icon = Some(extract_single_attr(reader, e.attributes(), "icon", "name")?)
                    }
                    b"generic-icon" if generic_icon == GenericIcon::None => {
                        generic_icon = GenericIcon::from_str(&extract_single_attr(
                            reader,
                            e.attributes(),
                            "generic-icon",
                            "name",
                        )?)
                    }
                    b"glob" => globs.push(Glob::from_xml(reader, e.attributes())?),
                    b"root-XML" => root_xmls.push(RootXML::from_xml(reader, e.attributes())?),
                    b"alias" => {
                        if let Ok(alias) =
                            extract_single_attr(reader, e.attributes(), "alias", "type")?.try_into()
                        {
                            aliases.push(alias)
                        }
                    }
                    b"sub-class-of" => sub_class_of.push(
                        extract_single_attr(reader, e.attributes(), "sub-class-of", "type")?
                            .try_into()?,
                    ),
                    b"glob-deleteall" => glob_delete_all = true,
                    b"magic-deleteall" => magic_delete_all = true,

                    _ => {}
                },
                Event::End(_) => break,
                _ => {}
            }
            buf.clear();
            skip_buf.clear();
        }

        Ok(Entry {
            comment,
            acronym,
            expanded_acronym,
            icon,
            generic_icon,
            globs,
            magics,
            tree_magics,
            root_xmls,
            aliases,
            sub_class_of,
            glob_delete_all,
            magic_delete_all,
        })
    }

    fn combine_with(&mut self, other: Self) {
        if !other.comment.is_empty() {
            self.comment = other.comment;
        }
        if other.acronym != None {
            self.acronym = other.acronym;
        }
        if other.expanded_acronym != None {
            self.expanded_acronym = other.expanded_acronym;
        }
        if other.icon != None {
            self.icon = other.icon;
        }
        if other.generic_icon != GenericIcon::None {
            self.generic_icon = other.generic_icon;
        }
        unsorted_union(&mut self.root_xmls, other.root_xmls);
        unsorted_union(&mut self.aliases, other.aliases);
        unsorted_union(&mut self.sub_class_of, other.sub_class_of);
        if other.glob_delete_all {
            self.globs = other.globs;
        } else {
            for glob in other.globs {
                match self.globs.iter_mut().find_map(|g| {
                    match (g.case_sensitive == glob.case_sensitive, glob.case_sensitive) {
                        (true, _) if g.pattern == glob.pattern => Some(Some(g)),
                        (false, true)
                            if glob.weight <= g.weight
                                && g.pattern == glob.pattern.to_ascii_lowercase() =>
                        {
                            Some(None)
                        }
                        _ => None,
                    }
                }) {
                    Some(Some(g)) => g.weight = glob.weight,
                    None => self.globs.push(glob),
                    _ => {}
                }
            }
        }
        if other.magic_delete_all {
            self.magics = other.magics;
        } else {
            for magic in other.magics {
                match self.magics.iter_mut().find(|m| m.matches == magic.matches) {
                    Some(m) => m.priority = magic.priority,
                    None => match self.magics.iter_mut().find(|m| m.priority == magic.priority) {
                        Some(m) => unsorted_union(&mut m.matches, magic.matches),
                        None => self.magics.push(magic),
                    },
                }
            }
        }
        for tree in other.tree_magics {
            match self.tree_magics.iter_mut().find(|t| t.matches == tree.matches) {
                Some(t) => t.priority = tree.priority,
                None => match self.tree_magics.iter_mut().find(|t| t.priority == tree.priority) {
                    Some(t) => unsorted_union(&mut t.matches, tree.matches),
                    None => self.tree_magics.push(tree),
                },
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct MimeInfo(pub HashMap<MimeType, Entry>);

impl MimeInfo {
    pub fn add_from_xml<R: BufRead>(&mut self, reader: &mut Reader<R>) -> Result<()> {
        let mut buf = vec![];
        loop {
            match reader.read_event(&mut buf)? {
                Event::End(_) => break,
                Event::Start(e) if e.name() == b"mime-type" => {
                    let mime_type =
                        extract_single_attr(reader, e.attributes(), "mime-type", "type")?
                            .try_into()?;
                    self.0.entry(mime_type).or_default().combine_with(Entry::from_xml(reader)?);
                }
                _ => {}
            }
            buf.clear();
        }
        Ok(())
    }

    pub fn add_from_file<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
        let reader = &mut Reader::from_file(path)?;
        reader.trim_text(true);
        self.add_from_xml(reader)?;
        Ok(())
    }

    pub fn add_from_dir<P: AsRef<Path>>(&mut self, dir: P) -> Result<()> {
        let mut override_xml = None;
        let mut paths: Vec<PathBuf> = fs::read_dir(dir)?
            .filter_map(|res| match res {
                Err(e) => Some(Err(Error::Io(e))),
                Ok(entry) => {
                    let path = entry.path();
                    if !path.is_file() || path.extension() != Some("xml".as_ref()) {
                        None
                    } else if path.file_name().unwrap() == "Override.xml" {
                        override_xml = Some(path);
                        None
                    } else {
                        Some(Ok(path))
                    }
                }
            })
            .collect::<Result<_>>()?;
        paths.sort();
        for path in paths {
            self.add_from_file(path)?;
        }
        if let Some(path) = override_xml {
            self.add_from_file(path)?;
        }
        Ok(())
    }

    pub fn from_xml<R: BufRead>(reader: &mut Reader<R>) -> Result<Self> {
        let mut entries = Self::default();
        entries.add_from_xml(reader)?;
        Ok(entries)
    }

    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
        let mut entries = Self::default();
        entries.add_from_file(path)?;
        Ok(entries)
    }

    pub fn from_files<P: AsRef<Path>>(paths: &[P]) -> Result<Self> {
        let mut entries = Self::default();
        for path in paths {
            entries.add_from_file(path)?;
        }
        Ok(entries)
    }

    pub fn from_dir<P: AsRef<Path>>(dir: P) -> Result<Self> {
        let mut entries = Self::default();
        entries.add_from_dir(dir)?;
        Ok(entries)
    }

    pub fn from_dirs<P: AsRef<Path>>(dirs: &[P]) -> Result<Self> {
        let mut entries = Self::default();
        for dir in dirs {
            entries.add_from_dir(dir)?;
        }
        Ok(entries)
    }

    //    pub fn process_entries(self) {
    //        let mut mime_types = Vec::with_capacity(self.0.len());
    //        for (i, (mime_type, entry)) in self.0.into_iter().enumerate() {
    //            mime_types.push(mime_type);
    //        }
    //    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_globs() {
        let tests = [
            (
                br#"<glob pattern="[0-9][0-9][0-9].vdr"/>"#.as_ref(),
                Glob { pattern: "[0-9][0-9][0-9].vdr".into(), weight: 50, case_sensitive: false },
            ),
            (
                br#"<glob pattern="*.anim[1-9j]"/>"#,
                Glob { pattern: "*.anim[1-9j]".into(), weight: 50, case_sensitive: false },
            ),
            (
                br#"<glob weight="60" pattern="*.appimage"/>"#,
                Glob { pattern: "*.appimage".into(), weight: 60, case_sensitive: false },
            ),
            (
                br#"<glob pattern="*.C" case-sensitive="true"/>"#,
                Glob { pattern: "*.C".into(), weight: 50, case_sensitive: true },
            ),
        ];
        for test in tests.iter() {
            let reader = &mut Reader::from_reader(test.0);
            reader.trim_text(true);
            let glob = match reader.read_event(&mut Vec::new()).unwrap() {
                Event::Empty(e) => Glob::from_xml(reader, e.attributes()).unwrap(),
                event => panic!("expected Event::Empty, found {:?}", event),
            };
            assert_eq!(glob, test.1);
        }
    }

    #[test]
    fn parse_magic() {
        let tests = [
            (
                br#"<magic priority="60">
                        <match type="little32" mask="0x8080ffff" value="0x0000081a" offset="0"/>
                    </magic>"#
                    .as_ref(),
                Magic {
                    priority: 60,
                    matches: vec![Match {
                        offset: Offset::Index(0),
                        value: vec![0x1a, 0x08],
                        mask: vec![],
                        matches: vec![Match {
                            offset: Offset::Index(2),
                            value: vec![0x00, 0x00],
                            mask: vec![0x80, 0x80],
                            matches: vec![],
                        }],
                    }],
                },
            ),
            (
                br#"<magic priority="50">
                        <match type="string" value="ADIF" offset="0"/>
                        <match type="big16" value="0xFFF0" mask="0xFFF6" offset="0"/>
                    </magic>"#,
                Magic {
                    priority: 50,
                    matches: vec![
                        Match {
                            offset: Offset::Index(0),
                            value: b"ADIF".to_vec(),
                            mask: vec![],
                            matches: vec![],
                        },
                        Match {
                            offset: Offset::Index(0),
                            value: vec![0xff],
                            mask: vec![],
                            matches: vec![Match {
                                offset: Offset::Index(1),
                                value: vec![0xf0],
                                mask: vec![0xf6],
                                matches: vec![],
                            }],
                        },
                    ],
                },
            ),
            (
                br#"<magic priority="50">
                        <match type="little16" value="0603" offset="0">
                            <match type="little16" mask="030000" value="020000" offset="22"/>
                        </match>
                    </magic>"#,
                Magic {
                    priority: 50,
                    matches: vec![Match {
                        offset: Offset::Index(0),
                        value: vec![0x83, 0x01],
                        mask: vec![],
                        matches: vec![Match {
                            offset: Offset::Index(23),
                            value: vec![0x20],
                            mask: vec![0x30],
                            matches: vec![],
                        }],
                    }],
                },
            ),
            (
                br#"<magic priority="50">
                        <match type="host32" value="0xa1b2c3d4" offset="0"/>
                        <match type="host32" value="0xd4c3b2a1" offset="0"/>
                    </magic>"#,
                Magic {
                    priority: 50,
                    matches: vec![
                        Match {
                            offset: Offset::Index(0),
                            value: 0xa1b2_c3d4u32.to_ne_bytes().to_vec(),
                            mask: vec![],
                            matches: vec![],
                        },
                        Match {
                            offset: Offset::Index(0),
                            value: 0xd4c3_b2a1u32.to_ne_bytes().to_vec(),
                            mask: vec![],
                            matches: vec![],
                        },
                    ],
                },
            ),
            (
                br#"<magic priority="60">
                        <match type="host16" value="0143561" offset="0"/>
                    </magic>"#,
                Magic {
                    priority: 60,
                    matches: vec![Match {
                        offset: Offset::Index(0),
                        value: 0o143_561u16.to_ne_bytes().to_vec(),
                        mask: vec![],
                        matches: vec![],
                    }],
                },
            ),
            (
                br#"<magic priority="50"><match type="string"
                mask="0xffffffffffffffffffffffff0000000000000000ffffffff"
                value="\x00\x00\x00\x0c\x6a\x50\x20\x20\x0d\x0a\x87\x0a        jpx\x20"
                offset="0"/></magic>"#,
                Magic {
                    priority: 50,
                    matches: vec![Match {
                        offset: Offset::Index(0),
                        value: vec![
                            0x00, 0x00, 0x00, 0x0c, 0x6a, 0x50, 0x20, 0x20, 0x0d, 0x0a, 0x87, 0x0a,
                        ],
                        mask: vec![],
                        matches: vec![Match {
                            offset: Offset::Index(20),
                            value: vec![0x6a, 0x70, 0x78, 0x20],
                            mask: vec![],
                            matches: vec![],
                        }],
                    }],
                },
            ),
            (
                br#"<magic priority="40">
                        <match type="string" value="\177ELF" offset="0">
                            <match type="byte" value="2" offset="5">
                                <match type="big16" value="2" offset="16"/>
                            </match>
                        </match>
                    </magic>"#,
                Magic {
                    priority: 40,
                    matches: vec![Match {
                        offset: Offset::Index(0),
                        value: vec![0x7f, 0x45, 0x4c, 0x46],
                        mask: vec![],
                        matches: vec![Match {
                            offset: Offset::Index(5),
                            value: vec![0x02],
                            mask: vec![],
                            matches: vec![Match {
                                offset: Offset::Index(16),
                                value: vec![0x00, 0x02],
                                mask: vec![],
                                matches: vec![],
                            }],
                        }],
                    }],
                },
            ),
            (
                br#"<magic priority="50">
                        <match type="string" value="!&lt;arch&gt;" offset="0">
                            <match type="string" value="debian" offset="8"/>
                        </match>
                    </magic>"#,
                Magic {
                    priority: 50,
                    matches: vec![Match {
                        offset: Offset::Index(0),
                        value: b"!<arch>".to_vec(),
                        mask: vec![],
                        matches: vec![Match {
                            offset: Offset::Index(8),
                            value: b"debian".to_vec(),
                            mask: vec![],
                            matches: vec![],
                        }],
                    }],
                },
            ),
            (
                br#"<magic priority="70">
                        <match type="string" value="PK\003\004" offset="0">
                            <!-- Comment Comment Comment <Comment> "Comment" -->
                            <match type="string" value=".fb2" offset="30:256"/>
                        </match>
                    </magic>"#,
                Magic {
                    priority: 70,
                    matches: vec![Match {
                        offset: Offset::Index(0),
                        value: b"PK\x03\x04".to_vec(),
                        mask: vec![],
                        matches: vec![Match {
                            offset: Offset::Range { from: 30, to: 256 },
                            value: b".fb2".to_vec(),
                            mask: vec![],
                            matches: vec![],
                        }],
                    }],
                },
            ),
            (
                br#"<magic priority="70">
                        <match type="string" value="PK\003\004" offset="0">
                            <match type="string" value="mimetype" offset="30">
                                <match type="string" value="application/epub+zip" offset="38"/>
                                <match type="string" value="application/epub+zip" offset="43"/>
                            </match>
                        </match>
                    </magic>"#,
                Magic {
                    priority: 70,
                    matches: vec![Match {
                        offset: Offset::Index(0),
                        value: b"PK\x03\x04".to_vec(),
                        mask: vec![],
                        matches: vec![
                            Match {
                                offset: Offset::Index(30),
                                value: b"mimetypeapplication/epub+zip".to_vec(),
                                mask: vec![],
                                matches: vec![],
                            },
                            Match {
                                offset: Offset::Index(30),
                                value: b"mimetype".to_vec(),
                                mask: vec![],
                                matches: vec![Match {
                                    offset: Offset::Index(43),
                                    value: b"application/epub+zip".to_vec(),
                                    mask: vec![],
                                    matches: vec![],
                                }],
                            },
                        ],
                    }],
                },
            ),
        ];

        for test in tests.iter() {
            let reader = &mut Reader::from_reader(test.0);
            reader.trim_text(true);
            let magic = match reader.read_event(&mut Vec::new()).unwrap() {
                Event::Start(e) => Magic::from_xml(reader, e.attributes()).unwrap(),
                event => panic!("expected Event::Start, found {:?}", event),
            };
            assert_eq!(magic, test.1);
        }
    }

    #[test]
    fn parse_mime_type() {
        let (test, expected) = (
            br#"<mime-type type="image/x-pict">
                        <comment>Macintosh Quickdraw/PICT drawing</comment>
                        <magic priority="50">
                            <match type="big16" value="0x0011" offset="10">
                                <match type="big16" value="0x02FF" offset="12">
                                    <match type="big16" value="0x0C00" offset="14">
                                        <match type="big16" value="0xFFFE" offset="16"/>
                                    </match>
                                </match>
                            </match>
                        </magic>
                        <magic priority="50">
                            <match type="big16" value="0x0011" offset="522">
                                <match type="big16" value="0x02FF" offset="524">
                                    <match type="big16" value="0x0C00" offset="526">
                                        <match type="big16" value="0xFFFE" offset="528"/>
                                    </match>
                                </match>
                            </match>
                        </magic>
                        <glob pattern="*.pct"/>
                        <glob pattern="*.pict"/>
                        <glob pattern="*.pict1"/>
                        <glob pattern="*.pict2"/>
                    </mime-type>"#
                .as_ref(),
            Entry {
                comment: "Macintosh Quickdraw/PICT drawing".into(),
                acronym: None,
                expanded_acronym: None,
                icon: None,
                generic_icon: GenericIcon::None,
                globs: vec![
                    Glob { pattern: "*.pct".into(), weight: 50, case_sensitive: false },
                    Glob { pattern: "*.pict".into(), weight: 50, case_sensitive: false },
                    Glob { pattern: "*.pict1".into(), weight: 50, case_sensitive: false },
                    Glob { pattern: "*.pict2".into(), weight: 50, case_sensitive: false },
                ],
                magics: vec![
                    Magic {
                        priority: 50,
                        matches: vec![Match {
                            offset: Offset::Index(10),
                            value: vec![0, 17, 2, 255, 12, 0, 255, 254],
                            mask: vec![],
                            matches: vec![],
                        }],
                    },
                    Magic {
                        priority: 50,
                        matches: vec![Match {
                            offset: Offset::Index(522),
                            value: vec![0, 17, 2, 255, 12, 0, 255, 254],
                            mask: vec![],
                            matches: vec![],
                        }],
                    },
                ],
                tree_magics: vec![],
                root_xmls: vec![],
                aliases: vec![],
                sub_class_of: vec![],
                glob_delete_all: false,
                magic_delete_all: false,
            },
        );
        let reader = &mut Reader::from_reader(test);
        reader.trim_text(true);
        let entry = match reader.read_event(&mut Vec::new()).unwrap() {
            Event::Start(_) => Entry::from_xml(reader).unwrap(),
            event => panic!("expected Event::Start, found {:?}", event),
        };
        assert_eq!(entry, expected);
    }
}
