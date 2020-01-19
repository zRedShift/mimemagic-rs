use quick_xml::Reader;
use std::{fs::File, io::BufReader, process};

fn run() -> mimegen::error::Result<()> {
    let f = File::open("shared-mime-info/freedesktop.org.xml.in")?;
    let mut reader = Reader::from_reader(BufReader::new(f));
    reader.trim_text(true);
    let mime_info = mimegen::MimeInfo::from_xml(&mut reader)?;
    dbg!(mime_info);
    Ok(())
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        process::exit(1);
    }
}
