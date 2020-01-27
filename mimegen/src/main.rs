fn run() -> mimegen::error::Result<()> {
    //    let f = File::open("shared-mime-info/freedesktop.org.xml.in")?;
    //    let mut reader = Reader::from_reader(BufReader::new(f));
    //    reader.trim_text(true);
    //    let mime_info = mimegen::MimeInfo::from_xml(&mut reader)?;
    let mime_info = mimegen::MimeInfo::from_dirs(&["/usr/share/mime/packages/"])?;
    dbg!(mime_info);
    Ok(())
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
