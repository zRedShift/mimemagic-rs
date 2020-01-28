fn run() -> mimegen::error::Result<()> {
    println!(
        "File {:#?}",
        mimegen::MimeInfo::from_file("shared-mime-info/freedesktop.org.xml.in")?
    );
    println!("Dir {:#?}", mimegen::MimeInfo::from_dir("/usr/share/mime/packages/")?);
    Ok(())
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
