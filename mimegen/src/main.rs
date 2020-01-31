use mimegen::*;
use std::collections::{BTreeMap, HashSet};

fn populate_hist<'a>(
    m: &'a Match,
    hist: &mut [u8],
    rset: &mut HashSet<&'a [u8]>,
    oset: &mut HashSet<(&'a [u8], usize)>,
    map: &mut BTreeMap<usize, (usize, usize, usize, usize)>,
) {
    match m.offset {
        Offset::Index(offset) if m.mask.is_empty() && oset.insert((&m.value, offset)) => {
            map.entry(offset)
                .and_modify(|entry| {
                    entry.0 += 1;
                    entry.1 += m.value.len();
                    entry.2 = entry.2.min(m.value.len());
                    entry.3 = entry.3.max(m.value.len());
                })
                .or_insert((1, m.value.len(), m.value.len(), m.value.len()));
        }
        Offset::Range(_) if rset.insert(&m.value) => hist[m.value.len()] += 1,
        _ => {}
    }
    for m in m.matches.iter() {
        populate_hist(m, hist, rset, oset, map);
    }
}

fn run() -> error::Result<()> {
    //    MimeInfo::from_file("shared-mime-info/freedesktop.org.xml.in")?;
    let mime_info = MimeInfo::from_dir("/usr/share/mime/packages/")?;
    let mut hist = [0u8; 256];
    let (mut rset, mut oset, mut map) = (HashSet::new(), HashSet::new(), BTreeMap::new());
    for m in mime_info.0.iter().flat_map(|e| e.1.magics.iter()).flat_map(|m| m.matches.iter()) {
        populate_hist(m, &mut hist, &mut rset, &mut oset, &mut map);
    }
    for (i, &x) in hist.iter().enumerate().filter(|(_, &x)| x != 0) {
        println!("{} length {} patterns", x, i);
    }
    for (offset, (count, tot, min, max)) in map {
        print!("{} patterns starting at {}, with lengths ", count, offset);
        println!("avg: {}, min: {}, max: {}", tot as f64 / count as f64, min, max);
    }
    Ok(())
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
