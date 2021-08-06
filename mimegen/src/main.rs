use mimegen::*;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::num::NonZeroUsize;
use std::ops::{Index, IndexMut, Range, RangeInclusive};
use std::slice::ChunksExact;

struct Edges {
    vec: Box<[bool]>,
    dim: usize,
}

impl Edges {
    fn with_dim(dim: NonZeroUsize) -> Self {
        let dim = dim.get();
        Edges { vec: vec![false; dim * dim].into_boxed_slice(), dim }
    }

    fn rows(&self) -> ChunksExact<bool> {
        self.vec.chunks_exact(self.dim)
    }
}

impl Index<usize> for Edges {
    type Output = [bool];
    fn index(&self, idx: usize) -> &Self::Output {
        self.vec.index(idx * (self.dim + 1) + 1..(idx + 1) * self.dim)
    }
}

impl Index<(usize, usize)> for Edges {
    type Output = bool;
    fn index(&self, (x, y): (usize, usize)) -> &Self::Output {
        let idx = self.dim * y + x;
        assert!(x < self.dim && idx < self.vec.len());
        self.vec.index(idx)
    }
}

impl IndexMut<(usize, usize)> for Edges {
    fn index_mut(&mut self, (x, y): (usize, usize)) -> &mut Self::Output {
        let idx = self.dim * y + x;
        assert!(x < self.dim && idx < self.vec.len());
        self.vec.index_mut(idx)
    }
}

const fn log2(x: usize) -> usize {
    std::mem::size_of::<usize>() * 8 - x.leading_zeros() as usize - 1
}

fn mutually_exclusive<'a>(
    mut first: &'a (usize, &'a [u8]),
    mut second: &'a (usize, &'a [u8]),
) -> bool {
    let mut diff = second.0 as isize - first.0 as isize;
    if diff < 0 {
        diff = -diff;
        std::mem::swap(&mut first, &mut second);
    }
    let shared = isize::min(first.1.len() as isize - diff, second.1.len() as isize);
    shared > 0 && first.1[diff as usize..(diff + shared) as usize] != second.1[..shared as usize]
}

const fn log2_ceil(x: usize) -> usize {
    let log2 = log2(x);
    if x == (1 << log2) {
        log2
    } else {
        log2 + 1
    }
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
enum MatchKind<'a> {
    Simple { offset: usize, signature: &'a [u8] },
    Masked { offset: usize, signature: &'a [u8], mask: &'a [u8] },
    Range { from: usize, to: usize, signature: &'a [u8] },
}

impl MatchKind<'_> {
    fn common_ranges(
        o_1: usize,
        l_1: usize,
        o_2: usize,
        l_2: usize,
    ) -> Option<(Range<usize>, Range<usize>)> {
        let diff = o_2 as isize - o_1 as isize;
        let shared;
        let ranges = if diff < 0 {
            let diff = -diff;
            shared = (l_2 as isize - diff).min(l_1 as isize);
            (0..shared as usize, diff as usize..(diff + shared) as usize)
        } else {
            shared = (l_1 as isize - diff).min(l_2 as isize);
            (diff as usize..(diff + shared) as usize, 0..shared as usize)
        };
        if shared > 0 {
            Some(ranges)
        } else {
            None
        }
    }

    fn mutually_exclusive_simple(o_1: usize, s_1: &[u8], o_2: usize, s_2: &[u8]) -> bool {
        match Self::common_ranges(o_1, s_1.len(), o_2, s_2.len()) {
            Some((r_1, r_2)) => s_1[r_1] != s_2[r_2],
            None => false,
        }
    }

    fn mutually_exclusive_masked(o: usize, s: &[u8], o_m: usize, s_m: &[u8], m: &[u8]) -> bool {
        if let Some((r_1, r_2)) = Self::common_ranges(o, s.len(), o_m, s_m.len()) {
            for (s, (s_m, m)) in s[r_1]
                .iter()
                .cloned()
                .zip(s_m[r_2.clone()].iter().cloned().zip(m[r_2].iter().cloned()))
            {
                if s & m != s_m {
                    return true;
                }
            }
        }
        false
    }

    fn mutually_exclusive(&self, other: &Self) -> bool {
        use MatchKind::*;
        match (self, other) {
            (&Simple { offset: o_1, signature: s_1 }, &Simple { offset: o_2, signature: s_2 }) => {
                Self::mutually_exclusive_simple(o_1, s_1, o_2, s_2)
            }
            (&Simple { offset, signature }, &Masked { offset: o_m, signature: s_m, mask })
            | (&Masked { offset: o_m, signature: s_m, mask }, &Simple { offset, signature }) => {
                Self::mutually_exclusive_masked(offset, signature, o_m, s_m, mask)
            }
            (&Simple { offset, signature }, &Range { from, to, signature: s_r })
            | (&Range { from, to, signature: s_r }, &Simple { offset, signature }) => {
                for o_r in from..=to {
                    if !Self::mutually_exclusive_simple(offset, signature, o_r, s_r) {
                        return false;
                    }
                }
                true
            }
            (&Masked { offset, signature, mask }, &Range { from, to, signature: s_r })
            | (&Range { from, to, signature: s_r }, &Masked { offset, signature, mask }) => {
                for o_r in from..=to {
                    if !Self::mutually_exclusive_masked(o_r, s_r, offset, signature, mask) {
                        return false;
                    }
                }
                true
            }
            (
                &Masked { offset: o_1, signature: s_1, mask: m_1 },
                &Masked { offset: o_2, signature: s_2, mask: m_2 },
            ) => match Self::common_ranges(o_1, s_1.len(), o_2, s_2.len()) {
                Some((r_1, r_2)) => {
                    assert!(r_1.end == r_1.start + 1 && r_2.end == r_2.start + 1);
                    let s_1 = s_1[r_1.start];
                    let s_2 = s_2[r_2.start];
                    let m_1 = m_1[r_1.start];
                    let m_2 = m_2[r_2.start];
                    for b in 0u8..=255u8 {
                        if (b & m_1 == s_1) == (b & m_2 == s_2) {
                            return false;
                        }
                    }
                    true
                }
                None => false,
            },
            (
                &Range { from: f_1, to: t_1, signature: s_1 },
                &Range { from: f_2, to: t_2, signature: s_2 },
            ) => {
                for o_1 in f_1..=t_1 {
                    for o_2 in f_2..=t_2 {
                        if !Self::mutually_exclusive_simple(o_1, s_1, o_2, s_2) {
                            return false;
                        }
                    }
                }
                true
            }
        }
    }
}

fn enumerate_matches<'a>(m: &'a Match, s: &mut HashSet<MatchKind<'a>>) {
    use MatchKind::*;
    match m.offset {
        Offset::Index(offset) => s.insert(if m.mask.is_empty() {
            Simple { offset, signature: &m.value }
        } else {
            Masked { offset, signature: &m.value, mask: &m.mask }
        }),
        Offset::Range { from, to } => s.insert(Range { from, to, signature: &m.value }),
    };
    for m in m.matches.iter() {
        enumerate_matches(m, s);
    }
}

fn bron_kerbosch(r: &mut HashSet<usize>, p: &mut HashSet<usize>, x: &mut HashSet<usize>) {}

fn populate_hist<'a>(
    m: &'a Match,
    hist: &mut [u8],
    ranges: &mut [u8],
    rset: &mut HashSet<&'a [u8]>,
    oset: &mut BTreeSet<(usize, &'a [u8])>,
    map: &mut BTreeMap<usize, (usize, usize, usize, usize)>,
    map2: &mut BTreeMap<usize, usize>,
) {
    match m.offset {
        Offset::Index(offset) if oset.insert((offset, &m.value)) => {
            if m.mask.is_empty() {
                map.entry(offset)
                    .and_modify(|entry| {
                        entry.0 += 1;
                        entry.1 += m.value.len();
                        entry.2 = entry.2.min(m.value.len());
                        entry.3 = entry.3.max(m.value.len());
                    })
                    .or_insert((1, m.value.len(), m.value.len(), m.value.len()));
                *map2.entry(offset + m.value.len() - 1).or_default() += 1;
            } else {
                println!("mask {:x?}", m.mask);
            }
        }
        Offset::Range { from, to } if rset.insert(&m.value) => {
            hist[m.value.len()] += 1;
            for x in &mut ranges[log2(from + 1)..=log2_ceil(to + 1)] {
                *x += 1;
            }
        }
        _ => {}
    }
    for m in m.matches.iter() {
        populate_hist(m, hist, ranges, rset, oset, map, map2);
    }
}

fn run() -> error::Result<()> {
    let mime_info = MimeInfo::from_file("shared-mime-info/data/freedesktop.org.xml.in")?;
    let mut matches = HashSet::new();
    for m in mime_info.0.iter().flat_map(|e| e.1.magics.iter().flat_map(|m| m.matches.iter())) {
        enumerate_matches(m, &mut matches);
    }
    let mut match_vec: Vec<_> = matches.iter().cloned().collect();
    match_vec.sort_unstable();
    let mut edges = Edges::with_dim(NonZeroUsize::new(match_vec.len()).unwrap());
    let mut iter = match_vec.iter().enumerate();
    let mut count = 0;
    while let Some((i, first)) = iter.next() {
        for (j, second) in iter.clone() {
            if first.mutually_exclusive(second) {
                edges[(i, j)] = true;
                edges[(j, i)] = true;
                count += 1;
            }
        }
    }

    for row in edges.rows() {
        for &x in row {
            print!("{}", x as u8);
        }
        println!();
    }
    println!("match_vec: {}", match_vec.len());
    println!("count: {}", count);
    Ok(())
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
