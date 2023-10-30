// Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! Frame a BERT message with a packet length

use crate::bert::Term;
use std::io::Error;
use std::io::Read;
use std::io::Write;
use std::result::Result;

/// Read a framed BERT message returning an Erlang Term
///
/// # Examples
///
/// ```
/// use std::io::BufReader;
/// use std::io::BufWriter;
/// use grimsby::bert;
/// use grimsby::burp;
///
/// let v = Vec::new();
/// let mut w = BufWriter::new(v);
/// let t1 = bert::Term::Integer(123_456_789);
/// burp::write(&mut w, &t1).unwrap();
///
/// let v = w.into_inner().unwrap();
///
/// let mut r = BufReader::new(v.as_slice());
/// let t2 = burp::read(&mut r).unwrap();
///
/// assert_eq!(t1, t2);
/// ```
pub fn read(r: &mut dyn Read) -> Result<Term, Error> {
    let mut buf = [0; 4];
    r.read_exact(&mut buf)?;
    let length = u32::from_be_bytes(buf);

    let mut v = vec![0; length as usize];

    r.read_exact(v.as_mut_slice())?;

    match crate::bert::binary_to_term(v.as_slice()) {
        Ok((_, term)) => Ok(term),
        Err(_) => panic!("{:#?}", v.as_slice()),
    }
}

/// Write a framed BERT message
///
/// # Examples
///
/// ```
/// use std::io::BufReader;
/// use std::io::BufWriter;
/// use grimsby::bert;
/// use grimsby::burp;
///
/// let v = Vec::new();
/// let mut w = BufWriter::new(v);
/// let t1 = bert::Term::Integer(123_456_789);
/// burp::write(&mut w, &t1).unwrap();
///
/// let v = w.into_inner().unwrap();
///
/// let mut r = BufReader::new(v.as_slice());
/// let t2 = burp::read(&mut r).unwrap();
///
/// assert_eq!(t1, t2);
/// ```
pub fn write(w: &mut dyn Write, term: &Term) -> Result<(), Error> {
    let encoded = crate::bert::term_to_binary(term)?;
    u32(w, &(encoded.len() as u32))?;
    w.write_all(encoded.as_slice())
}

fn u32(w: &mut dyn Write, value: &u32) -> Result<(), Error> {
    w.write_all(&value.to_be_bytes())
}

#[cfg(test)]
mod tests {
    use std::io::BufReader;
    use std::io::BufWriter;

    use super::*;
    #[test]
    fn write_read() {
        let v = Vec::new();
        let mut w = BufWriter::new(v);
        let t1 = Term::Integer(123_456_789);
        write(&mut w, &t1).unwrap();

        let v = w.into_inner().unwrap();

        let mut r = BufReader::new(v.as_slice());
        let t2 = read(&mut r).unwrap();

        assert_eq!(t1, t2);
    }
}
