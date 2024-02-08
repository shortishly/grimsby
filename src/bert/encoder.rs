use crate::bert::Pid;
use crate::bert::Port;
use crate::bert::Reference;
use crate::bert::Term;
use ordered_float::OrderedFloat;
use std::collections::BTreeMap;
use std::ffi::OsString;
use std::io::Error;
use std::io::Write;
use std::os::unix::prelude::OsStrExt;
use std::result::Result;

fn small_integer_ext(w: &mut dyn Write, integer: &i32) -> Result<(), Error> {
    u8(w, &super::SMALL_INTEGER_EXT)?;
    let small = *integer as u8;
    w.write_all(&small.to_be_bytes())
}

fn integer_ext(w: &mut dyn Write, integer: &i32) -> Result<(), Error> {
    u8(w, &super::INTEGER_EXT)?;
    w.write_all(&integer.to_be_bytes())
}

fn atom_ext(w: &mut dyn Write, value: &str) -> Result<(), Error> {
    u8(w, &super::ATOM_UTF8_EXT)?;
    u16(w, &(value.len() as u16))?;
    let mut os = OsString::new();
    os.push(value);
    w.write_all(os.as_os_str().as_bytes())
}

fn string_ext(w: &mut dyn Write, value: &str) -> Result<(), Error> {
    u8(w, &super::STRING_EXT)?;
    u16(w, &(value.len() as u16))?;
    let mut os = OsString::new();
    os.push(value);
    w.write_all(os.as_os_str().as_bytes())
}

fn map_ext(w: &mut dyn Write, contents: &BTreeMap<Term, Term>) -> Result<(), Error> {
    u8(w, &super::MAP_EXT)?;
    u32(w, &(contents.len() as u32))?;
    for (k, v) in contents.iter() {
        term(w, k)?;
        term(w, v)?;
    }
    Ok(())
}

fn port_ext(w: &mut dyn Write, node: &Term, id: &u32, creation: &u8) -> Result<(), Error> {
    u8(w, &super::PORT_EXT)?;
    term(w, node)?;
    u32(w, id)?;
    u8(w, creation)
}

fn new_port_ext(w: &mut dyn Write, node: &Term, id: &u32, creation: &u32) -> Result<(), Error> {
    u8(w, &super::NEW_PORT_EXT)?;
    term(w, node)?;
    u32(w, id)?;
    u32(w, creation)
}

fn v4_port_ext(w: &mut dyn Write, node: &Term, id: &u64, creation: &u32) -> Result<(), Error> {
    u8(w, &super::V4_PORT_EXT)?;
    term(w, node)?;
    u64(w, id)?;
    u32(w, creation)
}

fn pid_ext(
    w: &mut dyn Write,
    node: &Term,
    id: &u32,
    serial: &u32,
    creation: &u8,
) -> Result<(), Error> {
    u8(w, &super::PID_EXT)?;
    term(w, node)?;
    u32(w, id)?;
    u32(w, serial)?;
    u8(w, creation)
}

fn new_pid_ext(
    w: &mut dyn Write,
    node: &Term,
    id: &u32,
    serial: &u32,
    creation: &u32,
) -> Result<(), Error> {
    u8(w, &super::NEW_PID_EXT)?;
    term(w, node)?;
    u32(w, id)?;
    u32(w, serial)?;
    u32(w, creation)
}

fn new_float_ext(w: &mut dyn Write, value: &OrderedFloat<f64>) -> Result<(), Error> {
    u8(w, &super::NEW_FLOAT_EXT)?;
    w.write_all(&value.to_owned().to_be_bytes())
}

fn list_ext(w: &mut dyn Write, elements: &Vec<Term>) -> Result<(), Error> {
    u8(w, &super::LIST_EXT)?;
    vec(w, elements)?;
    term(w, &Term::Nil)
}

fn nil_ext(w: &mut dyn Write) -> Result<(), Error> {
    u8(w, &super::NIL_EXT)
}

fn new_reference_ext(
    w: &mut dyn Write,
    node: &Term,
    creation: &u8,
    ids: &Vec<u32>,
) -> Result<(), Error> {
    u8(w, &super::NEW_REFERENCE_EXT)?;
    u16(w, &(ids.len() as u16))?;
    term(w, node)?;
    u8(w, creation)?;
    for id in ids {
        u32(w, id)?;
    }
    Ok(())
}

fn newer_reference_ext(
    w: &mut dyn Write,
    node: &Term,
    creation: &u32,
    ids: &Vec<u32>,
) -> Result<(), Error> {
    u8(w, &super::NEWER_REFERENCE_EXT)?;
    u16(w, &(ids.len() as u16))?;
    term(w, node)?;
    u32(w, creation)?;
    for id in ids {
        u32(w, id)?;
    }
    Ok(())
}

fn binary_ext(w: &mut dyn Write, bytes: &Vec<u8>) -> Result<(), Error> {
    u8(w, &super::BINARY_EXT)?;
    u32(w, &(bytes.len() as u32))?;
    w.write_all(bytes.as_slice())?;
    Ok(())
}

fn bignum(w: &mut dyn Write, value: &i64) -> Result<(), Error> {
    let mut v = Vec::new();
    let mut acc = value.abs();
    let mut remainder = acc % 256;

    while remainder != 0 {
        v.push(remainder as u8);
        acc = acc.div_euclid(256);
        remainder = acc % 256;
    }

    if v.len() > (u8::MAX as usize) {
        u8(w, &super::LARGE_BIG_EXT)?;
        u32(w, &(v.len() as u32))?;
    } else {
        u8(w, &super::SMALL_BIG_EXT)?;
        u8(w, &(v.len() as u8))?;
    }

    match value.signum() {
        -1 => u8(w, &1)?,
        0 | 1 => u8(w, &0)?,
        otherwise => todo!("{:?}!", otherwise),
    }

    w.write_all(v.as_slice())?;
    Ok(())
}

fn tuple(w: &mut dyn Write, elements: &Vec<Term>) -> Result<(), Error> {
    match elements.len() {
        length @ 0..=255 => {
            u8(w, &super::SMALL_TUPLE_EXT)?;
            u8(w, &(length as u8))?;
        }
        length => {
            u8(w, &super::LARGE_TUPLE_EXT)?;
            u32(w, &(length as u32))?;
        }
    }
    for element in elements {
        term(w, element)?;
    }
    Ok(())
}

fn vec(w: &mut dyn Write, elements: &Vec<Term>) -> Result<(), Error> {
    u32(w, &(elements.len() as u32))?;
    for element in elements {
        term(w, element)?;
    }
    Ok(())
}

fn version(w: &mut dyn Write) -> Result<(), Error> {
    u8(w, &super::VERSION)
}

fn u8(w: &mut dyn Write, value: &u8) -> Result<(), Error> {
    w.write_all(&value.to_be_bytes())
}

fn u16(w: &mut dyn Write, value: &u16) -> Result<(), Error> {
    w.write_all(&value.to_be_bytes())
}

fn u32(w: &mut dyn Write, value: &u32) -> Result<(), Error> {
    w.write_all(&value.to_be_bytes())
}

fn u64(w: &mut dyn Write, value: &u64) -> Result<(), Error> {
    w.write_all(&value.to_be_bytes())
}

pub fn term(w: &mut dyn Write, t: &Term) -> Result<(), Error> {
    match t {
        Term::Integer(value @ 0..=255) => small_integer_ext(w, value),
        Term::Integer(value) => integer_ext(w, value),
        Term::Atom(value) => atom_ext(w, value),
        Term::Map(value) => map_ext(w, value),
        Term::Port(Port::PortExt { node, id, creation }) => port_ext(w, node, id, creation),
        Term::Port(Port::NewPortExt { node, id, creation }) => new_port_ext(w, node, id, creation),
        Term::Port(Port::V4PortExt { node, id, creation }) => v4_port_ext(w, node, id, creation),
        Term::Pid(Pid::PidExt {
            node,
            id,
            serial,
            creation,
        }) => pid_ext(w, node, id, serial, creation),
        Term::Pid(Pid::NewPidExt {
            node,
            id,
            serial,
            creation,
        }) => new_pid_ext(w, node, id, serial, creation),
        Term::Float(value) => new_float_ext(w, value),
        Term::List(elements) => list_ext(w, elements),
        Term::Nil => nil_ext(w),
        Term::Reference(Reference::NewReferenceExt {
            node,
            creation,
            ids,
        }) => new_reference_ext(w, node, creation, ids),
        Term::Reference(Reference::NewerReferenceExt {
            node,
            creation,
            ids,
        }) => newer_reference_ext(w, node, creation, ids),
        Term::Binary(value) => binary_ext(w, value),
        Term::Tuple(elements) => tuple(w, elements),
        Term::String(value) => string_ext(w, value),
        Term::Bignum(value) => bignum(w, value),
    }
}

pub fn term_to_binary(t: &Term) -> Result<Vec<u8>, Error> {
    let mut v = Vec::new();
    version(&mut v)?;
    term(&mut v, t)?;
    Ok(v)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn small_integer() {
        assert_eq!(vec![131, 97, 1], term_to_binary(&Term::Integer(1)).unwrap());
    }

    #[test]
    fn pos_integer() {
        let encoded: &[u8] = &[131, 98, 7, 91, 205, 21];
        assert_eq!(
            term_to_binary(&Term::Integer(123_456_789)).unwrap(),
            encoded
        );
    }

    #[test]
    fn neg_integer() {
        let encoded: &[u8] = &[131, 98, 255, 255, 255, 233];
        assert_eq!(term_to_binary(&Term::Integer(-23)).unwrap(), encoded);
    }

    #[test]
    fn atom() {
        assert_eq!(
            vec![131, 118, 0, 11, 104, 101, 108, 108, 111, 95, 119, 111, 114, 108, 100,],
            term_to_binary(&Term::Atom(String::from("hello_world"))).unwrap()
        );
    }

    #[test]
    fn string() {
        assert_eq!(
            vec![131, 107, 0, 12, 104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 33,],
            term_to_binary(&Term::String(String::from("hello world!"))).unwrap()
        );
    }

    #[test]
    fn map() {
        let mut left = BTreeMap::new();
        left.insert(Term::Atom(String::from("a")), Term::Integer(1));

        assert_eq!(
            term_to_binary(&Term::Map(left)).unwrap(),
            &[131, 116, 0, 0, 0, 1, 118, 0, 1, 97, 97, 1]
        );
    }

    #[test]
    fn new_port() {
        let encoded: &[u8] = &[
            131, 89, 118, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0,
            0, 0, 3, 0, 0, 0, 0,
        ];
        assert_eq!(
            term_to_binary(&Term::Port(Port::NewPortExt {
                node: Box::new(Term::Atom(String::from("nonode@nohost"))),
                id: 3,
                creation: 0
            }))
            .unwrap(),
            encoded
        );
    }

    #[test]
    fn float() {
        let encoded = &[131, 70, 63, 241, 153, 153, 153, 153, 153, 154];
        assert_eq!(
            term_to_binary(&Term::Float(OrderedFloat(1.1))).unwrap(),
            encoded
        );
    }

    #[test]
    fn list() {
        let encoded = &[
            131, 108, 0, 0, 0, 4, 118, 0, 1, 97, 118, 0, 1, 98, 98, 0, 0, 1, 65, 70, 63, 241, 153,
            153, 153, 153, 153, 154, 106,
        ];
        assert_eq!(
            term_to_binary(&Term::List(vec![
                Term::Atom(String::from("a")),
                Term::Atom(String::from("b")),
                Term::Integer(321),
                Term::Float(OrderedFloat(1.1))
            ]))
            .unwrap(),
            encoded
        );
    }

    #[test]
    fn newer_ref() {
        let encoded = &[
            131, 90, 0, 3, 118, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115,
            116, 0, 0, 0, 0, 0, 2, 105, 54, 11, 8, 0, 3, 188, 248, 216, 20,
        ];
        assert_eq!(
            term_to_binary(&Term::Reference(Reference::NewerReferenceExt {
                node: Box::new(Term::Atom(String::from("nonode@nohost"))),
                creation: 0,
                ids: vec![158006, 185073667, 3170424852]
            }))
            .unwrap(),
            encoded
        );
    }

    #[test]
    fn new_pid() {
        let encoded = &[
            131, 88, 118, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0,
            0, 0, 81, 0, 0, 0, 0, 0, 0, 0, 0,
        ];
        assert_eq!(
            term_to_binary(&Term::Pid(Pid::NewPidExt {
                node: Box::new(Term::Atom(String::from("nonode@nohost"))),
                id: 81,
                serial: 0,
                creation: 0
            }))
            .unwrap(),
            encoded
        );
    }

    #[test]
    fn binary() {
        let encoded = &[131, 109, 0, 0, 0, 3, 97, 98, 99];
        assert_eq!(
            term_to_binary(&Term::Binary(vec![97, 98, 99])).unwrap(),
            encoded
        );
    }

    #[test]
    fn tuple() {
        let encoded = &[
            131, 104, 3, 118, 0, 3, 97, 98, 99, 97, 123, 70, 64, 20, 0, 0, 0, 0, 0, 0,
        ];
        assert_eq!(
            term_to_binary(&Term::Tuple(vec![
                Term::Atom(String::from("abc")),
                Term::Integer(123),
                Term::Float(OrderedFloat(5.0))
            ]))
            .unwrap(),
            encoded
        );
    }

    #[test]
    fn small_big_positive() {
        let encoded = &[131, 110, 5, 0, 249, 1, 131, 182, 28];
        assert_eq!(
            term_to_binary(&Term::Bignum(123_321_123_321)).unwrap(),
            encoded
        );
    }

    #[test]
    fn small_big_i64_max() {
        let encoded = &[131, 110, 8, 0, 255, 255, 255, 255, 255, 255, 255, 127];
        assert_eq!(term_to_binary(&Term::Bignum(i64::MAX)).unwrap(), encoded);
    }

    #[test]
    fn small_big_i64_min_plus_one() {
        let encoded = &[131, 110, 8, 1, 255, 255, 255, 255, 255, 255, 255, 127];
        assert_eq!(
            term_to_binary(&Term::Bignum(i64::MIN + 1)).unwrap(),
            encoded
        );
    }

    #[test]
    fn small_big_negative() {
        let encoded = &[131, 110, 5, 1, 249, 1, 131, 182, 28];
        assert_eq!(
            term_to_binary(&Term::Bignum(-123_321_123_321)).unwrap(),
            encoded
        );
    }
}
