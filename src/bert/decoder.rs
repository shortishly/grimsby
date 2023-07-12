use crate::bert::Pid;
use crate::bert::Port;
use crate::bert::Reference;
use crate::bert::Term;
use nom::branch::alt;
use nom::bytes::streaming::tag;
use nom::bytes::streaming::take;
use nom::IResult;
use ordered_float::OrderedFloat;
use std::collections::BTreeMap;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::os::unix::prelude::OsStrExt;

fn integer(i: &[u8]) -> IResult<&[u8], Term> {
    alt((small_integer_ext, integer_ext))(i)
}

fn small_integer_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::SMALL_INTEGER_EXT])(i)?;
    let (i, value) = u8(i)?;
    Ok((i, Term::Integer(value as i32)))
}

fn integer_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::INTEGER_EXT])(i)?;
    let (i, value) = i32(i)?;
    Ok((i, Term::Integer(value)))
}

fn atom(i: &[u8]) -> IResult<&[u8], Term> {
    alt((atom_ext, atom_utf8_ext, small_atom_utf8_ext))(i)
}

fn atom_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::ATOM_EXT])(i)?;
    let (i, length) = u16(i)?;
    let (i, value) = take(length)(i)?;

    let mut content = OsString::new();
    content.push(OsStr::from_bytes(value));

    Ok((i, Term::Atom(content.into_string().unwrap())))
}

fn atom_utf8_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::ATOM_UTF8_EXT])(i)?;
    let (i, length) = u16(i)?;
    let (i, value) = take(length)(i)?;

    let mut content = OsString::new();
    content.push(OsStr::from_bytes(value));

    Ok((i, Term::Atom(content.into_string().unwrap())))
}

fn small_atom_utf8_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::SMALL_ATOM_UTF8_EXT])(i)?;
    let (i, length) = u8(i)?;
    let (i, value) = take(length)(i)?;

    let mut content = OsString::new();
    content.push(OsStr::from_bytes(value));

    Ok((i, Term::Atom(content.into_string().unwrap())))
}

fn string(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::STRING_EXT])(i)?;
    let (i, length) = u16(i)?;
    let (i, value) = take(length)(i)?;

    let mut content = OsString::new();
    content.push(OsStr::from_bytes(value));

    Ok((i, Term::String(content.into_string().unwrap())))
}

fn small_tuple_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::SMALL_TUPLE_EXT])(i)?;
    let (i, arity) = u8(i)?;
    let (i, contents) = tuple_contents(i, arity as u32)?;
    Ok((i, Term::Tuple(contents)))
}

fn large_tuple_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::LARGE_TUPLE_EXT])(i)?;
    let (i, arity) = u32(i)?;
    let (i, contents) = tuple_contents(i, arity)?;
    Ok((i, Term::Tuple(contents)))
}

fn tuple_contents(mut i: &[u8], arity: u32) -> IResult<&[u8], Vec<Term>> {
    let mut contents = Vec::new();
    for _ in 0..arity {
        let (remaining, content) = term(i)?;
        i = remaining;
        contents.push(content);
    }
    Ok((i, contents))
}

fn tuple(i: &[u8]) -> IResult<&[u8], Term> {
    alt((small_tuple_ext, large_tuple_ext))(i)
}

fn new_float_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::NEW_FLOAT_EXT])(i)?;
    let (i, value) = f64(i)?;
    Ok((i, Term::Float(OrderedFloat(value))))
}

fn binary_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::BINARY_EXT])(i)?;
    let (i, length) = u32(i)?;
    let (i, data) = take(length)(i)?;
    let mut contents = Vec::new();
    contents.extend_from_slice(data);
    Ok((i, Term::Binary(contents)))
}

fn map_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::MAP_EXT])(i)?;
    let (i, arity) = u32(i)?;
    let (i, contents) = map_entries(i, arity)?;
    Ok((i, Term::Map(contents)))
}

fn map_entries(mut i: &[u8], arity: u32) -> IResult<&[u8], BTreeMap<Term, Term>> {
    let mut contents = BTreeMap::new();
    for _ in 0..arity {
        let (remaining, (k, v)) = map_entry(i)?;
        i = remaining;
        contents.insert(k, v);
    }
    Ok((i, contents))
}

fn map_entry(i: &[u8]) -> IResult<&[u8], (Term, Term)> {
    let (i, k) = term(i)?;
    let (i, v) = term(i)?;
    Ok((i, (k, v)))
}

fn port(i: &[u8]) -> IResult<&[u8], Term> {
    alt((port_ext, new_port_ext, v4_port_ext))(i)
}

fn port_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::PORT_EXT])(i)?;
    let (i, node) = atom(i)?;
    let (i, id) = u32(i)?;
    let (i, creation) = u8(i)?;
    Ok((
        i,
        Term::Port(Port::PortExt {
            node: Box::new(node),
            id,
            creation,
        }),
    ))
}

fn new_port_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::NEW_PORT_EXT])(i)?;
    let (i, node) = atom(i)?;
    let (i, id) = u32(i)?;
    let (i, creation) = u32(i)?;
    Ok((
        i,
        Term::Port(Port::NewPortExt {
            node: Box::new(node),
            id,
            creation,
        }),
    ))
}

fn v4_port_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::V4_PORT_EXT])(i)?;
    let (i, node) = atom(i)?;
    let (i, id) = u64(i)?;
    let (i, creation) = u32(i)?;
    Ok((
        i,
        Term::Port(Port::V4PortExt {
            node: Box::new(node),
            id,
            creation,
        }),
    ))
}

fn pid(i: &[u8]) -> IResult<&[u8], Term> {
    alt((pid_ext, new_pid_ext))(i)
}

fn pid_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::PID_EXT])(i)?;
    let (i, node) = atom(i)?;
    let (i, id) = u32(i)?;
    let (i, serial) = u32(i)?;
    let (i, creation) = u8(i)?;

    Ok((
        i,
        Term::Pid(Pid::PidExt {
            node: Box::new(node),
            id,
            serial,
            creation,
        }),
    ))
}

fn new_pid_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::NEW_PID_EXT])(i)?;
    let (i, node) = atom(i)?;
    let (i, id) = u32(i)?;
    let (i, serial) = u32(i)?;
    let (i, creation) = u32(i)?;

    Ok((
        i,
        Term::Pid(Pid::NewPidExt {
            node: Box::new(node),
            id,
            serial,
            creation,
        }),
    ))
}

fn reference(i: &[u8]) -> IResult<&[u8], Term> {
    alt((new_reference_ext, newer_reference_ext))(i)
}

fn new_reference_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::NEW_REFERENCE_EXT])(i)?;
    let (i, length) = u16(i)?;
    let (i, node) = atom(i)?;
    let (i, creation) = u8(i)?;
    let (i, ids) = reference_ids(i, length)?;
    Ok((
        i,
        Term::Reference(Reference::NewReferenceExt {
            node: Box::new(node),
            creation,
            ids,
        }),
    ))
}

fn newer_reference_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::NEWER_REFERENCE_EXT])(i)?;
    let (i, length) = u16(i)?;
    let (i, node) = atom(i)?;
    let (i, creation) = u32(i)?;
    let (i, ids) = reference_ids(i, length)?;
    Ok((
        i,
        Term::Reference(Reference::NewerReferenceExt {
            node: Box::new(node),
            creation,
            ids,
        }),
    ))
}

fn reference_ids(mut i: &[u8], length: u16) -> IResult<&[u8], Vec<u32>> {
    let mut ids = Vec::new();
    for _ in 0..length {
        let (remaining, id) = u32(i)?;
        ids.push(id);
        i = remaining;
    }
    Ok((i, ids))
}

fn list_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::LIST_EXT])(i)?;
    let (i, length) = u32(i)?;
    let (i, mut contents) = list_contents(i, length)?;
    let (i, tail) = term(i)?;

    match tail {
        Term::Nil => (),
        otherwise => contents.push(otherwise),
    };

    Ok((i, Term::List(contents)))
}

fn list_contents(mut i: &[u8], length: u32) -> IResult<&[u8], Vec<Term>> {
    let mut contents = Vec::new();
    for _ in 0..length {
        let (remaining, content) = term(i)?;
        contents.push(content);
        i = remaining;
    }
    Ok((i, contents))
}

fn nil_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::NIL_EXT])(i)?;
    Ok((i, Term::Nil))
}

fn small_big_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::SMALL_BIG_EXT])(i)?;
    let (i, n) = u8(i)?;
    let (mut i, sign) = u8(i)?;
    let mut big = 0;

    for j in 0..n {
        let (remaining, d) = u8(i)?;
        big += (d as i64) * 256_i64.pow(j as u32);
        i = remaining;
    }

    if sign == 1 {
        big = -big;
    }

    Ok((i, Term::Bignum(big)))
}

fn large_big_ext(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _) = tag([super::LARGE_BIG_EXT])(i)?;
    let (i, n) = u32(i)?;
    let (mut i, sign) = u8(i)?;
    let mut big = 0;

    for j in 0..n {
        let (remaining, d) = u8(i)?;
        big += (d as i64) * 256_i64.pow(j);
        i = remaining;
    }

    if sign == 1 {
        big = -big;
    }

    Ok((i, Term::Bignum(big)))
}

fn bignum(i: &[u8]) -> IResult<&[u8], Term> {
    alt((small_big_ext, large_big_ext))(i)
}

fn f64(i: &[u8]) -> IResult<&[u8], f64> {
    let (i, value) = take(8u8)(i)?;
    Ok((i, f64::from_be_bytes(value.try_into().unwrap())))
}

fn u8(i: &[u8]) -> IResult<&[u8], u8> {
    let (i, value) = take(1u8)(i)?;
    Ok((i, u8::from_be_bytes(value.try_into().unwrap())))
}

fn u16(i: &[u8]) -> IResult<&[u8], u16> {
    let (i, value) = take(2u8)(i)?;
    Ok((i, u16::from_be_bytes(value.try_into().unwrap())))
}

fn u32(i: &[u8]) -> IResult<&[u8], u32> {
    let (i, value) = take(4u8)(i)?;
    Ok((i, u32::from_be_bytes(value.try_into().unwrap())))
}

fn u64(i: &[u8]) -> IResult<&[u8], u64> {
    let (i, value) = take(4u8)(i)?;
    Ok((i, u64::from_be_bytes(value.try_into().unwrap())))
}

fn i32(i: &[u8]) -> IResult<&[u8], i32> {
    let (i, value) = take(4u8)(i)?;
    Ok((i, i32::from_be_bytes(value.try_into().unwrap())))
}

fn term(i: &[u8]) -> IResult<&[u8], Term> {
    alt((
        integer,
        atom,
        string,
        tuple,
        new_float_ext,
        map_ext,
        list_ext,
        nil_ext,
        binary_ext,
        bignum,
        reference,
        pid,
        port,
    ))(i)
}

fn version(i: &[u8]) -> IResult<&[u8], u8> {
    let (i, version) = tag([super::VERSION])(i)?;
    Ok((i, u8::from_be_bytes(version.try_into().unwrap())))
}

pub fn decode(i: &[u8]) -> IResult<&[u8], Term> {
    let (i, _version) = version(i)?;
    let (i, term) = term(i)?;
    Ok((i, term))
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn small_integer() {
        let data: &[u8] = &[131, 97, 1];

        let (_, value) = decode(data).unwrap();
        assert_eq!(Term::Integer(1), value);
    }

    #[test]
    fn pos_integer() {
        let data: &[u8] = &[131, 98, 7, 91, 205, 21];
        let (_, value) = decode(data).unwrap();
        assert_eq!(Term::Integer(123_456_789), value);
    }

    #[test]
    fn neg_integer() {
        let data: &[u8] = &[131, 98, 255, 255, 255, 233];
        let (_, value) = decode(data).unwrap();
        assert_eq!(Term::Integer(-23), value);
    }

    #[test]
    fn atom() {
        let data: &[u8] = &[
            131, 100, 0, 11, 104, 101, 108, 108, 111, 95, 119, 111, 114, 108, 100,
        ];
        let (_, value) = decode(data).unwrap();
        assert_eq!(Term::Atom(String::from("hello_world")), value);
    }

    #[test]
    fn map() {
        let data: &[u8] = &[131, 116, 0, 0, 0, 1, 100, 0, 1, 97, 97, 1];
        let (_, value) = decode(data).unwrap();

        let mut left = BTreeMap::new();
        left.insert(Term::Atom(String::from("a")), Term::Integer(1));

        assert_eq!(Term::Map(left), value);
    }

    #[test]
    fn port() {
        let data = &[
            131, 102, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116,
            0, 0, 1, 94, 0,
        ];
        let (_, value) = decode(data).unwrap();
        assert_eq!(
            Term::Port(Port::PortExt {
                node: Box::new(Term::Atom(String::from("nonode@nohost"))),
                id: 350,
                creation: 0
            }),
            value
        );
    }

    #[test]
    fn new_port() {
        let data: &[u8] = &[
            131, 89, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0,
            0, 0, 3, 0, 0, 0, 0,
        ];
        let (_, value) = decode(data).unwrap();
        assert_eq!(
            Term::Port(Port::NewPortExt {
                node: Box::new(Term::Atom(String::from("nonode@nohost"))),
                id: 3,
                creation: 0
            }),
            value
        );
    }

    #[test]
    fn float() {
        let data = &[131, 70, 63, 241, 153, 153, 153, 153, 153, 154];
        let (_, value) = decode(data).unwrap();
        assert_eq!(Term::Float(OrderedFloat(1.1)), value);
    }

    #[test]
    fn list() {
        let data = &[
            131, 108, 0, 0, 0, 4, 100, 0, 1, 97, 100, 0, 1, 98, 98, 0, 0, 1, 65, 70, 63, 241, 153,
            153, 153, 153, 153, 154, 106,
        ];
        let (_, value) = decode(data).unwrap();
        assert_eq!(
            Term::List(vec![
                Term::Atom("a".to_string()),
                Term::Atom("b".to_string()),
                Term::Integer(321),
                Term::Float(OrderedFloat(1.1))
            ]),
            value
        );
    }

    #[test]
    fn new_reference() {
        let data = &[
            131, 114, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115,
            116, 0, 0, 0, 0, 29, 0, 0, 0, 3, 0, 0, 0, 0,
        ];
        let (_, value) = decode(data).unwrap();
        assert_eq!(
            Term::Reference(Reference::NewReferenceExt {
                node: Box::new(Term::Atom(String::from("nonode@nohost"))),
                creation: 0,
                ids: vec![29, 3, 0]
            }),
            value
        );
    }

    #[test]
    fn newer_reference() {
        let data = &[
            131, 90, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115,
            116, 0, 0, 0, 0, 0, 2, 105, 54, 11, 8, 0, 3, 188, 248, 216, 20,
        ];
        let (_, value) = decode(data).unwrap();
        assert_eq!(
            Term::Reference(Reference::NewerReferenceExt {
                node: Box::new(Term::Atom(String::from("nonode@nohost"))),
                creation: 0,
                ids: vec![158006, 185073667, 3170424852]
            }),
            value
        );
    }

    #[test]
    fn pid() {
        let data = &[
            131, 103, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116,
            0, 0, 0, 42, 0, 0, 0, 0, 0,
        ];
        let (_, value) = decode(data).unwrap();
        assert_eq!(
            Term::Pid(Pid::PidExt {
                node: Box::new(Term::Atom(String::from("nonode@nohost"))),
                id: 42,
                serial: 0,
                creation: 0
            }),
            value
        );
    }

    #[test]
    fn new_pid() {
        let data = &[
            131, 88, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0,
            0, 0, 81, 0, 0, 0, 0, 0, 0, 0, 0,
        ];
        let (_, value) = decode(data).unwrap();
        assert_eq!(
            Term::Pid(Pid::NewPidExt {
                node: Box::new(Term::Atom(String::from("nonode@nohost"))),
                id: 81,
                serial: 0,
                creation: 0
            }),
            value
        );
    }

    #[test]
    fn binary() {
        let data = &[131, 109, 0, 0, 0, 3, 97, 98, 99];
        let (_, value) = decode(data).unwrap();
        assert_eq!(Term::Binary(vec![97, 98, 99]), value);
    }

    #[test]
    fn tuple() {
        let data = &[
            131, 104, 3, 100, 0, 3, 97, 98, 99, 97, 123, 70, 64, 20, 0, 0, 0, 0, 0, 0,
        ];
        let (_, value) = decode(data).unwrap();
        assert_eq!(
            Term::Tuple(vec![
                Term::Atom(String::from("abc")),
                Term::Integer(123),
                Term::Float(OrderedFloat(5.0))
            ]),
            value
        );
    }

    #[test]
    fn string() {
        let data = &[
            131, 107, 0, 12, 104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 33,
        ];
        let (_, value) = decode(data).unwrap();
        assert_eq!(Term::String(String::from("hello world!")), value);
    }

    #[test]
    fn small_big_positive() {
        let data = &[131, 110, 5, 0, 249, 1, 131, 182, 28];
        let (_, value) = decode(data).unwrap();
        assert_eq!(Term::Bignum(123_321_123_321), value);
    }

    #[test]
    fn small_big_negative() {
        let data = &[131, 110, 5, 1, 249, 1, 131, 182, 28];
        let (_, value) = decode(data).unwrap();
        assert_eq!(Term::Bignum(-123_321_123_321), value);
    }

    #[test]
    fn newer_reference_with_utf_atom() {
        let data = &[
            131, 90, 0, 3, 119, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116,
            0, 0, 0, 0, 0, 1, 110, 234, 2, 200, 0, 2, 3, 48, 197, 195,
        ];
        let (_, value) = decode(data).unwrap();
        assert_eq!(
            Term::Reference(Reference::NewerReferenceExt {
                node: Box::new(Term::Atom(String::from("nonode@nohost"))),
                creation: 0,
                ids: vec![93930, 46661634, 53528003]
            }),
            value
        );
    }

    #[test]
    fn map_atom_to_string() {
        let data = &[
            131, 116, 0, 0, 0, 1, 119, 10, 101, 120, 101, 99, 117, 116, 97, 98, 108, 101, 107, 0,
            8, 47, 98, 105, 110, 47, 99, 97, 116,
        ];
        let (_, value) = decode(data).unwrap();

        let mut left = BTreeMap::new();
        left.insert(
            Term::Atom(String::from("executable")),
            Term::String(String::from("/bin/cat")),
        );

        assert_eq!(Term::Map(left), value);
    }
}
