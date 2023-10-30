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

//! [BERT]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html
//! Encoding and decoding between an Erlang Term and [BERT]
use nom::Finish;
use ordered_float::OrderedFloat;
use std::collections::BTreeMap;

mod decoder;
mod encoder;

const VERSION: u8 = 131;

const NEW_FLOAT_EXT: u8 = 70;
const NEW_PID_EXT: u8 = 88;
const NEW_PORT_EXT: u8 = 89;
const NEWER_REFERENCE_EXT: u8 = 90;
const SMALL_INTEGER_EXT: u8 = 97;
const INTEGER_EXT: u8 = 98;
const ATOM_EXT: u8 = 100;
const PORT_EXT: u8 = 102;
const PID_EXT: u8 = 103;
const SMALL_TUPLE_EXT: u8 = 104;
const LARGE_TUPLE_EXT: u8 = 105;
const NIL_EXT: u8 = 106;
const STRING_EXT: u8 = 107;
const LIST_EXT: u8 = 108;
const BINARY_EXT: u8 = 109;
const SMALL_BIG_EXT: u8 = 110;
const LARGE_BIG_EXT: u8 = 111;
const NEW_REFERENCE_EXT: u8 = 114;
const MAP_EXT: u8 = 116;
const ATOM_UTF8_EXT: u8 = 118;
const SMALL_ATOM_UTF8_EXT: u8 = 119;
const V4_PORT_EXT: u8 = 120;

/// Term
///
/// Variants:
/// - bignum as [SmallBigExt] or [LargeBigExt] within an i64
/// - integer as [SmallIntegerExt] or [IntegerExt] as an i32
/// - atom as [AtomExt]
/// - string as [StringExt]
/// - tuple as [SmallTupleExt] or [LargeTupleExt]
/// - float as [NewFloatExt]
/// - map as [MapExt]
/// - port as [PortExt], [NewPortExt] or [V4PortExt]
/// - pid as [PidExt] or [NewPidExt]
/// - list with [ListExt] and [NilExt]
/// - binary with [BinaryExt]
/// - reference as [NewReferenceExt] or [NewerReferenceExt]
///
/// [SmallBigExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#small_big_ext
/// [LargeBigExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#large_big_ext
/// [IntegerExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#integer_ext
/// [SmallIntegerExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#small_integer_ext
/// [SmallAtomUTF8Ext]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#small_atom_utf8_ext
/// [AtomExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#atom_ext--deprecated-
/// [StringExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#string_ext
/// [SmallTupleExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#small_tuple_ext
/// [LargeTupleExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#large_tuple_ext
/// [NewFloatExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#new_float_ext
/// [MapExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#map_ext
/// [PortExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#port_ext
/// [NewPortExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#new_port_ext
/// [V4PortExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#v4_port_ext
/// [PidExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#pid_ext
/// [NewPidExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#new_pid_ext
/// [ListExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#list_ext
/// [NilExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#nil_ext
/// [BinaryExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#binary_ext
/// [NewReferenceExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#new_reference_ext
/// [NewerReferenceExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#newer_reference_ext
///
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Term {
    Bignum(i64),
    Integer(i32),
    Atom(String),
    String(String),
    Tuple(Vec<Term>),
    Float(OrderedFloat<f64>),
    Map(BTreeMap<Term, Term>),
    Port(Port),
    Pid(Pid),
    List(Vec<Term>),
    Nil,
    Binary(Vec<u8>),
    Reference(Reference),
}

/// Port
///
/// Variants:
/// - [PortExt]
/// - [NewPortExt]
/// - [V4PortExt]
///
/// [PortExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#port_ext
/// [NewPortExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#new_port_ext
/// [V4PortExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#v4_port_ext
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Port {
    PortExt {
        node: Box<Term>,
        id: u32,
        creation: u8,
    },
    NewPortExt {
        node: Box<Term>,
        id: u32,
        creation: u32,
    },
    V4PortExt {
        node: Box<Term>,
        id: u64,
        creation: u32,
    },
}

/// Pid
///
/// Variants:
/// - [PidExt]
/// - [NewPidExt]
///
/// [PidExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#pid_ext
/// [NewPidExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#new_pid_ext
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Pid {
    PidExt {
        node: Box<Term>,
        id: u32,
        serial: u32,
        creation: u8,
    },
    NewPidExt {
        node: Box<Term>,
        id: u32,
        serial: u32,
        creation: u32,
    },
}

/// Reference
///
/// Variants:
/// - [NewReferenceExt]
/// - [NewerReferenceExt]
///
/// [NewReferenceExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#new_reference_ext
/// [NewerReferenceExt]: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#newer_reference_ext
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Reference {
    NewReferenceExt {
        node: Box<Term>,
        creation: u8,
        ids: Vec<u32>,
    },
    NewerReferenceExt {
        node: Box<Term>,
        creation: u32,
        ids: Vec<u32>,
    },
}

/// Converts Erlang External Term Format into an Erlang Term.
///
/// # Examples
///
/// ```
/// use grimsby::bert;
/// let data: &[u8] = &[
///     131, 100, 0, 11, 104, 101, 108, 108, 111, 95, 119, 111, 114, 108, 100,
/// ];
/// if let Ok(([], term)) = bert::binary_to_term(data) {
///   println!("{:#?}", term);
/// }
/// ```
pub fn binary_to_term(i: &[u8]) -> Result<(&[u8], Term), nom::error::Error<&[u8]>> {
    decoder::decode(i).finish()
}

/// Converts an Erlang term into Erlang External Term Format.
///
/// # Examples
///
/// ```
/// use grimsby::bert;
/// if let Ok(encoded) = bert::term_to_binary(&bert::Term::Integer(123_456_789)) {
///    println!("{:#?}", encoded);
/// }
/// ```
pub fn term_to_binary(t: &Term) -> Result<Vec<u8>, std::io::Error> {
    encoder::term_to_binary(t)
}

/// This module contains simple tests to check encoding and decoding of various Erlang terms
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integer() {
        encode_decode(Term::Integer(123_456_789));
    }

    #[test]
    fn atom() {
        encode_decode(Term::Atom(String::from("hello_world")))
    }

    #[test]
    fn map() {
        let mut t1 = BTreeMap::new();
        t1.insert(Term::Atom(String::from("a")), Term::Integer(1));
        encode_decode(Term::Map(t1))
    }

    #[test]
    fn port() {
        encode_decode(Term::Port(Port::PortExt {
            node: Box::new(Term::Atom(String::from("nonode@nohost"))),
            id: 350,
            creation: 0,
        }))
    }

    #[test]
    fn new_port() {
        encode_decode(Term::Port(Port::NewPortExt {
            node: Box::new(Term::Atom(String::from("nonode@nohost"))),
            id: 3,
            creation: 0,
        }))
    }

    #[test]
    fn float() {
        encode_decode(Term::Float(OrderedFloat(1.1)))
    }

    #[test]
    fn list() {
        encode_decode(Term::List(vec![
            Term::Atom("a".to_string()),
            Term::Atom("b".to_string()),
            Term::Integer(321),
            Term::Float(OrderedFloat(1.1)),
        ]))
    }

    #[test]
    fn new_reference() {
        encode_decode(Term::Reference(Reference::NewReferenceExt {
            node: Box::new(Term::Atom(String::from("nonode@nohost"))),
            creation: 0,
            ids: vec![29, 3, 0],
        }))
    }

    #[test]
    fn newer_reference() {
        encode_decode(Term::Reference(Reference::NewerReferenceExt {
            node: Box::new(Term::Atom(String::from("nonode@nohost"))),
            creation: 0,
            ids: vec![158006, 185073667, 3170424852],
        }))
    }

    #[test]
    fn pid() {
        encode_decode(Term::Pid(Pid::PidExt {
            node: Box::new(Term::Atom(String::from("nonode@nohost"))),
            id: 42,
            serial: 0,
            creation: 0,
        }))
    }

    #[test]
    fn new_pid() {
        encode_decode(Term::Pid(Pid::NewPidExt {
            node: Box::new(Term::Atom(String::from("nonode@nohost"))),
            id: 81,
            serial: 0,
            creation: 0,
        }))
    }

    #[test]
    fn binary() {
        encode_decode(Term::Binary(vec![97, 98, 99]))
    }

    #[test]
    fn tuple() {
        encode_decode(Term::Tuple(vec![
            Term::Atom(String::from("abc")),
            Term::Integer(123),
            Term::Float(OrderedFloat(5.0)),
        ]))
    }

    #[test]
    fn string() {
        encode_decode(Term::String(String::from("hello world!")))
    }

    fn encode_decode(t1: Term) {
        let (_, t2) = binary_to_term(term_to_binary(&t1).unwrap().as_slice()).unwrap();
        assert_eq!(t1, t2);
    }
}
