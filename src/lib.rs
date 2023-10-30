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

use crate::bert::Term;
use crate::process::Control;
use crate::process::Process;
use std::collections::BTreeMap;
use std::io;
use std::io::Write;
use std::os::unix::process::CommandExt;
use std::process::Command;
use std::sync::mpsc::channel;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;
use std::thread;
use std::thread::JoinHandle;
pub mod bert;
pub mod burp;
pub mod process;

struct Mapping {
    pid: u32,
    tx: Sender<Control>,
}

pub fn manager() {
    // source receives BERT format messages from the Erlang Port, wrapped in a
    // BURP (the BERT prefixed with a 32 big endian unsigned length)
    let (port_rx, source) = source();

    // sink sends BERT format messages to the Erlang Port, wrapped in a BURP.
    let (port_tx, sink) = sink();

    // maintain a mapping to managed spawned processes
    let mut mapping: BTreeMap<Term, Mapping> = BTreeMap::new();

    // receive and dispatch messages from the Erlang side of the port.
    while let Ok(term) = port_rx.recv() {
        // dispatch the term received from the Erlang side of the port,
        // and optionally replying back with another Erlang term (success or failure of that operation).
        //
        if let Some(reply) = dispatch(&mut mapping, port_tx.clone(), &term) {
            if port_tx.send(reply).is_err() {
                break;
            }
        }
    }

    source.join().unwrap_or_default();
    sink.join().unwrap_or_default();
}

fn dispatch(
    mapping: &mut BTreeMap<Term, Mapping>,
    port_tx: Sender<Term>,
    term: &Term,
) -> Option<Term> {
    match term {
        Term::Tuple(elements) => tuple(mapping, port_tx, elements),

        // the protocol expects Erlang tuple messages, otherwise crash and burn...
        _ => unreachable!(),
    }
}

fn tuple(
    mapping: &mut BTreeMap<Term, Mapping>,
    port_tx: Sender<Term>,
    elements: &[Term],
) -> Option<Term> {
    match elements[..] {
        // {spawn, ChildId, InteractionId, #{executable => "/bin/echo"}}.
        // Spawn a new process named "ChildId" using parameters in the supplied Map:
        // - "executable", is the only mandatory parameter (e.g., "/bin/echo")
        // - "args", is a list of Erlang String terms representing any arguments (default [])
        // - "arg0", is an optional key specifying the program name
        // - "cd", is an optional key specifying the working directory of spawned process
        //
        // Both "ChildId" and "InteractionId" are assumed to be Erlang References, but can
        // be any Erlang Term. The "ChildId" is the identifier for the Spawned process and
        // must be presented in subsequent operations (e.g., send, close, wait_for_exit).
        // Whereas "InteractionId" is an identifier of this "operation" and is used to
        // correlate any response, e.g., {ok, InteractionId, ChildId}, or {error, InteractionId, ...}.
        //
        // {spawn, ChildId :: reference(), InteractionId :: reference(), #{executable := string()}}
        [Term::Atom(ref command), ref child_id, ref interaction_id, Term::Map(ref kv)]
            if command == "spawn" =>
        {
            spawn(mapping, port_tx, child_id, interaction_id, kv)
        }

        // {all, InteractionId :: reference()} -> {reply, InteractionId, [{ChildId :: reference(), OsPid :: pos_integer()}]}.
        [Term::Atom(ref command), ref interaction_id] if command == "all" => {
            all(mapping, port_tx, interaction_id)
        }

        // {send, ChildId :: reference(), InteractionId :: reference(), Term :: string() | binary()} -> {reply, InteractionId, ok | error}
        [Term::Atom(ref command), ref child_id, ref interaction_id, ref data]
            if command == "send" =>
        {
            send(mapping, port_tx, child_id, interaction_id, data)
        }

        // {close, ChildId, InteractionId, stdin | stdout | stderr} -> {reply, InteractionId, ok | error}
        [Term::Atom(ref command), ref child_id, ref interaction_id, ref stream]
            if command == "close" =>
        {
            close(mapping, port_tx, child_id, interaction_id, stream)
        }

        // {wait_for_exit, ChildId, InteractionId} -> {reply, InteractionId, ok | error}
        [Term::Atom(ref command), ref child_id, ref interaction_id]
            if command == "wait_for_exit" =>
        {
            wait_for_exit(mapping, port_tx, child_id, interaction_id)
        }

        // {kill, ChildId, InteractionId} -> {reply, InteractionId, ok | error}
        [Term::Atom(ref command), ref child_id, ref interaction_id] if command == "kill" => {
            kill(mapping, port_tx, child_id, interaction_id)
        }

        // fallthrough, crash and burn...
        _ => unreachable!(),
    }
}

fn all(
    mapping: &mut BTreeMap<Term, Mapping>,
    _port_tx: Sender<Term>,
    interaction_id: &Term,
) -> Option<Term> {
    let mut reply = Vec::new();

    for (child_id, Mapping { pid, .. }) in mapping.iter() {
        reply.push(Term::Tuple(vec![
            child_id.clone(),
            Term::Integer(*pid as i32),
        ]));
    }

    reply_tuple(interaction_id, Term::List(reply))
}

fn wait_for_exit(
    mapping: &mut BTreeMap<Term, Mapping>,
    _port_tx: Sender<Term>,
    child_id: &Term,
    interaction_id: &Term,
) -> Option<Term> {
    if let Some(Mapping { tx, .. }) = mapping.get(child_id) {
        if let Ok(()) = tx.send(process::Control::WaitForExit) {
            mapping.remove(child_id);
            reply_ok(interaction_id)
        } else {
            reply_error(interaction_id, Term::Atom(String::from("send")))
        }
    } else {
        reply_error(interaction_id, child_id.clone())
    }
}

fn kill(
    mapping: &mut BTreeMap<Term, Mapping>,
    _port_tx: Sender<Term>,
    child_id: &Term,
    interaction_id: &Term,
) -> Option<Term> {
    if let Some(Mapping { tx, .. }) = mapping.get(child_id) {
        if let Ok(()) = tx.send(process::Control::Kill) {
            mapping.remove(child_id);
            reply_ok(interaction_id)
        } else {
            reply_error(interaction_id, Term::Atom(String::from("send")))
        }
    } else {
        reply_error(interaction_id, child_id.clone())
    }
}

fn send(
    mapping: &mut BTreeMap<Term, Mapping>,
    _port_tx: Sender<Term>,
    child_id: &Term,
    interaction_id: &Term,
    data: &Term,
) -> Option<Term> {
    if let Some(Mapping { tx, .. }) = mapping.get(child_id) {
        match data {
            Term::String(string) => {
                let mut contents = Vec::new();
                contents.extend_from_slice(string.as_bytes());

                if let Ok(()) = tx.send(process::Control::Send(contents)) {
                    reply_ok(interaction_id)
                } else {
                    reply_error(interaction_id, Term::Atom(String::from("send")))
                }
            }

            Term::Binary(value) => {
                if let Ok(()) = tx.send(process::Control::Send(value.clone())) {
                    reply_ok(interaction_id)
                } else {
                    reply_error(interaction_id, Term::Atom(String::from("send")))
                }
            }

            _otherwise => reply_error(interaction_id, data.clone()),
        }
    } else {
        reply_error(interaction_id, child_id.clone())
    }
}

fn close(
    mapping: &mut BTreeMap<Term, Mapping>,
    _port_tx: Sender<Term>,
    child_id: &Term,
    interaction_id: &Term,
    stream_term: &Term,
) -> Option<Term> {
    if let Some(Mapping { tx, .. }) = mapping.get(child_id) {
        if let Some(stream) = atom_to_stream(stream_term) {
            if let Ok(()) = tx.send(process::Control::Close(stream)) {
                reply_ok(interaction_id)
            } else {
                reply_error(interaction_id, Term::Atom(String::from("send")))
            }
        } else {
            reply_error(interaction_id, stream_term.clone())
        }
    } else {
        reply_error(interaction_id, child_id.clone())
    }
}

fn spawn(
    mapping: &mut BTreeMap<Term, Mapping>,
    port_tx: Sender<Term>,
    child_id: &Term,
    interaction_id: &Term,
    kv: &BTreeMap<Term, Term>,
) -> Option<Term> {
    if let Some(Term::String(executable)) = kv.get(&Term::Atom(String::from("executable"))) {
        // "executable" is a mandatory key, specifying the executable to be spawned
        let mut command = Command::new(executable);

        // "args" is an optional list of Erlang String terms that are parameters to the executable
        if let Some(Term::List(elements)) = kv.get(&Term::Atom(String::from("args"))) {
            for element in elements {
                match element {
                    Term::String(value) => {
                        command.arg(value.as_str());
                    }

                    // silently discarding args isn't great, should end up being an error response
                    _otherwise => {}
                }
            }
        }

        // "envs" is an optional Map of Erlang String terms that are the environment for the executable
        if let Some(Term::Map(envs)) = kv.get(&Term::Atom(String::from("envs"))) {
            for (k, v) in envs.iter() {
                match (k, v) {
                    (Term::String(name), Term::String(value)) => {
                        command.env(name, value);
                    }

                    // silently discarding args isn't great, should end up being an error response
                    _otherwise => {}
                }
            }
        }

        // "arg0" is an optional key, specifying the program name
        if let Some(Term::String(value)) = kv.get(&Term::Atom(String::from("arg0"))) {
            command.arg0(value);
        }

        // "cd" is an optional key, specifying the working directory of the spawned process
        if let Some(Term::String(value)) = kv.get(&Term::Atom(String::from("cd"))) {
            command.current_dir(value);
        }

        // transmitter and receiver communication channels for the spawned process
        let (parent_tx, rx) = channel::<process::Control>();

        match Process::new(parent_tx, &mut command) {
            Ok(Process { pid, tx, join, .. }) => {
                let child_id = child_id.clone();

                // map the client supplied child_id Erlang term with the transmitter channel
                // and OS pid of the created process
                mapping.insert(child_id.clone(), Mapping { tx, pid });

                thread::spawn(move || {
                    loop {
                        match rx.recv() {
                            Ok(process::Control::EndOfFile(stream)) => {
                                // advise the port that EOF has been reached on a specific
                                // stream (stdin, stdout or stderr) from the spawned process

                                if port_tx
                                    .send(Term::Tuple(vec![
                                        Term::Atom(String::from("eof")),
                                        child_id.clone(),
                                        stream_to_atom(&stream),
                                    ]))
                                    .is_err()
                                {
                                    break;
                                }
                            }

                            Ok(process::Control::Received { stream, data }) => {
                                // advise the port that some data has been received on a specific
                                // stream (stdin, stdout or stderr) from the spawned process

                                if port_tx
                                    .send(Term::Tuple(vec![
                                        stream_to_atom(&stream),
                                        child_id.clone(),
                                        Term::Binary(data),
                                    ]))
                                    .is_err()
                                {
                                    break;
                                }
                            }

                            Ok(process::Control::Exit(process::Exit::Code(status))) => {
                                // advise the port that the spawned process has exited with
                                // a status code
                                if port_tx
                                    .send(Term::Tuple(vec![
                                        Term::Atom(String::from("exit")),
                                        child_id.clone(),
                                        Term::Integer(status),
                                    ]))
                                    .is_err()
                                {
                                    break;
                                }
                            }

                            Ok(process::Control::Exit(process::Exit::Signal)) => {
                                // advise the port that spawned process has exited after being
                                // killed by a signal
                                if port_tx
                                    .send(Term::Tuple(vec![
                                        Term::Atom(String::from("exit")),
                                        child_id.clone(),
                                        Term::Atom(String::from("signal")),
                                    ]))
                                    .is_err()
                                {
                                    break;
                                }
                            }

                            Ok(process::Control::Error(stream)) => {
                                // advise the port that the spawned process has had an error
                                if port_tx
                                    .send(Term::Tuple(vec![
                                        Term::Atom(String::from("error")),
                                        child_id.clone(),
                                        stream_to_atom(&stream),
                                    ]))
                                    .is_err()
                                {
                                    break;
                                }
                            }

                            Ok(otherwise) => {
                                // debug only, it is an error to receieve messages others than
                                // those matched above
                                dbg!(otherwise);
                            }

                            Err(_error) => {
                                // error while receiving, crash and burn
                                break;
                            }
                        }
                    }

                    // having dropped out of the receive loop, join on the spawned process
                    // (ignoring any errors), and we are done
                    join.join().unwrap_or_default();
                });

                // reply indicating that the process has been spawned successfully
                reply_ok(interaction_id)
            }

            Err(error) => reply_error(interaction_id, Term::String(error.to_string())),
        }
    } else {
        reply_error(interaction_id, Term::Atom(String::from("executable")))
    }
}

fn reply_ok(interaction_id: &Term) -> Option<Term> {
    reply_tuple(interaction_id, Term::Atom(String::from("ok")))
}

fn reply_error(interaction_id: &Term, reason: Term) -> Option<Term> {
    reply_tuple(
        interaction_id,
        Term::Tuple(vec![Term::Atom(String::from("error")), reason]),
    )
}

fn reply_tuple(interaction_id: &Term, response: Term) -> Option<Term> {
    Some(Term::Tuple(vec![
        Term::Atom(String::from("reply")),
        interaction_id.clone(),
        response,
    ]))
}

fn stream_to_atom(stream: &process::Stream) -> Term {
    match stream {
        process::Stream::Error => Term::Atom(String::from("stderr")),
        process::Stream::Input => Term::Atom(String::from("stdin")),
        process::Stream::Output => Term::Atom(String::from("stdout")),
    }
}

fn atom_to_stream(term: &Term) -> Option<process::Stream> {
    match term {
        Term::Atom(ref stream) if stream == "stderr" => Some(process::Stream::Error),
        Term::Atom(ref stream) if stream == "stdout" => Some(process::Stream::Output),
        Term::Atom(ref stream) if stream == "stdin" => Some(process::Stream::Input),
        _ => None,
    }
}

// spawn a thread to receive port messages (BERT framed in a BURP) on stdin,
// which are transmitted to a channel
fn source() -> (Receiver<Term>, JoinHandle<()>) {
    let (tx, rx) = channel::<Term>();

    let spawned = thread::spawn(move || {
        let mut stdin = io::stdin();

        while let Ok(term) = burp::read(&mut stdin) {
            if tx.send(term).is_err() {
                break;
            }
        }
    });

    // returning the receiving channel for port messages, and the join
    // handle for the spawned thread
    (rx, spawned)
}

// spawn a thread receiving Erlang term messages on a channel that
// are written to stdout (BERT framed in a BURP)
fn sink() -> (Sender<Term>, JoinHandle<()>) {
    let (tx, rx) = channel::<Term>();

    let spawned = thread::spawn(move || {
        let mut stdout = io::stdout();

        while let Ok(term) = rx.recv() {
            burp::write(&mut stdout, &term).unwrap();
            if io::stdout().flush().is_err() {
                break;
            }
        }
    });

    // returning the transmitter channel and the join handle of
    // the spawned thread
    (tx, spawned)
}
