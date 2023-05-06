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

//! A process that is controlled by an Erlang port

use std::io;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;
use std::process::Child;
use std::process::Command;
use std::process::Stdio;
use std::sync::mpsc::channel;
use std::sync::mpsc::Sender;
use std::thread;
use std::thread::JoinHandle;

/// Stream represents stdin, stdout or stderr of the process
#[derive(Debug)]
pub enum Stream {
    Input,
    Output,
    Error,
}

/// Exit represents the status code or signal from the process
#[derive(Debug)]
pub enum Exit {
    Code(i32),
    Signal,
}

/// A control enumerates different commands that can be issued to the process,
/// or responses from the process back to the controller
#[derive(Debug)]
pub enum Control {
    /// An instruction from the controller to close a stream
    Close(Stream),
    /// A notification from the process that EOF has been reached on a stream
    EndOfFile(Stream),
    /// A request from the controller to send data to the stdin of the process
    Send(String),
    /// An error indicator from the process
    Error(Stream),
    /// A notification that the process has exited with a status code or signal
    Exit(Exit),
    /// A notification that the process has sent data on the stdout or stderr streams
    Received { stream: Stream, data: String },
    /// A request from the controller to wait for the process to exit
    WaitForExit,
    /// A request from the controller to kill the process with a signal
    Kill,
}

/// A process has a transmitter channel to communicate commands, and a join handle to wait for it to exit
pub struct Process {
    /// the OS pid of the process
    pub pid: u32,
    /// the sender used to control this process
    pub tx: Sender<Control>,
    /// the handle used to join waiting for the process to exit
    pub join: JoinHandle<()>,
}

impl Process {
    pub fn new(parent_tx: Sender<Control>, command: &mut Command) -> Result<Process, io::Error> {
        command.stdin(Stdio::piped());
        command.stdout(Stdio::piped());
        command.stderr(Stdio::piped());

        let mut child: Child = command.spawn()?;

        let (control_tx, control_rx) = channel::<Control>();

        let (stdin_tx, stdin) = stdin_handler(control_tx.clone(), &mut child).unwrap();
        let stdout = stdout_handler(control_tx.clone(), &mut child).unwrap();
        let stderr = stderr_handler(control_tx.clone(), &mut child).unwrap();

        Ok(Process {
            pid: child.id(),
            tx: control_tx,
            join: thread::spawn(move || {
                loop {
                    match control_rx.recv() {
                        Ok(Control::Close(Stream::Input)) => {
                            if let Err(..) = stdin_tx.send(Control::Close(Stream::Input)) {
                                break;
                            }
                        }

                        Ok(Control::EndOfFile(stream)) => {
                            if let Err(..) = parent_tx.send(Control::EndOfFile(stream)) {
                                break;
                            }
                        }

                        Ok(Control::Error(stream)) => {
                            if let Err(..) = parent_tx.send(Control::Error(stream)) {
                                break;
                            }
                        }

                        Ok(Control::Received { stream, data }) => {
                            if let Err(..) = parent_tx.send(Control::Received { stream, data }) {
                                break;
                            }
                        }

                        Ok(Control::Send(data)) => {
                            if let Err(..) = stdin_tx.send(Control::Send(data)) {
                                break;
                            }
                        }

                        Ok(Control::WaitForExit) => {
                            break;
                        }

                        Ok(Control::Kill) => {
                            child.kill().unwrap_or_default();
                            break;
                        }

                        Ok(otherwise) => {
                            dbg!(otherwise);
                            break;
                        }

                        Err(_error) => {
                            break;
                        }
                    }
                }

                if let Ok(status) = child.wait() {
                    match status.code() {
                        Some(code) => parent_tx
                            .send(Control::Exit(Exit::Code(code)))
                            .unwrap_or_default(),
                        None => parent_tx
                            .send(Control::Exit(Exit::Signal))
                            .unwrap_or_default(),
                    }
                }

                stdin.join().unwrap_or_default();
                stdout.join().unwrap_or_default();
                stderr.join().unwrap_or_default();
            }),
        })
    }
}

fn stdin_handler(
    control_tx: Sender<Control>,
    child: &mut Child,
) -> Option<(Sender<Control>, JoinHandle<()>)> {
    let (input_tx, input_rx) = channel::<Control>();

    let mut stdin = child.stdin.take()?;

    let sin = thread::spawn(move || loop {
        match input_rx.recv() {
            Ok(Control::Send(data)) => {
                if let Err(..) = stdin.write_all(data.as_bytes()) {
                    control_tx
                        .send(Control::Error(Stream::Input))
                        .unwrap_or_default();
                    break;
                }
            }

            Ok(Control::Close(Stream::Input)) => {
                control_tx
                    .send(Control::EndOfFile(Stream::Input))
                    .unwrap_or_default();
                break;
            }

            Ok(otherwise) => {
                dbg!(otherwise);
                break;
            }

            Err(_error) => {
                control_tx
                    .send(Control::Error(Stream::Input))
                    .unwrap_or_default();
                break;
            }
        }
    });

    Some((input_tx, sin))
}

fn stdout_handler(control_tx: Sender<Control>, child: &mut Child) -> Option<JoinHandle<()>> {
    let stdout = child.stdout.take()?;

    Some(thread::spawn(move || {
        let mut r = BufReader::new(stdout);

        loop {
            let mut buf = String::new();

            match r.read_line(&mut buf) {
                Ok(0) => {
                    control_tx
                        .send(Control::EndOfFile(Stream::Output))
                        .unwrap_or_default();
                    break;
                }

                Ok(..) => {
                    if let Err(..) = control_tx.send(Control::Received {
                        stream: Stream::Output,
                        data: buf,
                    }) {
                        break;
                    }
                }

                Err(error) => {
                    dbg!(error);
                    control_tx
                        .send(Control::Error(Stream::Output))
                        .unwrap_or_default();
                    break;
                }
            }
        }
    }))
}

fn stderr_handler(control_tx: Sender<Control>, child: &mut Child) -> Option<JoinHandle<()>> {
    let stderr = child.stderr.take()?;

    Some(thread::spawn(move || {
        let mut r = BufReader::new(stderr);

        loop {
            let mut buf = String::new();

            match r.read_line(&mut buf) {
                Ok(0) => {
                    control_tx
                        .send(Control::EndOfFile(Stream::Error))
                        .unwrap_or_default();
                    break;
                }

                Ok(..) => {
                    if let Err(..) = control_tx.send(Control::Received {
                        stream: Stream::Error,
                        data: buf,
                    }) {
                        break;
                    }
                }

                Err(..) => {
                    control_tx
                        .send(Control::Error(Stream::Error))
                        .unwrap_or_default();
                    break;
                }
            }
        }
    }))
}