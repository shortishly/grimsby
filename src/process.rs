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
    Send(Vec<u8>),
    /// An error indicator from the process
    Error(Stream),
    /// A notification that the process has exited with a status code or signal
    Exit(Exit),
    /// A notification that the process has sent data on the stdout or stderr streams
    Received { stream: Stream, data: Vec<u8> },
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
                            if stdin_tx.send(Control::Close(Stream::Input)).is_err() {
                                break;
                            }
                        }

                        Ok(Control::EndOfFile(stream)) => {
                            if parent_tx.send(Control::EndOfFile(stream)).is_err() {
                                break;
                            }
                        }

                        Ok(Control::Error(stream)) => {
                            if parent_tx.send(Control::Error(stream)).is_err() {
                                break;
                            }
                        }

                        Ok(Control::Received { stream, data }) => {
                            if parent_tx.send(Control::Received { stream, data }).is_err() {
                                break;
                            }
                        }

                        Ok(Control::Send(data)) => {
                            if stdin_tx.send(Control::Send(data)).is_err() {
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
                if let Ok(()) = stdin.write_all(data.as_slice()) {
                    if stdin.flush().is_err() {
                        control_tx
                            .send(Control::Error(Stream::Input))
                            .unwrap_or_default();
                        break;
                    }
                } else {
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
            match r.fill_buf() {
                Ok([]) => {
                    control_tx
                        .send(Control::EndOfFile(Stream::Output))
                        .unwrap_or_default();
                    break;
                }

                Ok(buffer) => {
                    let mut data = Vec::new();
                    data.extend_from_slice(buffer);
                    let amt = buffer.len();
                    r.consume(amt);

                    if control_tx
                        .send(Control::Received {
                            stream: Stream::Output,
                            data,
                        })
                        .is_err()
                    {
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
            match r.fill_buf() {
                Ok([]) => {
                    control_tx
                        .send(Control::EndOfFile(Stream::Error))
                        .unwrap_or_default();
                    break;
                }

                Ok(buffer) => {
                    let mut data = Vec::new();
                    data.extend_from_slice(buffer);
                    let amt = buffer.len();
                    r.consume(amt);

                    if control_tx
                        .send(Control::Received {
                            stream: Stream::Error,
                            data,
                        })
                        .is_err()
                    {
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
