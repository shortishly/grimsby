#-*- mode: makefile-gmake -*-
# Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
PROJECT = grimsby
PROJECT_DESCRIPTION = Erlang/Rust Port Manager
PROJECT_VERSION = ${shell git describe --tags}

DEPS = \
	envy

SHELL_OPTS = \
	-config dev.config \
	-s $(PROJECT) \
	-s sync \
	+pc unicode

SHELL_DEPS = \
	sync

PLT_APPS = \
	any \
	asn1 \
	compiler \
	crypto \
	inets \
	mnesia \
	public_key \
	runtime_tools \
	ssl \
	stdlib \
	syntax_tools \
	tools \
	xmerl

dep_envy = git https://github.com/shortishly/envy.git
dep_envy_commit = 0.7.2

include erlang.mk

.PHONY: priv/grimsby

priv/grimsby:
	cargo build --verbose --config net.git-fetch-with-cli=true
	cp -p target/debug/grimsby priv

app:: priv/grimsby
