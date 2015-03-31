/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(swish_trace, []).
:- use_module(library(pengines)).
:- use_module(library(http/term_html)).
:- use_module(library(http/html_write)).

/** <module>

Allow tracing pengine execution under SWISH.
*/

user:prolog_trace_interception(Port, Frame0, _CHP, Action) :-
	pengine_frame(Frame0, _Frame),
	prolog_frame_attribute(Frame0, goal, Goal),
	term_html(GoalString, Goal),
	functor(Port, PortName, _),
	pengine_input(_{type: trace,
			port: PortName,
			goal: GoalString
		       },
		      Action).

pengine_frame(Frame0, Frame) :-
	pengine_self(Me),
	parent_frame(Frame0, Frame),
	prolog_frame_attribute(Frame, predicate_indicator, Me:_Name/_Arity), !.

parent_frame(Frame, Frame).
parent_frame(Frame, Parent) :-
	prolog_frame_attribute(Frame, parent, Parent0),
	parent_frame(Parent0, Parent).

term_html(Term, HTMlString) :-
	pengine_self(Pengine),
	pengine_property(Pengine, module(Module)),
	phrase(html(\term(Term,
			  [ module(Module),
			    quoted(true)
			  ])), Tokens),
	with_output_to(string(HTMlString), print_html(Tokens)).
