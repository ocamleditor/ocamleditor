(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

  This file is part of OCamlEditor.

  OCamlEditor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  OCamlEditor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.

*)


open Miscellanea
open Printf

type verbosity = [
  | `DEBUG
  | `TRACE
  | `INFO
  | `WARN
  | `ERROR
  | `FATAL
  | `OFF
];;

let verbosity_of_string = function
  | "OFF"   -> `OFF
  | "FATAL" -> `FATAL
  | "ERROR" -> `ERROR
  | "WARN"  -> `WARN
  | "INFO"  -> `INFO
  | "DEBUG" -> `DEBUG
  | "TRACE" -> `TRACE
  | _       -> invalid_arg "verbosity_of_string"

let string_of_verbosity = function
  | `FATAL -> "FATAL"
  | `ERROR -> "ERROR"
  | `WARN  -> "WARN"
  | `INFO  -> "INFO"
  | `DEBUG -> "DEBUG"
  | `TRACE -> "TRACE"
  | `OFF   -> "OFF";;

let verbosities = List.mapi (fun i x -> x, i) [`DEBUG; `TRACE; `INFO; `WARN; `ERROR; `FATAL; `OFF]
let (>=) x y = List.assoc x verbosities >= List.assoc y verbosities

let timestamp () =
  let t = Unix.time () in
  let t = Unix.gmtime t in
  Printf.sprintf
    "%04d-%02d-%02dT%02d:%02d:%02d"
    (1900 + t.Unix.tm_year)
    (1 + t.Unix.tm_mon)
    t.Unix.tm_mday
    t.Unix.tm_hour
    t.Unix.tm_min
    t.Unix.tm_sec

module Make (X : sig
  val channel : out_channel
  val verbosity : verbosity
  val print_timestamp : bool
end) (*: S*) = struct

  module Make (Y : sig
    val prefix : string
  end) = struct

    let prefixes = ref []

    let _ =
      if List.mem Y.prefix !prefixes then invalid_arg ("Multiple log prefixes " ^ Y.prefix)
      else prefixes := Y.prefix :: !prefixes

    let verbosity = ref X.verbosity
    let set_verbosity x = verbosity := x

    let prefix = Some (Y.prefix ^ ": ")

    let log_formatter =
      Format.make_formatter
        (fun buf start len -> output X.channel buf start len)
        (fun () -> flush X.channel)

    let print level f =
      if level <> `OFF && level >= !verbosity then begin
        if X.print_timestamp then (Printf.fprintf X.channel "%s " (timestamp()));
        Printf.fprintf X.channel "[%s] " (string_of_verbosity level);
        Opt.may prefix (Printf.fprintf X.channel "%s");
        Printf.kfprintf flush X.channel f
      end else Printf.ifprintf X.channel f

    let println level f =
      if level <> `OFF && level >= !verbosity then begin
        if X.print_timestamp then (Printf.fprintf X.channel "%s " (timestamp()));
        Printf.fprintf X.channel "[%s] " (string_of_verbosity level);
        Opt.may prefix (fprintf X.channel "%s");
        Printf.kfprintf (function c -> Printf.fprintf c "\n%!") X.channel f
      end else Printf.ifprintf X.channel f

    let fprint level f =
      if level <> `OFF && level >= !verbosity then begin
        if X.print_timestamp then (Printf.kprintf (Format.pp_print_string log_formatter) "%s " (timestamp()));
        Printf.kprintf (Format.pp_print_string log_formatter) "[%s] " (string_of_verbosity level);
        Opt.may prefix (Format.pp_print_string log_formatter);
        Format.kfprintf (fun fmt -> Format.pp_print_flush fmt ()) log_formatter f
      end else Format.ifprintf Format.err_formatter f

    let fprintln level f =
      if level <> `OFF && level >= !verbosity then begin
        if X.print_timestamp then (Printf.kprintf (Format.pp_print_string log_formatter) "%s " (timestamp()));
        Printf.kprintf (Format.pp_print_string log_formatter) "[%s] " (string_of_verbosity level);
        Opt.may prefix (Format.pp_print_string log_formatter);
        Format.kfprintf (fun fmt -> Format.pp_print_newline fmt (); Format.pp_print_flush fmt ())
          log_formatter f
      end else Format.ifprintf Format.err_formatter f
  end
end
