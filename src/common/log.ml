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
    [@ deprected]
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

    let log_formatter channel =
      Format.make_formatter
        (fun buf start len -> output_substring channel buf start len)
        (fun () -> flush channel)

    let print level f =
      if level <> `OFF && level >= !verbosity then begin
        let channel = if level >= `WARN then stderr else stdout in
        if X.print_timestamp then (Printf.fprintf channel "%s " (timestamp()));
        Printf.fprintf channel "[%s] " (string_of_verbosity level);
        Option.iter (Printf.fprintf channel "%s") prefix;
        Printf.kfprintf flush channel f
      end else Printf.ifprintf stdout f

    let println level f =
      if level <> `OFF && level >= !verbosity then begin
        let channel = if level >= `WARN then stderr else stdout in
        if X.print_timestamp then (Printf.fprintf channel "%s " (timestamp()));
        Printf.fprintf channel "[%s] " (string_of_verbosity level);
        Option.iter (fprintf channel "%s") prefix;
        Printf.kfprintf (function c -> Printf.fprintf c "\n%!") channel f
      end else Printf.ifprintf stdout f

    let fprint level f =
      if level <> `OFF && level >= !verbosity then begin
        let channel = if level >= `WARN then stderr else stdout in
        let fmtr = log_formatter channel in
        if X.print_timestamp then (Printf.kprintf (Format.pp_print_string fmtr) "%s " (timestamp()));
        Printf.kprintf (Format.pp_print_string fmtr) "[%s] " (string_of_verbosity level);
        Option.iter (Format.pp_print_string fmtr) prefix;
        Format.kfprintf (fun fmt -> Format.pp_print_flush fmt ()) fmtr f
      end else Format.ifprintf Format.err_formatter f

    let fprintln level f =
      if level <> `OFF && level >= !verbosity then begin
        let channel = if level >= `WARN then stderr else stdout in
        let fmtr = log_formatter channel in
        if X.print_timestamp then (Printf.kprintf (Format.pp_print_string fmtr) "%s " (timestamp()));
        Printf.kprintf (Format.pp_print_string fmtr) "[%s] " (string_of_verbosity level);
        Option.iter (Format.pp_print_string fmtr) prefix;
        Format.kfprintf (fun fmt -> Format.pp_print_newline fmt (); Format.pp_print_flush fmt ())
          fmtr f
      end else Format.ifprintf Format.err_formatter f
  end
end
