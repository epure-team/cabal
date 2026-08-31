(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

type bounded = {deadline : Mtime.t; timeout : Eio.Time.Timeout.t}

type t = {now : unit -> Mtime.t; started_at : Mtime.t; bounded : bounded option}

type error = Invalid_timeout

let maximum_bounded_seconds = 9_000_000_000.0

let seconds_of_span span = Mtime.Span.to_float_ns span /. 1_000_000_000.0

let create clock seconds =
  match Float.classify_float seconds with
  | FP_nan -> Error Invalid_timeout
  | FP_infinite when seconds < 0.0 -> Error Invalid_timeout
  | FP_normal | FP_subnormal | FP_zero | FP_infinite ->
      if seconds < 0.0 then Error Invalid_timeout
      else
        let started_at = Eio.Time.Mono.now clock in
        let now () = Eio.Time.Mono.now clock in
        if seconds >= maximum_bounded_seconds then
          Ok {now; started_at; bounded = None}
        else
          let nanoseconds = Int64.of_float (seconds *. 1_000_000_000.0) in
          let span = Mtime.Span.of_uint64_ns nanoseconds in
          let deadline =
            match Mtime.add_span started_at span with
            | Some deadline -> deadline
            | None -> Mtime.max_stamp
          in
          Ok
            {
              now;
              started_at;
              bounded = Some {deadline; timeout = Eio.Time.Timeout.v clock span};
            }

let elapsed deadline =
  seconds_of_span (Mtime.span deadline.started_at (deadline.now ()))

let remaining deadline =
  match deadline.bounded with
  | None -> None
  | Some bounded ->
      let now = deadline.now () in
      if Mtime.compare now bounded.deadline >= 0 then Some 0.0
      else Some (seconds_of_span (Mtime.span now bounded.deadline))

let run deadline f =
  match deadline.bounded with
  | None -> `Completed (f ())
  | Some bounded -> (
      match Eio.Time.Timeout.run bounded.timeout (fun () -> Ok (f ())) with
      | Ok value -> `Completed value
      | Error `Timeout -> `Timeout)
