open Async.Std

open Sugar.Types

module MyError =
struct
  type t =
    | Resource_not_found
    | Unexpected of string
end

module MyResult = Sugar_async.MakeResult(MyError)

open MyError
open MyResult

 let logger =
  Log.create `Info [Log.Output.stdout ()] `Raise

let flush_logger () : unit result Deferred.t  =
  Log.flushed logger
  >>= fun () ->
  commit ()

let print_message m: unit result Deferred.t =
  after (Core.Time.Span.of_sec (Random.float 3.))
  >>= fun _ ->
  Log.info logger "%s" m;
  flush_logger () />
  commit

let load_list n: int list result Deferred.t =
  let l = [1; 2; 3] in
  let new_list = List.map (fun v -> v * n) l in
  commit new_list

let computation_failed _length: 'a promise =
  throw Resource_not_found

let error_handler e: string promise =
  match e with
  | Resource_not_found -> commit "recovered failure"
  | _ -> throw e

let main_handler (): unit result Deferred.t =
  print_message "1 - Concurrent threads" //>
  print_message "2 - Concurrent threads" //>
  print_message "3 - Concurrent threads" //>
  print_message "4 - Concurrent threads" //>
  load_list 10
  &&| List.length
  &&= computation_failed
  ||= error_handler
  &&= fun _ ->
  let message = "You can nearly do anything you want here." in
  print_message message
  &&= flush_logger


let _ =
  Random.self_init ();
  let _ = Deferred.bind (main_handler ()) (fun _ -> exit 0) in
  Core.Std.never_returns (Scheduler.go ())
