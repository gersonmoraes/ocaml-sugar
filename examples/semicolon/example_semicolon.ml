open Lwt
open Sugar.Types

(* Example of basic usage of Sugar to build an Error Handling Layer
 *
 * It uses a parametrized module to build sugar syntatic and useful functions
 * that only make sense in your project.
 *
 * We're hinting the functions result to make clear what's going on. But you
 * can effectively remove them, because we're also using a concise result type
 * for computations: ('a result).
 *)
 module MyError =
 struct
   type error = unit
 end
module MyResult = Sugar_lwt.Result.Make(MyError)

(* Start using them *)
open MyResult
open Lwt

let puts str () =
  Lwt_unix.sleep (Random.float 0.1)
  >>= fun () ->
  Lwt_log.notice str
  >>= commit

let puts_concurrent str =
  Lwt_unix.sleep (Random.float 2.)
  >>= fun () ->
  Lwt_log.notice str
  >>= commit

(* TODO: We're creating a blocking operator for semicolon,
 *       with operator (//>). Later on, will invert the meaning of (/>) and (//>)
 *)
let blocking () =
  puts "1 - Hello" ()     />
  puts "2 - World"        />
  puts "3 - Of"           />
  puts "4 - Blocking"     />
  puts "5 - Computations"

let nonblocking () =
  puts_concurrent "1 - Hello"         //>
  puts_concurrent "2 - World"         //>
  puts_concurrent "3 - Of"            //>
  puts_concurrent "4 - Non-Blocking"  //>
  puts_concurrent "5 - Computations"


let main =
  puts "Blocking example: " () />
  blocking />
  puts "Non-blocking example: " />
  nonblocking


let _ =
  Random.self_init ();
  Lwt_main.run (main)
