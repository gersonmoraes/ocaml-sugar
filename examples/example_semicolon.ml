open Lwt
open Sugar.Types

module MyResult = Sugar_lwt.MakeResult(struct type error = unit end)

open MyResult
open Lwt

let puts str () =
  Lwt_unix.sleep (Random.float 0.1)
  >>= fun () ->
  Lwt_log.notice str
  >>= commit

let puts_concurrent str =
  puts str ()

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
  puts "Blocking example: " ()  />
  blocking                      />
  puts "Non-blocking example: " />
  nonblocking


let _ =
  Random.self_init ();
  Lwt_main.run (main)
