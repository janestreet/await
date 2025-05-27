open! Base

let with_await terminator ~f =
  let await () trigger =
    while not (Await.Trigger.is_signalled trigger) do
      Domain.cpu_relax ()
    done
  in
  let await = Await.create terminator ~await () in
  f await [@nontail]
;;
