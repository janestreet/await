(module
   (import "env" "caml_raise_sys_error" (func $caml_raise_sys_error (param (ref eq))))

   (type $bytes (array (mut i8)))

   (@string $wait_failure "Futex.wait: cannot wait.")

   (global $await_blocking_futex (mut i32)
     (i32.const 0))

   (func (export "await_blocking_futex_get")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "await_blocking_futex_count")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (global.get $await_blocking_futex)))

   (func (export "await_blocking_futex_wait")
      (param (ref eq)) (param $count (ref eq)) (result (ref eq))
      (if (ref.eq (local.get $count) (ref.i31 (global.get $await_blocking_futex)))
        (then (call $caml_raise_sys_error (global.get $wait_failure))))
      (ref.i31 (global.get $await_blocking_futex)))

   (func (export "await_blocking_futex_signal")
      (param (ref eq)) (result (ref eq))
      (global.set $await_blocking_futex (i32.add (i32.const 1) (global.get $await_blocking_futex)))
      (ref.i31 (i32.const 0)))
)
