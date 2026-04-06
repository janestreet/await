//Provides: await_blocking_futex
let await_blocking_futex = 0

//Provides: await_blocking_futex_get
function await_blocking_futex_get() {
  return 0;
}

//Provides: await_blocking_futex_count
//Requires: await_blocking_futex
function await_blocking_futex_count(_t) {
  return await_blocking_futex;
}

//Provides: await_blocking_futex_wait
//Requires: caml_raise_sys_error
//Requires: await_blocking_futex
function await_blocking_futex_wait(_t, count) {
  if (await_blocking_futex === count)
    caml_raise_sys_error("Futex.wait: cannot wait.");
  return await_blocking_futex;
}

//Provides: await_blocking_futex_signal
//Requires: await_blocking_futex
function await_blocking_futex_signal(_t) {
  // Keep the [await_blocking_futex] as a 32-bit integer to ensure that every
  // [await_blocking_futex_signal] call will make it unequal to previous value.
  await_blocking_futex = await_blocking_futex + 1 | 0;
  return 0;
}
