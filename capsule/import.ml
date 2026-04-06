include Portable_kernel
include Await_kernel

module Await = struct
  include Await_kernel.Await
  include Await_sync.Await
end

module Sync = struct
  include Await_kernel.Sync
  include Await_sync.Sync
end
