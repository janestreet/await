module Sync = struct
  include Mutex_awaitable.Sync
  module Loc = Mutex_loc.Sync
end

module Await = struct
  include Mutex_awaitable.Await
  module Loc = Mutex_loc.Await
end
