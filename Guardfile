require "socket"

guard :shell do
  watch %r<(snaplets|src|static/css)/.*> do
    system "pkill -HUP -f src/Main.hs"
    wait_for_server_killed 8000
    Process.spawn "rh -Wall src/Main.hs"
  end
end

def wait_for_server_killed(port)
  loop do
    begin
      TCPServer.new(8000).close
      break
    rescue Errno::EADDRINUSE
      sleep 0.1
    end
  end
end
