## Zig port of a project of mine with the same name. This is meant as a more
## simple-to-compile alternative since it's covered in
## dotf.common.compile-scripts.
##
## The usage of sockets here inspired me: https://github.com/c-blake/kslog

import std/[posix, strutils, strformat, options, envvars, tables, cmdline]

type
  Addr[Backing] = object
    data: Backing
    len: SockLen
  ServerState = object
    data: TableRef[string, string]
  ClientState = object
    sock: SocketHandle

proc stringFromBuf(buf: pointer, length: cuint): string =
  var ret = newString(length)
  for i in 0..<length:
    ret[i] = cast[ptr UncheckedArray[char]](buf)[i]
  return ret

template getOrElse[T](exp: Option[T], orElse: untyped): T =
  # TODO: try using this lol
  block:
    let res = exp
    if res.isNone():
      orElse
    else:
      res.get()

proc die(msg: string, code: int = 1): void {.noReturn.} =
  stderr.writeLine(msg)
  quit(code)

proc handleResult(result: int, allowed: seq[cint] = @[]): void =
  if result < 0 and errno notin allowed:
    die(fmt"error while receiving ({errno}): {strerror(errno)}")

proc makePathAddress(path: string): Addr[Sockaddr_un] =
  var uds: Sockaddr_un
  if path.len > uds.sun_path.len - 1:
    die(fmt"Path too long for UDS: {path} (length {path.len}, but limit is {uds.sun_path.len - 1})")

  uds.sun_family = AF_UNIX.TSa_Family
  copyMem(uds.sun_path[0].addr, path[0].unsafeAddr, path.len + 1)
  return Addr[Sockaddr_un](data: uds, len: uds.sizeof.SockLen)

proc newUnixServerSocket(path: string): SocketHandle =
  let ret = socket(AF_UNIX, SOCK_STREAM, 0)
  let xaddr = makePathAddress(path)
  discard unlink(path) # delete socket file if it exists
  bindSocket(ret, cast[ptr SockAddr](xaddr.data.addr), xaddr.len).handleResult()
  ret.listen(0).handleResult()
  return ret

proc newUnixClientSocket(path: string): SocketHandle =
  let ret = socket(AF_UNIX, SOCK_STREAM, 0)
  let xaddr = makePathAddress(path)
  connect(ret, cast[ptr SockAddr](xaddr.data.addr), xaddr.len).handleResult()
  return ret

proc accept(sock: SocketHandle): Option[(SocketHandle, Addr[SockAddr])] =
  var sa: SockAddr
  var sl: SockLen
  let sock2 = sock.accept(sa.addr, sl.addr)
  if sock2.int < 0:
    handleResult(-1, @[ECONNRESET])
    return none((SocketHandle, Addr[SockAddr]))
  some((sock2, Addr[SockAddr](data: sa, len: sl)))

proc recv(sock: SocketHandle): Option[string] =
  var buf = newSeq[char](4096)
  let n = sock.recv(buf[0].addr.pointer, buf.len, 0.cint)
  if n < 0:
    handleResult(n)
    none(string)
  elif n == 0:
    none(string)
  else:
    some(stringFromBuf(buf[0].addr, n.cuint))

proc send(sock: SocketHandle, data: string): void =
  if data.len == 0:
    return
  sock.send(data[0].addr.pointer, data.len, 0.cint).handleResult()

proc parseMsg(msg: string, a0: var string, a1: var string): bool =
  let s = msg.split(":", maxsplit = 1)
  if s.len != 2:
    false
  else:
    a0 = s[0]
    a1 = s[1]
    true

proc serverMain(sock: SocketHandle): void =
  var ss = ServerState(data: newTable[string, string]())

  while (let r0 = sock.accept(); r0.isSome):
    let (csock, _) = r0.get()
    stderr.writeLine("Estabilished one connection")

    while (let r1 = csock.recv(); r1.isSome):
      var keepRunning = true

      for msgRaw in r1.get().split("\n"):
        let msg = msgRaw.strip()

        var a0: string
        var a1: string
        if msg.len == 0:
          discard
        elif msg.startsWith("get:"):
          if not parseMsg(msg, a0, a1):
            continue
          if ss.data.contains(a1):
            csock.send("ok:" & ss.data[a1] & "\n")
          else:
            csock.send("err:unknown entry\n")
        elif msg.startsWith("set:"):
          if not parseMsg(msg, a0, a1):
            continue
          let args = a1.split(":", maxsplit = 1)
          ss.data[args[0]] = args[1]
          csock.send("ok:\n")
        elif msg == "end":
          keepRunning = false
          break
        else:
          csock.send("err:unknown command\n")

      if not keepRunning:
        break

proc sendRecv(c: var ClientState, toSend: string, hadError: var bool): void =
  let toSend = toSend.strip()
  if toSend.len == 0:
    return

  # stderr.writeLine(fmt"Sending: {toSend}")
  c.sock.send(toSend)
  let msg = c.sock.recv().get()
  # stderr.writeLine(fmt"Response: {msg}")

  if msg.startsWith("err:"):
    hadError = true

  if toSend.startsWith("get:"):
    var a0: string
    var a1: string
    if not parseMsg(msg, a0, a1) or a0 == "err":
      stderr.write(msg)
    else:
      stdout.write(a1)
  elif toSend.startsWith("set:"):
    if not msg.startsWith("ok:"):
      stderr.write(msg)
  else:
    stderr.writeLine(fmt"warning: unknown response: {msg}")

proc clientMain(c: var ClientState, toSend: seq[string], hadError: var bool): void =
  for x in toSend:
    c.sendRecv(x, hadError)

proc showHelp(): void =
  let message = """
Usage: dotcfg { daemon | send [MESSAGES...] | stdin-send | help }

COMMANDS

  daemon: starts the daemon, taking into account the DOTCFG_SOCKET path.

  send: send one message per argument

  stdin-send: same as send, but instead of one command per argument it is one
  command per line

  help: show this message

MESSAGES

  To communicate with the daemon, the client uses messages, which are one-line strings with commands. They can
  be of the following types:

    set:<KEY>:<VALUE> to set a property
    get:<KEY> to get a property's value

    Characters not allowed for the key: ':', '\n'
    Characters not allowed for the value: '\n'

  Upon dealing with these commands, you can receive responses. The response for successful operations is
  simply a line containing the option's value.

  Error responses start with 'err:'.

  After outputting all responses, if at least one of them is an error, the program returns 1. If an invalid
  response is detected, though, the program exits 1 immediately.
"""

  stderr.writeLine(message)
  quit(2)

var isServer = false
var toSend: seq[string] = @[]
let args = commandLineParams()

if args.len == 0: showHelp()

if args[0] == "daemon":
  if args.len != 1: showHelp()
  isServer = true
elif args[0] == "send":
  if args.len <= 1: showHelp()
  toSend = args[1..^1]
elif args[0] == "stdin-send":
  if args.len != 1: showHelp()
  toSend = stdin.readAll().split("\n")
elif args[0] == "help":
  showHelp()
else:
  stderr.writeLine(fmt"error: unknown command {args[0]}")
  showHelp()

let tmpFolder = getEnv("TMPDIR", "/tmp")
let socketPath = getEnv("DOTCFG_SOCKET", fmt"{tmpFolder}/dotcfg.default.sock")

if isServer:
  stderr.writeLine(fmt"ARE WE CONNECTED? {socketPath}")
  let sock = newUnixServerSocket(socketPath)
  stderr.writeLine("EXCELLENT.")

  serverMain(sock)
  discard unlink(socketPath.cstring)
else:
  let sock = newUnixClientSocket(socketPath)
  var hadError = false

  var c = ClientState(sock: sock)
  c.clientMain(toSend, hadError)
  discard sock.close()
  quit(if hadError: 1 else: 0)
