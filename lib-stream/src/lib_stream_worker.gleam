import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import lib_dataflow as dataflow
import lib_error
import lib_stream_sink as sink
import lib_stream_source as source
import lib_time

pub type Worker

pub opaque type Client {
  Client(
    worker: Worker,
    context: dataflow.Context,
    next_connection_id: dataflow.Mutation(Int),
    pending: dataflow.Mutation(List(Pending)),
  )
}

pub opaque type Server {
  Server(
    state_context: dataflow.Context,
    time: lib_time.Context,
    runtime: RuntimeServer,
    services: dataflow.Mutation(List(ServiceSource)),
  )
}

pub type RuntimeServer

type Message

type Pending {
  Awaiting(id: Int, output: sink.WritableStream(String))
  Resolved(id: Int)
}

type Request {
  Request(connection_id: Int, input: source.ReadableStream(String))
}

type ServiceSource {
  ServiceSource(name: String, requests: source.Source(Request))
}

@external(javascript, "./worker_ffi.mjs", "new_worker")
pub fn new_worker(url: String) -> Worker

@external(javascript, "./worker_ffi.mjs", "new_server")
fn new_server() -> RuntimeServer

@external(javascript, "./worker_ffi.mjs", "client_connect")
fn client_connect(
  worker: Worker,
  service: String,
  connection_id: Int,
  input: source.ReadableStream(String),
) -> Nil

@external(javascript, "./worker_ffi.mjs", "client_listen")
fn client_listen(worker: Worker, on_message: fn(Message) -> Nil) -> Nil

@external(javascript, "./worker_ffi.mjs", "server_listen")
fn server_listen(server: RuntimeServer, on_message: fn(Message) -> Nil) -> Nil

@external(javascript, "./worker_ffi.mjs", "message_type")
fn message_type(message: Message) -> String

@external(javascript, "./worker_ffi.mjs", "message_connection_id")
fn message_connection_id(message: Message) -> Int

@external(javascript, "./worker_ffi.mjs", "message_server_stream")
fn message_server_stream(message: Message) -> source.ReadableStream(String)

@external(javascript, "./worker_ffi.mjs", "message_service")
fn message_service(message: Message) -> String

@external(javascript, "./worker_ffi.mjs", "message_client_stream")
fn message_client_stream(message: Message) -> source.ReadableStream(String)

@external(javascript, "./worker_ffi.mjs", "pipe")
fn pipe(
  readable: source.ReadableStream(value),
  writable: sink.WritableStream(value),
) -> Nil

@external(javascript, "./worker_ffi.mjs", "server_connect")
fn server_connect(
  server: RuntimeServer,
  connection_id: Int,
  output: source.ReadableStream(String),
  input: source.ReadableStream(String),
  output_sink: sink.WritableStream(String),
) -> Nil

pub fn client(worker: Worker) -> Client {
  let context = dataflow.dataflow()
  let client =
    Client(
      worker: worker,
      context: context,
      next_connection_id: dataflow.state(0),
      pending: dataflow.state([]),
    )
  client_listen(worker, fn(message) {
    case message_type(message) == "WorkerConnectionResponse" {
      True ->
        receive_response(
          client,
          message_connection_id(message),
          message_server_stream(message),
        )
      False -> Nil
    }
  })
  client
}

pub fn connect(
  client: Client,
  service: String,
  on_data: fn(String) -> Nil,
) -> source.Source(String) {
  let input = source.source()
  let output = sink.sink(on_data)
  let connection_id = begin_connection(client, sink.writable(output))
  let Client(worker, ..) = client
  client_connect(worker, service, connection_id, source.readable(input))
  input
}

pub fn connect_async(
  client: Client,
  service: String,
  on_data: sink.AsyncHandler(String, reason),
) -> source.Source(String) {
  let input = source.source()
  let output = sink.async_sink(on_data)
  let connection_id = begin_connection(client, sink.writable(output))
  let Client(worker, ..) = client
  client_connect(worker, service, connection_id, source.readable(input))
  input
}

pub fn server(context: lib_time.Context) -> Server {
  let state_context = dataflow.dataflow()
  let runtime = new_server()
  let server =
    Server(
      state_context: state_context,
      time: context,
      runtime: runtime,
      services: dataflow.state([]),
    )
  server_listen(runtime, fn(message) {
    case message_type(message) == "WorkerConnectionRequest" {
      True ->
        dispatch_request(
          server,
          message_connection_id(message),
          message_service(message),
          message_client_stream(message),
        )
      False -> Nil
    }
  })
  server
}

pub fn serve(
  server: Server,
  service: String,
  on_connect: fn(source.Source(String)) -> fn(String) -> Nil,
) -> Nil {
  register_sync_service(server, service, on_connect)
}

pub fn serve_async(
  server: Server,
  service: String,
  on_connect: fn(source.Source(String)) ->
    sink.AsyncHandler(String, source.SendError),
) -> Nil {
  register_async_data_service(server, service, on_connect)
}

/// Register a service whose connection setup is asynchronous but whose data
/// handler is synchronous. This is the explicit Gleam form of the source
/// `Promise<{ onData(data): void }>` callback.
pub fn serve_connect_async(
  server: Server,
  service: String,
  on_connect: fn(source.Source(String)) ->
    lib_error.Async(fn(String) -> Nil, reason),
) -> Nil {
  register_async_connect_service(server, service, on_connect)
}

/// Register a service with both asynchronous connection setup and data
/// handling. The source combines these two callback return unions; Gleam
/// keeps both asynchronous boundaries visible in the function type.
pub fn serve_connect_async_data_async(
  server: Server,
  service: String,
  on_connect: fn(source.Source(String)) ->
    lib_error.Async(sink.AsyncHandler(String, data_reason), connect_reason),
) -> Nil {
  register_async_connect_data_service(server, service, on_connect)
}

fn begin_connection(
  client: Client,
  output: sink.WritableStream(String),
) -> Int {
  let Client(_, context, next_connection_id, pending) = client
  let connection_id =
    dataflow.value(
      dataflow.to_computation(dataflow.mutation(next_connection_id)),
    )
  let current_pending =
    dataflow.value(dataflow.to_computation(dataflow.mutation(pending)))
  let assert Ok(Nil) =
    dataflow.txn(context, fn() {
      case dataflow.set(context, next_connection_id, connection_id + 1) {
        Error(reason) -> Error(reason)
        Ok(Nil) ->
          dataflow.set(
            context,
            pending,
            list.append(current_pending, [Awaiting(connection_id, output)]),
          )
      }
    })
  connection_id
}

fn receive_response(
  client: Client,
  connection_id: Int,
  readable: source.ReadableStream(String),
) -> Nil {
  let Client(_, context, _, pending) = client
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(pending)))
  case take_pending(current, connection_id, []) {
    Error(Nil) -> {
      let message = "Invalid connection ID " <> int.to_string(connection_id)
      panic as message
    }
    Ok(#(None, _)) -> Nil
    Ok(#(Some(output), remaining)) -> {
      let assert Ok(Nil) =
        dataflow.txn(context, fn() { dataflow.set(context, pending, remaining) })
      pipe(readable, output)
    }
  }
}

fn take_pending(
  pending: List(Pending),
  connection_id: Int,
  before: List(Pending),
) -> Result(#(Option(sink.WritableStream(String)), List(Pending)), Nil) {
  case pending {
    [] -> Error(Nil)
    [Awaiting(id, output), ..rest] ->
      case id == connection_id {
        True ->
          Ok(#(
            Some(output),
            list.append(before, [Resolved(connection_id), ..rest]),
          ))
        False ->
          take_pending(
            rest,
            connection_id,
            list.append(before, [Awaiting(id, output)]),
          )
      }
    [Resolved(id), ..rest] ->
      case id == connection_id {
        True -> Ok(#(None, list.append(before, [Resolved(id), ..rest])))
        False ->
          take_pending(rest, connection_id, list.append(before, [Resolved(id)]))
      }
  }
}

fn register_sync_service(
  server: Server,
  name: String,
  on_connect: fn(source.Source(String)) -> fn(String) -> Nil,
) -> Nil {
  let requests = service_source(server, name)
  let requests_sink =
    sink.sink(fn(request) { handle_sync_request(server, request, on_connect) })
  pipe(source.readable(requests), sink.writable(requests_sink))
}

fn register_async_data_service(
  server: Server,
  name: String,
  on_connect: fn(source.Source(String)) -> sink.AsyncHandler(String, reason),
) -> Nil {
  let requests = service_source(server, name)
  let requests_sink =
    sink.async_sink(fn(request) {
      fn(done) {
        handle_async_data_request(server, request, on_connect)
        done(Ok(Nil))
      }
    })
  pipe(source.readable(requests), sink.writable(requests_sink))
}

fn register_async_connect_service(
  server: Server,
  name: String,
  on_connect: fn(source.Source(String)) ->
    lib_error.Async(fn(String) -> Nil, reason),
) -> Nil {
  let requests = service_source(server, name)
  let requests_sink =
    sink.async_sink(fn(request) {
      fn(done) {
        handle_async_connect_request(server, request, on_connect, done)
      }
    })
  pipe(source.readable(requests), sink.writable(requests_sink))
}

fn register_async_connect_data_service(
  server: Server,
  name: String,
  on_connect: fn(source.Source(String)) ->
    lib_error.Async(sink.AsyncHandler(String, data_reason), connect_reason),
) -> Nil {
  let requests = service_source(server, name)
  let requests_sink =
    sink.async_sink(fn(request) {
      fn(done) {
        handle_async_connect_data_request(server, request, on_connect, done)
      }
    })
  pipe(source.readable(requests), sink.writable(requests_sink))
}

fn service_source(server: Server, name: String) -> source.Source(Request) {
  let Server(state_context, _, _, services) = server
  let current =
    dataflow.value(dataflow.to_computation(dataflow.mutation(services)))
  case find_service_source(current, name) {
    Some(requests) -> requests
    None -> {
      let requests = source.source()
      let assert Ok(Nil) =
        dataflow.txn(state_context, fn() {
          dataflow.set(state_context, services, [
            ServiceSource(name: name, requests: requests),
            ..current
          ])
        })
      requests
    }
  }
}

fn dispatch_request(
  server: Server,
  connection_id: Int,
  name: String,
  input: source.ReadableStream(String),
) -> Nil {
  let Server(_, time, _, _) = server
  let requests = service_source(server, name)
  source.send(
    time,
    requests,
    Request(connection_id: connection_id, input: input),
    fn(result) {
      case result {
        Ok(Nil) -> Nil
        Error(_) -> panic as "Worker connection request send failed"
      }
    },
  )
}

fn handle_sync_request(
  server: Server,
  request: Request,
  on_connect: fn(source.Source(String)) -> fn(String) -> Nil,
) -> Nil {
  let Request(connection_id, input) = request
  let output = source.source()
  let output_sink = sink.sink(on_connect(output))
  connect_request(
    server,
    connection_id,
    input,
    output,
    sink.writable(output_sink),
  )
}

fn handle_async_data_request(
  server: Server,
  request: Request,
  on_connect: fn(source.Source(String)) -> sink.AsyncHandler(String, reason),
) -> Nil {
  let Request(connection_id, input) = request
  let output = source.source()
  let output_sink = sink.async_sink(on_connect(output))
  connect_request(
    server,
    connection_id,
    input,
    output,
    sink.writable(output_sink),
  )
}

fn handle_async_connect_request(
  server: Server,
  request: Request,
  on_connect: fn(source.Source(String)) ->
    lib_error.Async(fn(String) -> Nil, reason),
  done: fn(Result(Nil, reason)) -> Nil,
) -> Nil {
  let Request(connection_id, input) = request
  let output = source.source()
  on_connect(output)(fn(result) {
    case result {
      Error(reason) -> done(Error(reason))
      Ok(on_data) -> {
        let output_sink = sink.sink(on_data)
        connect_request(
          server,
          connection_id,
          input,
          output,
          sink.writable(output_sink),
        )
        done(Ok(Nil))
      }
    }
  })
}

fn handle_async_connect_data_request(
  server: Server,
  request: Request,
  on_connect: fn(source.Source(String)) ->
    lib_error.Async(sink.AsyncHandler(String, data_reason), connect_reason),
  done: fn(Result(Nil, connect_reason)) -> Nil,
) -> Nil {
  let Request(connection_id, input) = request
  let output = source.source()
  on_connect(output)(fn(result) {
    case result {
      Error(reason) -> done(Error(reason))
      Ok(on_data) -> {
        let output_sink = sink.async_sink(on_data)
        connect_request(
          server,
          connection_id,
          input,
          output,
          sink.writable(output_sink),
        )
        done(Ok(Nil))
      }
    }
  })
}

fn connect_request(
  server: Server,
  connection_id: Int,
  input: source.ReadableStream(String),
  output: source.Source(String),
  output_sink: sink.WritableStream(String),
) -> Nil {
  let Server(_, _, runtime, _) = server
  server_connect(
    runtime,
    connection_id,
    source.readable(output),
    input,
    output_sink,
  )
}

fn find_service_source(
  services: List(ServiceSource),
  name: String,
) -> Option(source.Source(Request)) {
  case services {
    [] -> None
    [ServiceSource(service_name, requests), ..rest] ->
      case service_name == name {
        True -> Some(requests)
        False -> find_service_source(rest, name)
      }
  }
}
