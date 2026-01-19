import argv
import gleam/erlang/process
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/string

// === Блок 1: Типы данных ===
pub type Point {
  Point(x: Float, y: Float)
}

pub type InterpolationMethod {
  Linear
  Newton(window_size: Int)
}

pub type Config {
  Config(method: InterpolationMethod, step: Float)
}

pub type Message {
  AddPoint(Point)
  Shutdown
}

pub type State {
  State(points: List(Point), config: Config, last_output_x: Option(Float))
}

// === Блок 2: Парсинг аргументов командной строки ===
pub fn parse(args: List(String)) -> Result(Config, String) {
  do_parse(args, None, None)
}

fn do_parse(
  args: List(String),
  method: Option(InterpolationMethod),
  step: Option(Float),
) -> Result(Config, String) {
  case args {
    [] -> {
      case method, step {
        Some(m), Some(s) -> Ok(Config(method: m, step: s))
        None, _ -> Error("Не указан метод интерполяции")
        _, None -> Error("Не указан параметр step")
      }
    }

    ["linear", ..rest] -> do_parse(rest, Some(Linear), step)

    ["newton", "n", n_str, ..rest] -> {
      case int.parse(n_str) {
        Ok(n) if n >= 2 -> do_parse(rest, Some(Newton(window_size: n)), step)
        Ok(_) -> Error("Размер окна для Newton должен быть >= 2")
        Error(_) -> Error("Неверное значение n: " <> n_str)
      }
    }

    ["step", step_str, ..rest] -> {
      case float.parse(step_str) {
        Ok(s) if s >. 0.0 -> do_parse(rest, method, Some(s))
        Ok(_) -> Error("Шаг должен быть > 0")
        Error(_) -> Error("Неверное значение step: " <> step_str)
      }
    }

    [unknown, ..rest] -> {
      io.println("Предупреждение: неизвестный параметр '" <> unknown <> "'")
      do_parse(rest, method, step)
    }
  }
}

pub fn method_name(method: InterpolationMethod) -> String {
  case method {
    Linear -> "Linear"
    Newton(n) -> "Newton (n=" <> int.to_string(n) <> ")"
  }
}

pub fn format_float(f: Float) -> String {
  let rounded = int.to_float(float.round(f *. 1_000_000.0)) /. 1_000_000.0
  float.to_string(rounded)
}

pub fn print_usage() {
  io.println(
    "
Использование:
  gleam run -- linear step 0.7
  gleam run -- newton n 4 step 0.5

Параметры:
  linear              Линейная интерполяция
  newton n <число>    Интерполяция Ньютона с окном из N точек
  step <число>        Шаг дискретизации (обязательно)
",
  )
}

// === Блок 3: Ввод данных с консоли ===
@external(erlang, "io", "get_line")
fn get_line(prompt: String) -> String

pub fn read_loop(actor_subject: process.Subject(Message)) {
  let line = get_line("> ")
  let input = string.trim(line)

  case input {
    "exit" -> {
      process.send(actor_subject, Shutdown)
      process.sleep(50)
    }
    "" -> read_loop(actor_subject)
    _ -> {
      case parse_point(input) {
        Ok(point) -> {
          process.send(actor_subject, AddPoint(point))
          process.sleep(10)
          read_loop(actor_subject)
        }
        Error(_) -> {
          io.println("Ошибка: неверный формат. Используйте: x y")
          read_loop(actor_subject)
        }
      }
    }
  }
}

fn parse_point(input: String) -> Result(Point, Nil) {
  case string.split(input, " ") {
    [x_str, y_str] -> {
      case parse_number(x_str), parse_number(y_str) {
        Ok(x), Ok(y) -> Ok(Point(x: x, y: y))
        _, _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

fn parse_number(s: String) -> Result(Float, Nil) {
  case float.parse(s) {
    Ok(f) -> Ok(f)
    Error(_) -> {
      case int.parse(s) {
        Ok(i) -> Ok(int.to_float(i))
        Error(_) -> Error(Nil)
      }
    }
  }
}

// === Блок 4: Линейная интерполяция ===
pub fn linear_interpolate(points: List(Point), x: Float) -> Result(Float, Nil) {
  case find_surrounding_points(points, x) {
    Ok(#(p1, p2)) -> {
      let y = p1.y +. { x -. p1.x } *. { p2.y -. p1.y } /. { p2.x -. p1.x }
      Ok(y)
    }
    Error(_) -> Error(Nil)
  }
}

fn find_surrounding_points(
  points: List(Point),
  x: Float,
) -> Result(#(Point, Point), Nil) {
  do_find_surrounding(points, x, None)
}

fn do_find_surrounding(
  points: List(Point),
  x: Float,
  prev: Option(Point),
) -> Result(#(Point, Point), Nil) {
  case points {
    [] -> Error(Nil)
    [current, ..rest] -> {
      case prev {
        None -> {
          case current.x <=. x {
            True -> do_find_surrounding(rest, x, Some(current))
            False -> Error(Nil)
          }
        }
        Some(p1) -> {
          case current.x >=. x {
            True -> Ok(#(p1, current))
            False -> do_find_surrounding(rest, x, Some(current))
          }
        }
      }
    }
  }
}

// === Блок 5: Интерполяция Ньютона ===
pub fn newton_interpolate(points: List(Point), x: Float) -> Result(Float, Nil) {
  case list.length(points) {
    n if n >= 2 -> Ok(compute(points, x))
    _ -> Error(Nil)
  }
}

fn compute(points: List(Point), x: Float) -> Float {
  compute_terms(points, points, x, 0.0, 1.0)
}

fn compute_terms(
  all_points: List(Point),
  remaining: List(Point),
  x: Float,
  acc: Float,
  basis: Float,
) -> Float {
  case remaining {
    [] -> acc
    [_, ..rest] -> {
      let order = list.length(all_points) - list.length(remaining)
      let dd = divided_difference(all_points, order)
      let new_acc = acc +. dd *. basis

      let new_basis = case remaining {
        [p, ..] -> basis *. { x -. p.x }
        [] -> basis
      }

      compute_terms(all_points, rest, x, new_acc, new_basis)
    }
  }
}

fn divided_difference(points: List(Point), order: Int) -> Float {
  case order {
    0 -> {
      case list.first(points) {
        Ok(p) -> p.y
        Error(_) -> 0.0
      }
    }
    1 -> {
      case points {
        [p0, p1, ..] -> { p1.y -. p0.y } /. { p1.x -. p0.x }
        _ -> 0.0
      }
    }
    _ -> recursive_divided_diff(points, order)
  }
}

fn recursive_divided_diff(points: List(Point), order: Int) -> Float {
  let left = divided_difference(list.drop(points, 1), order - 1)
  let right = divided_difference(points, order - 1)

  case list.first(points), get_nth(points, order) {
    Ok(p0), Ok(pn) -> { left -. right } /. { pn.x -. p0.x }
    _, _ -> 0.0
  }
}

fn get_nth(points: List(Point), n: Int) -> Result(Point, Nil) {
  case n {
    0 -> list.first(points)
    _ -> {
      case points {
        [] -> Error(Nil)
        [_, ..rest] -> get_nth(rest, n - 1)
      }
    }
  }
}

// === Блок 6: Вывод и обработка сообщений ===
pub fn start(
  config: Config,
) -> Result(process.Subject(Message), actor.StartError) {
  case
    actor.new(State(points: [], config: config, last_output_x: None))
    |> actor.on_message(handle_message)
    |> actor.start
  {
    Ok(act) -> Ok(act.data)
    Error(e) -> Error(e)
  }
}

fn handle_message(state: State, msg: Message) -> actor.Next(State, Message) {
  case msg {
    AddPoint(point) -> {
      let new_points = list.append(state.points, [point])

      let min_points = case state.config.method {
        Linear -> 2
        Newton(n) -> n
      }

      case list.length(new_points) >= min_points {
        True -> {
          let window = case state.config.method {
            Linear -> new_points
            Newton(n) -> take_last_n(new_points, n)
          }

          let new_last_x =
            interpolate_and_output(window, state.config, state.last_output_x)

          actor.continue(State(
            points: new_points,
            config: state.config,
            last_output_x: Some(new_last_x),
          ))
        }
        False -> actor.continue(State(..state, points: new_points))
      }
    }

    Shutdown -> {
      let min_points = case state.config.method {
        Linear -> 2
        Newton(n) -> n
      }

      case list.length(state.points) >= min_points, list.last(state.points) {
        True, Ok(last_point) -> {
          case state.last_output_x {
            Some(last_x) -> {
              let window = case state.config.method {
                Linear -> state.points
                Newton(n) -> take_last_n(state.points, n)
              }

              let _ =
                output_range(
                  window,
                  state.config,
                  last_x +. state.config.step,
                  last_point.x,
                )
              Nil
            }
            None -> Nil
          }
        }
        _, _ -> Nil
      }
      actor.stop()
    }
  }
}

fn take_last_n(list: List(a), n: Int) -> List(a) {
  let len = list.length(list)
  case len <= n {
    True -> list
    False -> list.drop(list, len - n)
  }
}

pub fn interpolate_and_output(
  points: List(Point),
  cfg: Config,
  last_output_x: Option(Float),
) -> Float {
  let assert Ok(first) = list.first(points)
  let assert Ok(last) = list.last(points)

  let start_x = case last_output_x {
    None -> first.x
    Some(x) -> x +. cfg.step
  }

  let end_x = case last_output_x {
    None -> last.x
    Some(x) -> x +. cfg.step
  }

  output_range(points, cfg, start_x, end_x)
}

pub fn output_range(
  points: List(Point),
  cfg: Config,
  start_x: Float,
  end_x: Float,
) -> Float {
  do_output_range(points, cfg, start_x, end_x, start_x)
}

fn do_output_range(
  points: List(Point),
  cfg: Config,
  current_x: Float,
  end_x: Float,
  last_x: Float,
) -> Float {
  case current_x <=. end_x {
    True -> {
      case interpolate(points, cfg.method, current_x) {
        Ok(y) -> {
          let method_prefix = case cfg.method {
            Linear -> "linear"
            Newton(_) -> "newton"
          }
          io.println(
            "> "
            <> method_prefix
            <> ": "
            <> format_float(current_x)
            <> " "
            <> format_float(y),
          )
          do_output_range(points, cfg, current_x +. cfg.step, end_x, current_x)
        }
        Error(_) -> last_x
      }
    }
    False -> last_x
  }
}

fn interpolate(
  points: List(Point),
  method: InterpolationMethod,
  x: Float,
) -> Result(Float, Nil) {
  case method {
    Linear -> linear_interpolate(points, x)
    Newton(_) -> newton_interpolate(points, x)
  }
}

// === Блок 7: Главная функция ===
pub fn main() {
  let args = argv.load().arguments

  case parse(args) {
    Ok(cfg) -> {
      io.println(
        "Потоковая интерполяция: "
        <> method_name(cfg.method)
        <> ", step="
        <> format_float(cfg.step),
      )
      io.println("Формат ввода: x y (или 'exit' для завершения)")

      let assert Ok(actor_subject) = start(cfg)

      read_loop(actor_subject)
    }
    Error(msg) -> {
      io.println("Ошибка: " <> msg)
      print_usage()
    }
  }
}
