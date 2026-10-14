// ===== Готовый фрагмент: чтение CSV (скопируйте без изменений) =====
// readRows "data.csv" — читает файл рядом со скриптом,
// пропускает строку заголовков и пустые строки,
// возвращает список строк; каждая строка — массив ячеек.
let readRows (fileName: string) =
    let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, fileName)
    System.IO.File.ReadAllLines path
    |> Array.toList
    |> List.tail
    |> List.filter (fun line -> line.Trim() <> "")
    |> List.map (fun line ->
        line.Split ';' |> Array.map (fun cell -> cell.Trim()))

// Число с точкой или запятой: "3.5" и "3,5" -> 3.5
let toFloat (text: string) = float (text.Replace(',', '.'))

// Дата в виде "2026-10-05" или "05.10.2026" -> System.DateTime
let toDate (text: string) =
    System.DateTime.ParseExact(text, [| "yyyy-MM-dd"; "dd.MM.yyyy" |], null,
                               System.Globalization.DateTimeStyles.None)
// Дата для печати: showDate d -> "05.10.2026"
let showDate (date: System.DateTime) = date.ToString "dd.MM.yyyy"
// ===== Конец готового фрагмента =====

type Trip =
    { Date: System.DateTime
      Time: string
      Route: string
      Kind: string
      WaitMin: int
      RideMin: int
      Cost: int }

// Превращает массив ячеек одной строки CSV в запись Trip
let parseTrip (cells: string[]) =
    { Date = toDate cells[0]
      Time = cells[1]
      Route = cells[2]
      Kind = cells[3]
      WaitMin = int cells[4]
      RideMin = int cells[5]
      Cost = int cells[6] }

let trips = readRows "data.csv" |> List.map parseTrip

printfn $"Прочитано поездок: {List.length trips}"
